open OUnit
open Lwt

let ls_alR arch is_dir readdir =
  let rec ls_alR' acc dn =
    Array.fold_left
      (fun acc bn ->
        let fn = FilePath.concat dn bn in
        if is_dir arch fn then ls_alR' acc fn else fn :: acc)
      acc (readdir arch dn)
  in
  List.sort String.compare (ls_alR' [] "")

let read_tarball arch dump_fn =
  let lst = ls_alR arch Archive.Read.is_directory Archive.Read.readdir in
  let dump = Archive.Read.content arch dump_fn in
  (lst, dump)

let read_tarball_lwt input dump_fn =
  ArchiveLwt.Read.create input >>= fun arch ->
  let lst = ls_alR arch ArchiveLwt.Read.is_directory ArchiveLwt.Read.readdir in
  ArchiveLwt.Read.content arch dump_fn >|= fun ctnt -> (lst, ctnt)

module ListString = OUnitDiff.ListSimpleMake (struct
  type t = string

  let compare = String.compare
  let pp_printer = Format.pp_print_string
  let pp_print_sep = OUnitDiff.pp_comma_separator
end)

let open_unix fn =
  let fd = Unix.openfile fn [Unix.O_RDONLY] 0 in
  let read _ buf = Unix.read fd buf 0 (Bytes.length buf) in
  let skip _ request =
    ignore (Unix.lseek fd request Unix.SEEK_CUR);
    request
  in
  let a = ArchiveLow.Read.create () in
  ArchiveLow.Read.support_filter_all a;
  ArchiveLow.Read.support_format_all a;
  ArchiveLow.Read.set_seek_callback a (Unix.lseek fd);
  ArchiveLow.Read.open2 a Fun.id read skip Unix.close fd;
  a

let ([] | _ :: _) =
  run_test_tt_main
    ("ocaml-archive"
    >::: [
           ( "Simple" >:: fun () ->
             let _, dump =
               read_tarball
                 (Archive.Read.create
                    (`Filename "data/ocaml-data-notation-0.0.6.tar.gz"))
                 "ocaml-data-notation-0.0.6/_oasis"
             in
             assert_equal ~msg:"Digest of _oasis"
               ~printer:(fun s -> s)
               "c9b290271ca1da665261520256a8a7a1"
               (Digest.to_hex (Digest.string dump)) );
           ( "BadFile" >:: fun () ->
             let msg =
               try
                 let _arch =
                   Archive.Read.create (`Filename "data/Makefile.bz2")
                 in
                 assert_failure "Expecting to fail"
               with ArchiveLow.AFailure (_, msg) -> msg
             in
             assert_equal ~msg:"Exception message"
               ~printer:(fun s -> s)
               "Unrecognized archive format" msg );
           ( "read_open2" >:: fun () ->
             let exp_lst, exp_dump =
               read_tarball
                 (Archive.Read.create
                    (`Filename "data/ocaml-data-notation-0.0.6.tar.gz"))
                 "ocaml-data-notation-0.0.6/_oasis"
             in
             let lst, dump =
               read_tarball
                 (Archive.Read.create
                    (`Callback
                       ( "data/ocaml-data-notation-0.0.6.tar.gz",
                         (* Open callback *)
                         (fun fn -> open_in fn),
                         (* Read callback *)
                         (fun chn buf -> input chn buf 0 (Bytes.length buf)),
                         (* Skip callback *)
                         (fun chn off ->
                           let start = pos_in chn in
                           seek_in chn (start + off);
                           pos_in chn - start),
                         (* Close callback *)
                         fun chn -> close_in chn )))
                 "ocaml-data-notation-0.0.6/_oasis"
             in
             ListString.assert_equal ~msg:"directory listing" exp_lst lst;
             ListString.assert_equal ~msg:"_oasis content"
               (ExtLib.String.nsplit exp_dump "\n")
               (ExtLib.String.nsplit dump "\n") );
           ( "lwt" >:: fun () ->
             let exp_lst, exp_dump =
               read_tarball
                 (Archive.Read.create
                    (`Filename "data/ocaml-data-notation-0.0.6.tar.gz"))
                 "ocaml-data-notation-0.0.6/_oasis"
             in
             let lst, dump =
               Lwt_main.run
                 (read_tarball_lwt
                    (`Filename "data/ocaml-data-notation-0.0.6.tar.gz")
                    "ocaml-data-notation-0.0.6/_oasis")
             in
             ListString.assert_equal ~msg:"directory listing" exp_lst lst;
             ListString.assert_equal ~msg:"_oasis content"
               (ExtLib.String.nsplit exp_dump "\n")
               (ExtLib.String.nsplit dump "\n") );
         ])

let () = (* FIXME understand why it gets stuck when run by OUnit2 *)
             let exp_lst, exp_dump =
               read_tarball
                 (Archive.Read.create
                    (`Filename "data/ocaml-data-notation-0.0.6.tar.gz"))
                 "ocaml-data-notation-0.0.6/_oasis"
             in
             let a = open_unix "data/ocaml-data-notation-0.0.6.tar.gz" in
             let lst, dump =
               let e = ArchiveLow.Entry.create () in
               let target = "ocaml-data-notation-0.0.6/_oasis" in
               let rec f lst dump =
                 if ArchiveLow.Read.next_header2 a e then
                   let name = ArchiveLow.Entry.pathname e in
                   f (if String.ends_with ~suffix:"/" name then lst else name :: lst)
                     (if name = target then
                       let size = 2048 in
                       let buf = String.make size '\000' in
                       let rd = ArchiveLow.Read.data a buf 0 size in
                       String.sub buf 0 rd
                     else
                       dump)
                 else
                   List.sort compare lst, dump
               in
               f [] ""
             in
             ListString.assert_equal ~msg:"directory listing" exp_lst lst;
             ListString.assert_equal ~msg:"_oasis content"
               (ExtLib.String.nsplit exp_dump "\n")
               (ExtLib.String.nsplit dump "\n")
