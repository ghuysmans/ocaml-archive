(** Low level function to libarchive

    This is the low level function which are almost a 1:1 mapping of libarchive
    functions.

    Callbacks should only raise an {!AFailure} exception, this way they will
    trigger an [archive_set_error]. Any other exceptions will be mapped to a
    generic exception when setting error.

    @author Sylvain Le Gall *)

type 'a archive
type filename = string
type error_code = int

(* archive_read_open2 open callback *)
type ('a, 'b) open_callback = 'a -> 'b

(* archive_read_open2 read callback *)
type 'a read_callback = 'a -> bytes -> int

(* archive_read_open2 skip callback *)
type 'a skip_callback = 'a -> int -> int

(* archive_read_open2 close callback *)
type 'a close_callback = 'a -> unit

type 'a seek_callback = 'a -> int -> Unix.seek_command -> int

exception AEnd_of_file
exception AFailure of error_code * string

external init : unit -> unit = "caml_archive_init"

let is_inited = ref false

let init () =
  if not !is_inited then (
    Callback.register_exception "archive eof" AEnd_of_file;
    Callback.register_exception "archive.failure" (AFailure (0, "foo"));
    init ();
    is_inited := true)

let () = init ()

module Entry = struct
  type t

  external create : unit -> t = "caml_archive_entry_create"
  external clone : t -> t = "caml_archive_entry_clone"
  external pathname : t -> filename = "caml_archive_entry_pathname"
  external set_pathname : t -> filename -> unit = "caml_archive_entry_set_pathname"
  external stat : t -> Unix.LargeFile.stats = "caml_archive_entry_stat"
  external set_perm : t -> Unix.file_perm -> unit = "caml_archive_entry_set_perm"
  external set_filetype : t -> Unix.file_kind -> unit = "caml_archive_entry_set_filetype" (* FIXME portable consts *)
  external set_atime : t -> float -> unit = "caml_archive_entry_set_atime"
  external set_mtime : t -> float -> unit = "caml_archive_entry_set_mtime"
  external set_ctime : t -> float -> unit = "caml_archive_entry_set_ctime"
  external unset_atime : t -> float -> unit = "caml_archive_entry_unset_atime"
  external unset_mtime : t -> float -> unit = "caml_archive_entry_unset_mtime"
  external unset_ctime : t -> float -> unit = "caml_archive_entry_unset_ctime"
  external set_size : t -> int64 -> unit = "caml_archive_entry_set_size"
end

module Read = struct
  type t = [ `Read ] archive

  external create : unit -> t = "caml_archive_read_create"
  (** archive_read_new *)

  external support_filter_all : t -> unit
    = "caml_archive_read_support_filter_all"
  (** archive_read_support_filter_all *)

  external support_format_all : t -> unit
    = "caml_archive_read_support_format_all"
  (** archive_read_support_compress_all *)

  external open_filename : t -> filename -> int -> unit
    = "caml_archive_read_open_filename"
  (** archive_read_open_filename *)

  module Unsafe = struct
    external set_seek_callback : t -> _ seek_callback -> unit
      = "caml_archive_read_set_seek_callback"
    (** archive_read_set_seek_callback *)
  end

  external open2 :
    t ->
    ('a, 'b) open_callback ->
    'b read_callback ->
    'b skip_callback ->
    'b close_callback ->
    'a ->
    unit = "caml_archive_read_open2_bytecode" "caml_archive_read_open2_native"
  (** archive_read_open2 *)

  external next_header2 : t -> Entry.t -> bool
    = "caml_archive_read_next_header2"

  external data_skip : t -> unit = "caml_archive_read_data_skip"
  external data : t -> string -> int -> int -> int = "caml_archive_read_data"
  external close : t -> unit = "caml_archive_read_close"
end

module Write = struct
  type t = [ `Write ] archive

  external create : unit -> t = "caml_archive_write_create"
  (** archive_write_new *)

  external add_filter_none : t -> unit
    = "caml_archive_write_add_filter_none"
  (** archive_write_add_filter_none *)

  external set_format_zip : t -> unit
    = "caml_archive_write_set_format_zip"
  (** archive_write_set_format_zip *)

  external zip_set_compression_store : t -> unit
    = "caml_archive_write_zip_set_compression_store"
  (** archive_write_zip_set_compression_store *)

  external zip_set_compression_deflate : t -> unit
    = "caml_archive_write_zip_set_compression_deflate"
  (** archive_write_zip_set_compression_deflate *)

  external open_filename : t -> filename -> unit
    = "caml_archive_write_open_filename"
  (** archive_write_open_filename *)

  external header : t -> Entry.t -> unit = "caml_archive_write_header"
  external data : t -> string -> int -> int -> int = "caml_archive_write_data"
  external close : t -> unit = "caml_archive_write_close"
end
