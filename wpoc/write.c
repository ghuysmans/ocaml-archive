#include <stdio.h>
#include <string.h>
#include <archive.h>
#include <archive_entry.h>

int main(int argc, char *argv[]) {
	struct archive *a = archive_write_new();
	archive_write_add_filter_none(a);
	archive_write_set_format_zip(a);
	//archive_write_zip_set_compression_store(a);
	archive_write_zip_set_compression_deflate(a);
	archive_write_open_filename(a, "out.zip");
	struct archive_entry *e = archive_entry_new();
	archive_entry_set_perm(e, 0666);
	archive_entry_set_filetype(e, AE_IFREG); //portable constant
	archive_entry_set_pathname(e, "README");
	archive_entry_set_mtime(e, 2000 /* epoch */, 0 /* ns */);
	//or archive_entry_unset_mtime(e);
	const char *data = "hello world\nthis time, it's actually compressed.\n";
	const int ct = 100;
	archive_entry_set_size(e, ct*strlen(data));
	archive_write_header(a, e);
	for (int i=0; i<ct; i++)
		archive_write_data(a, data, strlen(data));
	archive_entry_free(e);
	archive_write_free(a);
	return 0;
}
