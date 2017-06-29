#ifndef SAFEIO_H
#define SAFEIO_H

typedef const char* safeio_string;

typedef enum {
  permission_denied = 0,
  io_error,
  not_found,
  bad_path,
  other_error
}  safeio_error;

void
can_stat (safeio_string path);

void
return_can_stat_ok ();

void
return_can_stat_error (safeio_error);

void
get_filetype (safeio_string path);

typedef enum {
  regular_file = 0,
  directory,
  other_filetype
}  safeio_filetype;

void
return_get_filetype_ok (safeio_filetype);

void
return_get_filetype_error (safeio_error);


#endif
