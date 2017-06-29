#include <sys/types.h>
#include <sys/stat.h>
#include <fcntl.h>
#include <errno.h>

#include "safeio.h"

static safeio_error
safeio_error_from_errno ()
{
    switch (errno) {
    case EACCES:
    case EPERM:
      return (permission_denied);
    case EIO:
      return (io_error);
    case ENOENT:
      return (not_found);
    case EBADF:
      return (bad_path);
    default:
      return (other_error);
    }
}

void
can_stat (safeio_string p)
{
  struct stat buffer;
  if (stat (p, &buffer) < 0) {
    return_can_stat_error (safeio_error_from_errno());
  } else {
    return_can_stat_ok ();
  }
}

void
get_filetype (safeio_string p)
{
  struct stat buffer;
  if (stat (p, &buffer) < 0) {
    return_get_filetype_error (safeio_error_from_errno());
  } else {
    return_get_filetype_ok (
        S_ISREG(buffer.st_mode) ?
         regular_file
      : S_ISDIR(buffer.st_mode) ?
         directory
      :
         other_filetype);
  }
}
