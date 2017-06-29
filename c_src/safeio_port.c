#include "safeio.h"

extern int sedge_main_loop ();

int
main (int  argc,
      char *argv[])
{
  int error_code = 0;
  error_code = sedge_main_loop ();
  return error_code;
}
