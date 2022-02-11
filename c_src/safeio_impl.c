/* BEGINNING OF GENERATED C-SOURCE FOR THIS INPUT FILE: */
#include "safeio.h"



/* System includes needed by sedge */

#include "ei.h"

#include <execinfo.h>
#include <stdint.h>
#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>
#include <stdarg.h>

#include <netinet/in.h>
#include <pthread.h>
/*
   NOTE: for the stack traces to work, pass '-rdynamic' to the linker (gcc).

 */

/** The output file for tracing. If NULL tracing is deactivated */
static FILE *sedge_trace_file = NULL;

/** The mutex protecting the output file for tracing. */
static pthread_mutex_t sedge_trace_file_mutex = PTHREAD_MUTEX_INITIALIZER;

/** If tracing is active, fetch the trace file mutex, and return 0
    otherwise return 1.
  */
int sedge_trace_begin() {
  if (sedge_trace_file) {
    if (pthread_mutex_lock(&sedge_trace_file_mutex)) {
      fprintf(stderr,
              "ERROR: sedge_trace_begin(): failed to lock mutex: %s.\n",
              strerror(errno));
      return 1;
    }
    return 0;
  }
  return 1;
}

/** Flush the output and release the trace file mutex.
  */
void sedge_trace_commit() {
  if (sedge_trace_file) {
    fflush(sedge_trace_file);
  }
  if (pthread_mutex_unlock(&sedge_trace_file_mutex)) {
    fprintf(stderr,
            "ERROR: sedge_trace_commit(): failed to unlock mutex: %s.\n",
            strerror(errno));
  }
}

/** Write a stacktrace to stderr and to the trace file. */
void sedge_print_stack_trace () {
  void *array[50];
  size_t size;
  char **strings;
  size_t i;
  size = backtrace(array, 50);
  strings = backtrace_symbols(array, size);

  if (!sedge_trace_begin()) {
    fprintf(sedge_trace_file, "\n* STACKTRACE: *\n");
    for (i = 1; i < size; i++)
      fprintf(sedge_trace_file, "%s\n", strings[i]);
    fprintf(sedge_trace_file, "* END OF STACKTRACE *\n");
    sedge_trace_commit();
  }

  fprintf(stderr, "\n* STACKTRACE: *");
  for (i = 1; i < size; i++) fprintf(stderr, "%s", strings[i]);
  fprintf(stderr, "* END OF STACKTRACE *\n");
  free(strings);
 }

/* Port IO Customizable compile time options: */

#ifndef SEDGE_PORT_BUF_SIZE
#define SEDGE_PORT_BUF_SIZE 65534
#endif //SEDGE_PORT_BUF_SIZE

#ifndef SEDGE_PORT_PACKET_LENGTH
#define SEDGE_PORT_PACKET_LENGTH 2
#endif //SEDGE_PORT_PACKET_LENGTH

#ifndef SEDGE_PORT_FD_INPUT
#define SEDGE_PORT_FD_INPUT 3
#endif //SEDGE_PORT_FD_INPUT

#ifndef SEDGE_PORT_FD_OUTPUT
#define SEDGE_PORT_FD_OUTPUT 4
#endif //SEDGE_PORT_FD_OUTPUT

#if SEDGE_PORT_PACKET_LENGTH == 4
#define SEDGE_PL_NTOH ntohl
#define SEDGE_PL_HTON htonl
#elif SEDGE_PORT_PACKET_LENGTH == 2
#define SEDGE_PL_NTOH ntohs
#define SEDGE_PL_HTON htons
#elif SEDGE_PORT_PACKET_LENGTH == 1
#define SEDGE_PL_NTOH
#define SEDGE_PL_HTON
#endif // SEDGE_PORT_PACKET_LENGTH


/* Port IO globals and constants: */
static pthread_mutex_t sedge_port_write_mutex = PTHREAD_MUTEX_INITIALIZER;

static char sedge_port_write_buffer[SEDGE_PORT_BUF_SIZE + 1];
static int sedge_port_write_index = 0;

static const char *SEDGE_RETURN_TAG = "return";
static const char *SEDGE_OK_TAG = "ok";
static const char *SEDGE_ERROR_TAG = "error";
static const char *SEDGE_EVENT_TAG = "event";



/* ************************* DECODING *************************** */

void sedge_decode_version(char *buf, int *index) {
  int _Ignored;
  if (0 != ei_decode_version(buf, index, &_Ignored)) {
    sedge_print_stack_trace();
    fprintf(stderr, "ERROR: ei_decode_version failed.\n");
    exit(1);
  }
}

char sedge_decode_char(char *buf, int *index) {
  char Result;
  int SafeIndex = *index;
  if (0 == ei_decode_char(buf, &SafeIndex, &Result)) {
    *index = SafeIndex;
    return Result;
  }
  sedge_print_stack_trace();
  fprintf(stderr, "ERROR: 'char' expected.\n");
  exit(1);
}

unsigned long sedge_decode_ulong(char *buf, int *index) {
  unsigned long LongRes;
  int SafeIndex = *index;
  if (0 == ei_decode_ulong(buf, &SafeIndex, &LongRes)) {
    *index = SafeIndex;
    return LongRes;
  }
  sedge_print_stack_trace();
  fprintf(stderr, "ERROR: 'unsigned long' expected.\n");
  exit(1);
}

long int sedge_decode_long(char *buf, int *index) {
  long LongRes;
  int SafeIndex = *index;
  if (0 == ei_decode_long(buf, &SafeIndex, &LongRes)) {
    *index = SafeIndex;
    return LongRes;
  }
  sedge_print_stack_trace();
  fprintf(stderr, "ERROR: 'signed long' expected.\n");
  exit(1);
}

unsigned long long sedge_decode_ulonglong(char *buf, int *index) {
  unsigned long long LongLongRes;
  int SafeIndex = *index;
  if (0 == ei_decode_ulonglong(buf, &SafeIndex, &LongLongRes)) {
    *index = SafeIndex;
    return LongLongRes;
  }
  sedge_print_stack_trace();
  fprintf(stderr, "ERROR: 'unsigned long long' expected.\n");
  exit(1);
}

long long sedge_decode_longlong(char *buf, int *index) {
  long long LongLongRes;
  int SafeIndex = *index;
  if (0 == ei_decode_longlong(buf, &SafeIndex, &LongLongRes)) {
    *index = SafeIndex;
    return LongLongRes;
  }
  sedge_print_stack_trace();
  fprintf(stderr, "ERROR: 'signed long long' expected.\n");
  exit(1);
}

uint8_t sedge_decode_uint8_t(char *buf, int *index) {
  return (uint8_t)sedge_decode_char(buf, index);
}

int8_t sedge_decode_int8_t(char *buf, int *index) {
  return (int8_t)sedge_decode_char(buf, index);
}

uint16_t sedge_decode_uint16_t(char *buf, int *index) {
  return (uint16_t) sedge_decode_ulong(buf, index);
}

int16_t sedge_decode_int16_t(char *buf, int *index) {
  return (int16_t) sedge_decode_long(buf, index);
}

uint32_t sedge_decode_uint32_t(char *buf, int *index) {
  return (uint32_t) sedge_decode_ulong(buf, index);
}

size_t sedge_decode_size_t(char *buf, int *index) {
  return sedge_decode_ulong(buf, index);
}

int32_t sedge_decode_int32_t(char *buf, int *index) {
  return (int32_t) sedge_decode_long(buf, index);
}

uint64_t sedge_decode_uint64_t(char *buf, int *index) {
  return (uint64_t) sedge_decode_ulonglong(buf, index);
}

int64_t sedge_decode_int64_t(char *buf, int *index) {
  return (int64_t) sedge_decode_longlong(buf, index);
}

int sedge_decode_size(char *buf, int *index) {
  int Type; int Len = 0;
  if (0 != ei_get_type (buf, index, &Type, &Len)) {
    sedge_print_stack_trace ();
    fprintf(stderr, "ERROR: ei_get_type failed.\n");
    exit(1);
  }
  return Len;
}

char *sedge_alloc(size_t Size) {
  char *Result = malloc (Size);
  if ( ! Result) {
    sedge_print_stack_trace ();
    fprintf(stderr, "ERROR: cannot allocate string buffer.\n");
    exit(1);
  }
  memset (Result, 0, Size);
  return Result;
}

char *sedge_decode_string(char *buf, int *index) {
  long Size = sedge_decode_size(buf, index);
  char *Result = sedge_alloc(Size + 1);

  if (0 == ei_decode_string(buf, index, Result)) {
    return Result;
  }
  int Arity = 0;
  if (0 == ei_decode_list_header(buf, index, &Arity)) {
    if (Arity != 0) {
      sedge_print_stack_trace();
      fprintf(stderr, "ERROR: Decoding of strings as lists not supported.\n");
      exit(1);
    }
    return Result;
  }
  sedge_print_stack_trace ();
  fprintf(stderr, "ERROR: string expected.\n");
  exit(1);
}

char *sedge_decode_atom(char *buf, int *index) {
  long Size = sedge_decode_size(buf, index);
  char *Result = sedge_alloc(Size + 1);
  if (0 != ei_decode_atom(buf, index, Result)) {
    sedge_print_stack_trace();
    fprintf(stderr, "ERROR: ei_decode_atom failed.");
    exit(1);
  }
  return Result;
}

void sedge_decode_binary(char *buf, int *index, void **Result, long *ResultLen) {
  *ResultLen = sedge_decode_size(buf, index);
  *Result = sedge_alloc(*ResultLen);
  if (0 != ei_decode_binary(buf, index, *Result, ResultLen)) {
    sedge_print_stack_trace();
    fprintf(stderr, "ERROR: ei_decode_binary failed.\n");
    exit(1);
  }
}

void sedge_decode_tuple_header_raw(char *buf, int *index,
                                   int ExpectedArity) {
  int Arity = -1;
  if (0 != ei_decode_tuple_header(buf, index, &Arity)) {
    sedge_print_stack_trace();
    fprintf(stderr, "ERROR: ei_decode_tuple_header_raw failed.\n");
    exit(1);
  }
  else if (Arity != ExpectedArity) {
    sedge_print_stack_trace();
    fprintf(stderr,
            "ERROR: ei_decode_tuple_header_raw failed: Invalid arity: %i. Expected: %i.\n",
            Arity, ExpectedArity);
    exit(1);
  }
}

void sedge_decode_tuple_header(char *buf, int *index,
                               int ExpectedArity,
                               char *ExpectedTag) {
  int Arity = -1;
  char Tag[MAXATOMLEN + 1];
  if (0 != ei_decode_tuple_header(buf, index, &Arity)) {
    sedge_print_stack_trace();
    fprintf(stderr,
            "ERROR: ei_decode_tuple_header failed. Expected struct %s/%i.\n",
            ExpectedTag, ExpectedArity);
    exit(1);
  }
  else if (Arity != ExpectedArity + 1) {
    sedge_print_stack_trace();
    fprintf(stderr,
            "ERROR: Invalid arity: %i. Expected struct type: %s/%i.\n",
            Arity, ExpectedTag, ExpectedArity);
    exit(1);
  }
  else if (ei_decode_atom(buf, index, Tag)) {
    sedge_print_stack_trace();
    fprintf(stderr,
            "ERROR: ei_decode_atom for tag atom \"%s\" failed. Expected struct %s/%i.\n",
            Tag, ExpectedTag, ExpectedArity);
    exit(1);
  }
  else if (strncmp(Tag, ExpectedTag, MAXATOMLEN)) {
    sedge_print_stack_trace();
    fprintf(stderr,
            "ERROR: Invalid tag: %s. Expected struct: %s/%i.\n",
            Tag, ExpectedTag, ExpectedArity);
    exit(1);
  }
}

int sedge_decode_list_header(char *buf, int *index) {
  int Arity = -1;
  if (0 != ei_decode_list_header(buf, index, &Arity)) {
    sedge_print_stack_trace();
    fprintf(stderr, "ERROR: ei_decode_list_header failed.\n");
    exit(1);
  }
  return Arity;
}

void sedge_decode_empty_list(char *buf, int *index) {
  int Arity = -1;
  if (0 != ei_decode_list_header(buf, index, &Arity)) {
    sedge_print_stack_trace();
    fprintf(stderr, "ERROR: ei_decode_list_header failed.\n");
    exit(1);
  }
  if (Arity != 0) {
    sedge_print_stack_trace();
    fprintf(stderr, "ERROR: expected empty list, actual arity: %i.\n", Arity);
    exit(1);
  }
}

/* **************** Encoding ****************** */

void sedge_encode_version(char *buf, int *index) {
  if (0 != ei_encode_version(buf, index)) {
    sedge_print_stack_trace();
    fprintf(stderr, "ERROR: ei_encode_version failed.\n");
    exit(1);
  }
}

void sedge_encode_tuple_header(char *buf, int *index, int arity) {
  if (0 != ei_encode_tuple_header(buf, index, arity)) {
    sedge_print_stack_trace();
    fprintf(stderr, "ERROR: ei_encode_tuple_header failed.\n");
    exit(1);
  }
}

void sedge_encode_list_header(char *buf, int *index, int arity) {
  if (0 != ei_encode_list_header(buf, index, arity)) {
    sedge_print_stack_trace();
    fprintf(stderr, "ERROR: ei_encode_list_header failed.\n");
    exit(1);
  }
}

void sedge_encode_binary(char *buf, int *index, const void *p, long len) {
  if (0 != ei_encode_binary(buf, index, p, len)) {
    sedge_print_stack_trace();
    fprintf(stderr, "ERROR: ei_encode_binary failed.\n");
    exit(1);
  }
}

void sedge_encode_empty_list(char *buf, int *index) {
  if (0 != ei_encode_empty_list(buf, index)) {
    sedge_print_stack_trace();
    fprintf(stderr, "ERROR: ei_encode_empty_list failed.\n");
    exit(1);
  }
}

void sedge_encode_atom(char *buf, int *index, const char *p) {
  if (0 != ei_encode_atom(buf, index, p)) {
    sedge_print_stack_trace();
    fprintf(stderr, "ERROR: ei_encode_atom failed.\n");
    exit(1);
  }
}

void sedge_encode_string(char *buf, int *index, const char *str) {
  if (0 != ei_encode_string(buf, index, str)) {
    sedge_print_stack_trace();
    fprintf(stderr, "ERROR: ei_encode_string failed.\n");
    exit(1);
  }
}

void sedge_encode_uint8_t(char *buf, int *index, uint8_t i) {
  if (0 != ei_encode_char(buf, index, (char)i)) {
    sedge_print_stack_trace();
    fprintf(stderr, "ERROR: ei_encode_char (from uint8_t) failed.\n");
    exit(1);
  }
}

void sedge_encode_int8_t(char *buf, int *index, int8_t i) {
  if (0 != ei_encode_char(buf, index, (char)i)) {
    sedge_print_stack_trace();
    fprintf(stderr, "ERROR: ei_encode_char (from int8_t) failed.\n");
    exit(1);
  }
}

void sedge_encode_uint16_t(char *buf, int *index, uint16_t i) {
  if (0 != ei_encode_ulong(buf, index, (unsigned long)i)) {
    sedge_print_stack_trace();
    fprintf(stderr, "ERROR: ei_encode_ulong (from uint16_t) failed.\n");
    exit(1);
  }
}

void sedge_encode_int16_t(char *buf, int *index, int16_t i) {
  if (0 != ei_encode_long(buf, index, (long)i)) {
    sedge_print_stack_trace();
    fprintf(stderr, "ERROR: ei_encode_long (from int16_t) failed.\n");
    exit(1);
  }
}

void sedge_encode_uint32_t(char *buf, int *index, uint32_t i) {
  if (0 != ei_encode_ulong(buf, index, (unsigned long)i)) {
    sedge_print_stack_trace();
    fprintf(stderr, "ERROR: ei_encode_ulong (from uint32_t) failed.\n");
    exit(1);
  }
}

void sedge_encode_int32_t(char *buf, int *index, int32_t i) {
  if (0 != ei_encode_long(buf, index, (long)i)) {
    sedge_print_stack_trace();
    fprintf(stderr, "ERROR: ei_encode_long (from int32_t) failed.\n");
    exit(1);
  }
}

void sedge_encode_uint64_t(char *buf, int *index, uint64_t i) {
  if (0 != ei_encode_ulonglong(buf, index, (unsigned long long)i)) {
    sedge_print_stack_trace();
    fprintf(stderr, "ERROR: ei_encode_ulong (from uint64_t) failed.\n");
    exit(1);
  }
}

void sedge_encode_int64_t(char *buf, int *index, int64_t i) {
  if (0 != ei_encode_longlong(buf, index, (long long)i)) {
    sedge_print_stack_trace();
    fprintf(stderr, "ERROR: ei_encode_longlong (from int64_t) failed.\n");
    exit(1);
  }
}

void sedge_encode_unsigned(char *buf, int *index, uint64_t i) {
  if (0 != ei_encode_ulonglong(buf, index, (unsigned long long)i)) {
    sedge_print_stack_trace();
    fprintf(stderr, "ERROR: ei_encode_ulong (from uint64_t) failed.\n");
    exit(1);
  }
}

void sedge_encode_signed(char *buf, int *index, int64_t i) {
  if (0 != ei_encode_longlong(buf, index, (long long)i)) {
    sedge_print_stack_trace();
    fprintf(stderr, "ERROR: ei_encode_longlong (from int64_t) failed.\n");
    exit(1);
  }
}
void sedge_encode_size_t(char *buf, int *index, size_t i) {
  if (0 != ei_encode_ulong(buf, index, i)) {
    sedge_print_stack_trace();
    fprintf(stderr, "ERROR: ei_encode_ulong (from size_t) failed.\n");
    exit(1);
  }
}

/* *** Section: encoding function prototypes *** */
/* End of section: encoding function prototypes */


/* *** Section: custom decoding function prototypes *** */
/* End of section: custom decoding function prototypes */


/* Port IO low-level read and write: */

/*
   Read len bytes from SEDGE_PORT_FD_INPUT into 'buf' which must be allocated to
   fit 'len' bytes. Return 0 on success and 1 on error.
 */
int sedge_port_read_exact(char *buf, size_t len)
{
  int i;
  size_t got = 0;

  do {
    if ((i = read(SEDGE_PORT_FD_INPUT, buf + got, len - got)) <= 0)
      return 1;
    got += i;
  } while (got < len);

  return 0;
}

/*
   Write len bytes to SEDGE_PORT_FD_OUTPUT from 'buf'. Return 0 on success and 1
   on error.
 */
int sedge_port_write_exact(char *buf, size_t len)
{
  int i;
  size_t wrote = 0;

  do {
    if ((i = write(SEDGE_PORT_FD_OUTPUT, buf + wrote, len - wrote)) <= 0)
      return 1;
    wrote += i;
  } while (wrote < len);

  return 0;
}

/*
   Read a binary and check the size, on error return 1 otherwise 0.  The integer
   pointed to by size is updated to reflect the size of the received data.  The
   buf pointer is expected to be allocated large enough.  The initial value of
   'size' must contain the size of the pre-allocated buffer.
 */
int sedge_port_read(char *buf, size_t *size)
{
#if SEDGE_PORT_PACKET_LENGTH == 4
  uint32_t payload_size;
#elif SEDGE_PORT_PACKET_LENGTH == 2
  uint16_t payload_size;
#elif SEDGE_PORT_PACKET_LENGTH == 1
  uint8_t payload_size;
#endif // SEDGE_PORT_PACKET_LENGTH

  if (sedge_port_read_exact((char*)&payload_size, SEDGE_PORT_PACKET_LENGTH)) {
    sedge_print_stack_trace();
    fprintf(stderr, "sedge_port_read: Failed to read the next payload size.\n");
    return 1;
  }

  payload_size = SEDGE_PL_NTOH(payload_size);

  if (payload_size >= SEDGE_PORT_BUF_SIZE) {
    sedge_print_stack_trace();
    fprintf(stderr,
            "read_binary: Buffersize exceeded. Requested: %i. "
            "Available per compile time define SEDGE_PORT_BUF_SIZE: %i.\n",
            payload_size, SEDGE_PORT_BUF_SIZE);
    return 1;
  }

  if (sedge_port_read_exact(buf, payload_size)) {
    sedge_print_stack_trace();
    fprintf(stderr,
            "read_binary: Failed to read %i bytes.\n",
            payload_size);
    return 1;
  }

  // set the size output parameter:

  (*size) = (int)payload_size;

  return 0;
}

/*
   Write a binary to SEDGE_PORT_FD_OUTPUT and return 0 on success, 1 on failure.
 */
int sedge_port_write(char *buf, size_t size)
{
  // write the size
#if SEDGE_PORT_PACKET_LENGTH == 4
  uint32_t payload_size = (uint32_t)size;
#elif SEDGE_PORT_PACKET_LENGTH == 2
  uint16_t payload_size = (uint16_t)size;
#elif SEDGE_PORT_PACKET_LENGTH == 1
  uint8_t payload_size = (uint8_t)size;
#endif // SEDGE_PORT_PACKET_LENGTH
  payload_size = SEDGE_PL_HTON(payload_size);
  if (sedge_port_write_exact((char*)&payload_size, SEDGE_PORT_PACKET_LENGTH)) {
    sedge_print_stack_trace();
    fprintf(stderr,
            "ERROR: write_binary: Failed to "
            "write the size of the next payload to send: %i.\n",
            SEDGE_PL_NTOH(payload_size));
    exit(1);
  }

  // write the payload
  if (sedge_port_write_exact(buf, size)) {
    sedge_print_stack_trace();
    fprintf(stderr,
            "ERROR: write_binary: Failed to write "
            "the actual payload, with a length of: %i.\n",
            SEDGE_PL_NTOH(payload_size));
    exit(1);
  }

  return 0;
}

/* Port IO high-level reading and writing*/

void
sedge_port_write_begin() {
  if (pthread_mutex_lock(&sedge_port_write_mutex)) {
    fprintf(stderr,
            "ERROR: sedge_write_lock: failed to lock mutex: %s.\n",
            strerror(errno));
    exit(1);
  }
  if (sedge_port_write_index != 0) {
    sedge_print_stack_trace();
    fprintf(stderr,
            "ERROR: sedge_write_begin(): Inconsistent state detected: "
            "sedge_port_write_index must be 0 at the begining of a "
            "write operation, instead it was: %i.\n",
            sedge_port_write_index);
    exit(1);
  }
  sedge_encode_version(sedge_port_write_buffer, &sedge_port_write_index);
}


/* send the data to the erlang process and release the mutex */
void
sedge_port_write_commit() {

  /* Trace the term send to the port */
  if (!sedge_trace_begin()) {
    fprintf(sedge_trace_file, "* About to send:\n");
    int local_index = 1;
    ei_print_term(sedge_trace_file,
                  sedge_port_write_buffer,
                  &local_index);
    fprintf(sedge_trace_file, "\n");
    sedge_trace_commit();
  }
  sedge_port_write(sedge_port_write_buffer, sedge_port_write_index);
  sedge_port_write_index = 0;
  if (pthread_mutex_unlock(&sedge_port_write_mutex)) {
    fprintf(stderr,
            "ERROR: sedge_write_lock: failed to unlock mutex: %s.\n",
            strerror(errno));
    return;
  }
}

/* encode {event, {Tag, Arg0, Arg1, ...}} */
void sedge_port_encode_event_header(const char *Tag,
                                    size_t PayloadArity) {
  sedge_encode_tuple_header(sedge_port_write_buffer,
                            &sedge_port_write_index, 2);
  sedge_encode_atom(sedge_port_write_buffer,
                    &sedge_port_write_index, SEDGE_EVENT_TAG);
  sedge_encode_tuple_header(sedge_port_write_buffer,
                            &sedge_port_write_index,
                            1 + PayloadArity);
  sedge_encode_atom(sedge_port_write_buffer,
                    &sedge_port_write_index, Tag);
}

/* encode {return, Tag, {ok/error, Arg_0, Arg_1, ... , Arg_PayloadArity}} */
void sedge_port_encode_return_header(const char *OkOrErrorTag,
                                     const char *Tag,
                                     size_t PayloadArity) {
  sedge_encode_tuple_header(sedge_port_write_buffer,
                            &sedge_port_write_index, 3);
  sedge_encode_atom(sedge_port_write_buffer,
                    &sedge_port_write_index, SEDGE_RETURN_TAG);
  sedge_encode_atom(sedge_port_write_buffer,
                    &sedge_port_write_index, Tag);
  if (PayloadArity > 0)
    sedge_encode_tuple_header(sedge_port_write_buffer,
                              &sedge_port_write_index, 1 + PayloadArity);
  sedge_encode_atom(sedge_port_write_buffer,
                    &sedge_port_write_index, OkOrErrorTag);
}



/* (generated) Dispatch 'can_stat' to the erlang port */
void return_can_stat_ok() {
  sedge_port_write_begin();
  sedge_port_encode_return_header(SEDGE_OK_TAG, "can_stat", 0);
  sedge_port_write_commit();
}


/* (generated) Dispatch 'get_filetype' to the erlang port */
void return_get_filetype_ok(safeio_filetype h_Arg_0) {
  sedge_port_write_begin();
  sedge_port_encode_return_header(SEDGE_OK_TAG, "get_filetype", 1);
  sedge_encode_unsigned(sedge_port_write_buffer, &sedge_port_write_index, h_Arg_0);
  sedge_port_write_commit();
}


/* (generated) Dispatch 'can_stat' to the erlang port */
void return_can_stat_error(safeio_error h_Arg_0) {
  sedge_port_write_begin();
  sedge_port_encode_return_header(SEDGE_ERROR_TAG, "can_stat", 1);
  sedge_encode_unsigned(sedge_port_write_buffer, &sedge_port_write_index, h_Arg_0);
  sedge_port_write_commit();
}


/* (generated) Dispatch 'get_filetype' to the erlang port */
void return_get_filetype_error(safeio_error h_Arg_0) {
  sedge_port_write_begin();
  sedge_port_encode_return_header(SEDGE_ERROR_TAG, "get_filetype", 1);
  sedge_encode_unsigned(sedge_port_write_buffer, &sedge_port_write_index, h_Arg_0);
  sedge_port_write_commit();
}



/* Command dispatching section: ***********************************************/


/** Encode the return value of a function identified by Tag. 
    This send '{return, Tag, ReturnValue}' to the erlang port.
  */
void sedge_port_cmd_encode_function_return_header(const char *Tag) {
  sedge_encode_tuple_header(sedge_port_write_buffer,
                            &sedge_port_write_index, 3);
  sedge_encode_atom(sedge_port_write_buffer,
                    &sedge_port_write_index, SEDGE_RETURN_TAG);
  sedge_encode_atom(sedge_port_write_buffer,
                    &sedge_port_write_index, Tag);
}

/** A command is a function that is called when a buffer was received from the
    port.  The function to be called will be selected based on the buffer
    contents an eventually a single function will be selected that consumes the
    data. There is exactly 0 or 1 term in the buffer at the given index.

    Parameters to a command:

    1. a pointer the the buffer that contains the read data
    2. the index into that buffer from where the command decodes it's arguments

    A wrapper function returns non-zero to indicate an error or exit condition.

  */
typedef int(*sedge_cmd_fun)(char *, int);


/** Each command that can be executed by `sedge_handle_next_request' is defined
    by an instance of this structure.
  */
typedef struct sedge_cmd_info {

  /** Pointer to the dispatcher function that executed the command. */
  sedge_cmd_fun funptr;

  /** The identifier for the command function as human readable string */
  const char * funname;

  /** The number parameters that a command must accept from the input. The
      payload tuple must have exactly this arity. */
  uint8_t param_count;

  /** The number of return values, either zero or one. */
  int return_values;
} sedge_cmd_info;


#define SEDGE_MAGIC_EXIT -1337


/** The command simply returns with SEDGE_MAGIC_EXIT so that the main loop will be left.
  */
int sedge_cmd_quit(char *buf, int index) {
  (void) buf;
  (void) index;
  if (!sedge_trace_begin()) {
    fprintf(sedge_trace_file, "\n\n*** Leaving Main Loop ***\n");
    sedge_trace_commit();
  }
  return (SEDGE_MAGIC_EXIT);
}


/** The command with the index 1 is always the built in heart beat command.  It
    will send the current time in seconds as a long value back to the port in
    form of an 'event'.

    In: {{'heart_beat', _Time1, Time2 (both non_neg_integer())}}

    Out: {event, {'heart_beat', Time2, MyTime :: non_neg_integer()}}

  */
int sedge_cmd_heart_beat(char *buf, int index) {
  sedge_decode_tuple_header(buf, &index, 2, "heart_beat");
  sedge_decode_uint64_t(buf, &index);
  uint64_t Time2 = (uint64_t)sedge_decode_uint64_t(buf, &index);
  uint64_t MyTime = (uint64_t)time(NULL);
  sedge_port_write_begin();
  sedge_port_encode_event_header("heart_beat", 2);
  sedge_encode_uint64_t(sedge_port_write_buffer,
                        &sedge_port_write_index,
                        Time2);
  sedge_encode_uint64_t(sedge_port_write_buffer,
                        &sedge_port_write_index,
                        MyTime);
  sedge_port_write_commit();
  return 0;
}

/** Built-in commmand '2' to enable/disable tracing to a file.

    In: { activate | deactivate, Filename }

    Out: -/-

  */
int sedge_cmd_trace(char *buf, int index) {
  char *FileName = NULL;
  char *Mode = "a";
  FILE *NewTraceFile = NULL;

  if (pthread_mutex_lock(&sedge_trace_file_mutex)) {
    fprintf(stderr,
            "sedge_cmd_trace(): failed to lock mutex: %s.\n",
            strerror(errno));
    return 1;
  }

  if (sedge_trace_file != NULL) {
    fprintf(sedge_trace_file, "\n\n*** TRACE DEACTIVATED ***\n");
    fflush(sedge_trace_file);
    fclose(sedge_trace_file);
    sedge_trace_file = 0;
  }
  FileName = sedge_decode_string(buf, &index);
  if (strlen(FileName) > 0) {
    NewTraceFile = fopen(FileName, Mode);
    if (NewTraceFile == NULL) {
      fprintf(stderr, "Can't open trace output file: %s\n%s\n",
              FileName,
              strerror(errno));
    } else {
      sedge_trace_file = NewTraceFile;
      fprintf(sedge_trace_file, "*** TRACE ACTIVATED ***\n");
      fflush(sedge_trace_file);
    }
  }
  free(FileName);
  if (pthread_mutex_unlock(&sedge_trace_file_mutex)) {
    fprintf(stderr,
            "sedge_cmd_trace(): failed to unlock mutex: %s.\n",
            strerror(errno));
    return 1;
  }
  return 0;
}

/** Generated custom commands: */

int sedge_cmd_D_can_stat(char *buf, int index){
  
  /* Declare parameter values: */
  char *h_path;
  
  /* Declare return value variable: */
  /* Decode parameter values: */
  h_path = sedge_decode_string(buf, &index);
  
  /* Invoke user API function: */
  can_stat(h_path);
  
  /* Free all data dynamically allocated by sedge_decode: */
  free(h_path);
  
  return 0;
}
int sedge_cmd_D_get_filetype(char *buf, int index){
  
  /* Declare parameter values: */
  char *h_path;
  
  /* Declare return value variable: */
  /* Decode parameter values: */
  h_path = sedge_decode_string(buf, &index);
  
  /* Invoke user API function: */
  get_filetype(h_path);
  
  /* Free all data dynamically allocated by sedge_decode: */
  free(h_path);
  
  return 0;
}


/** End of enerted custom commands. */


/** Global array of commands: */
static sedge_cmd_info sedge_cmd_table[] = {
  { &sedge_cmd_quit, "sedge_cmd_quit", 0, 0 }
  , { &sedge_cmd_heart_beat, "sedge_cmd_heart_beat", 1, 0 }
  , { &sedge_cmd_trace, "sedge_cmd_trace", 1, 0 }
   /* Custom commands: */
  , { &sedge_cmd_D_can_stat, "sedge_cmd_D_can_stat", 1, 0}
  , { &sedge_cmd_D_get_filetype, "sedge_cmd_D_get_filetype", 1, 0}
   /* End of custom commands. */
};


/** Wait for an incoming packet, decode the version and dispatch to the wrapped
    API function.

    When this is done, the function returns and must be called again, to handle
    the next message.

    \return 0 on success, 1 to indicate that there is reason for the program to exit.
  */
int sedge_handle_next_request() {
  size_t size = 0;
  static char buf [SEDGE_PORT_BUF_SIZE + 1];
  int index = 0;
  size_t CmdId = 0;
  sedge_cmd_info Cmd;
  int CmdError = 0;

  if(sedge_port_read (buf, &size)) {
    sedge_print_stack_trace ();
    fprintf(stderr, "sedge_handle_next_request: Failed to read payload.\n");
    return 1;
  }

  /* All reading is finished here; now decode and invoke: */

  sedge_decode_version (buf, &index);

  /* Command tuple: {CmdId :: non_neg_integer(), PayLoad :: term()} */

  sedge_decode_tuple_header_raw (buf, &index, 2);
  CmdId = sedge_decode_uint16_t (buf, &index);

  Cmd = sedge_cmd_table [CmdId];

  /* Trace the incoming command */
  if (!sedge_trace_begin()) {
    fprintf(sedge_trace_file, "* Received command: %s/%i -> %i\n",
            Cmd.funname, Cmd.param_count, Cmd.return_values);
    int local_index = index;
    ei_print_term(sedge_trace_file, buf, &local_index);
    fprintf(sedge_trace_file, "\n");
    sedge_trace_commit();
  }

  sedge_decode_tuple_header_raw (buf, &index, Cmd.param_count);

  CmdError = (*Cmd.funptr) (buf, index);

  if (CmdError && CmdError != SEDGE_MAGIC_EXIT) {
    sedge_print_stack_trace ();
    fprintf(stderr,
            "ERROR: sedge_handle_next_request: cmd: %s "
            "returned non-zero: %i.\n", Cmd.funname, CmdError);
    exit(1);
  }
  return CmdError;
}





/*******************************************************************************
 *  The main loop. Reads data from the port and dispatches the received commands
 *  in an endless loop until an exit command was received or an error occured.
 ******************************************************************************/

int
sedge_main_loop () {
  int res = 0;
  while (res == 0) {
    res = sedge_handle_next_request ();
    if (res == SEDGE_MAGIC_EXIT) return 0;
  }
  return (res);
}


/*                           END OF GENERATED CODE                            */
