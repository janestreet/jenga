#define _LARGEFILE64_SOURCE

#include <string.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <errno.h>

#include <caml/alloc.h>
#include <caml/memory.h>
#include <caml/mlvalues.h>
#include <caml/signals.h>
#include <caml/unixsupport.h>
#include <caml/fail.h>

static inline char * copy_to_c_string(value v_str)
{
  asize_t len = caml_string_length(v_str) + 1;
  char *p = caml_stat_alloc(len);
  memcpy(p, String_val(v_str), len);
  return p;
}

static inline void free_paths(char **paths, int n) {
  int i;
  for (i = 0; i < n; i++) {
    caml_stat_free(paths[i]);
  }
  caml_stat_free(paths);
}

/* Use of this function manages to improve from-scratch build times in jenga
   by ~5% in some cases (specifically,
   [PACKING=false X_LIBRARY_INLINING=true jenga app/fe app/hydra] in 2eb4e759bf23)
   compared to repeatedly calling [Core.Unix.stat].

   We think that most of the improvement comes from the fact that we
   don't reacquire the ocaml lock after every call to [stat].

   Some of the improvement also comes from the fact we avoid stat record allocation.
*/
CAMLprim value caml_batched_mtimes(value list_of_paths, value len_list)
{
  CAMLparam2(list_of_paths, len_list);
  CAMLlocal1(res);
  int n = Int_val(len_list);
  char** paths = caml_stat_alloc(sizeof(*paths) * n);
  double* mtimes = caml_stat_alloc(sizeof(*mtimes) * n);
  int i = 0;
  struct stat64 buf;
  while (Is_block(list_of_paths)) {
    paths[i] = copy_to_c_string(Field(list_of_paths, 0));
    list_of_paths = Field(list_of_paths, 1);
    ++i;
  };
  caml_enter_blocking_section();
  for (i = 0; i < n; i++) {
    int stat_ret;
    do {
      stat_ret = stat64(paths[i], &buf);
    } while (stat_ret == -1 && errno == EINTR);
    if (stat_ret == -1) {
      caml_leave_blocking_section();
      res = caml_copy_string(paths[i]);
      free_paths(paths, n);
      caml_stat_free(mtimes);
      uerror("stat", res);
    }
    #if HAS_NANOSECOND_STAT == 1
    #  define NSEC(field) st_##field##tim.tv_nsec
    #elif HAS_NANOSECOND_STAT == 2
    #  define NSEC(field) st_##field##timespec.tv_nsec
    #elif HAS_NANOSECOND_STAT == 3
    #  define NSEC(field) st_##field##timensec
    #else
    #  error "HAS_NANOSECOND_STAT not defined"
    #endif
    mtimes[i] = (double) buf.st_mtime + (buf.NSEC(m) / 1000000000.0);
    #undef NSEC
  }
  free_paths(paths, n);
  caml_leave_blocking_section();
  res = caml_alloc_float_array(n);
  for (i = 0; i < n; i++) {
    Store_double_field(res, i, mtimes[i]);
  }
  caml_stat_free(mtimes);
  CAMLreturn(res);
}
