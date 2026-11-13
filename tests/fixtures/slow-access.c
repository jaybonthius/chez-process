#define _GNU_SOURCE
#include <dlfcn.h>
#include <string.h>
#include <unistd.h>

/* Preloaded into a test's own Chez, so that looking a program up in PATH
   stalls as it would on a hung file system. */
int access(const char *path, int mode) {
  static int (*real)(const char *, int);
  if (!real) real = (int (*)(const char *, int))dlsym(RTLD_NEXT, "access");
  if (strstr(path, "/slow-path/")) sleep(1);
  return real(path, mode);
}
