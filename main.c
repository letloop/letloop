#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <sys/stat.h>
#include <string.h>
#include <sys/types.h>

#include "scheme.h"


static const char *argv0;

char letloopdir[] = "/tmp/letloop/exec-XXXXXX";
char bootfilename[] = "/tmp/letloop/exec-XXXXXX/boot";
char schemefilename[] = "/tmp/letloop/exec-XXXXXX/program";
const char *cleanup_bootfile = 0;
const char *cleanup_schemefile = 0;

const char scheme_boot[] = {~{0x~x,~}};
const unsigned int scheme_boot_size = sizeof(scheme_boot);

const char scheme_program[] = {~{0x~x,~}};
const unsigned int scheme_program_size = sizeof(scheme_program);

const char letloop_extra[] = {~{0x~x,~}};
const unsigned long long letloop_extra_size = sizeof(letloop_extra);

int setupterm(char *term, int fd, int *errret) {
	return 0;
}

int tputs(const char *str, int affcnt, int (*putc)(int)) {
	return 0;
}

void *cur_term;

const char *program_name(void) {
  return argv0;
}

const char *letloop_directory(void) {
  return letloopdir;
}

void custom_init(void) {
  Sregister_symbol("program-name", (void*) program_name);
  Sregister_symbol("letloop-extra", (void*) letloop_extra);
  Sregister_symbol("letloop-extra-size", (void*) letloop_extra_size);
  Sregister_symbol("letloop-directory", (void*) letloop_directory);
}

int run_program(int argc, const char **argv, const char *bootfilename, const char *schemefilename) {
  argv0 = argv[0];
  Sscheme_init(0);
  Sregister_boot_file(bootfilename);
  Sbuild_heap(0, custom_init);
  return Sscheme_program(schemefilename, argc, argv);
}

void cleanup(void) {
  if (cleanup_bootfile) unlink(bootfilename);
  if (cleanup_schemefile) unlink(schemefilename);
}

int makefile(char *filename, const char *contents, size_t size) {
  int fd;
  fd = open(filename, O_CREAT | O_RDWR, 0755);
  assert(fd >= 0);

  assert(write(fd, contents, size) == size);
  assert(lseek(fd, 0, SEEK_SET) == 0);
  return fd;
}

int main(int argc, const char **argv) {
  int bootfd;
  int schemefd;
  int ret;
  int e;

  struct stat sb;

  atexit(cleanup);

  if (stat("/tmp/letloop", &sb) == 0) {
    if(!S_ISDIR(sb.st_mode)) {
      assert(unlink("/tmp/letloop") == 0);
      assert(mkdir("/tmp/letloop", 0755) == 0);
    }
  } else {
    assert(mkdir("/tmp/letloop", 0755) == 0);
  }

  assert(mkdtemp(letloopdir) != NULL);

  assert(strncpy(bootfilename, letloopdir, strlen(letloopdir)) == bootfilename);
  bootfd = makefile(bootfilename, scheme_boot, scheme_boot_size);
  cleanup_bootfile = bootfilename;

  assert(strncpy(schemefilename, letloopdir, strlen(letloopdir)) == schemefilename);
  schemefd = makefile(schemefilename, scheme_program, scheme_program_size);
  cleanup_schemefile = schemefilename;

  ret = run_program(argc, argv, bootfilename, schemefilename);

  close(bootfd);
  close(schemefd);

  return ret;
}
