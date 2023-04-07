#include <assert.h>
#include <fcntl.h>
#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>
#include <stdint.h>

#include "scheme.h"

static const char *argv0;

char bootfilename[] = "/tmp/chezschemebootXXXXXX";
char schemefilename[] = "/tmp/schemeprogramXXXXXX";
const char *cleanup_bootfile = 0;
const char *cleanup_schemefile = 0;

const char scheme_boot[] = {~{0x~x,~}};
const unsigned int scheme_boot_size = sizeof(scheme_boot);

const char scheme_program[] = {~{0x~x,~}};
const unsigned int scheme_program_size = sizeof(scheme_program);

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

void custom_init(void) {
  Sregister_symbol("program_name", (void*)program_name);
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

int maketempfile(char *template, const char *contents, size_t size) {
  int fd;
  fd = mkstemp(template);
  assert(fd >= 0);

  assert(write(fd, contents, size) == size);
  assert(lseek(fd, 0, SEEK_SET) == 0);
  return fd;
}

int main(int argc, const char **argv) {
  int bootfd;
  int schemefd;
  int ret;

  atexit(cleanup);

  bootfd = maketempfile(bootfilename, scheme_boot, scheme_boot_size);
  cleanup_bootfile = bootfilename;

  schemefd = maketempfile(schemefilename, scheme_program, scheme_program_size);
  cleanup_schemefile = schemefilename;

  ret = run_program(argc, argv, bootfilename, schemefilename);

  close(bootfd);
  close(schemefd);

  return ret;
}
