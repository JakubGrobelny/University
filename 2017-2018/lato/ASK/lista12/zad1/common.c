#include <stdarg.h>
#include "common.h"

void fail(const char *fmt, ...) {
  va_list ap;
  va_start(ap, fmt);
  fprintf(stderr, "Failure: ");
  vfprintf(stderr, fmt, ap);
  fputc('\n', stderr);
  va_end(ap);
  exit(EXIT_FAILURE);
}

/* The state must be seeded with a nonzero value. */
static uint64_t __state;

/* This procedure gets called before main(). */
__attribute__((constructor)) void __init_fast_random() {
  __state = time(NULL);
}

void fast_srandom(uint64_t seed) {
  __state = seed;
}

uint64_t fast_random() {
  __state ^= __state >> 12;
  __state ^= __state << 25;
  __state ^= __state >> 27;
  return __state * 2685821657736338717UL;
}

void flush_cache() {
  void *tmp = NULL;
  posix_memalign(&tmp, getpagesize(), CACHE_SIZE);
  if (!tmp)
    fail("Failed to allocate cache flush buffer!");
  bzero(tmp, CACHE_SIZE);
  free(tmp);
}

void timer_reset(_timer_t *t) {
  memset(t, 0, sizeof(_timer_t));
}

void timer_start(_timer_t *t) {
  gettimeofday(&t->start, NULL);
}

void timer_stop(_timer_t *t) {
  struct timeval tv_end, tv_res;
  gettimeofday(&tv_end, NULL);
  timersub(&tv_end, &t->start, &tv_res);
  timeradd(&t->sum, &tv_res, &t->sum);
}

void timer_print(_timer_t *t) {
  printf("Time elapsed: %ld.%06ld seconds.\n", t->sum.tv_sec, t->sum.tv_usec);
}
