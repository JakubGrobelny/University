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
  void *tmp = malloc_page_aligned(CACHE_SIZE);
  bzero(tmp, CACHE_SIZE);
  free(tmp);
}

typedef struct timespec timespec_t;

#define SECOND 1000000000 /* in nanoseconds */

static void timespec_add(timespec_t *a, timespec_t *b, timespec_t *res) {
  if (a->tv_nsec + b->tv_nsec >= SECOND) {
    res->tv_sec = a->tv_sec + b->tv_sec + 1;
    res->tv_nsec = a->tv_nsec + b->tv_nsec - SECOND;
  } else {
    res->tv_sec = a->tv_sec + b->tv_sec;
    res->tv_nsec = a->tv_nsec + b->tv_nsec;
  }
}

static void timespec_sub(timespec_t *a, timespec_t *b, timespec_t *res) {
  if (a->tv_nsec - b->tv_nsec < 0) {
    res->tv_sec = a->tv_sec - b->tv_sec - 1;
    res->tv_nsec = a->tv_nsec - b->tv_nsec + SECOND;
  } else {
    res->tv_sec = a->tv_sec - b->tv_sec;
    res->tv_nsec = a->tv_nsec - b->tv_nsec;
  }
}

void timer_reset(_timer_t *t) {
  memset(t, 0, sizeof(_timer_t));
}

void timer_start(_timer_t *t) {
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &t->start);
}

void timer_stop(_timer_t *t) {
  timespec_t ts_end, ts_res;
  clock_gettime(CLOCK_PROCESS_CPUTIME_ID, &ts_end);
  timespec_sub(&ts_end, &t->start, &ts_res);
  timespec_add(&t->sum, &ts_res, &t->sum);
}

void timer_print(_timer_t *t) {
  printf("Time elapsed: %ld.%06ld seconds.\n", t->sum.tv_sec, 
         t->sum.tv_nsec / 1000);
}

void read_bytes(const char *pathname, void *buf, size_t n) {
  int fd = open(pathname, 0);
  read(fd, buf, n);
  close(fd);
}

void *malloc_page_aligned(size_t size) {
  void *ptr = NULL;
  posix_memalign((void **)&ptr, getpagesize(), size);
  if (ptr == NULL)
    fail("Cannot allocate %lld bytes!", size);
  return ptr;
}
