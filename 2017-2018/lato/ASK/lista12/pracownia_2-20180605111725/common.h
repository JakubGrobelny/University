#ifndef __COMMON_H__
#define __COMMON_H__

#include <inttypes.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <fcntl.h>
#include <time.h>
#include <unistd.h>
#include <sys/time.h>

/* Block size for Intel® x86-64. */
#define BLOCK_SIZE 64
/* Hopefully no one has more than 16MiB of L3 cache. */
#define CACHE_SIZE (1 << 24)

/* Number of items of type T in array of size S. */
#define NITEMS(S, T) ((S) / sizeof(T))
/* Function attribute that prevents compiler from inlining it. */
#define __noinline __attribute__ ((noinline))
/* Prevents compiler from reordering memory read & write operations. */
#define barrier() asm volatile("" ::: "memory")

static inline uint64_t rdtsc(void) {
  uint32_t hi, lo;
  asm volatile("rdtsc" : "=a"(lo), "=d"(hi));
  return (uint64_t)lo | ((uint64_t)hi << 32);
}

typedef struct _timer {
  struct timespec start, sum;
} _timer_t;

void fail(const char *fmt, ...);

void fast_srandom(uint64_t seed);
uint64_t fast_random();

void flush_cache();

void timer_reset(_timer_t *t);
void timer_start(_timer_t *t);
void timer_stop(_timer_t *t);
void timer_print(_timer_t *t);

void read_bytes(const char *pathname, void *buf, size_t n);
void *malloc_page_aligned(size_t size);

#endif
