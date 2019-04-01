#ifndef __COMMON_H__
#define __COMMON_H__

#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include <sys/time.h>

/* Block size for Intel® x86-64. */
#define BLOCK_SIZE 64
/* Hopefully no one has more than 16MiB of L3 cache. */
#define CACHE_SIZE (1 << 24)

typedef struct _timer {
  struct timeval start, sum;
} _timer_t;

void fail(const char *fmt, ...);

void fast_srandom(uint64_t seed);
uint64_t fast_random();

void flush_cache();

void timer_reset(_timer_t *t);
void timer_start(_timer_t *t);
void timer_stop(_timer_t *t);
void timer_print(_timer_t *t);

#endif
