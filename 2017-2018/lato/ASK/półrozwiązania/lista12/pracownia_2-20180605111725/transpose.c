/*
 * Matrix transposition with and without blocking.
 *
 * Intel® Core™ i5-6600 CPU @ 3.30GHz
 *
 * $ ./transpose -n 4096 -v 0
 * Time elapsed: 21.528841 seconds.
 * $ ./transpose -n 4096 -v 1
 * Time elapsed: 5.251710 seconds.
 */
#include "common.h"

/* You can modify following definitions to try out different settings. */ 
#define T int 
#define BLOCK 8

static void fill(T *dst, int n) {
  for (int i = 0; i < n; i++)
    for (int j = 0; j < n; j++)
      dst[i * n + j] = i * n + j;
}

static __noinline void transpose1(T *dst, T *src, int n) {
  for (int i = 0; i < n; i++)
    for (int j = 0; j < n; j++)
      dst[j * n + i] = src[i * n + j];
}

static __noinline void transpose2(T* dst, T* src, int n) 
{
    const int blocks = (n / BLOCK) + (n % BLOCK ? 1 : 0);

    // Following algorithm works for any n (not only multiples of 4)
    for (int i = 0; i < blocks; i++)
        for (int j = 0; j < blocks; j++)
            for (int k = i * BLOCK; (k < n) && k < (i * BLOCK + BLOCK); k++)
              for (int l = j * BLOCK; (l < n) && l < (j * BLOCK + BLOCK); l++)
                dst[l * n + k] = src[k * n + l];
}

int main(int argc, char **argv) {
  int opt, variant = -1, n = 0;
  bool err = false;

  while ((opt = getopt(argc, argv, "n:v:")) != -1) {
    if (opt == 'n')
      n = atoi(optarg);
    else if (opt == 'v')
      variant = atoi(optarg);
    else
      err = true;
  }

  if (err || n == 0 || variant < 0 || variant >= 2)
    fail("Usage: %s -n size -v variant\n", argv[0]);

  if (n % BLOCK)
    fail("Matrix size (%d) must be divisible by %d!", n, BLOCK);

  size_t size = n * n * sizeof(T);
  T *src = malloc_page_aligned(size);
  T *dst = malloc_page_aligned(size);

  printf("Generate matrix %d x %d (%ld KiB)\n", n, n, size >> 10);

  fill(src, n);
  bzero(dst, size);
  flush_cache();

  printf("Performing matrix transposition.\n");

  _timer_t timer;
  timer_reset(&timer);
  timer_start(&timer);
  if (variant == 0) 
    transpose1(dst, src, n);
  if (variant == 1) 
    transpose2(dst, src, n);
  timer_stop(&timer);
  timer_print(&timer);

  free(src);
  free(dst);

  return 0;
}
