/*
 * Binary search vs. heap search.
 *
 * Intel® Core™ i5-6600 CPU @ 3.30GHz
 *
 * $ ./bsearch -S 0x5bab3de5da7882ff -n 23 -t 24 -v 0
 * Time elapsed: 7.616777 seconds.
 * $ ./bsearch -S 0x5bab3de5da7882ff -n 23 -t 24 -v 1
 * Time elapsed: 2.884369 seconds.
 */
#include "common.h"

static int icmp(const void *a, const void *b) {
  int x = *(const int *)a;
  int y = *(const int *)b;
  if (x < y) return -1;
  if (x > y) return 1;
  return 0;
}

static void fill(int *arr, int n) {
  for (int i = 0; i < n; i++)
    arr[i] = fast_random() & n;

  qsort(arr, n, sizeof(int), icmp);
}

static void heapify(int* dst, int* src, int n) 
{
    long i = 0;
    long index = 0;

    while ((1 << i) < n)
    {
        long offset = (n-1) >> i;
        long removed_cnt = 0;
        
        for (long e = 0; e < (1 << i); e++)
        {
            long start = e * offset + removed_cnt;
            long end = (e + 1) * offset+ removed_cnt;

            long middle = (start + end) >> 1;

            removed_cnt++;
            dst[index++] = src[middle];
        }

        i++;
    }
}

static __noinline bool binary_search(int *arr, long size, int x) {
  do {
    size >>= 1;
    int y = arr[size];
    if (y == x)
      return true;
    if (y < x)
      arr += size + 1;
  } while (size > 0);
  return false;
}

static __noinline bool heap_search(int* arr, long size, int x) 
{
    long i = 1;
    do 
    {
        int y = arr[i - 1];
        long j = i * 2;
        
        if (y == x)
            return true;

        j |= (y < x);

        i = j;

    }
    while (i <= size);
    
    return false;
}

static __noinline int binary_search_test(int *arr, int n, int times) {
  int found = 0;
  for (int i = 0; i < times; i++)
    found += binary_search(arr, n, fast_random() & n);
  return found;
}

static __noinline int heap_search_test(int *arr, int n, int times) {
  int found = 0;
  for (int i = 0; i < times; i++)
      found += heap_search(arr, n, fast_random() & n);
  return found;
}

int main(int argc, char **argv) {
  int opt, exp = -1, times = -1, variant = -1;
  bool err = false;
  bool user_seed = false;
  uint64_t seed;

  while ((opt = getopt(argc, argv, "n:t:v:S:")) != -1) {
    if (opt == 'n')
      exp = atoi(optarg);
    else if (opt == 't')
      times = 1 << atoi(optarg);
    else if (opt == 'v')
      variant = atoi(optarg);
    else if (opt == 'S')
      user_seed = true, seed = strtoul(optarg, NULL, 16);
    else
      err = true;
  }

  if (err || exp < 0 || times < 0 || variant < 0 || variant >= 2) {
    fprintf(stderr,
            "Usage: %s -S hex_seed -n log2(size) -l log2(times) -v variant\n",
             argv[0]);
    return 1;
  }

  if (!user_seed)
    read_bytes("/dev/urandom", &seed, sizeof(seed));
  fast_srandom(seed);
  printf("Random number generator seed is 0x%" PRIx64 "\n", seed);

  /* Number of elements in fully populated binary tree of height (exp - 1) */
  int n = (1 << exp) - 1;
  int size = n * sizeof(int);
  int *arr = malloc_page_aligned(size);

  printf("Generate array of 2^%d-1 elements (%d KiB)\n", exp, size >> 10);

  fill(arr, n);
  if (variant == 1) {
    int *tmp = malloc_page_aligned(size);
    heapify(tmp, arr, n);
    free(arr);
    arr = tmp;
  }
  flush_cache();

  printf("Performing %d searches\n", times);

  unsigned found = 0;

  _timer_t timer;
  timer_reset(&timer);
  timer_start(&timer);
  if (variant == 0)
    found = binary_search_test(arr, n, times);
  if (variant == 1)
    found = heap_search_test(arr, n, times);
  timer_stop(&timer);
  timer_print(&timer);

  /* Make sure you get same results with given RNG seed! */
  printf("Number of searches that succeeded: %d/%d\n", found, times);

  free(arr);

  return 0;
}
