/*
 * Random walk optimized to make branch predictor less miserable.
 *
 * Intel® Core™ i5-6600 CPU @ 3.30GHz
 *
 * $ ./randwalk -S 0xea3495cc76b34acc -n 7 -s 16 -t 14 -v 0
 * Time elapsed: 6.635666 seconds.
 * $ ./randwalk -S 0xea3495cc76b34acc -n 7 -s 16 -t 14 -v 1
 * Time elapsed: 3.260372 seconds.
 */

#include "common.h"

static void fill(uint8_t *dst, int n) {
  for (int i = 0; i < n * n; i++)
    dst[i] = fast_random();
}

static __noinline int randwalk1(uint8_t *arr, int n, int len) {
  int sum = 0;
  int i, j, k = 0;
  uint64_t dir = 0;

  /* Start in the center of 2D array */
  i = n / 2, j = n / 2;

  do {
    k -= 2;
    if (k < 0) {
      k = 62;
      dir = fast_random();
    }

    int d = (dir >> k) & 3;

    sum += arr[i * n + j];

    /* 
     * We must avoid unpredictable branches in tight loops!
     *
     * GCC is not smart enough to translate following code using SETcc/CMOVcc
     * instructions. If that's not done, then branch predictor will suffer. 
     */
    if (d == 0) {
      if (i > 0)
        i--;
    } else if (d == 1) {
      if (i < n - 1)
        i++;
    } else if (d == 2) {
      if (j > 0)
        j--;
    } else {
      if (j < n - 1)
        j++;
    }
  } while (--len);

  return sum;
}

static __noinline int randwalk2(uint8_t* arr, int n, int len) 
{
    int sum = 0;
    int i, j, k = 0;
    uint64_t dir = 0;

    i = n / 2, j = n / 2;

    do
    {
        k -= 2;
        if (k < 0)
        {
            k = 62;
            dir = fast_random();
        }

        int d = (dir >> k) & 3;
        sum += arr[i * n + j];

        i -= (i > 0     && d == 0) ? 1 : 0;
        i += (i < n - 1 && d == 1) ? 1 : 0;

        j -= (j > 0     && d == 2) ? 1 : 0;
        j += (j < n - 1 && d == 3) ? 1 : 0;
    }
    while (--len);

    return sum;
}

static __noinline int test_randwalk1(uint8_t *arr, int n, int len, int times) {
  int sum = 0;
  for (int i = 0; i < times; i++)
    sum += randwalk1(arr, n, len);
  return sum;
}

static __noinline int test_randwalk2(uint8_t *arr, int n, int len, int times) {
  int sum = 0;
  for (int i = 0; i < times; i++)
    sum += randwalk2(arr, n, len);
  return sum;
}

int main(int argc, char **argv) {
  int opt, size = -1, steps = -1, times = -1, variant = -1;
  bool err = false;
  bool user_seed = false;
  uint64_t seed;

  while ((opt = getopt(argc, argv, "n:s:t:v:S:")) != -1) {
    if (opt == 'n')
      size = 1 << atoi(optarg);
    else if (opt == 's')
      steps = 1 << atoi(optarg);
    else if (opt == 't')
      times = 1 << atoi(optarg);
    else if (opt == 'v')
      variant = atoi(optarg);
    else if (opt == 'S')
      user_seed = true, seed = strtoul(optarg, NULL, 16);
    else
      err = true;
  }

  if (err || size < 0 || steps < 0 || times < 0 || variant < 0 || variant >= 2)
  {
    fprintf(stderr,
            "Usage: %s -S hex_seed-n log2(size) -s log2(steps) -t log2(times)"
            " -v variant\n", argv[0]);
    exit(EXIT_FAILURE);
  }

  if (!user_seed)
    read_bytes("/dev/urandom", &seed, sizeof(seed));
  fast_srandom(seed);
  printf("Random number generator seed is 0x%" PRIx64 "\n", seed);

  uint8_t *array = malloc_page_aligned(size * size);

  printf("Generate matrix %d x %d (%d KiB)\n", size, size, size * size >> 10);

  fill(array, size);
  flush_cache();

  printf("Performing %d random walks of %d steps.\n", times, steps);

  unsigned sum = 0;

  _timer_t timer;
  timer_reset(&timer);
  timer_start(&timer);
  if (variant == 0)
    sum = test_randwalk1(array, size, steps, times);
  if (variant == 1)
    sum = test_randwalk2(array, size, steps, times);
  timer_stop(&timer);
  timer_print(&timer);

  /* Make sure you get same results with given RNG seed! */
  printf("Walks accrued elements worth: %u\n", sum);

  free(array);

  return EXIT_SUCCESS;
}
