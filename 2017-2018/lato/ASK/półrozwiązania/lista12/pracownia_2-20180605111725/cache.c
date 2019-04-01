/*
 * Template for cache organization exploring assignment.
 *
 * $ ./cache -n 26 -s 26 -t 20
 * Time elapsed: 2.322265 seconds.
 */

#include "common.h"

#define DEBUG 0

/* Do not touch this procedure! */
int array_walk(volatile int *array, int steps) {
  int sum = 0, i = 0;

  do {
    if (i < 0)
      break;
#if DEBUG
    printf("%d -> %d\n", i, array[i]);
#endif
    i = array[i];
    sum += i;
  } while (--steps);

  return sum;
}

/* XXX: You are only allowed to change this procedure! */
void generate_permutation(int *array, int size) {
  /* Walk over every even element, then over every odd. */
  for (int i = 0; i < size - 1; i += 2) {
    array[i] = i + 2;
    array[i + 1] = i + 3;
  }

  /* Link the end of even list to the beginning of odd list. */
  array[size - 2] = 1;

  /* Finish the sequence, note it's not circular! */
  array[size - 1] = -1;
}

int main(int argc, char **argv) {
  int opt, size = -1, steps = -1, times = -1;
  bool error = false;

  while ((opt = getopt(argc, argv, "n:s:t:")) != -1) {
    if (opt == 'n') 
      size = 1 << atoi(optarg);
    else if (opt == 's')
      steps = 1 << atoi(optarg);
    else if (opt == 't')
      times = atoi(optarg);
    else
      error = true;
  }

  if (error || size < 0 || steps < 0 || times < 0) {
    printf("Usage: %s -n log2(size) -s log2(steps) -t times\n", argv[0]);
    exit(EXIT_FAILURE);
  }

  int *array = malloc_page_aligned(size * sizeof(int));

  printf("Generate array of %d elements (%ld KiB)\n", size, size * sizeof(int) >> 10);
  generate_permutation(array, size);
  flush_cache();

  printf("Perfom walk %d times with %d steps each.\n", times, steps);

  _timer_t t;
  timer_reset(&t);
  for (int i = 0; i < times; i++) {
    timer_start(&t);
    array_walk(array, steps);
    timer_stop(&t);
  }
  timer_print(&t);

  free(array);

  return EXIT_SUCCESS;
}
