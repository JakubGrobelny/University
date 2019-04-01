/*
 * Random walk optimized to make branch predictor less miserable.
 *
 * $ ./randwalk -n 7 -s 16 -t 14 -v 0
 * Time elapsed: 6.480445 seconds.
 * $ ./randwalk -n 7 -s 16 -t 14 -v 1
 * Time elapsed: 3.801035 seconds.
 */

#include "common.h"

void fill(uint8_t *dst, int n) 
{
    for (int i = 0; i < n * n; i++)
        dst[i] = rand();
}

int randwalk1(uint8_t *arr, int n, int len) 
{
    int i, j, k = 0;
    int sum = 0;
    uint64_t dir = 0;

    i = fast_random() % n;
    j = fast_random() % n;

    do 
    {
        k -= 2;
        if (k < 0) 
        {
            k = 62;
            dir = fast_random();
        }

        sum += arr[i * n + j];

        /* 
         * We must avoid unpredictable branches in tight loops!
         *
         * GCC is not smart enough to translate following code using cmov
         * instructions. If that's not done, then branch predictor will suffer. 
         */
        switch ((dir >> k) & 3) 
        {
            case 0:
                if (i > 0)
                    i--;
                break;
            case 1:
                if (i < n - 1)
                    i++;
                break;
            case 2:
                if (j > 0)
                    j--;
                break;
            case 3:
                if (j < n - 1)
                    j++;
                break;
        }
    } 
    while (--len);

    return sum;
}

int randwalk2(uint8_t *arr, int n, int len) 
{
    int i, j, k = 0;
    int sum = 0;
    uint64_t dir = 0;

    i = fast_random() & (n - 1);
    j = fast_random() & (n - 1);

    do
    {
        k -= 2;
        if (k < 0)
        {
            k = 62;
            dir = fast_random();
        }

        sum += arr[i * n + j];

        /* XXX: Replace switch statement with code that does not use branch
         * instructions. */
        int l = (dir >> k) & 3;
        
        i -= (i > 0     && l == 0) ? 1 : 0;
        i += (i < n - 1 && l == 1) ? 1 : 0;

        j -= (j > 0     && l == 2) ? 1 : 0;
        j += (j < n - 1 && l == 3) ? 1 : 0;

    }
    while (--len);

    return sum;
}

int main(int argc, char **argv)
{
    int opt, size = -1, steps = -1, times = -1, var = -1;
    bool err = false;

    while ((opt = getopt(argc, argv, "n:s:t:v:")) != -1) 
    {
        if (opt == 'n')
            size = 1 << atoi(optarg);
        else if (opt == 's')
            steps = 1 << atoi(optarg);
        else if (opt == 't')
            times = 1 << atoi(optarg);
        else if (opt == 'v')
            var = atoi(optarg);
        else
            err = true;
    }

    if (err || size < 0 || steps < 0 || times < 0 || var < 0 || var >= 2) 
    {
        fprintf(stderr, "Usage: %s -n log2(size) -l log2(length) -t log2(times) "
                                        "-v variant\n", argv[0]);
        exit(EXIT_FAILURE);
    }

    uint8_t *array = NULL;

    posix_memalign((void **)&array, getpagesize(), size * size);

    printf("Generate matrix %d x %d (%d KiB)\n", size, size, size * size >> 10);

    fill(array, size);
    flush_cache();

    printf("Performing %d random walks of %d steps.\n", times, steps);

    _timer_t timer;
    timer_reset(&timer);
    timer_start(&timer);
    for (int i = 0; i < times; i++) {
        if (var == 0)
            randwalk1(array, size, steps);
        else
            randwalk2(array, size, steps);
    }
    timer_stop(&timer);
    timer_print(&timer);

    free(array);

    return EXIT_SUCCESS;
}
