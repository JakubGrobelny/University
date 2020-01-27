#include "csapp.h"

static __unused void outc(char c) {
    Write(STDOUT_FILENO, &c, 1);
}

static void randsleep(void) {
    usleep(rand() % 5000 + 5000);
}

#define N 50

static pthread_t td[N];
static sem_t forks[N];
/* TODO: If you need extra shared state, define it here. */
static sem_t waiter;
static int free_count;
#define WAITER(action) { Sem_wait(&waiter); action; Sem_post(&waiter); }

void* philosopher(void* id) {
    int right = (intptr_t)id;
    int left = right == 0 ? N - 1 : right - 1;

    for (;;) {
        /* Think */
        printf("[%ld] Thinking\n", (intptr_t)id);
        randsleep();

        /* TODO: Take forks (without deadlock & starvation) */

        bool can_take = false;
        while (!can_take) {
            WAITER(
                if (free_count > 1) {
                    free_count--;
                    can_take = true;
                }
            );
        }

        Sem_wait(&forks[right]);
        WAITER(free_count--);
        Sem_wait(&forks[left]);

        /* Eat */
        printf("[%ld] Eating\n", (intptr_t)id);
        randsleep();

        /* TODO: Put forks (without deadlock & starvation) */

        Sem_post(&forks[left]);
        WAITER(free_count++);
        
        Sem_post(&forks[right]);
        WAITER(free_count++);
    }

    return NULL;
}

int main(void) {
    /* TODO: If you need extra shared state, initialize it here. */
    free_count = N;
    Sem_init(&waiter, 0, 1);

    for (int i = 0; i < N; i++)
        Sem_init(&forks[i], 0, 1);

    for (int i = 0; i < N; i++)
        Pthread_create(&td[i], NULL, philosopher, (void*)(intptr_t)i);

    for (int i = 0; i < N; i++)
        Pthread_join(td[i], NULL);
    
    return EXIT_SUCCESS;
}
