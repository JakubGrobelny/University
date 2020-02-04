# Spis treści

- [Zadanie 1](#zadanie-1)
- [Zadanie 2](#zadanie-2)
- Zadanie 3 – brak
- Zadanie 4 (bonus) – brak
- Zadanie 5 (P) – brak
- Zadanie 6 (P, bonus) - brak
- Zadanie 7 (P, bonus) – brak

***

# Zadanie 1

### Zdefiniuj <u>*sprawiedliwość*</u> (ang. *fairness*), która jest jedną z właściwości środków synchronizacji.

- **sprawiedliwość** (ang. *fairness*) – każdy wątek oczekujący na zasób ma szansę go otrzymać. Jeżeli środek synchronizacji jest sprawiedliwy, to wówczas nie może występować zagłodzenie. Wątek, który posiada zasób nie może użyć go ponownie po zwolneniu przed innym oczekującym procesem. 

### Standard `POSIX.1` oferuje muteksy, semafory i zmienne warunkowe, które są sprawiedliwe. Czasami sprawiedliwe semafory określa się również mianem <u>*silnych*</u> (ang. *strong semaphore*). Dodatkowo wymienione środki są odporne na zjawisko <u>*odwrócenia priorytetów*</u>. Wyjątkiem są blokady wirujące, które ani nie zapobiegają odwróceniu priorytetów, ani nie muszą być sprawiedliwe. Wyjaśnij skąd to wynika!

- **silne semafory** – semafor, w którym oczekujące procesy umieszczane są w kolejce FIFO, co sprawia, że procesy usuwane są z kolejki w kolejności w jakiej się w niej pojawiły.
- **odwrócenie priorytetów** (ang. *priority inversion*) – sytuacja, w której proces o wysokim priorytecie jest wywłaszczany na rzecz procesu o niższym priorytecie. Innymi słowy: zadania nie wykonują się zgodnie z regułami algorytmu szeregowania. Występuje gdy proces o niższym priorytecie otrzymał dostęp do sekcji krytycznej przed procesami o wyższym priorytecie. Środkiem zaradczym jest dziedziczenie priorytetów, czyli nadawanie zadaniu o niższym priorytecie priorytetu ważniejszego zadania na czas używania dzielonych zasobów. Sprawia to, że zadania o średnim priorytecie nie spowodują wywłaszczenia zadania o niskim priorytecie, które ma dostęp do zasobu.

Blokady wirujące nie zapobiegają odwróceniu priorytetów, bo w sytuacji gdzie istnieją dwa procesy T1 i T2, takie że priorytet(T2) > priorytet(T1). Załóżmy, że planista zawsze wybiera T2 przed T1 a T1 działa przed T2 tylko wtedy, gdy T2 jest nie może się wykonywać. Załóżmy, że T2 jest zablokowany a T1 nabywa spinlocka i wchodzi do sekcji krytycznej. Wówczas gdy T2 zostanie odblokowany, to T1 zostanie wywłaszczony. Gdy T2 spróbuje nabyć spinlocka to zacznie się kręcić ale nigdy nie zostanie wywłaszczony na rzecz T1, czyli powstanie zakleszczenie.

Blokady wirujące nie zapewniają sprawiedliwości, bo nie ma gwarancji, że procesy będą nabywać blokadę w kolejności w jakiej przyszły.

### W jaki sposób pozostałe środki synchronizacji zapewniają sprawiedliwość i zapobiegają odwróceniu priorytetów?

Pozostałe środki synchronizacji zapewniają sprawiedliwość poprzez usypianie wątków w kolejce.

*Mutexy* zapobiegają odwróceniu priorytetów poprzez nadanie wyższego priorytetu swojemu posiadaczowi (*priority ceiling protocol*).

*Semafory* posiadają kolejki a wątki budzone są od tych najdłużej czekających z najwyższymi priorytetami.

*Zmienne warunkowe* (Mesa, nie Hoare) – proces o wysokim priorytecie nie traci blokady robiąc `signal`. *???*

***

# Zadanie 2

### Podaj implementację (w języku C) <u>*semafora*</u> z operacjami `init`, `wait` oraz `post` używając wyłącznie muteksów i zmiennych warunkowych standardu `POSIX.1`. Pamiętaj, że wartość semafora musi być zawsze nieujemna.

```C
typedef struct sem_t {
    int count;
    pthread_mutex_t m;
    pthread_condition_t cv;
} sem_t;

void sem_init(sem_t* s, int value) {
    s->count = value;
    pthread_mutex_init(&s->m, NULL);
    pthread_cond_init(&s->cv, NULL);
}

void sem_post(sem_t *s) {
    pthread_mutex_lock(&s->m);
    s->count++;
    pthread_cond_signal(*s->cv); // budzimy śpiący wątek
    pthread_mutex_unlock(&s->m);
}

void sem_wait(sem_t* s) {
    pthread_mutex_lock(&s->m);
    while (s->count == 0) {
        pthread_cond_wait(&s->cv, &s->m);
    }
    s->count--;
    pthread_mutex_unlock(&s->m);
}

```
***















