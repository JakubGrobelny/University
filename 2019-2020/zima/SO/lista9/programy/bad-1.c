#include "../../lib/include/csapp.h"

#define N 10
#define M 5

int main(void) {
  int **p = malloc(N * sizeof(int));
  for (int i = 0; i < N; i++)
    p[i] = malloc(M * sizeof(int));
  return 0;
}
