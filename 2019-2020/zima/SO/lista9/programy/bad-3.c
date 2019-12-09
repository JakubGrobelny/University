#include "../../lib/include/csapp.h"

#define M 10
#define N 20

int main(void) {
  int *x = malloc(N * sizeof(int));
  memset(x, 0xf0, N * sizeof(int));
  free(x);
  int *y = malloc(M * sizeof(int));
  memset(y, 0x0f, N * sizeof(int));
  free(x);
  return 0;
}
