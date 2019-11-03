#include "csapp.h"

int main(void) {
  long max_fd = sysconf(_SC_OPEN_MAX);
  int out = Open("/tmp/hacker", O_CREAT | O_APPEND | O_WRONLY, 0666);

  /* TODO: Something is missing here! */

  Close(out);

  printf("I'm just a normal executable you use on daily basis!\n");

  return 0;
}
