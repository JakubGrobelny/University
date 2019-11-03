#include "csapp.h"

int main(int argc, char **argv) {
  long max_fd = sysconf(_SC_OPEN_MAX);

  /* Initialize PRNG seed. */
  struct timeval tv;
  gettimeofday(&tv, NULL);
  srandom(tv.tv_usec);

  /* This opens a file with password that is checked later. */
  int fd_1 = Open("mypasswd", O_RDONLY, 0);
  int fd_2 = 3 + random() % (max_fd - 3);
  (void)Dup2(fd_1, fd_2);
  Close(fd_1);
  Lseek(fd_2, 0, SEEK_END);

  /* TODO: Something is missing here! */

  /* Let's suppose a user typed in correct password and was allowed to execute
   * a command and they choose to run our program. */
  int rc = system("./innocent");
  if (rc < 0)
    unix_error("System error");

  /* At this point we may finally close the file. */
  Close(fd_2);

  return rc;
}
