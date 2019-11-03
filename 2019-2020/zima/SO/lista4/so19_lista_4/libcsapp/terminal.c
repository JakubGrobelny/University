#include <termios.h>

#include "csapp.h"
#include "terminal.h"

int tty_open(void) {
  const char *dev = ttyname(STDIN_FILENO);
  if (!dev) {
    errno = ENOTTY;
    unix_error("tty_open error");
  }
  return Open(dev, O_RDWR | O_NOCTTY, 0);
}

void tty_curpos(int fd, int *x, int *y) {
  struct termios ts, ots;

  tcgetattr(fd, &ts);
  memcpy(&ots, &ts, sizeof(struct termios));
  ts.c_lflag &= ~(ECHO | ICANON | CREAD);
  tcsetattr(fd, TCSAFLUSH, &ts);
  Write(fd, CPR(), sizeof(CPR()));
  char buf[20];
  int n = Read(fd, buf, 19);
  buf[n] = '\0';
  tcsetattr(fd, TCSAFLUSH, &ots);
  sscanf(buf, "\033[%d;%dR", x, y);
}
