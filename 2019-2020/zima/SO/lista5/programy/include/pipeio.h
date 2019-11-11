#ifndef _PIPEIO_H_
#define _PIPEIO_H_

#include "csapp.h"

typedef struct {
  int read;
  int write;
} pipe_t;

static inline pipe_t MakePipe(void) {
  int fds[2];
  Pipe(fds);
  return (pipe_t){.read = fds[0], .write = fds[1]};
}

static inline void CloseReadEnd(pipe_t p) {
  Close(p.read);
}

static inline void CloseWriteEnd(pipe_t p) {
  Close(p.write);
}

#endif /* !_PIPEIO_H_ */
