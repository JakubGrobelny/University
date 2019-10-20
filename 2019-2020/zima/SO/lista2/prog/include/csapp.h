#ifndef __CSAPP_H__
#define __CSAPP_H__

#include <sys/types.h>
#include <sys/wait.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

/* Our own error-handling functions */
void unix_error(const char *fmt, ...) __attribute__((format(printf, 1, 2)));
void app_error(const char *fmt, ...) __attribute__((format(printf, 1, 2)));

/* Process control wrappers */
pid_t Fork(void);
pid_t Waitpid(pid_t pid, int *iptr, int options);

/* Unix I/O wrappers */
int Open(const char *pathname, int flags, mode_t mode);
ssize_t Read(int fd, void *buf, size_t count);
off_t Lseek(int fildes, off_t offset, int whence);
void Close(int fd);

#endif /* __CSAPP_H__ */
