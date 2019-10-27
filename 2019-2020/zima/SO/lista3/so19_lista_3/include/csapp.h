#ifndef __CSAPP_H__
#define __CSAPP_H__

#include <sys/types.h>
#include <sys/mman.h>
#include <sys/prctl.h>
#include <sys/wait.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <unistd.h>

#define __unused __attribute__((unused))

/* Our own error-handling functions */
void unix_error(const char *fmt, ...) __attribute__((format(printf, 1, 2)));
void app_error(const char *fmt, ...) __attribute__((format(printf, 1, 2)));
void safe_printf(const char *fmt, ...) __attribute__((format(printf, 1, 2)));

/* Process control wrappers */
pid_t Fork(void);
pid_t Waitpid(pid_t pid, int *iptr, int options);
void Prctl(int option, long arg);

/* Signal control wrappers */
void Kill(pid_t pid, int sig);
void Sigprocmask(int how, const sigset_t *set, sigset_t *oldset);
void Sigaction(int signum, const struct sigaction *act,
               struct sigaction *oldact);
void Sigsuspend(const sigset_t *mask);

/* Process group control wrappers */
void Setpgid(pid_t pid, pid_t pgid);

/* Unix I/O wrappers */
int Open(const char *pathname, int flags, mode_t mode);
ssize_t Read(int fd, void *buf, size_t count);
off_t Lseek(int fildes, off_t offset, int whence);
void Close(int fd);

/* Memory mapped files & anonymous memory */
void *Mmap(void *addr, size_t length, int prot, int flags,
           int fd, off_t offset);
void Mprotect(void *addr, size_t len, int prot);

#endif /* __CSAPP_H__ */
