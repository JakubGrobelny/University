#ifndef __CSAPP_H__
#define __CSAPP_H__

#include <sys/types.h>
#include <sys/mman.h>
#ifdef LINUX
#include <sys/prctl.h>
#endif
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/wait.h>
#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <setjmp.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdnoreturn.h>
#include <string.h>
#include <unistd.h>

#define __unused __attribute__((unused))

extern char **environ;

/* Our own error-handling functions */
void unix_error(const char *fmt, ...) __attribute__((format(printf, 1, 2)));
void app_error(const char *fmt, ...) __attribute__((format(printf, 1, 2)));

/* Signal safe I/O functions */
void safe_printf(const char *fmt, ...) __attribute__((format(printf, 1, 2)));
void safe_error(const char *fmt, ...) __attribute__((format(printf, 1, 2)));

/* Process control wrappers */
pid_t Fork(void);
pid_t Waitpid(pid_t pid, int *iptr, int options);
#define Wait(iptr) Waitpid(-1, iptr, 0)
void Prctl(int option, long arg);

/* Signal control wrappers */
void (*Signal(int sig, void (*func)(int)))(int);
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
ssize_t Write(int fd, const void *buf, size_t count);
off_t Lseek(int fildes, off_t offset, int whence);
void Close(int fd);
int Dup2(int oldfd, int newfd);
void Pipe(int fds[2]);

/* File metadata access wrapper */
void Fstat(int fd, struct stat *statbuf);
size_t Readlink(const char *pathname, char *buf, size_t bufsiz);

/* Memory mapped files & anonymous memory */
void *Mmap(void *addr, size_t length, int prot, int flags, int fd,
           off_t offset);
void Mprotect(void *addr, size_t len, int prot);

/* Setjmp & longjmp implementation without sigprocmask */
typedef struct {
  long rbx;
  long rbp;
  long r12;
  long r13;
  long r14;
  long r15;
  void *rsp;
  void *rip;
} Jmpbuf[1];

int Setjmp(Jmpbuf env);
noreturn void Longjmp(Jmpbuf env, int val);

#endif /* __CSAPP_H__ */
