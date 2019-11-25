#ifndef __CSAPP_H__
#define __CSAPP_H__

#include <sys/types.h>
#include <sys/mman.h>
#ifdef LINUX
#include <sys/sysmacros.h>
#include <sys/prctl.h>
#endif
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/time.h>
#include <sys/uio.h>
#include <sys/wait.h>
#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <fcntl.h>
#include <grp.h>
#include <limits.h>
#include <pwd.h>
#include <setjmp.h>
#include <signal.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdbool.h>
#include <stdnoreturn.h>
#include <string.h>
#include <time.h>
#include <unistd.h>

#define min(a, b)                                                              \
  ({                                                                           \
    typeof(a) _a = (a);                                                        \
    typeof(b) _b = (b);                                                        \
    _a < _b ? _a : _b;                                                         \
  })

#define max(a, b)                                                              \
  ({                                                                           \
    typeof(a) _a = (a);                                                        \
    typeof(b) _b = (b);                                                        \
    _a > _b ? _a : _b;                                                         \
  })

#define powerof2(x) (((x) & ((x) - 1)) == 0)

#define __unused __attribute__((unused))

extern char **environ;

/* Our own error-handling functions */
noreturn void unix_error(const char *fmt, ...)
  __attribute__((format(printf, 1, 2)));
noreturn void app_error(const char *fmt, ...)
  __attribute__((format(printf, 1, 2)));

/* Signal safe I/O functions */
void safe_printf(const char *fmt, ...) __attribute__((format(printf, 1, 2)));
void safe_error(const char *fmt, ...) __attribute__((format(printf, 1, 2)));

/* Decent hashing function. */
#define HASHINIT 5381

uint32_t jenkins_hash(const void *key, size_t length, uint32_t initval);

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
size_t Read(int fd, void *buf, size_t count);
size_t Write(int fd, const void *buf, size_t count);
size_t Writev(int fd, const struct iovec *iov, int iovcnt);
off_t Lseek(int fildes, off_t offset, int whence);
void Close(int fd);
void Ftruncate(int fd, off_t length);
int Dup2(int oldfd, int newfd);
void Pipe(int fds[2]);
void Socketpair(int domain, int type, int protocol, int sv[2]);

/* Directory access (Linux specific) */
struct linux_dirent {
  unsigned long d_ino;     /* Inode number */
  unsigned long d_off;     /* Offset to next linux_dirent */
  unsigned short d_reclen; /* Length of this linux_dirent */
  char d_name[];           /* Filename (null-terminated) */
};

int Getdents(int fd, struct linux_dirent *dirp, unsigned count);

/* Directory operations */
void Rename(const char *oldpath, const char *newpath);
void Unlink(const char *pathname);

/* File metadata access wrapper */
void Fstat(int fd, struct stat *statbuf);
void Fstatat(int dirfd, const char *pathname, struct stat *statbuf, int flags);
size_t Readlink(const char *pathname, char *buf, size_t bufsiz);
size_t Readlinkat(int dirfd, const char *pathname, char *buf, size_t bufsiz);

/* Memory mapped files & anonymous memory */
void *Mmap(void *addr, size_t length, int prot, int flags, int fd,
           off_t offset);
void Mprotect(void *addr, size_t len, int prot);
void Munmap(void *addr, size_t len);
void Madvise(void *addr, size_t length, int advice);

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
