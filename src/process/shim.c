#define _GNU_SOURCE
#include <dlfcn.h>
#include <errno.h>
#include <fcntl.h>
#include <limits.h>
#include <poll.h>
#include <pthread.h>
#include <signal.h>
#include <spawn.h>
#include <stdatomic.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/eventfd.h>
#include <sys/ioctl.h>
#include <sys/stat.h>
#include <sys/syscall.h>
#include <sys/wait.h>
#include <termios.h>
#include <unistd.h>

#define EXPORT __attribute__((visibility("default")))

extern char **environ;

#define NEGATED(e) (-(int64_t)(e))

/* A stream is one end of a pipe or a pty master. A call in flight keeps the
   descriptor open: closing from another thread wakes the call through the
   eventfd, and the last one out closes the descriptor, so a blocked call
   never reads a number the kernel has given to another file. */
typedef struct {
  pthread_mutex_t mutex;
  int fd;
  int wake;
  int pty;
  int users;
  atomic_int closing;
} stream;

static int close_fd(int fd) {
  return fd >= 0 ? close(fd) : 0;
}

EXPORT stream *chez_process_stream_new(int fd, int pty) {
  stream *s = malloc(sizeof *s);
  if (!s) return NULL;
  int wake = eventfd(0, EFD_CLOEXEC | EFD_NONBLOCK);
  if (wake < 0) {
    free(s);
    return NULL;
  }
  pthread_mutex_init(&s->mutex, NULL);
  s->fd = fd;
  s->wake = wake;
  s->pty = pty;
  s->users = 0;
  atomic_init(&s->closing, 0);
  return s;
}

static int enter(stream *s) {
  pthread_mutex_lock(&s->mutex);
  int ok = !atomic_load(&s->closing);
  if (ok) s->users++;
  pthread_mutex_unlock(&s->mutex);
  return ok;
}

static void leave(stream *s) {
  pthread_mutex_lock(&s->mutex);
  if (--s->users == 0 && atomic_load(&s->closing)) {
    close_fd(s->fd);
    s->fd = -1;
  }
  pthread_mutex_unlock(&s->mutex);
}

EXPORT void chez_process_stream_close(stream *s) {
  pthread_mutex_lock(&s->mutex);
  if (!atomic_load(&s->closing)) {
    atomic_store(&s->closing, 1);
    uint64_t one = 1;
    ssize_t ignored = write(s->wake, &one, sizeof one);
    (void)ignored;
    if (s->users == 0) {
      close_fd(s->fd);
      s->fd = -1;
    }
  }
  pthread_mutex_unlock(&s->mutex);
}

EXPORT void chez_process_stream_free(stream *s) {
  chez_process_stream_close(s);
  close_fd(s->wake);
  pthread_mutex_destroy(&s->mutex);
  free(s);
}

static int await(stream *s, short events) {
  struct pollfd fds[2] = {{s->fd, events, 0}, {s->wake, POLLIN, 0}};
  for (;;) {
    if (atomic_load(&s->closing)) return 0;
    int n = poll(fds, 2, -1);
    if (n < 0) {
      if (errno == EINTR) continue;
      return -errno;
    }
    if (fds[1].revents) return 0;
    if (fds[0].revents) return 1;
  }
}

#define CLOSED INT64_MIN

/* A pty master reports the end of its session as EIO, which is its end of
   file. */
EXPORT int64_t chez_process_stream_read(stream *s, void *buffer, size_t count) {
  if (!enter(s)) return CLOSED;
  int64_t result;
  for (;;) {
    if (atomic_load(&s->closing)) {
      result = CLOSED;
      break;
    }
    ssize_t n = read(s->fd, buffer, count);
    if (n >= 0) {
      result = n;
      break;
    }
    if (errno == EINTR) continue;
    if (errno == EIO && s->pty) {
      result = 0;
      break;
    }
    if (errno != EAGAIN) {
      result = NEGATED(errno);
      break;
    }
    int ready = await(s, POLLIN);
    if (ready <= 0) {
      result = ready == 0 ? CLOSED : ready;
      break;
    }
  }
  leave(s);
  return result;
}

EXPORT int64_t chez_process_stream_write(stream *s, const void *buffer, size_t count) {
  if (!enter(s)) return CLOSED;
  int64_t result;
  for (;;) {
    if (atomic_load(&s->closing)) {
      result = CLOSED;
      break;
    }
    ssize_t n = write(s->fd, buffer, count);
    if (n >= 0) {
      result = n;
      break;
    }
    if (errno == EINTR) continue;
    if (errno != EAGAIN) {
      result = NEGATED(errno);
      break;
    }
    int ready = await(s, POLLOUT);
    if (ready <= 0) {
      result = ready == 0 ? CLOSED : ready;
      break;
    }
  }
  leave(s);
  return result;
}

static int set_nonblocking(int fd) {
  int flags = fcntl(fd, F_GETFL);
  return flags < 0 ? -1 : fcntl(fd, F_SETFL, flags | O_NONBLOCK);
}

/* A spec gathers what Scheme passes, one call each, so that no call needs
   more than a few arguments. */

enum { STDIO_INHERIT, STDIO_NULL, STDIO_PIPE, STDIO_FD, STDIO_STDOUT, STDIO_PTY };

typedef struct {
  char **argv;
  size_t argc, arg_capacity;
  char **envp;
  size_t envc, env_capacity;
  int inherit_environment;
  char *directory;
  char *program;
  char *pty;
  int kind[3], fd[3];
  int session;
  int group;
  int result[5];
} spec;

EXPORT spec *chez_process_spec_new(void) {
  spec *s = calloc(1, sizeof *s);
  if (!s) return NULL;
  s->inherit_environment = 1;
  s->group = -1;
  for (int i = 0; i < 3; i++) s->fd[i] = -1;
  return s;
}

static void free_strings(char **v, size_t n) {
  for (size_t i = 0; i < n; i++) free(v[i]);
  free(v);
}

EXPORT void chez_process_spec_free(spec *s) {
  free_strings(s->argv, s->argc);
  free_strings(s->envp, s->envc);
  free(s->directory);
  free(s->program);
  free(s->pty);
  free(s);
}

static int push(char ***v, size_t *n, size_t *capacity, const char *string) {
  if (*n + 2 > *capacity) {
    size_t c = *capacity ? *capacity * 2 : 8;
    char **grown = realloc(*v, c * sizeof **v);
    if (!grown) return -1;
    *v = grown;
    *capacity = c;
  }
  char *copy = strdup(string);
  if (!copy) return -1;
  (*v)[(*n)++] = copy;
  (*v)[*n] = NULL;
  return 0;
}

static int replace(char **field, const char *string) {
  char *copy = strdup(string);
  if (!copy) return -1;
  free(*field);
  *field = copy;
  return 0;
}

EXPORT int chez_process_spec_argument(spec *s, const char *a) {
  return push(&s->argv, &s->argc, &s->arg_capacity, a);
}

EXPORT int chez_process_spec_environment(spec *s, const char *binding) {
  s->inherit_environment = 0;
  return push(&s->envp, &s->envc, &s->env_capacity, binding);
}

EXPORT void chez_process_spec_empty_environment(spec *s) {
  s->inherit_environment = 0;
}

EXPORT int chez_process_spec_program(spec *s, const char *p) {
  return replace(&s->program, p);
}

EXPORT int chez_process_spec_directory(spec *s, const char *d) {
  return replace(&s->directory, d);
}

EXPORT int chez_process_spec_pty(spec *s, const char *path) {
  return replace(&s->pty, path);
}

EXPORT void chez_process_spec_stdio(spec *s, int which, int kind, int fd) {
  s->kind[which] = kind;
  s->fd[which] = fd;
}

EXPORT void chez_process_spec_session(spec *s, int session, int group) {
  s->session = session;
  s->group = group;
}

static const char *lookup(char **envp, const char *name) {
  size_t n = strlen(name);
  for (char **e = envp; e && *e; e++) {
    if (strncmp(*e, name, n) == 0 && (*e)[n] == '=') return *e + n + 1;
  }
  return NULL;
}

/* PATH is searched in the child's environment, as execvp would search it in
   the child. A file found but not executable is remembered, as execvp
   reports EACCES rather than ENOENT then. */
static int resolve(const spec *s, char **out) {
  if (strchr(s->program, '/')) return (*out = strdup(s->program)) ? 0 : ENOMEM;
  if (!*s->program) return ENOENT;
  const char *path = lookup(s->inherit_environment ? environ : s->envp, "PATH");
  if (!path) path = "/bin:/usr/bin";
  int denied = 0;
  size_t length = strlen(s->program);
  for (const char *p = path;; p++) {
    const char *end = strchrnul(p, ':');
    size_t dir = end - p;
    char *candidate = malloc(dir + length + 3);
    if (!candidate) return ENOMEM;
    if (dir == 0) {
      memcpy(candidate, "./", 2);
      dir = 2;
    } else {
      memcpy(candidate, p, dir);
      candidate[dir++] = '/';
    }
    memcpy(candidate + dir, s->program, length + 1);
    struct stat st;
    if (stat(candidate, &st) == 0 && S_ISREG(st.st_mode)) {
      if (access(candidate, X_OK) == 0) {
        *out = candidate;
        return 0;
      }
      denied = 1;
    }
    free(candidate);
    if (!*end) break;
    p = end;
  }
  return denied ? EACCES : ENOENT;
}

typedef int (*pidfd_spawn_function)(int *, const char *, const posix_spawn_file_actions_t *,
                                    const posix_spawnattr_t *, char *const[], char *const[]);
typedef pid_t (*pidfd_getpid_function)(int);

/* Found at run time, so that the shared object also loads on a glibc older
   than 2.39, which first had pidfd_spawn. The lookup names libc's handle:
   RTLD_DEFAULT would resolve by the caller's address, which a tail call or
   a constructor run by the dynamic loader makes the loader's own. */
static pidfd_spawn_function pidfd_spawn_entry;
static pidfd_getpid_function pidfd_getpid_entry;
static pthread_once_t pidfd_spawn_once = PTHREAD_ONCE_INIT;

static void find_pidfd_spawn(void) {
  void *libc = dlopen("libc.so.6", RTLD_NOW | RTLD_NOLOAD);
  if (!libc) return;
  pidfd_spawn_function spawn = (pidfd_spawn_function)dlsym(libc, "pidfd_spawn");
  pidfd_getpid_function getpid_of = (pidfd_getpid_function)dlsym(libc, "pidfd_getpid");
  if (spawn && getpid_of) {
    pidfd_spawn_entry = spawn;
    pidfd_getpid_entry = getpid_of;
  }
  dlclose(libc);
}

EXPORT int chez_process_uses_pidfd_spawn(void) {
  pthread_once(&pidfd_spawn_once, find_pidfd_spawn);
  return pidfd_spawn_entry != NULL;
}

static int pidfd_open(pid_t pid) {
  return (int)syscall(SYS_pidfd_open, pid, 0);
}

EXPORT int chez_process_spec_result(spec *s, int i) {
  return s->result[i];
}

/* The result lands in the spec, which is C memory, so that this call can
   let collections run while it searches PATH and waits for the exec. */
EXPORT int chez_process_spec_spawn(spec *s) {
  int *out = s->result;
  int pipes[3][2] = {{-1, -1}, {-1, -1}, {-1, -1}};
  posix_spawn_file_actions_t actions;
  posix_spawnattr_t attributes;
  char *path = NULL;
  int error = resolve(s, &path);
  if (error) return error;
  if ((error = posix_spawn_file_actions_init(&actions))) {
    free(path);
    return error;
  }
  if ((error = posix_spawnattr_init(&attributes))) {
    posix_spawn_file_actions_destroy(&actions);
    free(path);
    return error;
  }

  for (int i = 0; i < 3 && !error; i++) {
    if (s->kind[i] == STDIO_PIPE) {
      if (pipe2(pipes[i], O_CLOEXEC) < 0) error = errno;
    }
  }

  /* Opening the slave as a session leader with no controlling terminal
     makes it the controlling terminal; glibc applies setsid before the file
     actions. Standard input is wired last, so that the slave is on 0 while
     standard output and error are taken from it. */
  if (!error && s->pty)
    error = posix_spawn_file_actions_addopen(&actions, 0, s->pty, O_RDWR, 0);
  static const int order[3] = {1, 2, 0};
  for (int k = 0; k < 3 && !error; k++) {
    int i = order[k];
    switch (s->kind[i]) {
      case STDIO_INHERIT:
        break;
      case STDIO_NULL:
        error = posix_spawn_file_actions_addopen(&actions, i, "/dev/null",
                                                 i == 0 ? O_RDONLY : O_WRONLY, 0);
        break;
      case STDIO_PIPE:
        error = posix_spawn_file_actions_adddup2(&actions, pipes[i][i == 0 ? 0 : 1], i);
        break;
      case STDIO_FD:
        error = posix_spawn_file_actions_adddup2(&actions, s->fd[i], i);
        break;
      case STDIO_STDOUT:
        error = posix_spawn_file_actions_adddup2(&actions, 1, 2);
        break;
      case STDIO_PTY:
        if (i != 0) error = posix_spawn_file_actions_adddup2(&actions, 0, i);
        break;
    }
  }
  if (!error) error = posix_spawn_file_actions_addclosefrom_np(&actions, 3);
  if (!error && s->directory) error = posix_spawn_file_actions_addchdir_np(&actions, s->directory);

  /* Chez ignores SIGPIPE, and an ignored signal stays ignored across exec;
     a mask blocked for signalfd would be inherited too. */
  short flags = POSIX_SPAWN_SETSIGDEF | POSIX_SPAWN_SETSIGMASK;
  sigset_t all, none;
  sigfillset(&all);
  sigemptyset(&none);
  if (!error) error = posix_spawnattr_setsigdefault(&attributes, &all);
  if (!error) error = posix_spawnattr_setsigmask(&attributes, &none);
  if (s->session) {
    flags |= POSIX_SPAWN_SETSID;
  } else if (s->group >= 0) {
    flags |= POSIX_SPAWN_SETPGROUP;
    if (!error) error = posix_spawnattr_setpgroup(&attributes, s->group);
  }
  if (!error) error = posix_spawnattr_setflags(&attributes, flags);

  char **envp = s->inherit_environment ? environ : (s->envp ? s->envp : (char *[]){NULL});
  pid_t pid = -1;
  int pidfd = -1;
  if (!error) {
    pthread_once(&pidfd_spawn_once, find_pidfd_spawn);
    if (pidfd_spawn_entry) {
      error = pidfd_spawn_entry(&pidfd, path, &actions, &attributes, s->argv, envp);
      if (!error && (pid = pidfd_getpid_entry(pidfd)) < 0) {
        error = errno;
        siginfo_t info;
        syscall(SYS_pidfd_send_signal, pidfd, SIGKILL, NULL, 0);
        while (waitid(P_PIDFD, pidfd, &info, WEXITED) < 0 && errno == EINTR) {}
        close(pidfd);
        pidfd = -1;
      }
    } else {
      error = posix_spawn(&pid, path, &actions, &attributes, s->argv, envp);
      /* The child cannot be reaped before this, so its pid is not reused. A
         child no pidfd can follow is ended here rather than left a zombie. */
      if (!error && (pidfd = pidfd_open(pid)) < 0) {
        error = errno;
        kill(pid, SIGKILL);
        while (waitpid(pid, NULL, 0) < 0 && errno == EINTR) {}
      }
    }
  }

  posix_spawn_file_actions_destroy(&actions);
  posix_spawnattr_destroy(&attributes);
  free(path);

  for (int i = 0; i < 3; i++) {
    int child_end = i == 0 ? 0 : 1;
    close_fd(pipes[i][child_end]);
    int parent_end = pipes[i][1 - child_end];
    if (error) {
      close_fd(parent_end);
      parent_end = -1;
    } else if (parent_end >= 0) {
      set_nonblocking(parent_end);
    }
    out[2 + i] = parent_end;
  }
  out[0] = pid;
  out[1] = pidfd;
  return error;
}

EXPORT int chez_process_await(int pidfd, int timeout) {
  struct pollfd fd = {pidfd, POLLIN, 0};
  for (;;) {
    int n = poll(&fd, 1, timeout);
    if (n >= 0) return n;
    if (errno != EINTR) return -errno;
  }
}

EXPORT int chez_process_reap(int pidfd, int *out) {
  siginfo_t info;
  memset(&info, 0, sizeof info);
  int r;
  do r = waitid(P_PIDFD, pidfd, &info, WEXITED | WNOHANG);
  while (r < 0 && errno == EINTR);
  if (r < 0) return errno;
  switch (info.si_code) {
    case CLD_EXITED: out[0] = 1; break;
    case CLD_KILLED: out[0] = 2; break;
    case CLD_DUMPED: out[0] = 3; break;
    default: out[0] = 0; break;
  }
  if (info.si_pid == 0) out[0] = 0;
  out[1] = info.si_status;
  return 0;
}

EXPORT int chez_process_signal(int pidfd, int signal) {
  return syscall(SYS_pidfd_send_signal, pidfd, signal, NULL, 0) < 0 ? errno : 0;
}

EXPORT int chez_process_signal_group(int group, int signal) {
  return kill(-group, signal) < 0 ? errno : 0;
}

EXPORT int chez_process_own_group(void) {
  return getpgrp();
}

static const int signal_numbers[] = {SIGHUP,  SIGINT,  SIGQUIT, SIGKILL, SIGUSR1, SIGUSR2,
                                     SIGTERM, SIGCONT, SIGSTOP, SIGTSTP, SIGTTIN, SIGTTOU,
                                     SIGWINCH, SIGALRM, SIGPIPE, SIGCHLD, SIGABRT, SIGSEGV};

EXPORT int chez_process_signal_number(int index) {
  return index >= 0 && index < (int)(sizeof signal_numbers / sizeof *signal_numbers)
             ? signal_numbers[index]
             : -1;
}

EXPORT int chez_process_signal_count(void) {
  return NSIG - 1;
}

/* Input is written while both outputs are read, so that neither side waits
   on a full pipe. */

typedef struct {
  char *data;
  size_t size, capacity;
} buffer;

static int grow(buffer *b, size_t need) {
  if (b->size + need <= b->capacity) return 0;
  size_t c = b->capacity ? b->capacity : 65536;
  while (c < b->size + need) c *= 2;
  char *data = realloc(b->data, c);
  if (!data) return ENOMEM;
  b->data = data;
  b->capacity = c;
  return 0;
}

typedef struct {
  buffer out, err;
} exchange;

EXPORT int chez_process_exchange(stream *in, const char *input, size_t input_size, stream *out,
                                 stream *err, exchange **result) {
  exchange *x = calloc(1, sizeof *x);
  if (!x) return ENOMEM;
  *result = x;
  stream *streams[3] = {in, out, err};
  int entered[3] = {0, 0, 0};
  int open[3];
  int error = 0;
  for (int i = 0; i < 3; i++) {
    entered[i] = streams[i] && enter(streams[i]);
    open[i] = entered[i];
  }
  size_t written = 0;
  for (;;) {
    /* The child sees end of file only once its input is closed. */
    if (open[0] && written == input_size) open[0] = 0;
    if (entered[0] && !open[0]) {
      leave(streams[0]);
      entered[0] = 0;
      chez_process_stream_close(streams[0]);
    }
    if (error || !(open[0] || open[1] || open[2])) break;
    struct pollfd fds[6];
    int map[6], n = 0;
    for (int i = 0; i < 3; i++) {
      if (!open[i]) continue;
      fds[n] = (struct pollfd){streams[i]->fd, i == 0 ? POLLOUT : POLLIN, 0};
      map[n++] = i;
      fds[n] = (struct pollfd){streams[i]->wake, POLLIN, 0};
      map[n++] = -1 - i;
    }
    int r = poll(fds, n, -1);
    if (r < 0) {
      if (errno == EINTR) continue;
      error = errno;
      break;
    }
    for (int k = 0; k < n && !error; k++) {
      if (!fds[k].revents) continue;
      int i = map[k];
      if (i < 0) {
        open[-1 - i] = 0;
        continue;
      }
      if (!open[i]) continue;
      if (i == 0) {
        ssize_t w = write(streams[0]->fd, input + written, input_size - written);
        if (w >= 0) {
          written += w;
        } else if (errno == EPIPE) {
          open[0] = 0;
        } else if (errno != EAGAIN && errno != EINTR) {
          error = errno;
        }
      } else {
        buffer *b = i == 1 ? &x->out : &x->err;
        for (;;) {
          if ((error = grow(b, 65536))) break;
          ssize_t got = read(streams[i]->fd, b->data + b->size, b->capacity - b->size);
          if (got > 0) {
            b->size += got;
            continue;
          }
          if (got == 0 || (errno == EIO && streams[i]->pty)) {
            open[i] = 0;
          } else if (errno != EAGAIN && errno != EINTR) {
            error = errno;
          }
          break;
        }
      }
    }
  }
  for (int i = 0; i < 3; i++) {
    if (entered[i]) leave(streams[i]);
  }
  return error;
}

EXPORT void chez_process_copy_in(void *to, const void *from, size_t size) {
  memcpy(to, from, size);
}

EXPORT size_t chez_process_exchange_size(exchange *x, int which) {
  return which == 1 ? x->out.size : x->err.size;
}

EXPORT void chez_process_exchange_take(exchange *x, int which, void *into) {
  buffer *b = which == 1 ? &x->out : &x->err;
  if (b->size) memcpy(into, b->data, b->size);
}

EXPORT void chez_process_exchange_free(exchange *x) {
  if (!x) return;
  free(x->out.data);
  free(x->err.data);
  free(x);
}

/* The slave is never opened here, so that the session's end reaches the
   master once the child's copies are gone. */

EXPORT int chez_process_pty_open(int rows, int columns, int x, int y, int *out, char *name,
                                 size_t name_size) {
  int master = posix_openpt(O_RDWR | O_NOCTTY | O_CLOEXEC);
  if (master < 0) return errno;
  if (grantpt(master) < 0 || unlockpt(master) < 0 || ptsname_r(master, name, name_size) != 0) {
    int e = errno;
    close(master);
    return e;
  }
  struct winsize size = {(unsigned short)rows, (unsigned short)columns, (unsigned short)x,
                         (unsigned short)y};
  if (ioctl(master, TIOCSWINSZ, &size) < 0) {
    int e = errno;
    close(master);
    return e;
  }
  int writer = fcntl(master, F_DUPFD_CLOEXEC, 0);
  if (writer < 0 || set_nonblocking(master) < 0) {
    int e = errno;
    close(master);
    close_fd(writer);
    return e;
  }
  out[0] = master;
  out[1] = writer;
  return 0;
}

EXPORT int chez_process_pty_resize(stream *s, int rows, int columns, int x, int y) {
  if (!enter(s)) return EBADF;
  struct winsize size = {(unsigned short)rows, (unsigned short)columns, (unsigned short)x,
                         (unsigned short)y};
  int r = ioctl(s->fd, TIOCSWINSZ, &size) < 0 ? errno : 0;
  leave(s);
  return r;
}

EXPORT int chez_process_pty_size(stream *s, int *out) {
  if (!enter(s)) return EBADF;
  struct winsize size;
  int r = ioctl(s->fd, TIOCGWINSZ, &size) < 0 ? errno : 0;
  leave(s);
  if (!r) {
    out[0] = size.ws_row;
    out[1] = size.ws_col;
    out[2] = size.ws_xpixel;
    out[3] = size.ws_ypixel;
  }
  return r;
}

EXPORT int chez_process_pty_foreground(stream *s) {
  if (!enter(s)) return -EBADF;
  pid_t g = tcgetpgrp(s->fd);
  int r = g < 0 ? -errno : g;
  leave(s);
  return r;
}

/* Probing what the library needs from the kernel, so that an old one fails
   when the library loads rather than at the first spawn. */
EXPORT int chez_process_probe(void) {
  int fd = pidfd_open(getpid());
  if (fd < 0) return errno;
  siginfo_t info;
  /* Linux before 5.4 does not know P_PIDFD and says EINVAL; a later one
     says ECHILD, since this process is not its own child. */
  int r = waitid(P_PIDFD, fd, &info, WEXITED | WNOHANG);
  int e = r < 0 && errno != ECHILD ? errno : 0;
  close(fd);
  return e;
}

EXPORT void chez_process_strerror(int e, char *into, size_t size) {
  char message[256];
  snprintf(into, size, "%s", strerror_r(e, message, sizeof message));
}
