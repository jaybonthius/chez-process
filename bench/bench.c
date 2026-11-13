#define _GNU_SOURCE
#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <spawn.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/pidfd.h>
#include <sys/wait.h>
#include <time.h>
#include <unistd.h>

extern char **environ;

static double now(void) {
  struct timespec t;
  clock_gettime(CLOCK_MONOTONIC, &t);
  return t.tv_sec + t.tv_nsec / 1e9;
}

static void die(const char *what, int e) {
  fprintf(stderr, "%s: %s\n", what, strerror(e));
  exit(1);
}

static int spawn(char **argv, int pipes, int pty, int *out) {
  posix_spawn_file_actions_t actions;
  posix_spawnattr_t attributes;
  posix_spawn_file_actions_init(&actions);
  posix_spawnattr_init(&attributes);
  int ends[3][2];
  int master = -1;
  if (pty) {
    master = posix_openpt(O_RDWR | O_NOCTTY | O_CLOEXEC);
    grantpt(master);
    unlockpt(master);
    char name[64];
    ptsname_r(master, name, sizeof name);
    posix_spawn_file_actions_addopen(&actions, 0, name, O_RDWR, 0);
    posix_spawn_file_actions_adddup2(&actions, 0, 1);
    posix_spawn_file_actions_adddup2(&actions, 0, 2);
    posix_spawnattr_setflags(&attributes, POSIX_SPAWN_SETSID);
  } else if (!pipes) {
    for (int i = 0; i < 3; i++)
      posix_spawn_file_actions_addopen(&actions, i, "/dev/null", i == 0 ? O_RDONLY : O_WRONLY, 0);
  } else {
    for (int i = 0; i < 3; i++) {
      if (pipe2(ends[i], O_CLOEXEC) < 0) die("pipe2", errno);
      posix_spawn_file_actions_adddup2(&actions, ends[i][i == 0 ? 0 : 1], i);
    }
  }
  posix_spawn_file_actions_addclosefrom_np(&actions, 3);
  int pidfd;
  int e = pidfd_spawn(&pidfd, argv[0], &actions, &attributes, argv, environ);
  if (e) die("pidfd_spawn", e);
  posix_spawn_file_actions_destroy(&actions);
  posix_spawnattr_destroy(&attributes);
  if (pty) {
    *out = master;
  } else if (pipes) {
    for (int i = 0; i < 3; i++) close(ends[i][i == 0 ? 0 : 1]);
    close(ends[0][1]);
    close(ends[2][0]);
    *out = ends[1][0];
  }
  return pidfd;
}

static void reap(int pidfd) {
  siginfo_t info;
  if (waitid(P_PIDFD, pidfd, &info, WEXITED) < 0) die("waitid", errno);
  close(pidfd);
}

static long drain(int fd) {
  static char buffer[65536];
  long total = 0;
  for (;;) {
    ssize_t n = read(fd, buffer, sizeof buffer);
    if (n > 0) total += n;
    else if (n == 0 || errno == EIO) break;
    else if (errno != EINTR) die("read", errno);
  }
  close(fd);
  return total;
}

int main(int argc, char **argv) {
  if (argc != 4) {
    fprintf(stderr, "usage: %s spawn-count pipe-bytes pty-bytes\n", argv[0]);
    return 2;
  }
  int count = atoi(argv[1]);
  char *bytes = argv[2];
  char *true_argv[] = {"/bin/true", NULL};

  for (int i = 0; i < 100; i++) reap(spawn(true_argv, 0, 0, NULL));
  double start = now();
  for (int i = 0; i < count; i++) reap(spawn(true_argv, 0, 0, NULL));
  printf("spawn-null %.9f\n", (now() - start) / count);

  start = now();
  for (int i = 0; i < count; i++) {
    int out;
    int pidfd = spawn(true_argv, 1, 0, &out);
    drain(out);
    reap(pidfd);
  }
  printf("spawn-pipes %.9f\n", (now() - start) / count);

  char *head_argv[] = {"/usr/bin/head", "-c", bytes, "/dev/zero", NULL};
  int out;
  start = now();
  int pidfd = spawn(head_argv, 1, 0, &out);
  long total = drain(out);
  reap(pidfd);
  printf("pipe %.9f %ld\n", now() - start, total);

  head_argv[2] = argv[3];
  start = now();
  pidfd = spawn(head_argv, 0, 1, &out);
  total = drain(out);
  reap(pidfd);
  printf("pty %.9f %ld\n", now() - start, total);
  return 0;
}
