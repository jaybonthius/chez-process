# chez-process

Processes and pseudo-terminals for threaded Chez Scheme 10.4.1: spawn programs with argument vectors, an environment, a directory, pipes or a pty, then signal them and wait for their exit status, without blocking the collector.

```sh
make SCHEME=/path/to/chez-10.4.1/scheme build
make SCHEME=/path/to/chez-10.4.1/scheme test
make SCHEME=/path/to/chez-10.4.1/scheme sanitize
make SCHEME=/path/to/chez-10.4.1/scheme bench
```

Requires Linux 5.4 or later and glibc 2.34 or later.
