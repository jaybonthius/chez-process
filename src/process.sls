(library (process)
  (export
    &system-error system-error? system-error-errno system-error-operation
    &spawn-error spawn-error? spawn-error-program

    process-options make-process-options process-options? stdio signal-name

    spawn-process process? process-id process-group-id process-stdin process-stdout
    process-stderr process-wait process-status process-signal! process-group-signal!
    process-kill!

    exit-status? exit-status-code exit-status-signal exit-status-core-dumped?
    exit-status-success?

    run-process process-communicate process-result? process-result-status
    process-result-stdout process-result-stderr

    open-pty pty? pty-name pty-input-port pty-output-port pty-size pty-resize!
    pty-foreground-process-group pty-close!

    environment-variables)
  (import (except (chezscheme) process))

  ;; Loading on a system that lacks what the shim calls would otherwise fail
  ;; with the dynamic linker's message, or at the first spawn.
  (define minimum-glibc '(2 34))

  (define glibc
    (guard (c [(error? c)
               (errorf 'process "needs Linux with glibc ~{~a~^.~} or later" minimum-glibc)])
      (load-shared-object "libc.so.6")))

  (define (version-numbers text)
    (let loop ([i 0] [start 0] [acc '()])
      (cond
        [(= i (string-length text))
         (reverse (cons (string->number (substring text start i)) acc))]
        [(char=? (string-ref text i) #\.)
         (loop (+ i 1) (+ i 1) (cons (string->number (substring text start i)) acc))]
        [else (loop (+ i 1) start acc)])))

  (define glibc-checked
    (let ([version (version-numbers ((foreign-procedure "gnu_get_libc_version" () string)))])
      (unless (and (for-all number? version) (pair? (cdr version))
                   (or (> (car version) (car minimum-glibc))
                       (and (= (car version) (car minimum-glibc))
                            (>= (cadr version) (cadr minimum-glibc)))))
        (errorf 'process "needs glibc ~{~a~^.~} or later, not ~{~a~^.~}" minimum-glibc version))))

  (define shared-object
    (unless (foreign-entry? "chez_process_spec_new")
      (let ([name "process/libchez-process.so"])
        (let loop ([directories (library-directories)])
          (cond
            [(null? directories)
             (errorf 'process "~a is in none of the library directories ~s" name
               (library-directories))]
            [(file-exists? (path-build (cdar directories) name))
             (load-shared-object (path-build (cdar directories) name))]
            [else (loop (cdr directories))])))))

  (define-condition-type &system-error &error
    make-system-error system-error?
    (errno system-error-errno)
    (operation system-error-operation))

  (define-condition-type &spawn-error &system-error
    make-spawn-error spawn-error?
    (program spawn-error-program))

  (define %strerror (foreign-procedure "chez_process_strerror" (int u8* size_t) void))

  (define (bytevector-c-string bv)
    (let* ([n (let loop ([i 0]) (if (fx= (bytevector-u8-ref bv i) 0) i (loop (fx+ i 1))))]
           [s (make-bytevector n)])
      (bytevector-copy! bv 0 s 0 n)
      (utf8->string s)))

  (define (errno-message errno)
    (let ([bv (make-bytevector 256 0)])
      (%strerror errno bv (bytevector-length bv))
      (bytevector-c-string bv)))

  (define (raise-system-error who operation errno . irritants)
    (raise
      (condition
        (make-system-error errno operation)
        (make-who-condition who)
        (make-message-condition (format "~a failed: ~a" operation (errno-message errno)))
        (make-irritants-condition irritants))))

  (define kernel-checked
    (let ([errno ((foreign-procedure "chez_process_probe" () int))])
      (unless (fx= errno 0)
        (errorf 'process "needs Linux 5.4 or later for process file descriptors: ~a"
          (errno-message errno)))))

  (define-syntax check
    (syntax-rules ()
      [(_ who ok? x what)
       (let ([v x]) (unless (ok? v) (assertion-violationf who "~s is not ~a" v what)))]))

  (define (c-string? s)
    (and (string? s)
         (let loop ([i 0])
           (or (fx= i (string-length s))
               (and (not (char=? (string-ref s i) #\nul)) (loop (fx+ i 1)))))))

  (define (c-string s) (string->utf8 (string-append s "\x0;")))

  (define (environment-binding? b)
    (and (pair? b) (c-string? (car b)) (c-string? (cdr b))
         (fx> (string-length (car b)) 0)
         (not (memv #\= (string->list (car b))))))

  (define (c-string-at address)
    (let* ([n (let count ([n 0])
                (if (fx= (foreign-ref 'unsigned-8 address n) 0) n (count (fx+ n 1))))]
           [bv (make-bytevector n)])
      (do ([k 0 (fx+ k 1)]) ((fx= k n) (utf8->string bv))
        (bytevector-u8-set! bv k (foreign-ref 'unsigned-8 address k)))))

  (define (environment-variables)
    (let ([environ (foreign-ref 'uptr (foreign-entry "environ") 0)]
          [size (foreign-sizeof 'uptr)])
      (let loop ([i 0] [acc '()])
        (let ([entry (foreign-ref 'uptr environ (fx* i size))])
          (if (eqv? entry 0)
              (reverse acc)
              (let* ([binding (c-string-at entry)]
                     [eq (let find ([k 0])
                           (cond
                             [(fx= k (string-length binding)) #f]
                             [(char=? (string-ref binding k) #\=) k]
                             [else (find (fx+ k 1))]))])
                (loop (fx+ i 1)
                  (if eq
                      (cons (cons (substring binding 0 eq)
                                  (substring binding (fx+ eq 1) (string-length binding)))
                            acc)
                      acc))))))))

  (define-enumeration stdio (pipe inherit null stdout) stdio-modes)

  (define-enumeration signal-name
    (hup int quit kill usr1 usr2 term cont stop tstp ttin ttou winch alrm pipe chld abrt segv)
    signal-names)

  (define %signal-number (foreign-procedure "chez_process_signal_number" (int) int))
  (define signal-count ((foreign-procedure "chez_process_signal_count" () int)))
  (define signal-index (enum-set-indexer (signal-names)))

  (define (signal-number who signal)
    (cond
      [(and (fixnum? signal) (fx<= 1 signal signal-count)) signal]
      [(and (symbol? signal) (signal-index signal)) => %signal-number]
      [else (assertion-violationf who "~s is not a signal name or number" signal)]))

  (define %stream-new (foreign-procedure __errno "chez_process_stream_new" (int boolean) uptr))
  (define %stream-close (foreign-procedure "chez_process_stream_close" (uptr) void))
  (define %stream-free (foreign-procedure "chez_process_stream_free" (uptr) void))
  (define %stream-read
    (foreign-procedure __collect_safe "chez_process_stream_read" (uptr uptr size_t) integer-64))
  (define %stream-write
    (foreign-procedure __collect_safe "chez_process_stream_write" (uptr uptr size_t) integer-64))
  (define closed-result (- (expt 2 63)))
  (define %close (foreign-procedure "close" (int) int))
  (define esrch 3)
  (define ebadf 9)
  (define enomem 12)
  (define enotty 25)

  (define-record-type stream
    (nongenerative)
    (sealed #t)
    (fields address))

  (define stream-guardian (make-guardian))
  (define pending-mutex (make-mutex))
  (define pending-processes '())

  (define (make-owned-stream who fd pty?)
    (let-values ([(address errno) (%stream-new fd pty?)])
      (when (eqv? address 0)
        (%close fd)
        (raise-system-error who "eventfd" errno))
      (let ([s (make-stream address)])
        (stream-guardian s address)
        s)))

  ;; A buffer handed to a collect-safe call must not move while C writes it.
  (define-syntax with-locked
    (syntax-rules ()
      [(_ (bv address start) body ...)
       (with-interrupts-disabled
         (lock-object bv)
         (let ([result (let ([address (+ (object->reference-address bv) start)]) body ...)])
           (unlock-object bv)
           result))]))

  (define port-buffer-size 65536)

  (define (stream-input-port name s)
    (parameterize ([custom-port-buffer-size port-buffer-size])
      (letrec ([port
                (make-custom-binary-input-port name
                  (lambda (bv start count)
                    (let ([n (with-locked (bv address start)
                               (%stream-read (stream-address s) address count))])
                      (keep-live s)
                      (cond
                        [(>= n 0) n]
                        [(= n closed-result) 0]
                        [else
                         (raise
                           (condition
                             (make-i/o-read-error)
                             (make-i/o-port-error port)
                             (make-system-error (- n) "read")
                             (make-who-condition 'get-bytevector-some)
                             (make-message-condition
                               (format "read failed: ~a" (errno-message (- n))))))])))
                  #f #f
                  (lambda () (%stream-close (stream-address s))))])
        port)))

  (define (stream-output-port name s)
    (parameterize ([custom-port-buffer-size port-buffer-size])
      (letrec ([port
                (make-custom-binary-output-port name
                  (lambda (bv start count)
                    (let ([n (with-locked (bv address start)
                               (%stream-write (stream-address s) address count))])
                      (keep-live s)
                      (if (> n 0)
                          n
                          (raise
                            (condition
                              (make-i/o-write-error)
                              (make-i/o-port-error port)
                              (make-system-error (if (= n closed-result) ebadf (- n)) "write")
                              (make-who-condition 'put-bytevector)
                              (make-message-condition
                                (if (= n closed-result)
                                    "the port was closed while writing"
                                    (format "write failed: ~a" (errno-message (- n))))))))))
                  #f #f
                  (lambda () (%stream-close (stream-address s))))])
        port)))

  (define-record-type exit-status
    (nongenerative)
    (sealed #t)
    (fields code signal core-dumped?))

  (define (exit-status-success? status)
    (check 'exit-status-success? exit-status? status "an exit status")
    (eqv? (exit-status-code status) 0))

  (define installed-exit-status-writer
    (record-writer (record-type-descriptor exit-status)
      (lambda (s port write)
        (if (exit-status-code s)
            (fprintf port "#<exit-status code ~a>" (exit-status-code s))
            (fprintf port "#<exit-status signal ~a~a>" (exit-status-signal s)
              (if (exit-status-core-dumped? s) " core dumped" ""))))))

  (define-record-type (options make-process-options process-options?)
    (nongenerative)
    (sealed #t)
    (fields directory environment stdin stdout stderr pty new-session process-group argv0)
    (protocol
      (lambda (new)
        (lambda (directory environment stdin stdout stderr pty new-session process-group argv0)
          (define who 'make-process-options)
          (check who (lambda (x) (or (not x) (c-string? x))) directory "a directory string or #f")
          (check who (lambda (x) (or (not x) (and (list? x) (for-all environment-binding? x))))
            environment "an alist of variable names and values, or #f")
          (for-each
            (lambda (x stderr?)
              (check who
                (lambda (x)
                  (or (not x) (memq x '(pipe inherit null)) (and stderr? (eq? x 'stdout))
                      (pty? x) (and (port? x) (file-port? x))))
                x "a stdio mode, a file port, a pty or #f"))
            (list stdin stdout stderr) '(#f #f #t))
          (check who (lambda (x) (or (not x) (pty? x))) pty "a pty or #f")
          (check who boolean? new-session "a boolean")
          (check who
            (lambda (x) (or (not x) (eq? x 'new) (process? x) (and (fixnum? x) (fx> x 0))))
            process-group "#f, new, a process or a process group id")
          (check who (lambda (x) (or (not x) (c-string? x))) argv0 "a string or #f")
          (when (and (or new-session pty) process-group (not (eq? process-group 'new)))
            (assertion-violationf who
              "a process in a new session leads its own group, so it cannot join ~s"
              process-group))
          (new directory environment stdin stdout stderr pty new-session process-group argv0)))))

  (define omitted (list 'omitted))

  (define (option value default) (if (eq? value omitted) default value))

  (define (options-from directory environment stdin stdout stderr pty new-session process-group
            argv0)
    (make-process-options (option directory #f) (option environment #f) (option stdin #f)
      (option stdout #f) (option stderr #f) (option pty #f) (option new-session #f)
      (option process-group #f) (option argv0 #f)))

  (define-syntax process-options
    (lambda (x)
      (define names
        '(directory environment stdin stdout stderr pty new-session process-group argv0))
      (define (check-names given)
        (let loop ([given given] [seen '()])
          (unless (null? given)
            (let ([name (syntax->datum (car given))])
              (unless (memq name names)
                (syntax-violation 'process-options "unknown option" x (car given)))
              (when (memq name seen)
                (syntax-violation 'process-options "option given twice" x (car given)))
              (loop (cdr given) (cons name seen))))))
      (syntax-case x ()
        [(_ [name value] ...)
         (for-all identifier? #'(name ...))
         (begin
           (check-names #'(name ...))
           (with-syntax ([(temporary ...) (generate-temporaries #'(name ...))])
             (with-syntax ([(argument ...)
                            (let ([given (map cons (syntax->datum #'(name ...)) #'(temporary ...))])
                              (map (lambda (name)
                                     (cond [(assq name given) => cdr] [else #'omitted]))
                                   names))])
               #'(let* ([temporary value] ...) (options-from argument ...)))))]
        [_ (syntax-violation 'process-options "expected (process-options [name value] ...)" x)])))

  ;; The pidfd is closed once the status is known and no thread is polling
  ;; it: open pidfds are copied into every later child's descriptor table,
  ;; and spawning slows as they accumulate. The guardian returns this, never
  ;; the process, which would stay reachable through it. The process's mutex
  ;; guards the mutable fields.
  (define-record-type native-process
    (nongenerative)
    (sealed #t)
    (fields (mutable pidfd) (mutable status) (mutable pollers) mutex)
    (protocol (lambda (new) (lambda (pidfd) (new pidfd #f 0 (make-mutex))))))

  (define-record-type (process %make-process process?)
    (nongenerative)
    (sealed #t)
    (fields id group-id native stdin stdout stderr stdin-stream stdout-stream stderr-stream))

  (define installed-process-writer
    (record-writer (record-type-descriptor process)
      (lambda (p port write) (fprintf port "#<process ~a>" (process-id p)))))

  (define process-guardian (make-guardian))

  (define %await (foreign-procedure __collect_safe "chez_process_await" (int int) int))
  (define %reap (foreign-procedure "chez_process_reap" (int u8*) int))

  ;; The caller holds the native process's mutex.
  (define (reap who n)
    (let ([status
           (or (native-process-status n)
               (let* ([out (make-bytevector 8 0)] [errno (%reap (native-process-pidfd n) out)])
                 (unless (fx= errno 0) (raise-system-error who "waitid" errno))
                 (let ([kind (bytevector-s32-native-ref out 0)]
                       [value (bytevector-s32-native-ref out 4)])
                   (and (not (fx= kind 0))
                        (let ([status (if (fx= kind 1)
                                          (make-exit-status value #f #f)
                                          (make-exit-status #f value (fx= kind 3)))])
                          (native-process-status-set! n status)
                          status)))))])
      (release-pidfd! n)
      status))

  (define (release-pidfd! n)
    (when (and (native-process-status n) (fx= (native-process-pollers n) 0)
               (native-process-pidfd n))
      (%close (native-process-pidfd n))
      (native-process-pidfd-set! n #f)))

  ;; A dropped process still running is kept here, so that it leaves no
  ;; zombie once it exits. Taking from a guardian is not atomic, so threads
  ;; release in turn.
  (define (release-dropped!)
    (with-mutex pending-mutex
      (let loop ()
        (let ([s (stream-guardian)])
          (when s (%stream-free s) (loop))))
      (set! pending-processes
        (let loop ([acc pending-processes])
          (let ([n (process-guardian)])
            (if n (loop (cons n acc)) acc))))
      (set! pending-processes
        (filter
          (lambda (n)
            (with-mutex (native-process-mutex n)
              (and (native-process-pidfd n)
                   (not (guard (c [(system-error? c)
                                   (native-process-status-set! n c)
                                   (release-pidfd! n)
                                   #t])
                          (reap 'spawn-process n))))))
          pending-processes))))

  (define %spec-new (foreign-procedure "chez_process_spec_new" () uptr))
  (define %spec-free (foreign-procedure "chez_process_spec_free" (uptr) void))
  (define %spec-argument (foreign-procedure "chez_process_spec_argument" (uptr u8*) int))
  (define %spec-environment (foreign-procedure "chez_process_spec_environment" (uptr u8*) int))
  (define %spec-empty-environment
    (foreign-procedure "chez_process_spec_empty_environment" (uptr) void))
  (define %spec-program (foreign-procedure "chez_process_spec_program" (uptr u8*) int))
  (define %spec-directory (foreign-procedure "chez_process_spec_directory" (uptr u8*) int))
  (define %spec-pty (foreign-procedure "chez_process_spec_pty" (uptr u8*) int))
  (define %spec-stdio (foreign-procedure "chez_process_spec_stdio" (uptr int int int) void))
  (define %spec-session (foreign-procedure "chez_process_spec_session" (uptr boolean int) void))
  (define %spec-spawn (foreign-procedure __collect_safe "chez_process_spec_spawn" (uptr) int))
  (define %spec-result (foreign-procedure "chez_process_spec_result" (uptr int) int))
  (define %own-group (foreign-procedure "chez_process_own_group" () int))

  ;; These match the shim's enumeration of stdio kinds.
  (define (stdio-code mode)
    (case mode
      [(inherit) 0]
      [(null) 1]
      [(pipe) 2]
      [(descriptor) 3]
      [(stdout) 4]
      [(pty) 5]))

  (define spawn-process
    (case-lambda
      [(program arguments) (spawn 'spawn-process program arguments default-options)]
      [(program arguments options)
       (check 'spawn-process process-options? options "process options")
       (spawn 'spawn-process program arguments options)]))

  (define (spawn who program arguments options)
    (check who c-string? program "a program name without NUL")
    (check who (lambda (x) (and (list? x) (for-all c-string? x))) arguments
      "a list of strings without NUL")
    (release-dropped!)
    (let* ([pty (options-pty options)]
           [default (if pty 'pty 'pipe)]
           [modes (map (lambda (mode) (or mode default))
                    (list (options-stdin options) (options-stdout options)
                          (options-stderr options)))]
           [session? (or (options-new-session options) (and pty #t))]
           [group (options-process-group options)]
           [group-code (cond
                         [(not group) -1]
                         [(eq? group 'new) 0]
                         [(process? group) (process-group-id group)]
                         [else group])]
           [spec (%spec-new)]
           [out (make-bytevector 20 0)])
      (when (eqv? spec 0) (raise-system-error who "malloc" enomem))
      (let ([errno
             (dynamic-wind
               void
               (lambda ()
                 (define (ok r) (unless (fx= r 0) (raise-system-error who "malloc" enomem)))
                 (ok (%spec-program spec (c-string program)))
                 (ok (%spec-argument spec (c-string (or (options-argv0 options) program))))
                 (for-each (lambda (a) (ok (%spec-argument spec (c-string a)))) arguments)
                 (let ([environment (options-environment options)])
                   (when environment
                     (%spec-empty-environment spec)
                     (for-each
                       (lambda (b)
                         (ok (%spec-environment spec
                               (c-string (string-append (car b) "=" (cdr b))))))
                       environment)))
                 (let ([directory (options-directory options)])
                   (when directory (ok (%spec-directory spec (c-string directory)))))
                 (when pty (ok (%spec-pty spec (c-string (pty-name pty)))))
                 (for-each
                   (lambda (which mode)
                     (cond
                       [(eq? mode 'pty) (%spec-stdio spec which (stdio-code 'pty) -1)]
                       [(pty? mode)
                        (unless (eq? mode pty)
                          (assertion-violationf who
                            "a standard stream can be only the pty given as the pty option"))
                        (%spec-stdio spec which (stdio-code 'pty) -1)]
                       [(port? mode)
                        ;; A closed port's old number may belong to another
                        ;; file by now.
                        (when (port-closed? mode)
                          (assertion-violationf who "~s is closed" mode))
                        (when (output-port? mode) (flush-output-port mode))
                        (%spec-stdio spec which (stdio-code 'descriptor)
                          (port-file-descriptor mode))]
                       [else (%spec-stdio spec which (stdio-code mode) -1)]))
                   '(0 1 2) modes)
                 (%spec-session spec session? group-code)
                 (let ([errno (%spec-spawn spec)])
                   (for-each (lambda (mode) (when (port? mode) (keep-live mode))) modes)
                   (do ([i 0 (fx+ i 1)]) ((fx= i 5))
                     (bytevector-s32-native-set! out (fx* 4 i) (%spec-result spec i)))
                   errno))
               (lambda () (%spec-free spec)))])
        (define (fd i) (bytevector-s32-native-ref out (fx* 4 i)))
        (unless (fx= errno 0)
          (raise
            (condition
              (make-spawn-error errno "posix_spawn" program)
              (make-who-condition who)
              (make-message-condition
                (format "cannot run ~a: ~a" program (errno-message errno)))
              (make-irritants-condition (list program arguments)))))
        (let* ([pid (fd 0)]
               [native (make-native-process (fd 1))]
               [name (lambda (what) (format "process ~a ~a" pid what))]
               [registered? #f])
          ;; A child already runs: if wrapping it fails, it is still reaped.
          (dynamic-wind
            void
            (lambda ()
              (let* ([streams (let loop ([i 2] [acc '()])
                                (if (fx= i 5)
                                    (reverse acc)
                                    (loop (fx+ i 1)
                                      (cons (and (fx>= (fd i) 0)
                                                 (let ([s (make-owned-stream who (fd i) #f)])
                                                   (bytevector-s32-native-set! out (fx* 4 i) -1)
                                                   s))
                                            acc))))]
                     [p (%make-process pid
                          (cond
                            [(or session? (eqv? group-code 0)) pid]
                            [(fx> group-code 0) group-code]
                            [else (%own-group)])
                          native
                          (and (car streams) (stream-output-port (name "stdin") (car streams)))
                          (and (cadr streams) (stream-input-port (name "stdout") (cadr streams)))
                          (and (caddr streams) (stream-input-port (name "stderr") (caddr streams)))
                          (car streams) (cadr streams) (caddr streams))])
                (process-guardian p native)
                (set! registered? #t)
                p))
            (lambda ()
              (unless registered?
                (do ([i 2 (fx+ i 1)]) ((fx= i 5)) (when (fx>= (fd i) 0) (%close (fd i))))
                (with-mutex pending-mutex
                  (set! pending-processes (cons native pending-processes))))))))))

  (define (check-process who p) (check who process? p "a process"))

  (define (reap! who p)
    (let ([n (process-native p)])
      (with-mutex (native-process-mutex n) (reap who n))))

  (define (await who p milliseconds)
    (let ([n (process-native p)])
      (or (with-mutex (native-process-mutex n)
            (or (native-process-status n)
                (begin
                  (native-process-pollers-set! n (fx+ (native-process-pollers n) 1))
                  #f)))
          (let ([ready (%await (native-process-pidfd n) milliseconds)])
            (with-mutex (native-process-mutex n)
              (native-process-pollers-set! n (fx- (native-process-pollers n) 1))
              (cond
                [(fx< ready 0) (release-pidfd! n) (raise-system-error who "poll" (fx- ready) p)]
                [(fx= ready 0) (release-pidfd! n) #f]
                [else (reap who n)]))))))

  (define (duration->milliseconds who timeout)
    (check who (lambda (t) (and (time? t) (eq? (time-type t) 'time-duration)
                                (>= (time-second t) 0)))
      timeout "a nonnegative time-duration")
    (min #x7fffffff
      (+ (* 1000 (time-second timeout)) (quotient (+ (time-nanosecond timeout) 999999) 1000000))))

  (define process-wait
    (case-lambda
      [(p)
       (check-process 'process-wait p)
       (let loop () (or (await 'process-wait p -1) (loop)))]
      [(p timeout)
       (check-process 'process-wait p)
       (await 'process-wait p (duration->milliseconds 'process-wait timeout))]))

  (define (process-status p)
    (check-process 'process-status p)
    (reap! 'process-status p))

  (define %signal (foreign-procedure "chez_process_signal" (int int) int))
  (define %signal-group (foreign-procedure "chez_process_signal_group" (int int) int))
  ;; #f when the process has already ended: signalling races with its end,
  ;; and an exited process is never confused with one that reused its id.
  (define (process-signal! p signal)
    (check-process 'process-signal! p)
    (let ([number (signal-number 'process-signal! signal)])
      (let ([n (process-native p)])
        (with-mutex (native-process-mutex n)
          (and (not (native-process-status n))
               (let ([errno (%signal (native-process-pidfd n) number)])
               (cond
                 [(fx= errno 0) #t]
                 [(fx= errno esrch) #f]
                 [else (raise-system-error 'process-signal! "pidfd_send_signal" errno p)])))))))

  (define (process-kill! p) (process-signal! p 'kill))

  (define (process-group-signal! p signal)
    (check-process 'process-group-signal! p)
    (unless (eqv? (process-id p) (process-group-id p))
      (assertion-violationf 'process-group-signal! "~s does not lead its own process group" p))
    (let ([errno (%signal-group (process-group-id p)
                   (signal-number 'process-group-signal! signal))])
      (cond
        [(fx= errno 0) #t]
        [(fx= errno esrch) #f]
        [else (raise-system-error 'process-group-signal! "kill" errno p)])))

  (define-record-type process-result
    (nongenerative)
    (sealed #t)
    (fields status stdout stderr))

  (define %exchange
    (foreign-procedure __collect_safe "chez_process_exchange" (uptr uptr size_t uptr uptr uptr)
      int))
  (define %exchange-size (foreign-procedure "chez_process_exchange_size" (uptr int) size_t))
  (define %exchange-take (foreign-procedure "chez_process_exchange_take" (uptr int u8*) void))
  (define %exchange-free (foreign-procedure "chez_process_exchange_free" (uptr) void))
  (define %copy-in (foreign-procedure "chez_process_copy_in" (uptr u8* size_t) void))

  (define (input-bytes who input)
    (cond
      [(not input) #f]
      [(string? input) (string->utf8 input)]
      [(bytevector? input) input]
      [else (assertion-violationf who "~s is not a string, bytevector or #f" input)]))

  (define process-communicate
    (case-lambda
      [(p) (communicate 'process-communicate p #f)]
      [(p input) (communicate 'process-communicate p input)]))

  ;; Reads the descriptors directly, so bytes already taken into a port's
  ;; buffer are not part of the result.
  (define (communicate who p input)
    (check-process who p)
    (let ([bytes (input-bytes who input)])
      (when (and bytes (or (not (process-stdin p)) (port-closed? (process-stdin p))))
        (assertion-violationf who "~s has no open stdin pipe to write the input to" p))
      (when (and (process-stdin p) (not (port-closed? (process-stdin p))))
        (flush-output-port (process-stdin p)))
      (let* ([size (if bytes (bytevector-length bytes) 0)]
             [aligned (fxand (fx+ size 7) -8)]
             [memory (foreign-alloc (fx+ aligned 8))]
             [result (fx+ memory aligned)])
        (define (address s) (if s (stream-address s) 0))
        (dynamic-wind
          (lambda () (void))
          (lambda ()
            (foreign-set! 'uptr result 0 0)
            (when bytes (%copy-in memory bytes size))
            (let ([errno (%exchange (address (process-stdin-stream p)) memory size
                           (address (process-stdout-stream p)) (address (process-stderr-stream p))
                           result)])
              (keep-live p)
              (let ([exchange (foreign-ref 'uptr result 0)])
                (define (take which present?)
                  (and present?
                       (let ([bv (make-bytevector (%exchange-size exchange which))])
                         (%exchange-take exchange which bv)
                         bv)))
                (dynamic-wind
                  void
                  (lambda ()
                    (unless (fx= errno 0) (raise-system-error who "poll" errno p))
                    (let ([stdout (take 1 (process-stdout-stream p))]
                          [stderr (take 2 (process-stderr-stream p))])
                      (for-each (lambda (port) (when port (close-port port)))
                        (list (process-stdin p) (process-stdout p) (process-stderr p)))
                      (make-process-result (process-wait p) stdout stderr)))
                  (lambda () (%exchange-free exchange))))))
          (lambda () (foreign-free memory))))))

  (define run-process
    (case-lambda
      [(program arguments) (run 'run-process program arguments #f default-options)]
      [(program arguments input) (run 'run-process program arguments input default-options)]
      [(program arguments input options)
       (check 'run-process process-options? options "process options")
       (run 'run-process program arguments input options)]))

  (define (run who program arguments input options)
    (when (and (input-bytes who input)
               (not (eq? (or (options-stdin options) (if (options-pty options) 'pty 'pipe))
                         'pipe)))
      (assertion-violationf who "input needs stdin to be a pipe, not ~s" (options-stdin options)))
    (communicate who (spawn who program arguments options) input))

  (define-record-type (pty %make-pty pty?)
    (nongenerative)
    (sealed #t)
    (fields name input-port output-port reader writer))

  (define %pty-open
    (foreign-procedure "chez_process_pty_open" (int int int int u8* u8* size_t) int))
  (define %pty-resize (foreign-procedure "chez_process_pty_resize" (uptr int int int int) int))
  (define %pty-size (foreign-procedure "chez_process_pty_size" (uptr u8*) int))
  (define %pty-foreground (foreign-procedure "chez_process_pty_foreground" (uptr) int))

  (define (dimension? x) (and (fixnum? x) (fx<= 0 x 65535)))

  (define open-pty
    (case-lambda
      [(rows columns) (open-pty rows columns 0 0)]
      [(rows columns x-pixels y-pixels)
       (for-each (lambda (n) (check 'open-pty dimension? n "a size from 0 to 65535"))
         (list rows columns x-pixels y-pixels))
       (release-dropped!)
       (let* ([fds (make-bytevector 8 0)]
              [name (make-bytevector 128 0)]
              [errno (%pty-open rows columns x-pixels y-pixels fds name (bytevector-length name))])
         (unless (fx= errno 0) (raise-system-error 'open-pty "posix_openpt" errno))
         (let* ([path (bytevector-c-string name)]
                [reader (make-owned-stream 'open-pty (bytevector-s32-native-ref fds 0) #t)]
                [writer (make-owned-stream 'open-pty (bytevector-s32-native-ref fds 4) #t)])
           (%make-pty path (stream-input-port (string-append path " input") reader)
             (stream-output-port (string-append path " output") writer) reader writer)))]))

  (define (check-pty who p) (check who pty? p "a pty"))

  ;; The two ends are separate descriptors; either answers for the terminal.
  (define (with-open-end who p operation call)
    (let ([errno (call (stream-address (pty-reader p)))])
      (let ([errno (if (fx= errno ebadf) (call (stream-address (pty-writer p))) errno)])
        (keep-live p)
        (unless (fx= errno 0) (raise-system-error who operation errno p)))))

  (define pty-resize!
    (case-lambda
      [(p rows columns) (pty-resize! p rows columns 0 0)]
      [(p rows columns x-pixels y-pixels)
       (check-pty 'pty-resize! p)
       (for-each (lambda (n) (check 'pty-resize! dimension? n "a size from 0 to 65535"))
         (list rows columns x-pixels y-pixels))
       (with-open-end 'pty-resize! p "ioctl"
         (lambda (address) (%pty-resize address rows columns x-pixels y-pixels)))]))

  (define (pty-size p)
    (check-pty 'pty-size p)
    (let ([out (make-bytevector 16 0)])
      (with-open-end 'pty-size p "ioctl" (lambda (address) (%pty-size address out)))
      (values (bytevector-s32-native-ref out 0) (bytevector-s32-native-ref out 4)
        (bytevector-s32-native-ref out 8) (bytevector-s32-native-ref out 12))))

  (define (pty-foreground-process-group p)
    (check-pty 'pty-foreground-process-group p)
    (let* ([group (%pty-foreground (stream-address (pty-reader p)))]
           [group (if (fx= group (fx- ebadf))
                      (%pty-foreground (stream-address (pty-writer p)))
                      group)])
      (keep-live p)
      (cond
        [(fx> group 0) group]
        [(or (fx= group 0) (fx= group (fx- enotty))) #f]
        [else (raise-system-error 'pty-foreground-process-group "tcgetpgrp" (fx- group) p)])))

  (define (pty-close! p)
    (check-pty 'pty-close! p)
    (close-port (pty-input-port p))
    (close-port (pty-output-port p)))

  (define default-options (process-options)))
