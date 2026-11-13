(import (chezscheme) (process) (check) (support))

(define (raised thunk)
  (guard (c [#t c]) (thunk) #f))

(test "echo writes its arguments and exits 0"
  (let ([p (spawn-process "echo" '("hello" "world"))])
    (check (process? p))
    (check (fixnum? (process-id p)))
    (check-equal? "hello world\n" (read-all-text (process-stdout p)))
    (let ([status (process-wait p)])
      (check (exit-status? status))
      (check (exit-status-success? status))
      (check-equal? 0 (exit-status-code status))
      (check-equal? #f (exit-status-signal status))
      (check-equal? #f (exit-status-core-dumped? status)))))

(test "cat returns what it is given"
  (let ([p (spawn-process "cat" '())])
    (put-bytevector (process-stdin p) (string->utf8 "line one\nline two\n"))
    (close-port (process-stdin p))
    (check-equal? "line one\nline two\n" (read-all-text (process-stdout p)))
    (check-equal? "" (read-all-text (process-stderr p)))
    (check (exit-status-success? (process-wait p)))))

(test "arguments pass unchanged, with no shell"
  (let ([arguments '("two words" "" "$HOME" "*" "é λ 🙂" "a\"b'c" "-n")])
    (check-equal? (apply string-append (map (lambda (a) (string-append a "\n")) arguments))
      (output-of "printf" (cons "%s\\n" arguments)))))

(test "argv0 sets the name the program sees"
  (check-equal? "-renamed\n"
    (utf8->string
      (process-result-stdout
        (run-process "sh" '("-c" "echo $0") #f (process-options [argv0 "-renamed"]))))))

(test "the environment is inherited, replaced or emptied"
  (let ([inherited (output-of "env" '())])
    (check (string-contains? inherited "PATH=")))
  (check-equal? "A=1\nB=two words\n"
    (utf8->string
      (process-result-stdout
        (run-process "/usr/bin/env" '() #f
          (process-options [environment '(("A" . "1") ("B" . "two words"))])))))
  (check-equal? ""
    (utf8->string
      (process-result-stdout
        (run-process "/usr/bin/env" '() #f (process-options [environment '()]))))))

(test "environment-variables lists this process's environment"
  (let ([variables (environment-variables)])
    (check (pair? variables))
    (check-equal? (getenv "PATH") (cdr (assoc "PATH" variables)))
    (check (for-all (lambda (b) (and (string? (car b)) (string? (cdr b)))) variables))))

(test "the directory option sets the child's working directory"
  (check-equal? "/tmp\n"
    (utf8->string
      (process-result-stdout (run-process "pwd" '() #f (process-options [directory "/tmp"])))))
  (let ([c (raised (lambda () (spawn-process "pwd" '() (process-options [directory "/no/such/dir"]))))])
    (check (spawn-error? c))
    (check-equal? 2 (system-error-errno c))))

(test "a name without a slash is searched in the child's PATH"
  (let ([dir (temporary-directory)])
    (call-with-output-file (string-append dir "/greet") (lambda (out) (display "#!/bin/sh\necho greeted\n" out)))
    (chmod (string-append dir "/greet") #o755)
    (check-equal? "greeted\n"
      (utf8->string
        (process-result-stdout
          (run-process "greet" '() #f
            (process-options [environment (list (cons "PATH" (string-append "/nonexistent:" dir)))])))))
    (check (spawn-error? (raised (lambda () (spawn-process "greet" '())))))
    (call-with-output-file (string-append dir "/locked") (lambda (out) (display "x" out)))
    (let ([c (raised (lambda ()
                       (spawn-process "locked" '()
                         (process-options [environment (list (cons "PATH" dir))]))))])
      (check (spawn-error? c))
      (check-equal? 13 (system-error-errno c)))
    (remove-tree dir)))

(test "a missing program raises a spawn error naming it"
  (let ([c (raised (lambda () (spawn-process "no-such-program-xyz" '("a"))))])
    (check (spawn-error? c))
    (check (system-error? c))
    (check (error? c))
    (check-equal? "no-such-program-xyz" (spawn-error-program c))
    (check-equal? 2 (system-error-errno c))
    (check-equal? 'spawn-process (condition-who c))
    (check (string-contains? (condition-message c) "No such file")))
  (check (spawn-error? (raised (lambda () (spawn-process "/etc/passwd" '())))))
  (check (spawn-error? (raised (lambda () (spawn-process "" '()))))))

(test "every exit code comes back"
  (for-each
    (lambda (code)
      (check-equal? code
        (exit-status-code (process-wait (spawn-process "sh" (list "-c" (format "exit ~a" code)))))))
    '(0 1 2 3 7 42 126 127 128 200 254 255)))

(test "a signal that ends the child is its status"
  (for-each
    (lambda (signal number)
      (let ([status (process-wait
                      (spawn-process "sh" (list "-c" (format "kill -~a $$; sleep 5" number))))])
        (check-equal? #f (exit-status-code status))
        (check-equal? number (exit-status-signal status))
        (check (not (exit-status-success? status)))))
    '(hup int term kill usr1 alrm)
    '(1 2 15 9 10 14)))

(test "a core dump is reported, when the system writes one"
  (let ([status (process-wait (spawn-process "sh" '("-c" "ulimit -c unlimited; kill -ABRT $$")))])
    (check-equal? 6 (exit-status-signal status))
    (check (boolean? (exit-status-core-dumped? status)))))

(test "standard streams can be null, inherited, merged or a file port"
  (let ([r (run-process "sh" '("-c" "echo out; echo err >&2") #f
             (process-options [stderr (stdio stdout)]))])
    (check-equal? "out\nerr\n" (utf8->string (process-result-stdout r)))
    (check-equal? #f (process-result-stderr r)))
  (let ([p (spawn-process "cat" '() (process-options [stdin (stdio null)]))])
    (check-equal? #f (process-stdin p))
    (check-equal? "" (read-all-text (process-stdout p)))
    (check (exit-status-success? (process-wait p))))
  (let ([p (spawn-process "true" '() (process-options [stdin 'inherit] [stdout 'inherit] [stderr 'inherit]))])
    (check-equal? '(#f #f #f) (list (process-stdin p) (process-stdout p) (process-stderr p)))
    (check (exit-status-success? (process-wait p))))
  (let* ([dir (temporary-directory)] [path (string-append dir "/out")]
         [port (open-file-output-port path)])
    (process-wait (spawn-process "echo" '("to a file") (process-options [stdout port])))
    (close-port port)
    (check-equal? "to a file\n" (call-with-input-file path get-string-all))
    (remove-tree dir)))

(test "a closed port given as a stream is refused, not wired to its old number"
  (let* ([dir (temporary-directory)]
         [closed (open-file-output-port (string-append dir "/closed"))])
    (close-port closed)
    (let ([other (open-file-output-port (string-append dir "/other"))])
      (check-raises assertion-violation?
        (spawn-process "echo" '("must not appear") (process-options [stdout closed])))
      (close-port other)
      (check (eof-object? (call-with-input-file (string-append dir "/other") get-string-all))))
    (remove-tree dir)))

(test "an output port's buffered bytes are written before the child starts"
  (let* ([dir (temporary-directory)] [path (string-append dir "/out")]
         [port (open-file-output-port path (file-options) (buffer-mode block)
                 (native-transcoder))])
    (put-string port "parent-first\n")
    (process-wait (spawn-process "echo" '("child") (process-options [stdout port])))
    (close-port port)
    (check-equal? "parent-first\nchild\n" (call-with-input-file path get-string-all))
    (remove-tree dir)))

(test "run-process refuses input without a stdin pipe, before starting the child"
  (let* ([dir (temporary-directory)] [marker (string-append dir "/started")])
    (for-each
      (lambda (options)
        (check-raises assertion-violation?
          (run-process "sh" (list "-c" (string-append "echo started > " marker)) "input"
            options))
        (sleep (seconds 0.2))
        (check (not (file-exists? marker))))
      (list (process-options [stdin (stdio null)])
            (process-options [stdin (stdio inherit)])
            (process-options [stdin (open-file-input-port "/dev/null")])
            (process-options [pty (open-pty 24 80)])))
    (remove-tree dir)))

(test "spawning lets other threads collect while it searches PATH"
  (let* ([dir (string-append (temporary-directory) "/slow-path")]
         [preload (let loop ([dirs (library-directories)])
                    (cond
                      [(null? dirs) (errorf 'test "slow-access.so is in no library directory")]
                      [(file-exists? (string-append (cdar dirs) "/slow-access.so"))
                       (string-append (cdar dirs) "/slow-access.so")]
                      [else (loop (cdr dirs))]))])
    (mkdir dir)
    (call-with-output-file (string-append dir "/slow-program")
      (lambda (out) (display "#!/bin/sh\n" out)))
    (chmod (string-append dir "/slow-program") #o755)
    (let* ([libdirs (apply string-append
                      (map (lambda (d) (string-append (car d) "::" (cdr d) ":"))
                        (library-directories)))]
           [r (run-process (getenv "SCHEME")
                (list "--libdirs" libdirs "--script" "tests/fixtures/collect-during-spawn.ss" dir)
                #f
                (process-options
                  [environment
                   (cons (cons "LD_PRELOAD"
                           (if sanitizer-preload
                               (string-append sanitizer-preload ":" preload)
                               preload))
                         (environment-variables))]))]
           [milliseconds (string->number
                           (car (lines (utf8->string (process-result-stdout r)))))])
      (check (exit-status-success? (process-result-status r)))
      (check (< milliseconds 400)))
    (remove-tree (path-parent dir))))

(test "text goes through a transcoded port"
  (let* ([p (spawn-process "cat" '())]
         [in (transcoded-port (process-stdin p) (native-transcoder))]
         [out (transcoded-port (process-stdout p) (native-transcoder))])
    (put-string in "λ text\n")
    (close-port in)
    (check-equal? "λ text" (get-line out))
    (process-wait p)))

(test "glibc 2.39 or later spawns with pidfd_spawn"
  (let ([version ((foreign-procedure "gnu_get_libc_version" () string))]
        [uses? (= 1 ((foreign-procedure "chez_process_uses_pidfd_spawn" () int)))])
    (check (eq? uses? (>= (string->number (substring version 2 (string-length version))) 39)))))

(test "process-options checks its fields"
  (check-raises assertion-violation? (process-options [directory 5]))
  (check-raises assertion-violation? (process-options [environment '(("A=B" . "c"))]))
  (check-raises assertion-violation? (process-options [environment '(("A" . 1))]))
  (check-raises assertion-violation? (process-options [stdin 'stdout]))
  (check-raises assertion-violation? (process-options [stdout 'socket]))
  (check-raises assertion-violation? (process-options [new-session 'yes]))
  (check-raises assertion-violation? (process-options [new-session #t] [process-group 1]))
  (check-raises assertion-violation? (process-options [process-group -1]))
  (check-raises syntax-violation?
    (eval '(process-options [nope 1]) (environment '(chezscheme) '(process))))
  (check-raises syntax-violation?
    (eval '(process-options [stdin 'null] [stdin 'null]) (environment '(chezscheme) '(process))))
  (check-raises syntax-violation? (eval '(stdio socket) (environment '(chezscheme) '(process))))
  (check (process-options? (make-process-options #f #f 'null #f #f #f #f #f #f))))

(test "spawn-process checks its arguments"
  (check-raises assertion-violation? (spawn-process 'echo '()))
  (check-raises assertion-violation? (spawn-process "echo" "hi"))
  (check-raises assertion-violation? (spawn-process "echo" '("a\x0;b")))
  (check-raises assertion-violation? (spawn-process "echo" '() 'options))
  (check-raises assertion-violation? (process-wait 'process))
  (check-raises assertion-violation? (process-wait (spawn-process "true" '()) 5))
  (check-raises assertion-violation? (process-signal! (spawn-process "true" '()) 'sigterm))
  (check-raises assertion-violation? (run-process "cat" '() 'input)))

(test-exit)
