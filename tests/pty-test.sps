(import (chezscheme) (process) (check) (support))

;; A reader thread collects the pty's output, so that a test waiting for
;; text gives up after a time rather than block for ever.
(define (make-screen pty)
  (let ([mutex (make-mutex)] [seen ""] [done? #f])
    (fork-thread
      (lambda ()
        (let loop ()
          (let ([bytes (guard (c [(i/o-error? c) (eof-object)])
                         (get-bytevector-some (pty-input-port pty)))])
            (if (eof-object? bytes)
                (with-mutex mutex (set! done? #t))
                (begin
                  (with-mutex mutex (set! seen (string-append seen (utf8->string bytes))))
                  (loop)))))))
    (lambda (message . arguments)
      (case message
        [(text) (with-mutex mutex seen)]
        [(ended?) (with-mutex mutex done?)]
        [(wait-for)
         (let ([want (car arguments)])
           (let loop ([tries 0])
             (cond
               [(string-contains? (with-mutex mutex seen) want) #t]
               [(= tries 500) #f]
               [else (sleep (seconds 0.01)) (loop (+ tries 1))])))]))))

(define (type pty text)
  (put-bytevector (pty-output-port pty) (string->utf8 text))
  (flush-output-port (pty-output-port pty)))

(test "a pty has a name and a size"
  (let ([pty (open-pty 24 80)])
    (check (pty? pty))
    (check (string-contains? (pty-name pty) "/dev/pts/"))
    (check-equal? '(24 80 0 0) (call-with-values (lambda () (pty-size pty)) list))
    (pty-resize! pty 50 132 1056 800)
    (check-equal? '(50 132 1056 800) (call-with-values (lambda () (pty-size pty)) list))
    (check-equal? #f (pty-foreground-process-group pty))
    (pty-close! pty)))

(test "a child in a pty has it as its terminal, and its size"
  (let* ([pty (open-pty 33 101)]
         [p (spawn-process "sh" '("-c" "tty; stty size; test -t 0 && test -t 1 && test -t 2 && echo all-terminals")
              (process-options [pty pty]))])
    (check-equal? '(#f #f #f) (list (process-stdin p) (process-stdout p) (process-stderr p)))
    (let ([text (read-all-text (pty-input-port pty))])
      (check (string-contains? text (pty-name pty)))
      (check (string-contains? text "33 101"))
      (check (string-contains? text "all-terminals")))
    (check (exit-status-success? (process-wait p)))
    (pty-close! pty)))

(test "the end of the session is the end of the pty's input"
  (let* ([pty (open-pty 24 80)]
         [p (spawn-process "echo" '("bye") (process-options [pty pty]))])
    (check-equal? "bye\r\n" (read-all-text (pty-input-port pty)))
    (check (eof-object? (get-bytevector-some (pty-input-port pty))))
    (process-wait p)
    (pty-close! pty)))

(test "a shell in a pty reads typed input and echoes it"
  (let* ([pty (open-pty 24 80)]
         [p (spawn-process "sh" '("-c" "read name; echo \"hello, $name\"") (process-options [pty pty]))]
         [screen (make-screen pty)])
    (type pty "world\n")
    (check (screen 'wait-for "hello, world"))
    (check (string-contains? (screen 'text) "world\r\n"))
    (check (exit-status-success? (process-wait p)))
    (pty-close! pty)))

(test "resizing tells the foreground job with SIGWINCH"
  (let* ([pty (open-pty 24 80)]
         [p (spawn-process "sh" '("-c" "trap 'echo resized $(stty size)' WINCH; echo ready; while :; do sleep 0.05; done")
              (process-options [pty pty]))]
         [screen (make-screen pty)])
    (check (screen 'wait-for "ready"))
    (pty-resize! pty 40 120)
    (check (screen 'wait-for "resized 40 120"))
    (process-kill! p)
    (process-wait p)
    (pty-close! pty)))

(test "control characters typed into the pty signal the foreground group"
  (let* ([pty (open-pty 24 80)]
         [p (spawn-process "cat" '() (process-options [pty pty]))])
    (type pty "line\n")
    (sleep (seconds 0.2))
    (type pty "\x3;")
    (check-equal? 2 (exit-status-signal (process-wait p (seconds 5))))
    (pty-close! pty))
  (let* ([pty (open-pty 24 80)]
         [p (spawn-process "cat" '() (process-options [pty pty]))])
    (type pty "\x4;")
    (check (exit-status-success? (process-wait p (seconds 5))))
    (pty-close! pty)))

(test "the foreground process group follows job control"
  (let* ([pty (open-pty 24 80)]
         [p (spawn-process "sh" '("-i") (process-options [pty pty] [environment '(("PS1" . "$ ") ("PATH" . "/usr/bin:/bin"))]))]
         [screen (make-screen pty)])
    (check (screen 'wait-for "$ "))
    (check-equal? (process-id p) (pty-foreground-process-group pty))
    (type pty "sleep 3\n")
    (let loop ([tries 0])
      (when (and (< tries 200) (eqv? (process-id p) (pty-foreground-process-group pty)))
        (sleep (seconds 0.01))
        (loop (+ tries 1))))
    (let ([group (pty-foreground-process-group pty)])
      (check (fixnum? group))
      (check (not (eqv? group (process-id p))))
      (type pty "\x3;")
      (let loop ([tries 0])
        (when (and (< tries 300) (not (eqv? (process-id p) (pty-foreground-process-group pty))))
          (sleep (seconds 0.01))
          (loop (+ tries 1))))
      (check-equal? (process-id p) (pty-foreground-process-group pty)))
    (type pty "exit 5\n")
    (check-equal? 5 (exit-status-code (process-wait p (seconds 5))))
    (pty-close! pty)))

(test "one stream can be a pipe while the others are the pty"
  (let* ([pty (open-pty 24 80)]
         [p (spawn-process "sh" '("-c" "tty; test -t 1 && echo out-is-terminal")
              (process-options [pty pty] [stdin (stdio pipe)]))])
    (close-port (process-stdin p))
    (let ([text (read-all-text (pty-input-port pty))])
      (check (string-contains? text "not a tty"))
      (check (string-contains? text "out-is-terminal")))
    (process-wait p)
    (pty-close! pty)))

(test "pty procedures check their arguments"
  (check-raises assertion-violation? (open-pty -1 80))
  (check-raises assertion-violation? (open-pty 24 70000))
  (check-raises assertion-violation? (pty-resize! 'pty 1 1))
  (let ([pty (open-pty 24 80)])
    (check-raises assertion-violation?
      (spawn-process "true" '() (process-options [stdin pty])))
    (check-raises assertion-violation?
      (spawn-process "true" '() (process-options [pty pty] [stdout (open-pty 1 1)])))
    (pty-close! pty)
    (check (system-error? (guard (c [#t c]) (pty-size pty))))))

(test-exit)
