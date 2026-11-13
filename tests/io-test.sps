(import (chezscheme) (process) (check) (support))

(define (spawn thunk)
  (let* ([result (box #f)]
         [thread (fork-thread (lambda () (set-box! result (guard (c [#t c]) (thunk)))))])
    (lambda () (thread-join thread) (unbox result))))

(define (drain port)
  (let ([buffer (make-bytevector 65536)])
    (let loop ([total 0])
      (let ([n (get-bytevector-some! port buffer 0 65536)])
        (if (eof-object? n) total (loop (+ total n)))))))

(define (pattern size)
  (let ([bv (make-bytevector size)])
    (do ([i 0 (fx+ i 1)]) ((fx= i size) bv)
      (bytevector-u8-set! bv i (fxand (fx* i 7) 255)))))

(test "256 MB of output arrives whole"
  (let ([p (spawn-process "head" '("-c" "268435456" "/dev/zero"))])
    (check-equal? 268435456 (drain (process-stdout p)))
    (check (exit-status-success? (process-wait p)))))

(test "run-process moves 32 MB each way without deadlock"
  (let* ([input (pattern (* 32 1024 1024))]
         [r (run-process "cat" '() input)])
    (check (exit-status-success? (process-result-status r)))
    (check (bytevector=? input (process-result-stdout r)))
    (check-equal? 0 (bytevector-length (process-result-stderr r)))))

(test "run-process captures large stdout and stderr at once"
  (let ([r (run-process "sh" '("-c" "head -c 20000000 /dev/zero; head -c 30000000 /dev/zero >&2"))])
    (check-equal? 20000000 (bytevector-length (process-result-stdout r)))
    (check-equal? 30000000 (bytevector-length (process-result-stderr r)))))

(test "process-communicate finishes a running child"
  (let ([p (spawn-process "sh" '("-c" "read line; echo \"got $line\"; echo bye >&2"))])
    (let ([r (process-communicate p "hello\n")])
      (check-equal? "got hello\n" (utf8->string (process-result-stdout r)))
      (check-equal? "bye\n" (utf8->string (process-result-stderr r)))
      (check (exit-status-success? (process-result-status r)))
      (check (port-closed? (process-stdout p))))))

(test "process-communicate refuses input once stdin is closed"
  (let ([p (spawn-process "cat" '())])
    (close-port (process-stdin p))
    (check-raises assertion-violation? (process-communicate p "abc"))
    (let ([r (process-communicate p)])
      (check (exit-status-success? (process-result-status r)))
      (check-equal? 0 (bytevector-length (process-result-stdout r))))))

(test "input the child does not read is dropped, not an error"
  (let ([r (run-process "true" '() (pattern 1000000))])
    (check (exit-status-success? (process-result-status r)))))

(test "writing to a child that has exited raises an i/o write error"
  (let ([p (spawn-process "true" '())])
    (process-wait p)
    (let ([c (guard (c [#t c])
               (put-bytevector (process-stdin p) (pattern 100000))
               (flush-output-port (process-stdin p))
               #f)])
      (check (i/o-write-error? c))
      (check-equal? 32 (system-error-errno c)))))

(test "closing a port wakes a thread blocked reading it"
  (let* ([p (spawn-process "sleep" '("30"))]
         [reader (spawn (lambda () (get-bytevector-some (process-stdout p))))])
    (sleep (seconds 0.2))
    (let ([start (real-time)])
      (close-port (process-stdout p))
      (check (eof-object? (reader)))
      (check (< (- (real-time) start) 1000)))
    (process-kill! p)
    (process-wait p)))

(test "the collector runs while threads are blocked reading"
  (let* ([processes (map (lambda (i) (spawn-process "sleep" '("30"))) (iota 4))]
         [readers (map (lambda (p) (spawn (lambda () (get-bytevector-some (process-stdout p)))))
                    processes)])
    (sleep (seconds 0.1))
    (let ([start (real-time)])
      (do ([i 0 (+ i 1)]) ((= i 20)) (make-vector 100000) (collect-rendezvous))
      (check (< (- (real-time) start) 2000)))
    (for-each process-kill! processes)
    (check (for-all eof-object? (map (lambda (join) (join)) readers)))
    (for-each process-wait processes)))

(test "a reader and a writer on different threads talk to one child"
  (let* ([p (spawn-process "cat" '())]
         [input (pattern (* 8 1024 1024))]
         [writer (spawn (lambda ()
                          (put-bytevector (process-stdin p) input)
                          (close-port (process-stdin p))
                          #t))]
         [output (get-bytevector-all (process-stdout p))])
    (check (writer))
    (check (bytevector=? input output))
    (process-wait p)))

(test "the child gets only standard input, output and error"
  (let ([held (map (lambda (i) (open-file-input-port "/dev/null")) (iota 12))])
    (let ([fds (map string->number (lines (output-of "ls" '("/proc/self/fd"))))])
      (check (for-all (lambda (fd) (< fd 4)) fds)))
    (for-each close-port held)))

(test "ports of a finished process close their descriptors"
  (let ([before (open-descriptors)])
    (do ([i 0 (+ i 1)]) ((= i 50))
      (let ([p (spawn-process "true" '())])
        (process-wait p)
        (close-port (process-stdin p))
        (close-port (process-stdout p))
        (close-port (process-stderr p))))
    (collect (collect-maximum-generation))
    (spawn-process "true" '())
    (check (<= (open-descriptors) (+ before 8)))))

(test-exit)
