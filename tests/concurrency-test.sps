(import (chezscheme) (process) (check) (support))

(define (spawn thunk)
  (let* ([result (box #f)]
         [thread (fork-thread (lambda () (set-box! result (guard (c [#t c]) (thunk)))))])
    (lambda () (thread-join thread) (unbox result))))

(define (remove-duplicates items)
  (fold-left (lambda (acc x) (if (memv x acc) acc (cons x acc))) '() items))

(test "200 processes run at once"
  (let* ([processes (map (lambda (i) (spawn-process "sh" (list "-c" (format "sleep 0.3; echo ~a" i))))
                      (iota 200))]
         [outputs (map (lambda (p) (read-all-text (process-stdout p))) processes)]
         [statuses (map process-wait processes)])
    (check-equal? (map (lambda (i) (format "~a\n" i)) (iota 200)) outputs)
    (check (for-all exit-status-success? statuses))
    (check-equal? 200 (length (remove-duplicates (map process-id processes))))))

(test "threads spawn, exchange with and wait for processes concurrently"
  (let* ([workers
          (map (lambda (t)
                 (spawn
                   (lambda ()
                     (let loop ([i 0] [ok 0])
                       (if (= i 40)
                           ok
                           (let* ([input (format "thread ~a item ~a\n" t i)]
                                  [r (run-process "cat" '() input)])
                             (loop (+ i 1)
                               (if (and (exit-status-success? (process-result-status r))
                                        (string=? input (utf8->string (process-result-stdout r))))
                                   (+ ok 1)
                                   ok))))))))
               (iota 8))]
         [results (map (lambda (join) (join)) workers)])
    (check-equal? (make-list 8 40) results)))

(test "each thread reads its own process while others collect"
  (let* ([processes (map (lambda (i) (spawn-process "head" '("-c" "20000000" "/dev/zero"))) (iota 6))]
         [readers (map (lambda (p)
                         (spawn
                           (lambda ()
                             (let ([buffer (make-bytevector 65536)])
                               (let loop ([total 0])
                                 (let ([n (get-bytevector-some! (process-stdout p) buffer 0 65536)])
                                   (if (eof-object? n) total (loop (+ total n)))))))))
                    processes)])
    (do ([i 0 (+ i 1)]) ((= i 30)) (make-vector 200000) (collect-rendezvous))
    (check-equal? (make-list 6 20000000) (map (lambda (join) (join)) readers))
    (check (for-all exit-status-success? (map process-wait processes)))))

(test "processes and ptys made on many threads are all freed"
  (let ([before (open-descriptors)])
    (for-each (lambda (join) (join))
      (map (lambda (t)
             (spawn
               (lambda ()
                 (do ([i 0 (+ i 1)]) ((= i 30))
                   (let ([pty (open-pty 10 10)])
                     (process-wait (spawn-process "true" '() (process-options [pty pty])))
                     (pty-close! pty))))))
           (iota 6)))
    (collect (collect-maximum-generation))
    (process-wait (spawn-process "true" '()))
    (check (<= (open-descriptors) (+ before 10)))))

(test-exit)
