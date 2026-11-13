(import (chezscheme) (process) (check) (support))

(define (spawn thunk)
  (let* ([result (box #f)]
         [thread (fork-thread (lambda () (set-box! result (guard (c [#t c]) (thunk)))))])
    (lambda () (thread-join thread) (unbox result))))

(test "process-status is #f while the child runs, then its status"
  (let ([p (spawn-process "sleep" '("0.3"))])
    (check-equal? #f (process-status p))
    (let ([status (process-wait p)])
      (check (exit-status-success? status))
      (check (eq? status (process-status p)))
      (check (eq? status (process-wait p))))))

(test "a wait with a timeout returns #f and leaves the child running"
  (let ([p (spawn-process "sleep" '("5"))])
    (let ([start (real-time)])
      (check-equal? #f (process-wait p (seconds 0.2)))
      (check (<= 150 (- (real-time) start) 2000)))
    (check-equal? #f (process-wait p (seconds 0)))
    (check-equal? #f (process-status p))
    (check (process-kill! p))
    (check-equal? 9 (exit-status-signal (process-wait p (seconds 5))))))

(test "many threads wait on one process and see one status"
  (let* ([p (spawn-process "sleep" '("0.3"))]
         [waiters (map (lambda (i) (spawn (lambda () (process-wait p)))) (iota 8))]
         [statuses (map (lambda (join) (join)) waiters)])
    (check (for-all exit-status? statuses))
    (check (for-all (lambda (s) (eq? s (car statuses))) statuses))))

(test "signalling an ended process returns #f"
  (let ([p (spawn-process "true" '())])
    (process-wait p)
    (check-equal? #f (process-signal! p 'term))
    (check-equal? #f (process-kill! p))))

(test "signals stop, continue and end a child"
  (let ([p (spawn-process "sleep" '("5"))])
    (check (process-signal! p (signal-name stop)))
    (check (process-signal! p (signal-name cont)))
    (check-equal? #f (process-status p))
    (check (process-signal! p 15))
    (check-equal? 15 (exit-status-signal (process-wait p)))))

(test "a child traps a signal and exits on its own terms"
  (let ([p (spawn-process "sh" '("-c" "trap 'echo got usr1; exit 4' USR1; echo ready; while :; do sleep 0.05; done"))])
    (check-equal? "ready" (get-line (transcoded-port (process-stdout p) (native-transcoder))))
    (check (process-signal! p (signal-name usr1)))
    (check-equal? 4 (exit-status-code (process-wait p)))))

(test "signals reset to their defaults in the child"
  (let ([status (output-of "grep" '("-E" "^Sig(Ign|Blk)" "/proc/self/status"))])
    (check-equal? '("SigBlk:\t0000000000000000" "SigIgn:\t0000000000000000") (lines status))))

(test "a new process group can be signalled whole"
  (let* ([p (spawn-process "sh" '("-c" "sleep 30 & sleep 30 & echo $!; wait")
              (process-options [process-group 'new]))]
         [grandchild (string->number (get-line (transcoded-port (process-stdout p) (native-transcoder))))])
    (check-equal? (process-id p) (process-group-id p))
    (check (memv (process-state grandchild) '(#\S #\R)))
    (check (process-group-signal! p 'term))
    (check-equal? 15 (exit-status-signal (process-wait p)))
    (let loop ([tries 0])
      (when (and (< tries 100) (memv (process-state grandchild) '(#\S #\R)))
        (sleep (seconds 0.01))
        (loop (+ tries 1))))
    (check (not (memv (process-state grandchild) '(#\S #\R))))))

(test "a process can join another's group"
  (let* ([leader (spawn-process "sleep" '("30") (process-options [process-group 'new]))]
         [member (spawn-process "sleep" '("30") (process-options [process-group leader]))])
    (check-equal? (process-id leader) (process-group-id member))
    (check-raises assertion-violation? (process-group-signal! member 'term))
    (process-group-signal! leader 'kill)
    (check-equal? 9 (exit-status-signal (process-wait member)))
    (check-equal? 9 (exit-status-signal (process-wait leader)))))

(test "a new session leads its own group, and an ordinary child stays in ours"
  (let ([p (spawn-process "sh" '("-c" "ps -o sid= -o pgid= -p $$") (process-options [new-session #t]))])
    (check-equal? (process-id p) (process-group-id p))
    (let ([fields (read (open-input-string (string-append "(" (read-all-text (process-stdout p)) ")")))])
      (check-equal? (list (process-id p) (process-id p)) fields))
    (process-wait p))
  (let ([p (spawn-process "true" '())])
    (check (not (eqv? (process-id p) (process-group-id p))))
    (process-wait p)))

(test-exit)
