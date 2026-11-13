(import (chezscheme) (process) (check) (support))

(define (collect-all!)
  (collect (collect-maximum-generation))
  (collect (collect-maximum-generation)))

(define (gone? pid)
  (not (process-state pid)))

(define (settle! pids)
  (let loop ([tries 0])
    (collect-all!)
    (process-wait (spawn-process "true" '()))
    (unless (or (= tries 100) (for-all gone? pids))
      (sleep (seconds 0.02))
      (loop (+ tries 1)))))

(test "dropped processes that have exited leave no zombie"
  (let ([pids (map (lambda (i) (process-id (spawn-process "true" '()))) (iota 100))])
    (settle! pids)
    (check (for-all gone? pids))))

(test "a dropped process still running is not killed, and is reaped after it exits"
  (let ([pid (process-id (spawn-process "sleep" '("0.5")))])
    (collect-all!)
    (process-wait (spawn-process "true" '()))
    (check (memv (process-state pid) '(#\S #\R)))
    (sleep (seconds 0.7))
    (check-equal? #\Z (process-state pid))
    (settle! (list pid))
    (check (gone? pid))))

(test "dropped ports and ptys release their descriptors"
  (collect-all!)
  (process-wait (spawn-process "true" '()))
  (let ([before (open-descriptors)])
    (do ([i 0 (+ i 1)]) ((= i 200))
      (spawn-process "true" '())
      (open-pty 24 80))
    (settle! '())
    (check (<= (open-descriptors) (+ before 10)))))

(test "waited processes free their pidfds when dropped"
  (collect-all!)
  (process-wait (spawn-process "true" '()))
  (let ([before (open-descriptors)])
    (do ([i 0 (+ i 1)]) ((= i 300))
      (let ([p (spawn-process "true" '() (process-options [stdin 'null] [stdout 'null] [stderr 'null]))])
        (process-wait p)))
    (settle! '())
    (check (<= (open-descriptors) (+ before 10)))))

(test-exit)
