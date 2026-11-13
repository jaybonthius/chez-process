(import (chezscheme) (process))

(define directory (car (command-line-arguments)))

(define spawned
  (fork-thread
    (lambda ()
      (process-wait
        (spawn-process "slow-program" '()
          (process-options [environment (list (cons "PATH" directory))]))))))

(sleep (make-time 'time-duration 200000000 0))
(let ([start (real-time)])
  (collect-rendezvous)
  (printf "~a\n" (- (real-time) start)))
(thread-join spawned)
