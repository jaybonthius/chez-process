(library (support)
  (export sanitizer-preload output-of read-all-text seconds string-contains? temporary-directory remove-tree
    open-descriptors process-state lines)
  (import (chezscheme) (process))

  ;; The sanitizer build preloads its runtimes into Chez; the programs the
  ;; tests start are not built for them and fail to load with them.
  (define sanitizer-preload (getenv "LD_PRELOAD"))

  (define unset-preload
    ((foreign-procedure "unsetenv" (string) int) "LD_PRELOAD"))

  (define (read-all-text port)
    (let ([bytes (get-bytevector-all port)])
      (if (eof-object? bytes) "" (utf8->string bytes))))

  (define (output-of program arguments)
    (let ([result (run-process program arguments)])
      (unless (exit-status-success? (process-result-status result))
        (errorf 'output-of "~a ~s ended with ~s: ~a" program arguments
          (process-result-status result) (utf8->string (process-result-stderr result))))
      (utf8->string (process-result-stdout result))))

  (define (seconds s)
    (let ([whole (exact (floor s))])
      (make-time 'time-duration (exact (round (* 1e9 (- s whole)))) whole)))

  (define (string-contains? s part)
    (let ([n (string-length part)])
      (let loop ([i 0])
        (and (<= (+ i n) (string-length s))
             (or (string=? (substring s i (+ i n)) part) (loop (+ i 1)))))))

  (define (lines s)
    (let loop ([i 0] [start 0] [acc '()])
      (cond
        [(= i (string-length s))
         (reverse (if (= start i) acc (cons (substring s start i) acc)))]
        [(char=? (string-ref s i) #\newline) (loop (+ i 1) (+ i 1) (cons (substring s start i) acc))]
        [else (loop (+ i 1) start acc)])))

  (define (temporary-directory)
    (string-trim (output-of "mktemp" '("-d"))))

  (define (string-trim s)
    (let loop ([end (string-length s)])
      (if (and (> end 0) (char-whitespace? (string-ref s (- end 1))))
          (loop (- end 1))
          (substring s 0 end))))

  (define (remove-tree path)
    (output-of "rm" (list "-rf" path)))

  (define (open-descriptors)
    (length (directory-list "/proc/self/fd")))

  (define (process-state pid)
    (let ([path (format "/proc/~a/stat" pid)])
      (and (file-exists? path)
           (guard (c [(i/o-error? c) #f])
             (let* ([text (call-with-input-file path get-string-all)]
                    [close (let loop ([i (- (string-length text) 1)])
                             (if (char=? (string-ref text i) #\)) i (loop (- i 1))))])
               (string-ref text (+ close 2))))))))
