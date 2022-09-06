;; wrapper for clock functions from <time.h>
(define-module (emturner clock)
  #:use-module (ice-9 format)
  #:use-module (system foreign)
  #:export (clock-gettime->monotomic))

(load-extension "clock-time" "init_clock_time")

(define-wrapped-pointer-type ptr-timespec
  ptr-timespec?
  wrap-ptr-timespec unwrap-ptr-timespec
  (lambda (t p)
    (format p "#<ptr: timespec at ~x>"
            (pointer-address (unwrap-ptr-timespec t)))))

(define (clock-gettime->monotomic)
  (wrap-ptr-timespec (clock-gettime-monotomic)))
