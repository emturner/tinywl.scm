;; wrapper for clock functions from <time.h>
(define-module (tinywl-core wrapper)
  #:use-module (ice-9 format)
  #:use-module (system foreign)
  #:export (tinywl-run))

(load-extension "tinywl" "init_tinywl_wrapper")

(define (tinywl-run startup-cmd)
  (run startup-cmd))
