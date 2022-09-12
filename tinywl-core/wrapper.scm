;; wrapper for clock functions from <time.h>
(define-module (tinywl-core wrapper)
  #:use-module (ice-9 format)
  #:use-module (system foreign)
  #:export (tinywl-run
            get-handle-keybinding))

(load-extension "tinywl" "init_tinywl_wrapper")

(define-wrapped-pointer-type tinywl-handle-keybinding
  tinywl-handle-keybinding?
  wrap-tinywl-handle-keybinding unwrap-tinywl-handle-keybinding
  (lambda (wlr-b prt)
    (format prt "#<tinywl-handle-keybinding at ~x>"
        (pointer-address (unwrap-tinywl-handle-keybinding wlr-b)))))

(define (get-handle-keybinding)
  (wrap-tinywl-handle-keybinding
   (handle-keybinding)))

(define (tinywl-run startup-cmd handle-keybinding)
  (let ((handle-kb (unwrap-tinywl-handle-keybinding handle-keybinding)))
    (run startup-cmd handle-kb)))
