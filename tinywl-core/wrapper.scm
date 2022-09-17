(define-module (tinywl-core wrapper)
  #:use-module (ice-9 format)
  #:use-module (system foreign)
  #:use-module (wayland-server-core)
  #:use-module (emturner util)
  #:export (tinywl-run
            handle-keybinding
            unwrap-tinywl-server
            server->wl-display
            tinywl-cycle-focus-next-view))

(load-extension "tinywl" "init_tinywl_wrapper")

(define-wrapped-pointer-type tinywl-server
  tinywl-server?
  wrap-tinywl-server unwrap-tinywl-server
  (lambda (wlr-b prt)
    (format prt "#<tinywl-server at ~x>"
        (pointer-address (unwrap-tinywl-server wlr-b)))))

(define (server->wl-display server)
  (let* ((svr (unwrap-tinywl-server server))
         (dsp (server-get-wl-display svr)))
    (wrap-wl-display dsp)))

(define (tinywl-cycle-focus-next-view server)
  (focus-next-view (unwrap-tinywl-server server)))

(define (tinywl-run startup-cmd handle-keybinding)
  (let* ((server (wrap-tinywl-server (init-server)))
         (handle-kb (handle-keybinding server)))
    (run (unwrap-tinywl-server server) startup-cmd handle-kb)))
