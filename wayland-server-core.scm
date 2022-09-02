;; --------------------------------------------
;; Wrappers for <wayland-server-core.h>
;; --------------------------------------------
(define-module (wayland-server-core)
  #:use-module (ice-9 format)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:use-module (wayland dylib)
  #:use-module (wayland util)
  #:export (wl-notify-func-t
            proc->wl-notify-func-t
            wl-listener
            <wl-listener>
            wl-listener-c-type
            wl-display
            wl-display-create)
  #:export-syntax (unwrap-wl-display))


(define-wrapped-pointer-type wl-display
  wl-display?
  wrap-wl-display unwrap-wl-display
  (lambda (wl-d prt)
    (format prt "#<wl-display at ~x>"
        (pointer-address (unwrap-wl-display wl-d)))))

(define wl-display-create
  (let ((create (foreign-library-function wlroots "wl_display_create"
                      #:return-type '*
                      #:arg-types '())))
    (lambda ()
      "Create a wl-display"
      (wrap-wl-display (create)))))

;;  "A wl-notify-func-t expects a *wl_listener, and a void* data."
(define-wrapped-pointer-type wl-notify-func-t
  wl-notify-func-t?
  wrap-wl-notify-func-t unwrap-wl-notify-func-t
  (lambda (output prt)
    (format prt "#<wl-notify-func-t at ~x>"
        (pointer-address (unwrap-wl-notify-func-t output)))))

(define (proc->wl-notify-func-t proc)
  (let ((ptr (procedure->pointer void proc '(* *))))
    (wrap-wl-notify-func-t ptr)))

;; "A wl-listener contains a wl-list link, and a wl-notify-func-t"
(define-wrapped-pointer-type wl-listener
  wl-listener?
  wrap-wl-listener unwrap-wl-listener
  (lambda (output prt)
    (format prt "#<wl-listener at ~x>"
        (pointer-address (unwrap-wl-notify-func-t output)))))

(define-class <wl-listener> (<wl-list>)
  (self        #:init-value %null-pointer
               #:getter wl-listener->self
               #:init-keyword #:self)
  (notify-func #:init-value %null-pointer
               #:accessor wl-listener->notify-func
               #:init-keyword #:notify-func))

(define wl-listener-c-type `(,wl-list-c-type *))

(define-method (initialize (self <wl-listener>) initargs)
  (let* ((listener-raw (make-c-struct wl-listener-c-type
                                      `((,%null-pointer ,%null-pointer)
                                        ,%null-pointer)))
         (lst-raw listener-raw)
         (notify-raw (cadr (parse-c-struct listener-raw
                                           wl-listener-c-type)))
         (listener
          (next-method self
                       (list #:self (wrap-wl-listener listener-raw)
                             #:inner-list (wrap-wl-list lst-raw)
                             #:notify-fun (wrap-wl-notify-func-t notify-raw)))))
    (wl-list-init listener)
    listener))
