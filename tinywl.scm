(use-modules (system foreign)
         (system foreign-library)
         (rnrs enums)
         (srfi srfi-9)
         (ice-9 format)
         (wayland dylib))

;; -----
;; Utils
;; -----
(define cstdbool int)

(define (cstdbool->bool cbool)
  (not (eq? 0 cbool)))

(define (bool->cstdbool bool)
  (if bool 0 1))

;; ------------------------------------
;; Wrappers for 'wayland-server-core.h'
;; ------------------------------------
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

;; -----------------------------
;; Wrappers for 'wlr/util/log.h'
;; -----------------------------
(define wlr-log-importance
  (make-enumeration '(wlr-silent
              wlr-error
              wlr-info
              wlr-debug
              wlr-log-importance-last)))

;; TODO: Passes NULL callback - should be able to set this
(define (wlr-log-init verbosity)
  "Will log all messages less than or equal to `verbosity`"
  (let ((init (foreign-library-function wlroots "wlr_log_init"
                    #:return-type void
                    #:arg-types `(,int *)))
    (wlr-log-level ((enum-set-indexer wlr-log-importance)
            verbosity)))
    (if wlr-log-level
    (init wlr-log-level %null-pointer))))

;; ----------------------------
;; Wrappers for 'wlr/backend.h'
;; WARNING: use of unstable api
;; ----------------------------
(define-wrapped-pointer-type wlr-backend
  wlr-backend?
  wrap-wlr-backend unwrap-wlr-backend
  (lambda (wlr-b prt)
    (format prt "#<wlr-backend at ~x>"
        (pointer-address (unwrap-wlr-backend wlr-b)))))

(define wlr-backend-auto-create
  (let ((autocreate
     (foreign-library-function wlroots "wlr_backend_autocreate"
                   #:return-type '*
                   #:arg-types '(*))))
    (lambda (display)
      "Automatically initializes the most suitable backend given the
environment. The backend is created but not started. Returns NULL on failure."
      (let ((backend (autocreate
              (unwrap-wl-display display))))
    (if (null-pointer? backend)
        (throw "wlr_backend_autocreate returned null")
        (wrap-wlr-backend backend))))))

;; ------------------------------------
;; Wrappers for 'wlr/render/renderer.h'
;; WARNING: use of unstable api
;; ------------------------------------
(define-wrapped-pointer-type wlr-renderer
  wlr-renderer?
  wrap-wlr-renderer unwrap-wlr-renderer
  (lambda (wlr-r prt)
    (format prt "#<wlr-renderer at ~x>"
        (pointer-address (unwrap-wlr-renderer wlr-r)))))

(define wlr-renderer-auto-create
  (let ((autocreate
     (foreign-library-function wlroots "wlr_renderer_autocreate"
                   #:return-type '*
                   #:arg-types '(*))))
    (lambda (backend)
      "Automatically initializes the renderer"
      (wrap-wlr-renderer
       (autocreate (unwrap-wlr-backend backend))))))

(define wlr-renderer-init-wl-display
  (let ((init-display
     (foreign-library-function wlroots "wlr_renderer_init_wl_display"
                   #:return-type cstdbool
                   #:arg-types '(* *))))
    (lambda (renderer display)
      "Creates necessary shm and invokes the initialization of the
implementation.

Returns #f on failure."
      (let ((r (unwrap-wlr-renderer renderer))
        (d (unwrap-wl-display display)))
    (cstdbool->bool
     (init-display r d))))))

;; ----------------------------------------
;; Wrappers for 'wlr/types/wlr_xdg_shell.h'
;; WARNING: use of unstable api
;; ----------------------------------------
(define-wrapped-pointer-type wlr-xdg-shell
  wlr-xdg-shell?
  wrap-wlr-xdg-shell unwrap-wlr-xdg-shell
  (lambda (xdg-shell prt)
    (format prt "#<wlr-xdg-shell at ~x>"
        (pointer-address (unwrap-wlr-xdg-shell xdg-shell)))))

(define wlr-xdg-shell-create
  (let ((create
     (foreign-library-function wlroots "wlr_xdg_shell_create"
                   #:return-type '*
                   #:arg-types '(*))))
    (lambda (display)
      "Initializes the xdg-shell"
      (wrap-wlr-xdg-shell
       (create (unwrap-wl-display display))))))

;; -----------------------------------------
;; Wrappers for 'wlr/types/wlr_compositor.h'
;; WARNING: use of unstable api
;; -----------------------------------------
(define-wrapped-pointer-type wlr-compositor
  wlr-compositor?
  wrap-wlr-compositor unwrap-wlr-compositor
  (lambda (compositor prt)
    (format prt "#<wlr-compositor at ~x>"
        (pointer-address (unwrap-wlr-compositor compositor)))))

(define wlr-compositor-create
  (let ((create
     (foreign-library-function wlroots "wlr_compositor_create"
                   #:return-type '*
                   #:arg-types '(* *))))
    (lambda (display renderer)
      "Create the compositor - necessary for clients to allocate surfaces."
      (wrap-wlr-compositor
       (create (unwrap-wl-display display)
           (unwrap-wlr-renderer renderer))))))

;; --------------------------------------------------
;; Wrappers for 'wlr/types/wlr_data_device_manager.h'
;; WARNING: use of unstable api
;; --------------------------------------------------
(define-wrapped-pointer-type wlr-data-device-manager
  wlr-data-device-manager?
  wrap-wlr-data-device-manager unwrap-wlr-data-device-manager
  (lambda (data-device-manager prt)
    (format prt "#<wlr-data-device-manager at ~x>"
        (pointer-address (unwrap-wlr-data-device-manager data-device-manager)))))

(define wlr-data-device-manager-create
  (let ((create
     (foreign-library-function wlroots "wlr_data_device_manager_create"
                   #:return-type '*
                   #:arg-types '(*))))
    (lambda (display)
      "Create the data-device-manager - handles the clipboard."
      (wrap-wlr-data-device-manager
       (create (unwrap-wl-display display))))))

;; --------------------------------------------
;; Wrappers for 'wlr/types/wlr_output_layout.h'
;; WARNING: use of unstable api
;; --------------------------------------------
(define-wrapped-pointer-type wlr-output-layout
  wlr-output-layout?
  wrap-wlr-output-layout unwrap-wlr-output-layout
  (lambda (output-layout prt)
    (format prt "#<wlr-output-layout at ~x>"
        (pointer-address (unwrap-wlr-output-layout output-layout)))))

(define wlr-output-layout-create
  (let ((create
     (foreign-library-function wlroots "wlr_output_layout_create"
                   #:return-type '*
                   #:arg-types '())))
    (lambda ()
      "Create an output layout - utility for working with arrangement of
screens in a physical layout."
      (wrap-wlr-output-layout (create)))))

;; -------------
;; tinywl.c port
;; -------------
(define tinywl-cursor-mode
  (make-enumeration '(tinywl-cursor-passthrough
                      tinywl-cursor-move
                      tinywl-cursor-resize)))

(define (server-new-output-notify)
  "This event is raised by the backend when a new output (aka a display
or a monitor) becomes available."
  (pointer->procedure void
                      (lambda (listener data)
                        (let ((output (wrap-wlr-output data)))
                          void))
                      '(* *)))

;(define-record-type <tinywl-server>
;  (make-tinywl-server

(define (run verbosity)
  (wlr-log-init verbosity)
  ;; TODO: startup command
  (let*
      ;; The wayland display is managed by libwayland. It handles accepting
      ;; clients from the Unix socket, managing Wayland globals, and so on.
      ((display (wl-display-create))
       ;; The backend is a wlroots feature which abstracts the underlying input
       ;; and output hardware. The autocreate option will choose the most
       ;; suitable backend based on the current environment, such as opening
       ;; an X11 window if an X11 server is running.
       (backend (wlr-backend-auto-create display))
       ;; If we don't provide a renderer, autocreate makes a GLES2 renderr for
       ;; us.  The renderer is responsible for defining the various pixel
       ;; formats it supports for shared memory, this configures that for
       ;; clients.
       (renderer (wlr-renderer-auto-create backend))
       (_ (or (wlr-renderer-init-wl-display renderer display)
          (throw "failed to initialise wl-display")))
       (_ (wlr-compositor-create display renderer))
       (_ (wlr-data-device-manager-create display))
       (output-layout (wlr-output-layout-create)))
    output-layout))

(define (check)
  (run 'wlr-error))
