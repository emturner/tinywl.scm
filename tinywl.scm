(define-module (tinywl)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:use-module (rnrs enums)
  #:use-module (srfi srfi-9)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:use-module (wayland-server-core)
  #:use-module (wayland dylib)
  #:use-module (wayland util)
  #:use-module (wlr types wlr-output)
  #:use-module (emturner util)
  #:use-module (emturner clock)
  #:use-module (tinywl-core wrapper)
  #:use-module (ice-9 optargs)
  #:export (run check))

;; -----------------------------
;; Wrappers for 'wlr/util/log.h'
;; -----------------------------
(define wlr-log-importance
  (make-enumeration '(wlr-log-silent
                      wlr-log-error
                      wlr-log-info
                      wlr-log-debug
                      wlr-log-log-importance-last)))

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

(define-class <tinywl-server> ()
  ;; struct wl_display *wl_display;
  (display #:init-value %null-pointer
           #:accessor tinywl-server->display)
  ;; struct wlr_backend *backend;
  (backend #:init-value %null-pointer
           #:accessor tinywl-server->backend)
  ;; struct wlr_renderer *renderer;
  (renderer #:init-value %null-pointer
            #:accessor tinywl-server->renderer)
  ;; struct wlr_xdg_shell *xdg_shell;
  (xdg-shell #:init-value %null-pointer
             #:accessor tinywl-server->xdg-shell)
  ;; struct wl_listener new_xdg_surface;
  (new-xdg-surface #:init-value (make <wl-listener>)
                   #:accessor tinywl-server->new-xdg-surface)
  ;; struct wl_list views; FIXME views subclass
  (views #:init-value (make <wl-list>)
         #:accessor tinywl-server->views)
  ;; struct wlr_cursor *cursor;
  (cursor #:init-value %null-pointer
          #:accessor tinywl-server->cursor)
  ;; struct wlr_xcursor_manager *cursor_mgr;
  (xcursor-mgr #:init-value %null-pointer
               #:accessor tinywl-server->xcursor-mgr)
  ;; struct wl_listener cursor_motion;
  (cursor-motion #:init-value (make <wl-listener>)
                 #:accessor tinywl-server->cursor-motion)
  ;; struct wl_listener cursor_motion_absolute;
  (cursor-motion-absolute #:init-value (make <wl-listener>)
                          #:accessor tinywl-server->cursor-motion-absolute)
  ;; struct wl_listener cursor_button;
  (cursor-button #:init-value (make <wl-listener>)
                 #:accessor tinywl-server->cursor-button)
  ;; struct wl_listener cursor_axis;
  (cursor-axis #:init-value (make <wl-listener>)
               #:accessor tinywl-server->cursor-axis)
  ;; struct wl_listener cursor_frame;
  (cursor-frame #:init-value (make <wl-listener>)
                #:accessor tinywl-server->cursor-frame)
  ;; struct wlr_seat *seat;
  (seat #:init-value %null-pointer
        #:accessor tinywl-server->seat)
  ;; struct wl_listener new_input;
  (new-input #:init-value (make <wl-listener>)
             #:accessor tinywl-server->new-input)
  ;; struct wl_listener request_cursor; 
  (request-cursor #:init-value (make <wl-listener>)
                  #:accessor tinywl-server->request-cursor)
  ;; struct wl_listener request_set_selection;
  (request-set-selection #:init-value (make <wl-listener>)
                         #:accessor tinywl-server->request_set-selection)
  ;; struct wl_list keyboards; FIXME keyboard subclass
  (keyboards #:init-value (make <wl-list>)
             #:accessor tinywl-server->keyboards)
  ;; enum tinywl_cursor_mode cursor_mode;
  (cursor-mode #:init-value 'tinywl-cursor-passthrough
               #:accessor tinywl-server->cursor-mode)
  ;; struct tinywl_view *grabbed_view;
  (grabbed-view #:init-value %null-pointer
                #:accessor tinywl-server->grabbed-view)
  ;; double grab_x, grab_y;
  (grab-coords #:init-value '(0.0 . 0.0)
               #:accessor tinywl-server->grab-coords)
  ;; struct wlr_box grab_geobox; FIXME pointer
  (grab-geobox #:init-value %null-pointer
               #:accessor tinywl-server->grab-geobox)
  ;; uint32_t resize_edges;
  (resize-edges #:init-value 0
                #:accessor tinywl-server->resize-edges)
  ;; struct wlr_output_layout *output_layout;
  (output-layout #:init-value %null-pointer
                 #:accessor tinywl-server->output-layout)
  ;; struct wl_list outputs; FIXME output subclass
  (outputs #:init-value (make <wl-list>)
           #:accessor tinywl-server->outputs)
  ;; struct wl_listener new_output;
  (new-output #:init-value (make <wl-listener>)
              #:accessor tinywl-server->new-output))

(define-class <tinywl-output> (<wl-list>)
  ;; struct tinywl_server *server;
  (server #:getter tinywl-output->server
          #:init-keyword #:server)
  ;; struct wlr_output *wlr_output;
  (output #:getter tinywl-output->wlr-output
          #:init-keyword #:output)
  ;; struct wl_listener frame;
  (listener #:init-value (make <wl-listener>)
            #:getter tinywl-output->frame
            #:init-keyword #:frame))

;; (define (output-frame tinywl-output)
;;   "This function is called every time an output is ready to display a frame,
;; generally at the output's refresh rate (e.g. 60Hz)."
;;   (proc->wl-notify-func-t
;;    (lambda (listener-raw-ptr data-raw-ptr)
;;      (let ((server (tinywl-output->server tinywl-output))
;;            (renderer (tinywl-server->renderer))
;;            (now (clock-gettime->monotomic)))
;;        ;; /* wlr_output_attach_render makes the OpenGL context current. */
;;        (if (attach-render (tinywl-output->wlr-output tinywl-output))
;;            (let ((res (effective-resolution tinywl-output->wlr-output tinywl-output)))
;;              #f))))))


(define (server-new-output-notify server)
  "This event is raised by the backend when a new output (aka a display
or a monitor) becomes available."
  (proc->wl-notify-func-t
   (lambda (listener-raw-ptr data-raw-ptr)
    (let ((wlr-output (make <wlr-output>
                        #:ptr (wrap-wlr-output-ptr data-raw-ptr))))
      ;; Some backends don't have modes. DRM+KMS does,
      ;; and we need to set a mode before we can use the output.
      ;; The mode is a tuple of (width, height, refresh rate), and
      ;; each monitor supports only a specific set of modes. We just
      ;; pick the monitor's preferred mode, a more sophisticated
      ;; compositor would let the user configure it.
      (and (set-preferred-mode wlr-output)
           ;; Allocates and configures our state for this output
           (make <tinywl-output>
             #:output wlr-output
             #:server server)))))) ;; FIXME: frame callback: static void output_frame

;; (define (run verbosity)
;;   (wlr-log-init verbosity)
;;   ;; TODO: startup command
;;   (define server (make <tinywl-server>))
;;   ;; The wayland display is managed by libwayland. It handles accepting
;;   ;; clients from the Unix socket, managing Wayland globals, and so on.
;;   (set! (tinywl-server->display server) (wl-display-create))
;;   ;; The backend is a wlroots feature which abstracts the underlying input
;;   ;; and output hardware. The autocreate option will choose the most
;;   ;; suitable backend based on the current environment, such as opening
;;   ;; an X11 window if an X11 server is running.
;;   (set! (tinywl-server->backend server)
;;         (wlr-backend-auto-create (tinywl-server->display server)))
;;   ;; If we don't provide a renderer, autocreate makes a GLES2 renderr for
;;   ;; us.  The renderer is responsible for defining the various pixel
;;   ;; formats it supports for shared memory, this configures that for
;;   ;; clients.
;;   (set! (tinywl-server->renderer server)
;;         (wlr-renderer-auto-create (tinywl-server->backend server)))
;;   (or (wlr-renderer-init-wl-display
;;        (tinywl-server->renderer server)
;;        (tinywl-server->display server))
;;       (throw "failed to initialise wl-display"))
;;   (wlr-compositor-create (tinywl-server->display server)
;;                          (tinywl-server->renderer server))
;;   (wlr-data-device-manager-create (tinywl-server->display server))
;;   (set! (tinywl-server->output-layout server)
;;         (wlr-output-layout-create))

;;   ;; TODO Configure a listener to be notified when new outputs are available
;;   ;; on the backend.
;;   ;; (server_new_output_notify ('(todo func)))
;;   server)

;; (define (check)
;;   (run 'wlr-error))

(define (make-handle-keybinding server)
  (procedure->pointer
   cstdbool
   (lambda (sym)
     "Here we handle compositor keybindings. This is when the compositor is processing keys,
rather than passing them on to the client for its own processing.

This function assumes Alt is held down."
     (let ((result (match sym
                     ;; XKB_KEY_Escape
                     (#xff1b
                      (or (wl-display-terminate (server->wl-display server))
                          #t))
                     ;; XKB_KEY_F1
                     (#xffbe
                      (tinywl-cycle-focus-next-view server))
                     ;; default
                     (_ #f))))
       (bool->cstdbool result)))
   `(,uint32)))

(define* (gwwm-run #:key (startup-cmd #f)
                         (log-level   'wlr-log-error)
                         (handle-kb   make-handle-keybinding))
  (wlr-log-init log-level)
  (tinywl-run startup-cmd handle-kb))

(define (check)
  (gwwm-run #:startup-cmd "alacritty"
            #:log-level 'wlr-log-debug))
