(define-module (wlr types wlr-output)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
  #:use-module (ice-9 optargs)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:use-module (wayland-server-core)
  #:use-module (wayland dylib)
  #:use-module (wayland util)
  #:use-module (emturner util)
  #:export (wlr-output-ptr
            wlr-output-ptr?
            wrap-wlr-output-ptr
            unwrap-wlr-output-ptr
            wlr-output-c-type
            <wlr-output>
            set-preferred-mode))

;; Wrapper for <wlr/types/wlr_output.h>
;; WARNING: use of unstable api

;; Holds the double-buffered output state.
(define wlr-output-state-c-type
  `(,uint32 ;; uint32_t committed; // enum wlr_output_state_field
    ,int32;; pixman_region32_t damage; // output-buffer-local coordinates FIXME: unsure of actual size?
    ,cstdbool ;; bool enabled;
    ,float ;; float scale;
    ,int ;; enum wl_output_transform transform;
    ,cstdbool ;; bool adaptive_sync_enabled;

    ;; only valid if WLR_OUTPUT_STATE_BUFFER
    ,int ;; enum wlr_output_state_buffer_type buffer_type;
    * ;; struct wlr_buffer *buffer; // if WLR_OUTPUT_STATE_BUFFER_SCANOUT

    ;; only valid if WLR_OUTPUT_STATE_MODE
    ,int ;; enum wlr_output_state_mode_type mode_type;
    * ;; struct wlr_output_mode *mode;
    (,int32 ,int32 ;; int32_t width, height;
     ,int32 ;; int32_t refresh; // mHz, may be zero
     ) ;; custom_mode;

    ;; only valid if WLR_OUTPUT_STATE_GAMMA_LUT
    ,uint16 ;; uint16_t *gamma_lut;
    ,size_t));; size_t gamma_lut_size;

(define-wrapped-pointer-type wlr-output-ptr
  wlr-output-ptr?
  wrap-wlr-output-ptr unwrap-wlr-output-ptr
  (lambda (output-ptr prt)
    (format prt "#<wlr-output-ptr at ~x>"
        (pointer-address (unwrap-wlr-output-ptr output-ptr)))))

(define wlr-output-c-type
  `(* ;; const struct wlr_output_impl *impl
    * ;; struct wlr_backend *backend
    * ;; struct wl_display * display
    * ;; struct wl_global *global;
    ,wl-list-c-type ;; struct wl_list resources;
    ,(repeat int8 24) ;;  char name[24];
    * ;;  char *description; // may be NULL
    ,(repeat int8 56) ;;  char make[56];
    ,(repeat int8 16) ;;  char model[16];
    ,(repeat int8 16) ;;  char serial[16];
    ,int32 ;;  int32_t phys_width, phys_height; // mm
    ;;  Note: some backends may have zero modes
    ,wl-list-c-type ;;  struct wl_list modes; // wlr_output_mode::link
    * ;;  struct wlr_output_mode *current_mode;
    ,int32 ,int32 ;;  int32_t width, height;
    ,int32 ;;  int32_t refresh; // mHz, may be zero
    ,cstdbool ;;  bool enabled;
    ,float ;;  float scale;
    ,int ;; enum wl_output_subpixel subpixel;
    ,int ;; enum wl_output_transform transform;
    ,int ;; enum wlr_output_adaptive_sync_status adaptive_sync_status;
    ,cstdbool ;;  bool needs_frame;
    ;; damage for cursors and fullscreen surface, in output-local coordinates
    ,cstdbool ;;  bool frame_pending;
    ,float ;;  float transform_matrix[9];
    ,wlr-output-state-c-type ;; struct wlr_output_state pending;
    ;; Commit sequence number. Incremented on each commit, may overflow.
    ,uint32 ;; uint32_t commit_seq;
    (;;     struct events
     ;; Request to render a frame
     ,wl-signal-c-type;; struct wl_signal frame;
     ;; Emitted when software cursors or backend-specific logic damage the
     ;; output
     ,wl-signal-c-type;; struct wl_signal damage; // wlr_output_event_damage
     ;; Emitted when a new frame needs to be committed (because of
     ;; backend-specific logic)
     ,wl-signal-c-type;; struct wl_signal needs_frame;
     ;; Emitted right before commit
     ,wl-signal-c-type;; struct wl_signal precommit; // wlr_output_event_precommit
     ;; Emitted right after commit
     ,wl-signal-c-type;; struct wl_signal commit; // wlr_output_event_commit
     ;; Emitted right after the buffer has been presented to the user
     ,wl-signal-c-type;; struct wl_signal present; // wlr_output_event_present
     ;; Emitted after a client bound the wl_output global
     ,wl-signal-c-type;; struct wl_signal bind; // wlr_output_event_bind
     ,wl-signal-c-type;; struct wl_signal enable;
     ,wl-signal-c-type;; struct wl_signal mode;
     ,wl-signal-c-type;; struct wl_signal description;
     ,wl-signal-c-type;; struct wl_signal destroy;
     )
     * ;; struct wl_event_source *idle_frame;
     * ;; struct wl_event_source *idle_done;
     ,int;; int attach_render_locks; // number of locks forcing rendering
     ,wl-list-c-type ;; struct wl_list cursors; // wlr_output_cursor::link
     * ;; struct wlr_output_cursor *hardware_cursor;
     * ;; struct wlr_swapchain *cursor_swapchain;
     * ;; struct wlr_buffer *cursor_front_buffer;
     ,int ;; int software_cursor_locks; // number of locks forcing software cursors
     * ;; struct wlr_swapchain *swapchain;
     * ;; struct wlr_buffer *back_buffer;
     ,wl-listener-c-type ;; struct wl_listener display_destroy;
     * ;; void *data;
     ))

;; A compositor output region. This typically corresponds to a monitor that
;; displays part of the compositor space.
;;
;; The `frame` event will be emitted when it is a good time for the compositor
;; to submit a new frame.
;;
;; To render a new frame, compositors should call `wlr_output_attach_render`,
;; render and call `wlr_output_commit`. No rendering should happen outside a
;; `frame` event handler or before `wlr_output_attach_render`.
(define-class <wlr-output> ()
  (self        #:init-value %null-pointer
               #:getter wlr-output->self
               #:init-keyword #:self)
  (modes #:init-value %null-pointer
               #:getter wlr-output->modes
               #:init-keyword #:modes))

(define (wlr-output-get-modes ptr)
  (let* ((ptr-raw (unwrap-wlr-output-ptr ptr))
         (output-ptr (parse-c-struct ptr-raw wlr-output-c-type))
         (modes (list-ref output-ptr 11)))
    (make <wl-list> #:wl-list-inner (wrap-wl-list modes))))

(define-method (initialize (self <wlr-output>) initargs)
  (let-keywords initargs #f (ptr)
                (next-method self
                             (list #:self ptr
                                   #:modes (wlr-output-get-modes ptr)))))

(define-wrapped-pointer-type wlr-output-mode-ptr
  wlr-output-mode-ptr?
  wrap-wlr-output-mode-ptr unwrap-wlr-output-mode-ptr
  (lambda (mode prt)
    (format prt "#<wlr-output-mode-ptr at ~x>"
        (pointer-address (unwrap-wlr-output-mode-ptr mode)))))

(define wlr-output-preferred-mode
  (let ((f (foreign-library-function wlroots "wlr_output_preferred_mode"
                                     #:return-type '*
                                     #:arg-types '(*))))
    (lambda (wlr-output)
      "Returns the preferred mode for this output. If the output doesn't
support modes, returns NULL."
      (wrap-wlr-output-mode-ptr
       (f (unwrap-wlr-output-ptr wlr-output))))))

(define wlr-output-set-mode
  (let ((f (foreign-library-function wlroots "wlr_output_set_mode"
                                     #:return-type void
                                     #:arg-types '(* *))))
    (lambda (wlr-output mode)
      "Sets the output mode. The output needs to be enabled.

Mode is double-buffered state, see `wlr-output-commit`."
       (f (unwrap-wlr-output-ptr wlr-output)
          (unwrap-wlr-output-mode-ptr mode)))))

(define wlr-output-commit
  (let ((f (foreign-library-function wlroots "wlr_output_commit"
                                     #:return-type cstdbool
                                     #:arg-types '(*))))
    (lambda (wlr-output)
      "Commit the pending output state. If `wlr-output-attach-render` has been
called, the pending frame will be submitted for display and a `frame` event
will be scheduled.

On failure, the pending changes are rolled back."
       (cstdbool->bool (f (unwrap-wlr-output-ptr wlr-output))))))

(define-method (set-mode (self <wlr-output>) mode)
  (wlr-output-set-mode (wlr-output->self self) mode))

(define-method (set-preferred-mode (self <wlr-output>))
  (or (wl-list-empty? (wlr-output->modes self))
      (let ((preferred-mode (wlr-output-preferred-mode
                             (wlr-output->self self))))
        (set-mode self preferred-mode)
        (wlr-output-commit (wlr-output->self self)))))
