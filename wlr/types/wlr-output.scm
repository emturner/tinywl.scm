(define-module (wlr types wlr-output)
  #:use-module (ice-9 format)
  #:use-module (ice-9 match)
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
            wlr-output-c-type))

;; Wrapper for <wlr/types/wlr_output.h>
;; WARNING: use of unstable api

(define-wrapped-pointer-type wlr-output-ptr
  wlr-output-ptr?
  wrap-wlr-output-ptr unwrap-wlr-output-ptr
  (lambda (output prt)
    (format prt "#<wlr-output-ptr at ~x>"
        (pointer-address (unwrap-wlr-output-ptr output)))))

;; A compositor output region. This typically corresponds to a monitor that
;; displays part of the compositor space.
;;
;; The `frame` event will be emitted when it is a good time for the compositor
;; to submit a new frame.
;;
;; To render a new frame, compositors should call `wlr_output_attach_render`,
;; render and call `wlr_output_commit`. No rendering should happen outside a
;; `frame` event handler or before `wlr_output_attach_render`.
(define-class <wlr-output> ())

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
    ,int32 ;;  int32_t width, height;
    ,int32 ;;  int32_t refresh; // mHz, may be zero
    ,cstdbool ;;  bool enabled;
    ,float ;;  float scale;
    ;; FIXME  enum wl_output_subpixel subpixel;
    ;; FIXME  enum wl_output_transform transform;
    ;; FIXME  enum wlr_output_adaptive_sync_status adaptive_sync_status;
    ,cstdbool ;;  bool needs_frame;
    ;; damage for cursors and fullscreen surface, in output-local coordinates
    ,cstdbool ;;  bool frame_pending;
    ,float ;;  float transform_matrix[9];
    ;; FIXME struct wlr_output_state pending;
    ;; Commit sequence number. Incremented on each commit, may overflow.
    ,uint32 ;; uint32_t commit_seq;
    (;;     struct events
     ;; Request to render a frame
     ;; FIXME struct wl_signal frame;
     ;; Emitted when software cursors or backend-specific logic damage the
     ;; output
     ;; FIXME struct wl_signal damage; // wlr_output_event_damage
     ;; Emitted when a new frame needs to be committed (because of
     ;; backend-specific logic)
     ;; FIXME struct wl_signal needs_frame;
     ;; Emitted right before commit
     ;; FIXME struct wl_signal precommit; // wlr_output_event_precommit
     ;; Emitted right after commit
     ;; FIXME struct wl_signal commit; // wlr_output_event_commit
     ;; Emitted right after the buffer has been presented to the user
     ;; FIXME struct wl_signal present; // wlr_output_event_present
     ;; Emitted after a client bound the wl_output global
     ;; FIXME struct wl_signal bind; // wlr_output_event_bind
     ;; FIXME struct wl_signal enable;
     ;; FIXME struct wl_signal mode;
     ;; FIXME struct wl_signal description;
     ;; FIXME struct wl_signal destroy;
     )
     * ;; struct wl_event_source *idle_frame;
     * ;; struct wl_event_source *idle_done;
     ,int;; int attach_render_locks; // number of locks forcing rendering
     ,wl-list-c-type ;; struct wl_list cursors; // wlr_output_cursor::link
     ;; FIXME struct wlr_output_cursor *hardware_cursor;
     ;; FIXME struct wlr_swapchain *cursor_swapchain;
     ;; FIXME struct wlr_buffer *cursor_front_buffer;
     ,int ;; int software_cursor_locks; // number of locks forcing software cursors
     * ;; struct wlr_swapchain *swapchain;
     * ;; struct wlr_buffer *back_buffer;
     ,wl-listener-c-type ;; struct wl_listener display_destroy;
     * ;; void *data;
     ))
