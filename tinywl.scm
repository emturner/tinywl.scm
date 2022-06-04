(use-modules (system foreign)
	     (system foreign-library)
	     (rnrs enums)
	     (srfi srfi-9))

;; You'll need to run something like this first:
;; export GUILE_EXTENSIONS_PATH="/gnu/store/<...>-profile/lib":$GUILE_EXTENSIONS_PATH
;; to make guile pick up libwlroots.so
(define wlroots (load-foreign-library "libwlroots"))

;; Wrappers for 'wayland-server-core.h'
;;(define wl-display-create
;;  (foreign-library-function wlroots "wl_display_create"
;;			    #:return-type *
;;			    #:arg-types '()))
		      

;; -------------
;; tinywl.c port
;; -------------
(define tinywl-cursor-mode
  (make-enumeration '(tinywl-cursor-passthrough
		      tinywl-cursor-move
		      tinywl-cursor-resize)))

;(define-record-type <tinywl-server>
;  (make-tinywl-server
