;; --------------------------------------------
;; Wrappers for <wayland-server-protocol.h>
;; --------------------------------------------
(define-module (wayland-server-core)
  #:use-module (ice-9 format)
  #:use-module (rnrs enums)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:use-module (wayland dylib)
  #:use-module (wayland util)
  #:export (wl-output-subpixel)
  #:export-syntax ())

;; @ingroup iface_wl_output
;; subpixel geometry information
;;
;; This enumeration describes how the physical
;; pixels on an output are laid out.
(define wl-output-subpixel
  (make-enumeration '(wl-output-subpixel-unknown
                      wl-output-subpixel-none
                      wl-output-subpixel-horizontal-rgb
                      wl-output-subpixel-horizontal-bgr
                      wl-output-subpixel-vertical-rgb
                      wl-output-subpixel-vertical-bgr)))

