;; Base module to dynamicaly load libwlroots
(define-module (wayland dylib)
  #:use-module (system foreign-library)
  #:export (wlroots))

;; You'll need to run something like this first:
;; export GUILE_EXTENSIONS_PATH="$GUIX_ENVIRONMENT":$GUILE_EXTENSIONS_PATH
;; to make guile pick up libwlroots.so
(define wlroots (load-foreign-library "libwlroots"))
