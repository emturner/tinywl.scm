;; Scheme wrapper for <wayland-util.h>
(define-module (wayland util)
  #:use-module (ice-9 match)
  #:use-module (oop goops)
  #:use-module (system foreign)
  #:use-module (system foreign-library)
  #:use-module (wayland dylib)
  #:export (<wl-list>
	    wl-list-inner
	    wl-list-length
	    wl-list-empty?
	    wl-list-remove))

(define-wrapped-pointer-type wl-list
  wl-list?
  wrap-wl-list unwrap-wl-list
  (lambda (lst prt)
    (let* ((unwrapped (unwrap-wl-list lst))
	   (parsed (parse-c-struct unwrapped '(* *)))
	   (address (pointer-address unwrapped))
	   (prev (pointer-address (car parsed)))
	   (next (pointer-address (cadr parsed))))
      (format prt "#<wl-list at ~x | prev ~x | next ~x>"
	      address
	      prev
	      next))))

(define inner-init
  (let ((init (foreign-library-function wlroots "wl_list_init"
					#:return-type void
					#:arg-types '(*))))
    (lambda (lst)
      "Initialise the list"
      (init (unwrap-wl-list lst)))))

(define (make-wl-list)
  "Creates a new, initialised wl-list instance"
  (let ((lst (wrap-wl-list
	      (make-c-struct '(* *)
			     `(,%null-pointer ,%null-pointer)))))
    (inner-init lst)
    lst))

(define-class <wl-list> ()
  (lst #:init-value (make-wl-list)
       #:accessor wl-list-inner
       #:init-value #:wl-list-inner))

(define inner-length
  (let ((len (foreign-library-function wlroots "wl_list_length"
				       #:return-type int
				       #:arg-types '(*))))
    (lambda (lst)
      (len (unwrap-wl-list lst)))))

(define-method (wl-list-length (l <wl-list>))
  "Returns the length of the wl-list `lst`"
  (inner-length (wl-list-inner l)))

(define inner-empty?
  (let ((is-empty (foreign-library-function wlroots "wl_list_empty"
					    #:return-type int
					    #:arg-types '(*))))
    (lambda (lst)
      (is-empty (unwrap-wl-list lst)))))

(define-method (wl-list-empty? (l <wl-list>))
  "Returns #t if the list is empty, #f otherwise"
  (eq? 1
       (inner-empty?
	(wl-list-inner l))))

(define inner-remove
  (let ((remove (foreign-library-function wlroots "wl_list_remove"
					    #:return-type void
					    #:arg-types '(*))))
    (lambda (lst)
      (remove (unwrap-wl-list lst)))))

;; TODO wl-list-link
(define-method (wl-list-remove (l <wl-list>))
  "Removes the element l from the containing list, and reinitialises l."
  (inner-remove (wl-list-inner l))
  (inner-init (wl-list-inner l)))

;;(define-method (wl-list-

;; (define-class <wl-linkable> (<wl-list>))
