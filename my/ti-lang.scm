;; My language extensions for Chicken Scheme.
;;
;; Usage:
;;     (include "ti-lang")

(module ti-lang
    (
        times
        set!-values
        letnil
        )

    (import scheme)
    (import chicken)

    ;; Repeat function N times.
    ;;
    ;; Usage:
    ;;     (times 100 (lambda () (print "yes")))
    (define (times count function . args)
        (let loop ((times-i 0))
            (when (< times-i count)
                (apply function args)
                (loop (+ times-i 1)))))


    ;; Set multiple variables from list.
    ;;
    ;; Usage:
    ;;     (set!-values (a b) '(100 200))
    (define-syntax set!-values
        (syntax-rules ()
            ((_ (sym ...) '(val ...)) (begin (set! sym val) ...))))


    ;; Create variables with undefined start values.
    ;;
    ;; Usage:
    ;;     (letnil (a b)
    ;;         (set! a 200))
    (define-syntax letnil
        (syntax-rules ()
            ((_ (sym ...) expr . exprs)
                (let ((sym (if #f #f)) ...) expr . exprs))))

    )
