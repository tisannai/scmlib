;; My language extensions for Chicken Scheme.
;;
;; Usage:
;;     (include "ti-lang")

(module ti-lang
    (
        times
        set!-many
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


    ;; Let multiple variables from list.
    ;;
    ;; Usage:
    ;;     (let-many (a b) '(100 200)
    ;;         (print a))
    (define-syntax let-many
        (syntax-rules ()
            ((_ (syms ...) vals expr . exprs)
                (let ((syms (if #f #f)) ...)
                    (set!-values (syms ...) (apply values vals))
                    expr . exprs))))

    ;; Set multiple variables from list.
    ;;
    ;; Usage:
    ;;     (set!-many (a b) '(100 200))
    (define-syntax set!-many
        (ir-macro-transformer
            (lambda (exp inject compare)
                (let ((syms (cadr exp))
                         (vals (caddr exp)))
                    `(set!-values ,syms (apply values ,vals))))))


    ;; Create variables with undefined start values.
    ;;
    ;; Usage:
    ;;     (let-nil (a b)
    ;;         (set! a 200))
    (define-syntax let-nil
        (syntax-rules ()
            ((_ (sym ...) expr . exprs)
                (let ((sym (if #f #f)) ...) expr . exprs))))

    )
