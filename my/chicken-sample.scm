;; ============================================================
;; Chicken Scheme code samples.
;; ============================================================

;; ------------------------------------------------------------
;; Coops:

(use coops)

;; "<counter>" is class name.
(define-class <counter> ()
    ((count 0)))

;; "count" is method.
;; "counter" is object.
;; "<counter>" is class.
;; "slot-value <obj> <member>" can be used as lvalue and rvalue.
(define-method (count (counter <counter>) step)
    (set! (slot-value counter 'count) (+ (slot-value counter 'count) step)))

(define-method (value (counter <counter>))
    (slot-value counter 'count))

;; Create class instance with "make".
(define cnt (make <counter>))

(count cnt 2)
(value cnt)


;; ------------------------------------------------------------
;; Record:

;; Define record "unit".
;; Members: "name", "vars".
(define-record unit
    name
    vars)

;; Record "constructor".
(define (unit-new name)
    ;; Some documenting text here.
    (make-unit name '()))

(define u (unit-new "foobar"))

;; Default Record funcs.
(unit-name u)
(unit-name-set! u "diiduu")


;; ------------------------------------------------------------
;; Macros:

;; Syntax-rules example:
;;     (madd 1 + 2)
(define-syntax madd
    (syntax-rules ()
        ((_ a + b) (+ a b))))

;; Syntax-rules for "when".
(define-syntax mwhen
    (syntax-rules ()
        ((_ kond (stmt ...))
            (if kond
                (begin
                    stmt
                    ...
                    )))))

;; er-macro-transformer style version of "madd".
(define-syntax er-add
    (er-macro-transformer
        (lambda (exp rename compare)
            (let ((plussign +)
                     (a1 (cadr exp))
                     (a2 (cadddr exp)))
                `(,plussign ,a1 ,a2)))))

;; ir-macro-transformer style version of "mwhen".
;;
;; (ir-when (= a 1)
;;    (print a)
;;    (print a))
;; ->
;; (if (= a 1)
;;    (begin
;;        (print a)
;;        (print a)))
(define-syntax ir-when
    (ir-macro-transformer
        (lambda (exp inject compare)
            (let ((kond (cadr exp))
                     (body (cddr exp)))
                `(if ,kond (begin ,@body))))))


;; ------------------------------------------------------------
;; Variable arguments:

;; Lambda without parens means variable args.
(define var-args-open
    (lambda args
        (print (length args))))

(var-args-open 1 2 3 4) ; 4

;; Lambda with dot.
(define var-args
    (lambda (first . args)
        (print (length (append (list first) args)))))

(var-args 1 2 3 4) ; 4


;; ------------------------------------------------------------
;; Quasiquote:

;; Quoting exceptions are marked with ",".
`(1 2 ,(+ 1 2) 4)     ; (1 2 3 4)

;; Complete list can be unquoted with "@".
`(1 ,@(list 2 3 4))   ; (1 2 3 4)

