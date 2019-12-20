#lang racket

;; ============================================================
;; Racket code samples.
;; ============================================================


;; ------------------------------------------------------------
;; File access:

(define filename "sample.txt")
(when (file-exists? filename)
  (displayln (~a "Temporary file \"" filename "\" exists already. Exiting for safety reasons..."))
  (exit 1))

;; Scheme standard access.

;; Write lines to a file using seperate commands.
(define fh (open-output-file filename))
(display "This is sample text line 1.\n" fh)
(displayln "This is sample text line 2." fh)
(close-output-port fh)

;; Write lines to a file using high-order function.
(call-with-output-file filename
  (lambda (fh)
    (displayln "This is sample text line 1." fh)
    (displayln "This is sample text line 2." fh))
  #:exists 'replace)

;; Print lines from file using separate commands.
(let ((fh (open-input-file filename)))
  (let loop ((line (read-line fh)))
    (when (not (eof-object? line))
      (displayln line)
      (loop (read-line fh))))
  (close-input-port fh))

;; Print lines from file using high-order function.
(call-with-input-file filename
  (lambda (fh)
    (let loop ((line (read-line fh)))
      (when (not (eof-object? line))
        (displayln line)
        (loop (read-line fh))))))

;; Read the whole file to string.
(define content (file->lines filename))
;; Read the whole file to lines.
(define lines (file->lines filename))

;; Remove file.
(delete-file filename)


;; ------------------------------------------------------------
;; Class:

(define counter%
  (class object%
    (super-new)

    (init-field
     name:
     (count: 0))

    (define/public (count step)
      (set! count: (+ count: step)))

    ;; Constructor.
    (set! count: 0)

    ))

;; Create counter object with name.
(define counter (new counter% (name: "my-counter")))

;; Count with counter.
(send counter count 2)
(send counter count 1)
(displayln (~a "Counter value is: " (get-field count: counter)))

(set-field! count: counter 12)
(displayln (~a "Counter value is: " (get-field count: counter)))


;; ------------------------------------------------------------
;; Struct (mutable):

;; Define struct "bundle".
(struct bundle
  (
   name
   vars
   )
  #:mutable)

;; Create bundle.
(define b (bundle "my-bundle" '(foo bar)))

;; Print bundle name.
(displayln (bundle-name b))

;; Set new name for bundle.
(set-bundle-name! b "new-name")
(displayln (bundle-name b))


;; ------------------------------------------------------------
;; Macros:

;; Syntax-rules example:
;;     (my-add 1 + 2)
(define-syntax-rule (my-add a + b)
  (+ a b))

;; Syntax-rules for "when".
(define-syntax my-when
  (syntax-rules ()
    ((_ kond stmt ...)
     (when kond
       stmt ...))))

;; Require racket features for compile-time.
(begin-for-syntax
  (require racket))

;; Procedural-macro version of "my-add".
(define-syntax (my-add2 stx)
  (define parts (syntax->datum stx))
  (datum->syntax stx `(+ ,(second parts) ,(fourth parts))))

;; Procedural-macro version of "my-when".
(define-syntax (my-when2 stx)
  (define parts (syntax->datum stx))
  (datum->syntax
   stx
   `(when ,(second parts)
      ,@(drop parts 2))))
   

;; ------------------------------------------------------------
;; Variable arguments:

;; Lambda without parens means variable args.
(define var-args-open
  (lambda args
    (displayln (length args))))

(var-args-open 1 2 3 4) ; 4

;; Lambda with dot.
(define var-args
  (lambda (first . args)
    (displayln (length (append (list first) args)))))

(var-args 1 2 3 4) ; 4


;; ------------------------------------------------------------
;; Quasiquote:

;; Quoting exceptions are marked with ",".
`(1 2 ,(+ 1 2) 4)     ; (1 2 3 4)

;; Complete list can be unquoted with "@".
`(1 ,@(list 2 3 4))   ; (1 2 3 4)



;; ------------------------------------------------------------
;; Vector:

;; Immutable vector.
(define vec #(1 2 3))
(vector-ref vec 2)
(vector-length vec)
vec

;; Mutable vector.
(set! vec (vector 1 2 3))
(vector-ref vec 2)
(vector-length vec)
(vector-set! vec 0 12)
vec


;; ------------------------------------------------------------
;; Loops and iterations:

(define coll empty)

(let named-let ((i 0))
  (when (< i 4)
    (displayln i)
    (set! coll (append coll (list (+ i 1))))
    (named-let (+ i 1))))

(map
 (lambda (item)
   (* item 2))
 coll)

(for ((i coll))
  (displayln i))

(foldr
 (lambda (a x)
   (displayln (~a "x:" x))
   (+ a (* x x)))
 0
 '(1 2 3 4 5))

(foldl
 (lambda (a x)
   (displayln (~a "a:" a))
   (displayln (~a "x:" x))
   (+ a (* x x)))
 0
 '(1 2 3 4 5))

(foldr
 (lambda (a x)
   (displayln (~a "x:" x))
   (+ a (* x x)))
 0
 '(1 2 3 4 5))

;; Reverse list.
(foldl cons '() '(1 2 3 4))

;; Direct list.
(foldr cons '() '(1 2 3 4))


;; ------------------------------------------------------------
;; Hash (mutable):

(define h (make-hash
           (list
            (cons "foo" "bar")
            (cons "dii" "duu"))))

(hash-ref h "foo")
(hash-keys h)
(hash-set! h "jii" "haa")
(hash-set! h "foo" "haa")
(hash-has-key? h "jii")
(hash-remove! h "jii")
(hash-has-key? h "jii")
h


;; ------------------------------------------------------------
;; YAML:

(require yaml)

;; Load YAML file (to hash).
(define (load-yaml filename)
  (with-input-from-file
    (string->path filename)
    (lambda ()
      (read-yaml))))

;; Dump data (hash) to YAML file.
(define (dump-yaml filename data)
  (with-output-to-file
    (string->path filename)
    (lambda ()
      (write-yaml data))
    #:exists 'replace))


;; ------------------------------------------------------------
;; JSON:

(require json)

;; Load JSON file (to hash).
(define (load-json filename)
  (with-input-from-file
    (string->path filename)
    (lambda ()
      (read-json))))

;; Dump data (hash) to JSON file.
(define (dump-json filename data)
  (with-output-to-file
    (string->path filename)
    (lambda ()
      (write-json data))
    #:exists 'replace))

;; Dump data (hash) to JSON file with the help of "jq". Use subprocess
;; based piping.
(define (dump-pretty-json filename data)
  (let*-values
      ;; Open the JSON-file port for "jq".
      ([(ofh) (open-output-file filename #:mode 'text #:exists 'replace)]
       ;; Create "jq" subprocess with stdout as "ofh".
       ;;    sub->  ->sub sub->               out in err
       [(sub stdout stdin stderr) (subprocess ofh #f #f (find-executable-path "jq" #f) ".")])
    (write-json data stdin)
    (close-output-port stdin)
    (close-output-port ofh)
    (close-input-port stderr)))


;; ------------------------------------------------------------
;; Regular expressions:

(define re #rx"my")
(define msg "This is my message.")
(let ((m (car (regexp-match-positions re msg))))
  (substring msg (car m) (cdr m)))
(car (regexp-match re msg))
(regexp-split re msg)
(regexp-replace re msg "your")


;; ------------------------------------------------------------
;; Load code to current module:
(define-namespace-anchor example-ns)
(parameterize ((current-namespace (namespace-anchor->namespace example-ns)))
  (load "racket-sample.rkt"))


;; ------------------------------------------------------------
;; Module usage:

(module example racket

  (provide
   hello-world
   )

;;  (require racket)

  (define (hello-world)
    (displayln "hello world!"))
  )

(require 'example)
(hello-world)


;; ------------------------------------------------------------
;; Exceptions:

(with-handlers ((exn:fail:filesystem?
                 (lambda (exn) (displayln "no such file..."))))
  (delete-file "file-does-not-exist.txt"))


;; ------------------------------------------------------------
;; Debugging:

;; NOTE: (debug-repl) is also available with:
;;     (require debug/repl)

;; Load debug language.
#lang debug racket

;; Print expression and its value.
#R(+ 1 2)

;; Open REPL and inspect values.
(define a 12)
(debug-repl)

;; ...
;; -> (displayln a)
;; Control-D to quit

