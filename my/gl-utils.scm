(define-module (my gl-utils))

(export
 cli-args
 strcat
 shell-capture
 print
 dir-list
 string->procedure
 eval-str
 )

(use-modules
 (ice-9 format)
 (ice-9 popen)
 (ice-9 rdelim)
 (ice-9 ftw)
 )


;; Return command line arguments, excluding the program.
(define (cli-args)
  (cdr (command-line)))


;; Concatenate given strings.
;
;;     (strcat "foo" "bar")
(define strcat
  (lambda args
    (apply string-concatenate (list args))))


;; Capture shell command output.
;;     (define s (shell-capture "ls -lF"))
(define (shell-capture cmd)
  (with-input-from-port (open-input-pipe "git rev-parse --abbrev-ref HEAD")
    (lambda ()
      (read-delimited "" (current-input-port)))))


;; Print all arguments and end with newline.
(define print
  (lambda args
    (when (pair? args)
      (let loop ((tail args))
        (when (pair? tail)
          (display (car tail))
          (loop (cdr tail))))
      (newline))))


;; List given directory entries without the dot files.
(define (dir-list dir)
  (list-tail (scandir dir) 2))


;; Convert string to procedure.
(define (string->procedure str)
  (eval (read (open-input-string str)) (interaction-environment)))

;; Evaluate string as Scheme code.
;;     (eval-str "(car '(1 2))")
(define (eval-str str . args)
  (apply (string->procedure str) args))
