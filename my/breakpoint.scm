;; -*-scheme-chicken-*-

;; Breakpoint module provides a dummy procedure where script execution
;; can stop.
;;
;; Breakpoint module requires 'trace' egg and loading of both 'trace'
;; and including 'breakpoint'. Example:
;;     (include "breakpoin")
;;     (use trace)
;;     ...
;;     (break breakpoint#here)
;;
;;     --> REPL is entered here...
;;
;; In order to run "my-script" and stop at breakpoint, perform:
;;     shell> csi my-script -- arg1 arg2 
;;
;; "arg1", "arg2", etc. will become cli arguments for the script.

(module breakpoint
    (
        here
        )
    
    (import scheme)
    (import chicken)

    (define (here) #t)
    )
