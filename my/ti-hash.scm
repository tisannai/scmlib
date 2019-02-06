;; -*-scheme-chicken-*-

;; ti-hash module provides short names for common hash functions.

(module ti-hash
    (
        h-new
        h-set
        h-get
        h-del
        h-each
        h-keys
        h-values
        )

    (import scheme)
    (import chicken)

    (use srfi-69)

    (define h-new make-hash-table)
    (define h-set hash-table-set!)
    (define h-get hash-table-ref)
    (define h-del hash-table-delete!)
    (define h-each hash-table-for-each)
    (define h-keys hash-table-keys)
    (define h-vals hash-table-values)
    (define h-has? hash-table-exists?)
    )
