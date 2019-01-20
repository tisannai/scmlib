;; -*-scheme-chicken-*-

;; Module for Graph (DAG) based operations.
(module graph
    (
        node-list
        node-name
        node-visited
        node-data
        node-new
        node-new-data
        node-new-var
        node-connect-pair
        node-connect-pair-with-value
        node-connect-pairs
        node-connect-pairs-with-values
        node-visit
        node-unvisit
        node-visited?
        node-first
        node-for-all
        node-unvisit-all
        node-find
        edge-node
        edge-value
        dfs-common
        dfs
        dfs-env
        bfs-common
        bfs
        topological-sort
;;        shortest-or-longest-path-common
        shortest-path-between-nodes
        longest-path-between-nodes
        )

    (import scheme)
    (import chicken)
    (use srfi-1)

    ;; List of all created nodes.
    (define node-list '())

    ;; Node definition.
    ;;
    ;; Data is collection of user data.
    (define-record node
        name       ; Node name
        visited    ; Node visited (support for algos)
        ilink      ; Links left:  ( (<node> value) ... )
        olink      ; Links right: ( (<node> value) ... )
        data       ; Generic data storage.
        )

    ;; Create new node with name.
    (define (node-new name)
        (make-node name #f '() '() #f))

    ;; Create new node with name and data.
    (define (node-new-data name data)
        (make-node name #f '() '() data))

    ;; Create new node var with name "var" and given data.
    ;;
    ;;     (node-new-var task1 12)
    ;;
    ;; Creates var task1 with name "task1" and value 12.
    (define-syntax node-new-var
        (syntax-rules ()
            ((_ name data)
                (begin
                    (define name (node-new-data (symbol->string (quote name)) data))
                    (set! node-list (append node-list (list name)))))))


    ;; Connect "a" and "b".
    ;;
    ;; Both "olink" list of "a" and "ilink" of "b" is updated.
    (define (node-connect-pair a b)
        (node-olink-set! a (append (node-olink a) (list (list b 0))))
        (node-ilink-set! b (append (node-ilink b) (list (list a 0)))))


    ;; Connect "a" and "b" and add edge value, i.e. (<node> <value>).
    (define (node-connect-pair-with-value a b value)
        (node-olink-set! a (append (node-olink a) (list (cons b value))))
        (node-ilink-set! b (append (node-ilink b) (list (cons a value)))))


    ;; Multiple pair connection tool.
    ;;
    ;;     (node-connect-pairs a b a c)
    ;;
    (define node-connect-pairs
        (lambda connections
            (let each ((cs connections))
                (when (pair? cs)
                    (node-connect-pair (car cs) (cadr cs))
                    (each (cddr cs))))))


    ;; Multiple pair (with value/weight) connection tool.
    ;;
    ;;     (node-connect-pairs-with-values a b a c)
    ;;
    (define node-connect-pairs-with-values
        (lambda connections
            (let each ((cs connections))
                (when (pair? cs)
                    (node-connect-pair-with-value (car cs) (cadr cs) (caddr cs))
                    (each (cdddr cs))))))


    ;; Visit node.
    (define (node-visit node)
        (node-visited-set! node #t))

    ;; Unvisit node.
    (define (node-unvisit node)
        (node-visited-set! node #f))

    ;; Return visit status.
    (define (node-visited? node)
        (node-visited node))

    ;; Return first node.
    (define (node-first)
        (car node-list))


    ;; Iterate over all nodes.
    (define (node-for-all fn)
        (let each-node ((nodes node-list))
            (when (pair? nodes)
                (fn (car nodes))
                (each-node (cdr nodes)))))


    ;; Unvisit all nodes.
    (define (node-unvisit-all)
        (node-for-all
            (lambda (node)
                (node-unvisit node))))


    ;; Find given node and return it (false if not found).
    (define (node-find node)
        (call/cc
            (lambda (cc)
                (let each ((nodes node-list))
                    (when (pair? nodes)
                        (when (eq? node (car nodes))
                            (cc node))
                        (each (cdr nodes))))
                #f)))


    ;; Return edge node.
    (define (edge-node pair)
        (car pair))

    ;; Return edge value.
    (define (edge-value pair)
        (cdr pair))


    ;; ------------------------------------------------------------
    ;; Depth First Search:

    ;; Depth first travel of graph with starting node as "node".
    ;;
    ;; Args:
    ;;     node: Node.
    ;;     ifh:  Node input action.
    ;;     ofh:  Node output action.
    ;;     env:  Action environment.
    ;;
    ;;     (dfs-common node (lambda (node) (print (node-name node))) #f #f)
    ;;
    (define (dfs-common node ifn ofn env)
        (when node
            (call/cc
                (lambda (exit-cc)
                    (define (dfs+ exit-cc node ifn ofn env)
                        (call/cc
                            (lambda (stop-cc)
                                (if ifn
                                    (case (ifn node env)
                                        ((exit) (exit-cc #f))
                                        ((stop) (stop-cc #f))))
                                (let each-olink ((olink (node-olink node)))
                                    (when (pair? olink)
                                        (if (not (node-visited? (car (edge-node olink))))
                                            (dfs+ exit-cc (car (edge-node olink)) ifn ofn env))
                                        (node-visit (car (edge-node olink)))
                                        (each-olink (cdr olink))))
                                (if ofn
                                    (case (ofn node env)
                                        ((exit) (exit-cc #f))
                                        ((stop) (stop-cc #f)))))))
                    (dfs+ exit-cc node ifn ofn env)))
            (node-unvisit-all)))

    ;; Depth-first-search from "node" with action "fn" on input.
    (define (dfs node fn)
        (dfs-common node fn #f #f))

    ;; Depth-first-search from "node" with action "fn" on input with "env:.
    (define (dfs-env node fn env)
        (dfs-common node fn #f env))


    ;; ------------------------------------------------------------
    ;; Breath First Search:

    ;; Breath first travel of graph with starting node as "node".
    ;;
    ;; Args:
    ;;     node: Node.
    ;;     ifh:  Node input action.
    ;;     ofh:  Node output action.
    ;;     env:  Action environment.
    ;;
    ;;     (bfs-common node (lambda (node) (print (node-name node))) #f #f)
    (define (bfs-common node ifn ofn env)
        (when node

            (call/cc
                (lambda (exit-cc)
                    (define (bfs+ exit-cc nodes ifn ofn env)
                        (let ((next-nodes '()))
                            (let each-node ((nodes nodes))
                                (when (pair? nodes)
                                    (let each-olink ((olink (node-olink (car nodes))))
                                        (when (pair? olink)
                                            (when (not (node-visited? (car (edge-node olink))))
                                                (node-visit (car (edge-node olink)))
                                                (call/cc
                                                    (lambda (stop-cc)
                                                        (if ifn
                                                            (case (ifn (car nodes) env)
                                                                ((exit) (exit-cc #f))
                                                                ((stop) (stop-cc #f))))
                                                        (set!
                                                            next-nodes
                                                            (append next-nodes (list (car (edge-node olink)))))
                                                        (each-olink (cdr olink)))))))
                                    (if ofn
                                        (case (ofn (car nodes) env)
                                            ((exit) (exit-cc #f))))
                                    (each-node (cdr nodes))))
                            (when (pair? next-nodes)
                                (bfs+ exit-cc next-nodes ifn ofn env))))

                    (if ifn
                        (case (ifn node env)
                            ((exit stop) (exit-cc #f))))
                    (node-visit node)
                    (bfs+ exit-cc (list node) ifn ofn env)))
            (node-unvisit-all)))


    ;; Breath-first-search from "node" with action "fn" on input.
    (define (bfs node fn)
        (bfs-common node fn #f #f))


    ;; ------------------------------------------------------------
    ;; Topological sort:

    (define (topological-sort)

        ;; Input function.
        (define (topo-sort-ifn node env)
            (if (not (node-visited? node))
                (begin
                    (node-visit node)
                    'continue)
                'stop))

        ;; Output function.
        (define (topo-sort-ofn node env)
            (set-car! env (cons node (car env))))

        ;; DFS based ordering using stack.
        (let ([ordered '(())])
            (dfs-common (node-first) topo-sort-ifn topo-sort-ofn ordered)
            (car ordered)))


    ;; ------------------------------------------------------------
    ;; Shortest and longest path:

    ;; Return (node . length) pairs from "a".
    (define (shortest-or-longest-path-common a mode) ; INTERNAL

        (let ([inf #f] [compare #f])
            (case mode
                ((shortest-path)
                    (set! inf +inf.0)
                    (set! compare >))
                ((longest-path)
                    (set! inf -inf.0)
                    (set! compare <)))

                (define (val-get node)
                    (car (node-data node)))

                (define (val-set! node val)
                    (set-car! (node-data node) val))

                ;; Set all nodes to positive infinity.
                (node-for-all
                    (lambda (node)
                        ;; Put inf in front of old data.
                        (node-data-set! node (cons inf (node-data node)))))

                (val-set! a 0)
                (let each-node ([sorted (topological-sort)])
                    (when (pair? sorted)
                        (let each-olink ([olink (node-olink (car sorted))])
                            (when (pair? olink)
                                (let ([node (edge-node (car olink))]
                                         [new-value (+ (val-get (car sorted)) (edge-value (car olink)))])
                                    (when (compare (val-get node) new-value)
                                        (val-set! node new-value)))
                                (each-olink (cdr olink))))
                        (each-node (cdr sorted))))
                (let ([dist (map
                                (lambda (i)
                                    (cons i (val-get i)))
                                node-list)])
                    (node-for-all
                        (lambda (node)
                            ;; Restore original data.
                            (node-data-set! node (cdr (node-data node)))))
                    dist)))


    ;; Return shortest distance between nodes "a" and "b".
    (define (shortest-path-between-nodes a b)
        (let ([dist (shortest-or-longest-path-common a 'shortest-path)])
            (cdr (find (lambda (i) (eqv? b (car i))) dist))))


    ;; Return longest distance between nodes "a" and "b".
    (define (longest-path-between-nodes a b)
        (let ([dist (shortest-or-longest-path-common a 'longest-path)])
            (cdr (find (lambda (i) (eqv? b (car i))) dist))))

    )
