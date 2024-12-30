(define (extract-identifiers expr)
  (cond
    ((symbol? expr)
     (if (or (string-prefix? "input" (symbol->string expr))
             (string-prefix? "node" (symbol->string expr))
             (string-prefix? "output" (symbol->string expr)))
         (list expr)  
         '()))
    ((list? expr) 
     (apply append (map extract-identifiers expr)))
    (else '())))  


(define find-undeclared-identifiers
  (lambda (lcd)
    (let* ((declarations (car lcd))       
           (assignments (cadr lcd))     
           (evaluations (caddr lcd))      

         
           (declared-identifiers
            (apply append (map cdr declarations)))

           
           (filter-undeclared
            (lambda (ids declared)
              (filter (lambda (id) (not (member id declared))) ids)))

           
           (process-assignments
            (lambda (assignments)
              (apply append
                     (map (lambda (assignment)
                            (let ((lhs (car assignment)) 
                                  (rhs (cdr assignment)))
                              (append
                               (if (member lhs declared-identifiers) '() (list lhs)) 
                               (filter-undeclared (extract-identifiers rhs) declared-identifiers)))) 
                          assignments))))

           
           (process-evaluations
            (lambda (evaluations)
              (apply append
                     (map (lambda (evaluation)
                            (let ((input-inits (caddr evaluation)))
                              (filter-undeclared (map car input-inits) declared-identifiers)))
                          evaluations)))))
      
      
      (append
       (process-assignments assignments)
       (process-evaluations evaluations)))))



(define extract-idents
  (lambda (declarations)
    (apply append (map (lambda (decl) (cdr decl)) declarations)))) 

(define find-duplicates
  (lambda (idents)
    (letrec ((check-duplicates
              (lambda (seen idents)
                (cond ((null? idents) '()) 
                      ((memv (car idents) seen) 
                       (cons (car idents) (check-duplicates seen (cdr idents)))) 
                      (else 
                       (check-duplicates (cons (car idents) seen) (cdr idents))))))) 
      (check-duplicates '() idents)))) 

(define find-multiple-declarations
  (lambda (lcd)
    (let* ((declarations (car lcd)) 
           (all-idents (extract-idents declarations))) 
      (find-duplicates all-idents))))




(define check-inputs-in-evaluation
  (lambda (lcd)
    (define (filter-inputs identifiers)
      (filter (lambda (id) 
                (string-prefix? "input" (symbol->string id)))
              identifiers))

    (let* ((declarations (car lcd))  
           (evaluations (caddr lcd)) 
           
           (declared-identifiers
            (apply append (map cdr declarations))) 
           
  
           (declared-inputs (filter-inputs declared-identifiers))

          
           (unassigned-inputs
            (lambda (eval)
              (let ((input-names (map car (caddr eval)))) 
                (filter (lambda (input) (not (member input input-names))) declared-inputs))))  

     
           (circuits-with-unassigned-inputs
            (map cadr  
                 (filter (lambda (eval)
                           (not (null? (unassigned-inputs eval)))) 
                         evaluations)))

           
           (multiple-assignments
            (lambda (eval)
              (let* ((input-names (map car (caddr eval)))  
                     (duplicate-inputs (filter (lambda (input)
                                                (> (length (filter (lambda (x) (equal? x input)) input-names)) 1))
                                              input-names)))   
                (not (null? duplicate-inputs))))) 

   
           (circuits-with-multiple-assignments
            (map cadr 
                 (filter (lambda (eval)
                           (multiple-assignments eval))  
                         evaluations))))

      
      (list circuits-with-unassigned-inputs circuits-with-multiple-assignments))))




(define check-identifier-usage
  (lambda (lcd)
    (let* ((declarations (car lcd))
           (assignments (cadr lcd))
           (evaluations (caddr lcd))

          
           (declared-identifiers (apply append (map cdr declarations)))

           
           (filter-by-prefix
            (lambda (prefix lst)
              (filter (lambda (id)
                        (and (symbol? id) 
                             (string-prefix? prefix (symbol->string id))))
                      lst)))

         
           (declared-inputs (filter-by-prefix "input" declared-identifiers))
           (declared-nodes-and-outputs
            (filter (lambda (id) 
                      (or (string-prefix? "node" (symbol->string id))
                          (string-prefix? "output" (symbol->string id))))
                    declared-identifiers))

           
           (lhs-identifiers
            (map (lambda (assignment)
                   (if (and (list? assignment) (eq? '= (cadr assignment)))
                       (car assignment)
                       '()))
                 assignments))

           (rhs-identifiers
            (apply append
                   (map (lambda (assignment)
                          (if (and (list? assignment) (eq? '= (cadr assignment)))
                              (extract-identifiers (cddr assignment))
                              '()))
                        assignments)))

          
           (unassigned-inputs
            (filter (lambda (input) (not (member input rhs-identifiers))) declared-inputs))

           ;; node and outputs lists are not seperate because i want to preserve the program order
           (unassigned-nodes-outputs
            (filter (lambda (id) (not (member id lhs-identifiers)))
                    declared-nodes-and-outputs))

           
           (find-duplicates
            (lambda (lst)
              (define seen '())  
              (define duplicates '())
              (for-each
               (lambda (id)
                 (if (member id seen)
                     (if (not (member id duplicates))
                         (set! duplicates (cons id duplicates))))
                 (set! seen (cons id seen)))
               lst)
              (reverse duplicates)))  ;; Reverse to preserve lcd order

          
           (duplicate-idents (find-duplicates lhs-identifiers)))

  
      (list unassigned-inputs unassigned-nodes-outputs duplicate-idents))))





(define check-incorrect-assignments
  (lambda (program)
    
    (define (flatten lst)
      (cond
        ((null? lst) '())  
        ((not (pair? (car lst)))  
         (cons (car lst) (flatten (cdr lst))))  
        (else 
         (append (flatten (car lst)) (flatten (cdr lst))))))  

    (let* ((declarations (car program))  ;; Extract declarations block
           (assignments (cadr program))   ;; Extract assignments block
           (evaluations (caddr program))  ;; Extract evaluations block
           (flattened-evaluations (flatten evaluations)))  
           
     
      (define incorrect-inputs
        (filter
          (lambda (assignment)
            (string-prefix? "input" (symbol->string (car assignment)))) 
          assignments))  
       
      
      (define incorrect-input-names
        (map car incorrect-inputs)) 
      
      
      (define incorrect-outputs-nodes '())

     
      (for-each
        (lambda (item)
          (if (and (symbol? item)
                   (or (string-prefix? "output" (symbol->string item))
                       (string-prefix? "node" (symbol->string item))))
              (set! incorrect-outputs-nodes (cons item incorrect-outputs-nodes))))
        flattened-evaluations)

      ;; Reverse list to keep prpgram order
      (set! incorrect-outputs-nodes (reverse incorrect-outputs-nodes))

     
      (list incorrect-input-names incorrect-outputs-nodes))))

