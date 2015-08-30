#lang racket

(require (for-syntax racket/syntax))

(define (zip lst1 lst2)
  (cond [(or (empty? lst1) (empty? lst2)) '()]
        [else (cons (cons (car lst1) (car lst2))
                    (zip (cdr lst1) (cdr lst2)))]))

(define-syntax (struct-json stx)
    (syntax-case stx ()
      [(_ id (fields ...) extras ...)
       (with-syntax ([json->id (format-id stx "json->~a" #'id)]
                     [id->json (format-id stx "~a->json" #'id)]
                     [accs (cons list (map (lambda (f) (format-id stx "~a-~a" #'id f))
                                           (syntax->list #'(fields ...))))])
         #`(begin
             (struct id (fields ...) extras ...)
             (define (id->json v)
               (make-hasheq
                (map (lambda (f)
                       (let ((val ((car f) v))
                             (lbl (cdr f)))
                         (cons lbl val)))
                     (zip accs '(fields ...)))))
             (define (json->id v)
               (apply id (map (lambda (f) (hash-ref v f)) '(fields ...))))
             ))]))

