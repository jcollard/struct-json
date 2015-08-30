#lang racket

(provide struct-json)

(require (for-syntax racket/syntax))

; Helper function... this has to be defined somewhere already...
(define (zip lst1 lst2)
  (cond [(or (empty? lst1) (empty? lst2)) '()]
        [else (cons (cons (car lst1) (car lst2))
                    (zip (cdr lst1) (cdr lst2)))]))

; Works just like struct but provides an two additional functions
; id->json and json->id. Note: This only works for simple values
; TODO: add syntax for (struct-json id ((x to-json-fun) (y to-json-fun)))
; > (require json)
; > (struct-json pt (x y) #:transparent)
; > (jsexpr->string (pt->json (pt 5 6)))
; "{\"y\":6,\"x\":5}"
; > (pt? (json->pt (string->jsexpr "{\"y\":6,\"x\":5}")))
; #t
; > (json->pt (string->jsexpr "{\"y\":6,\"x\":5}"))
; (pt 5 6)

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

