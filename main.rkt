#lang racket

(provide launch
         serve
         entity
         add-call)

(require web-server/servlet
         web-server/servlet-env
         json
         simple-http
         racket/runtime-path)

(define-runtime-path here ".")

(define mac-path
  (build-path here "clients" "UnityClient.app" "Contents" "MacOS" "UnityClient"))

(define linux-path
  (build-path here "clients" "LinuxClient.x86_64"))

(define windows-path
  (build-path here "clients" "Windows" "CodeSpellsFragments.exe"))

(define (launch)
  (thread
    (thunk
      (system 
        (cond
          [(eq? 'macosx (system-type)) (~a mac-path)]
          [(eq? 'unix (system-type)) (~a linux-path)]
          [(eq? 'windows (system-type)) (~a windows-path)]
          [else (error (~a "Unsupported OS: " (system-type)))]
          )))))


(define (serve . entities)
  (define random-version
    (~a (random 1000000)))

  (define (my-app req)

    (response/xexpr
      `(entities
         (version ,random-version)
         ,@(map entity-struct->list (flatten entities)))))


  (with-handlers
    ([exn:fail? (lambda (e)
                  (displayln e)
                  (displayln "No running server.  Starting new one."))])
    (get
      (update-port (update-host json-requester "localhost") 8080)
      "/quit"))

  (sleep 2)

  (thread
    (thunk


      (serve/servlet my-app
                     #:quit? #t
                     #:port 8080
                     #:servlet-path "/"
                     #:launch-browser? #f))))

(struct entity-struct (name calls))

(define (entity-struct->list e)
  (cond
    [(entity-struct? e)
     `(entity
        ,(entity-struct-name e)
        ,(map entity-struct->list (entity-struct-calls e)))]
    [(list? e)
     (map entity-struct->list e)]
    [else e]))

(define-syntax-rule (entity name (component-name method-name arg-value ...) ...)
  (entity-struct
    `(prefab ,(~a 'name))
    `(calls
       (call
         (component ,(~a 'component-name))
         (method ,(~a 'method-name))
         (args
           (arg ,(marshal-arg arg-value))
           ...))
       ...)))


(define (marshal-arg arg)
  (if (entity-struct? arg)
    arg
    (~a arg)))




(provide add-as-child
         add-children
         combine
         fpc

         set-position)

(define (add-as-child c p)
  (add-call p
            (Children Add c)))

(define (add-children p cs)
  (foldl add-as-child p cs))

(define (combine cs)
  (define cube
    (entity primitive:Empty))

  (add-children cube cs))


(define-syntax-rule (add-call e (component-name method-name arg-value ...))
  (struct-copy entity-struct e
               [calls
                 (append (entity-struct-calls e)
                         (list `(call
                                  (component ,(~a 'component-name))
                                  (method ,(~a 'method-name))
                                  (args
                                    (arg ,(marshal-arg arg-value)) ...))))]))

(define (fpc x y z)
  (entity FirstPersonController
          (SetPosition To x y z)))

(define (set-position e x y z)
  (add-call e
            (SetPosition To x y z)))
