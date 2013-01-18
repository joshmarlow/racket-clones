; Created: January 12, 2013
; Author: Josh Marlow

#lang racket

(require racket/cmdline)

(define (echo-handler in-1 out-1 in-2 out-2)
  ; Cross the ports so that the in-1 -> out-2 and in-2 _> out-1
  (define cust (make-custodian))
  (parameterize ([current-custodian cust])
      ; Define a function for reading from one port and writing to another
      (define (pipe in out)
        (copy-port in out)
        (flush-output out)
        (custodian-shutdown-all cust))

      ; Spawn pipes for each direction
      (define p1 (thread (lambda ()
          (pipe in-1 out-2))))
      (define p2 (thread (lambda ()
          (pipe in-2 out-1))))

      ; Wait on the threads to do their thing
      (thread-wait p1)
      (thread-wait p2)))

(define (sub-process-handler command-name in-1 out-1 in-2 out-2)
  (let ([command-path (find-executable-path command-name)])
    (if command-path
      (let ((command-path (path->string (find-executable-path command-name))))
       (let ((cust (make-custodian)))
       (parameterize ([current-custodian cust])
           (define-values (subproc p-out p-in p-err)
             (subprocess #f #f #f command-path))
           (if subproc
              (and
                (echo-handler p-out p-in in-1 out-1)
                (close-input-port p-out)
                (close-output-port p-in))
             (display (string-append "Could not execute command: " command-path))))))
      (display
        (string-append "Could not locate command: " command-name)
        (current-error-port)))))

(define (parse-cmd-options)
  (define (make-defaults-ht)
    ; define some default behavior in case we don't get any parameters
    (let ((ht (make-hash)))
      (hash-set! ht 'mode 'client)
      (hash-set! ht 'host "localhost")
      (hash-set! ht 'port 80)
      (hash-set! ht 'io-handler echo-handler)
      ht))

  ; parse the command line options so the program knows what to do
  (let ((cmd-options (make-defaults-ht)))
    (command-line
     #:program "nc"
     #:once-each
     [("-l" "--listen") "Run as a server"
                     (hash-set! cmd-options 'mode `server)]
     [("-e" "--exec") command-name
                     "Execute a command"
                     (hash-set! cmd-options 'io-handler
                        (lambda (in-1 out-1 in-2 out-2)
                            (sub-process-handler command-name in-1 out-1 in-2 out-2)))]
     [("-p" "--port") port-number
                      "Specify a port explicitly"
                     (hash-set! cmd-options 'port (string->number port-number))]
     #:args
     arg-list (and (if (> (length arg-list) 0)
                     (hash-set! cmd-options 'host (first arg-list))
                     #t)
                   (if (> (length arg-list) 1)
                     (hash-set! cmd-options 'port
                                (string->number (first (rest arg-list))))
                     #t)))
    cmd-options))

(define (launch-as-server host port io-handler)
    ; Launch application in Server mode
    (define listener (tcp-listen port 1 #t))
    (define-values (in out) (tcp-accept listener))
    (io-handler in out (current-input-port) (current-output-port))
    (close-input-port in)
    (close-output-port out))

(define (launch-as-client host port io-handler)
    ; Launch application in client mode
    (define-values (in out) (tcp-connect host port))
    (io-handler in out (current-input-port) (current-output-port))
    (close-input-port in)
    (close-output-port out))

(define (main)
    ; Main application logic
    (let ((cmd-options (parse-cmd-options)))
      (cond
        ((eq? (hash-ref cmd-options 'mode) 'server)
         (launch-as-server(hash-ref cmd-options 'host)
                           (hash-ref cmd-options 'port)
                           (hash-ref cmd-options 'io-handler)))
        ((eq? (hash-ref cmd-options 'mode) 'client)
         (launch-as-client (hash-ref cmd-options 'host)
                           (hash-ref cmd-options 'port)
                           (hash-ref cmd-options 'io-handler))))))

(main)
