; Created: January 12, 2013
; Author: Josh Marlow

#lang racket

(require racket/cmdline)

(define (parse-cmd-options)
  (define (make-defaults-ht)
    ; define some default behavior in case we don't get any parameters
    (let ((ht (make-hash)))
      (and (hash-set! ht 'mode 'server)
           (hash-set! ht 'host "localhost")
           (hash-set! ht 'port 80))
      ht))

  ; parse the command line options so the program knows what to do
  (let ((cmd-options (make-defaults-ht)))
    (command-line
     #:program "nc"
     #:once-each
     [("-l" "--listen") "Run as a server"
                        (hash-set! cmd-options 'mode `client)]
     #:args
     (host port) (and (hash-set! cmd-options 'host host)
                      (hash-set! cmd-options 'port (string->number port))))
    cmd-options))

(define (handle in-1 out-1 in-2 out-2)
  ; Cross the ports so that the in-1 -> out-2 and in-2 _> out-1
  (define cust (make-custodian))
  (parameterize ([current-custodian cust])
      ; Define a function for reading from one port and writing to another
      (define (pipe in out)
        (for ([byte in])
         (write-byte byte out)
         (flush-output out))
        (custodian-shutdown-all cust))

      ; Spawn pipes for each direction
      (define p1 (thread (lambda ()
          (pipe in-1 out-2))))
      (define p2 (thread (lambda ()
          (pipe in-2 out-1))))

      ; Wait on the threads to do their thing
      (thread-wait p1)
      (thread-wait p2)))

(define (launch-as-server host port)
    ; Launch application in Server mode
    (define listener (tcp-listen port 1 #t))
    (define-values (in out) (tcp-accept listener))
    (handle in out (current-input-port) (current-output-port))
    (close-input-port in)
    (close-output-port out))

(define (launch-as-client host port)
    ; Launch application in client mode
    (define-values (in out) (tcp-connect host port))
    (handle in out (current-input-port) (current-output-port))
    (close-input-port in)
    (close-output-port out))

(define (main)
    ; Main application logic
    (let ((cmd-options (parse-cmd-options)))
      (cond
        ((eq? (hash-ref cmd-options 'mode) 'client)
         (launch-as-server(hash-ref cmd-options 'host) (hash-ref cmd-options 'port)))
        ((eq? (hash-ref cmd-options 'mode) 'server)
         (launch-as-client (hash-ref cmd-options 'host) (hash-ref cmd-options 'port))))))

(main)
