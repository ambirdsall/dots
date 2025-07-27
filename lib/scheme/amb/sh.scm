(define-module (amb sh)
  #:use-module (ice-9 popen)
  #:use-module (ice-9 textual-ports)
  #:use-module (srfi srfi-9)
  #:export (sh sh* echo))

(define (sh cmd-string)
  "Runs cmd-string as a shell command, returning the output as a string."
  (let* ((cmd-pipe (open-pipe cmd-string OPEN_READ))
         (output (get-string-all cmd-pipe)))
    (close-pipe cmd-pipe)
    output))

(define-record-type shell-result
  (make-shell-result stdout stderr exit-code success)
  shell-result?
  (stdout shell-result-stdout)
  (stderr shell-result-stderr)
  (exit-code shell-result-exit-code)
  (success shell-result-success))

(define (sh* cmd-string)
  "Runs cmd-string as a shell command, returning structured data as a shell-result record.

Returns: A shell-result record with stdout, stderr, exit-code, and success fields."
  (let* ((cmd-pipe (open-pipe cmd-string OPEN_BOTH))
         (stdout (get-string-all cmd-pipe))
         (exit-code (status:exit-val (close-pipe cmd-pipe))))
    (make-shell-result stdout
                       ""  ; Note: OPEN_BOTH doesn't separate stderr in ice-9 popen
                       (or exit-code 0)
                       (zero? (or exit-code 0)))))

(define (echo . args)
  "Given any number of string or symbol arguments, prints them in order (multiple arguments
separated by spaces) followed by a newline."
  (define (arg->string arg)
    (cond
     ((string? arg) arg)
     ((symbol? arg) (symbol->string arg))
     (else (object->string arg))))

  (when (not (null? args))
    (display (string-join (map arg->string args) " ")))
  (newline))
