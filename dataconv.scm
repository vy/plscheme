;;;;
;;;; PL/scheme, SQL<->Scheme Data Conversion Routines
;;;; $Id: dataconv.scm,v 1.5 2006/12/09 19:52:07 knt Exp $
;;;;
;;;; Copyright (c) 2006, Volkan YAZICI <yazicivo@ttnet.net.tr>
;;;; All rights reserved.
;;;;
;;;; Redistribution and use in source and binary forms, with or without
;;;; modification, are permitted provided that the following conditions are met:
;;;; 
;;;; - Redistributions of source code must retain the above copyright notice, this
;;;;   list of conditions and the following disclaimer.
;;;; - Redistributions in binary form must reproduce the above copyright notice,
;;;;   this list of conditions and the following disclaimer in the documentation
;;;;   and/or other materials provided with the distribution.
;;;;
;;;; THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS"
;;;; AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;;;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
;;;; ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT OWNER OR CONTRIBUTORS BE
;;;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;;;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;;;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS
;;;; INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN
;;;; CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE)
;;;; ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
;;;; POSSIBILITY OF SUCH DAMAGE.
;;;;


;;;
;;; array? - Array data type predicate. (Actually, there is no easy way to learn
;;;          if a type is array or not; especiall by looking at its name.
;;;          Programmers should consider about this limitation while intoducing
;;;          new conversion routines for custom array data types.)
;;;
(define (array? type)
  (char=? (string-ref type 0) #\_))


;;;
;;; array-basetype - Return base type of an array. (Same restrictions are applied
;;;                  as in array? predicate.)
;;;
(define (array-basetype type)
  (string-copy type 1))


;;;
;;; array-type - Reverse of the above method.
;;;
(define (array-type basetype)
  (string-append "_" basetype))


;;;
;;; array-tokenize - Explodes a given array input string into a list of tokens in
;;;                  string format. (This is a very primitive array data type
;;;                  parsing routine. For a complete implementation, see
;;;                  array_in() and ReadArrayStr() functions in the PostgreSQL
;;;                  source code.)
;;;
;;;                  Raises an invalid-text-representation exception with (input
;;;                  description) arguments in case of a parsing error.
;;;
(define (array-tokenize str)
  (let ((len (string-length str))
        (ret '())
        (in-quotes #f))

	;; Check whether we have `{' and `}' characters at the beginning and end of
	;; the array data.
    (if (or (not (char=? #\{ (string-ref str 0)))
            (not (char=? #\} (string-ref str (- len 1)))))
		(throw 'invalid-text-representation str
			   (string-append "Expecting `{' and `}' characters at the "
							  "beginning and end of the array.")))

	;; Remove brackets.
	(set! str (string-copy str 1 (- len 1)))
	(set! len (- len 2))

	;; We'll use exceptions to exit from the loop when there remains no
	;; characters, while calle' requests for one.
    (catch 'out-of-loop
		   (lambda ()
			 (let* ((i 0)
					(prev-was-bslash #f)
					(first-turn #t)
					(p '())

					;; Current character.
					(curr-char
					 (lambda ()
					   (if (< i len)
						   (string-ref str i)

						   ;; There remained no characters, while calle' waits
						   ;; for one. Process any remaining input in string
						   ;; port and then get out of the loop.
						   (begin
							 (if (not (null? p))
								 (begin
								   (set! ret (cons (get-output-string p) ret))
								   (close-output-port p)))
							 (throw 'out-of-loop)))))

					;; Move to next character.
					(next-char
					 (lambda ()
					   (set! i (+ i 1))
					   (curr-char)))
					
					;; Skip recent whitespace.
					(skip-whitespace
					 (lambda ()
					   (let tmp-loop ()
						 (if (char-whitespace? (curr-char))
							 (begin
							   (next-char)
							   (tmp-loop))))))
					
					;; Report current cursor position.
					(report-position
					 (lambda ()
					   (string-append
						"(Char.: " (string (curr-char)) " at "
						"position " (number->string (+ i 1)) ")"))))

			   (let token-loop ()
				 ;; Create a new string port.
				 (set! p (open-output-string))

				 ;; Skip leading whitespace.
				 (skip-whitespace)

				 ;; If this is not our first turn, there must a preceding comma.
				 (cond
				  ;; Turn first-turn flag off.
				  (first-turn (set! first-turn #f))

				  ;; Else, check for a comma.
				  ((char=? #\, (curr-char))
				   ;; We found a comma. Skip it and any following whitespace.
				   (next-char)
				   (skip-whitespace))

				  ;; We're not in our first turn and there's still no comma!
				  (else (throw 'invalid-text-representation str
							   (string-append
								"Expecting a comma just after the token. "
								(report-position)))))

				 ;; Will we parse sth that's in double quotes?
				 (if (char=? #\" (curr-char))
					 (begin
					   (write-char #\" p)
					   (set! in-quotes #t)
					   (next-char)))
				 
				 (catch 'break
						(lambda ()
						  (let char-loop ((c (curr-char)))
							(case c
							  ((#\\ )
							   (set! prev-was-bslash #t))
							  
							  ((#\")
							   (cond
								(prev-was-bslash
								 (begin
								   (set! prev-was-bslash #f)
								   (write-char c p)))

								;; We assume that a `"' indicates the end of a
								;; token when we previously met with another
								;; `"'.
								(in-quotes
								 (set! prev-was-bslash #f)
								 (write-char c p)
								 (set! in-quotes #f)
								 (next-char)
								 (throw 'break))

								;; This is not an expected situation.
								(else
								 (throw 'invalid-text-representation str
										(string-append
										 "Unexpected double quote. "
										 (report-position))))))

							  ((#\,)
							   (if in-quotes
								   (begin
									 (set! prev-was-bslash #f)
									 (write-char c p))
								   (throw 'break)))
							  
							  (else
							   (set! prev-was-bslash #f)
							   (write-char c p)))

							(char-loop (next-char))))

						;;; 'break handler.
						(lambda (key . args ) args))

				 ;;; Append accumulated result and close port.
				 (set! ret (cons (get-output-string p) ret))
				 (close-output-port p)

				 (token-loop))))

		   ;;; 'out-of-loop handler.
		   (lambda (key . args) args))

	;;; Check whether we have any unclosed double quotes.
    (if in-quotes
		(throw 'invalid-text-representation str
			   "Unexpected end of token. Double quote wasn't closed."))

	;; Remove trailing whitespaces.
    (let* ((res '())
           (trimmer (lambda (s)
                      (set! res (cons (string-trim-both s) res)))))
      (for-each trimmer ret)
      res)))


;;;
;;; pl-data-imposers - Data imposition routines association list.
;;;					   (list
;;;						(list <Type family.>
;;;							  <Matching SQL types listing.>
;;;							  <Data conversion function.>)
;;;						...)
;;;
(define pl-data-imposers
  (list
   ;;
   ;; Boolean type.
   ;;
   (list
	'("bool")
	(lambda (value)
	  (equal? (char-downcase (string-ref value 0))
			  #\t)))

   ;;
   ;; Numerics.
   ;;
   (list
	'("int2" "int4" "int8" "float4" "float8" "oid")
	(lambda (value)
	  (let ((res (string->number value)))
		(if (number? res) res value))))))


;;;
;;; pl-data-imposers-apply - Accessor function for pl-data-imposers list. Try
;;;							 each imposer on the given data with type.
;;;
(define (pl-data-imposers-apply value type imposers)
  ;; Return null when imposers get exhausted.
  (if (null? imposers) '()
	  (let* ((imposer (car imposers))
			 (types (car imposer))
			 (convfn (cadr imposer)))

		;; Apply imposer if it matches with any type specified
		;; for the related imposer.
		(if (member type types) (convfn value)
			(pl-data-imposers-apply value type (cdr imposers))))))


;;;
;;; pl-data-impose - Transform given data with type into its corresponding
;;;					 object in Guile.
;;;
(define (pl-data-impose value type)
  (cond
   ;;
   ;; Array type.
   ;;
   ((array? type)
	(let* ((basetype (array-basetype type))
		   (tokens (array-tokenize value))
		   (vec (make-vector (length tokens))))
	  (let loop ((tokens tokens)
				 (i 0))
		(if (null? tokens) vec
			(let* ((token (car tokens))
				   (len (string-length token))
				   (has-quotes
					(and (char=? #\" (string-ref token 0))
						 (char=? #\" (string-ref token (- len 1)))))
				   (res '()))

			  ;; Remove any surrounding double quotes.
			  (if has-quotes
				  (set! token (string-copy token 1 (- len 1))))

			  (if (string=? token "NULL")
				  (if has-quotes
					  ;; If token = NULL and it has double quotes, than it
					  ;; must be a string with value "NULL". Otherwise, it's
					  ;; a NULL and leave res as is, '().
					  (set! res token))

				  ;; Otherwise, apply imposition to the current value.
				  (set! res (pl-data-impose token basetype)))

			  ;; Set related vector item to the imposed value.
			  (vector-set! vec i res)

			  (loop (cdr tokens) (+ i 1)))))

	  ;; Return created vector.
	  vec))

   (else
	(let ((imposers-prod
		   (pl-data-imposers-apply value type pl-data-imposers)))
	  (if (not (null? imposers-prod)) imposers-prod
		  ;; Let the unrecognized types get imposed as is. (In other
		  ;; words, as string.)
		  value)))))


;;;
;;; pl-data-exposers - Association list of data exposition routines.
;;;					   (list
;;;                     (list <Type test function>
;;;                           <Data conversion function>
;;;							  <SQL type function>)
;;;						...)
;;;
(define pl-data-exposers
  (list
   ;;
   ;; Boolean type.
   ;;
   (list
	(lambda (obj) (boolean? obj))
	(lambda (obj) (if obj "true" "false"))
	(lambda (obj) "bool"))

   ;;
   ;; String type.
   ;;
   (list
	(lambda (obj) (string? obj))
	(lambda (obj) obj) ; Pass strings asis.
	(lambda (obj) "text"))

   ;;
   ;; Numerics
   ;;
   (list
	(lambda (obj) (number? obj))
	(lambda (obj) (number->string obj))
	(lambda (obj) (if (exact? obj) "int4" "float4")))))


;;;
;;; array-escape - Escape given string to place it into an array input string.
;;;
(define (array-escape str)
  (call-with-output-string
    (lambda (p)
      (string-for-each
        (lambda (c)
          (case c
            ((#\\)
             (write-char #\\ p)
             (write-char #\\ p))
            ((#\")
             (write-char #\\ p)
             (write-char #\" p))
            (else
              (write-char c p))))
        str))))


;;;
;;; array-detokenize - Transform a vector into its SQL array data type
;;; 				   representation.
;;;
(define (array-detokenize vec pl-data-expose)
  (let* ((basetype '())
		 (ret
		  (call-with-output-string
		   (lambda (p)
			 (let ((first-turn #t))
			   (write-char #\{ p)

			   ;; Traverse vector items.
			   (for-each
				(lambda (token)
				  ;; Sorry, no nested arrays yet.
				  (if (vector? token)
					  (throw 'invalid-text-representation vec
							 "PL/scheme doesn't support nested arrays yet."))

				  ;; Place the comma seperator.
				  (if first-turn
					  (set! first-turn #f)
					  (write-char #\, p))

				  (if (null? token)
					  ;; Handle NULL case.
					  (display "NULL" p)

					  (begin
						;; First double quote.
						(write-char #\" p)

						(let* ((lst (pl-data-expose token))
							   (val (car lst))
							   (typ (cadr lst)))
						  ;; Set type for just the first time.
						  (if (null? basetype)
							  (set! basetype typ))
						  ;; Print escaped output.
						  (display (array-escape val) p))

						;; Close double quotes.
						(write-char #\" p))))

				;; for-each input.
				(vector->list vec))

			   (write-char #\} p))))))

	;; If we still don't have a basetype, set it to text.
	(if (null? basetype)
		(set! basetype "text"))

	(list ret (array-type basetype))))


;;;
;;; pl-data-exposers-apply - Accessor function for pl-data-exposers list. Try
;;;							 each exposer function on the given object.
;;;
(define (pl-data-exposers-apply obj exposers)
  ;; When list gets exhausted, return a null.
  (if (null? exposers) '()
	  (let* ((exposer (car exposers))
			 (testfn (car exposer))
			 (convfn (cadr exposer))
			 (typefn (caddr exposer)))
		(if (testfn obj)
			;; If test succeeds, return a list of string representation and
			;; data's SQL type pair.
			(list (convfn obj) (typefn obj))
			(pl-data-exposers-apply obj (cdr exposers))))))


;;;
;;; pl-data-expose - Convert given object into its string form. Function will
;;;                  return a couple whose car is the string form of the object,
;;;                  while cadr holds the guessed type of the object. (We'll use
;;;                  latter while returning an attribute of record type. Other
;;;                  attributes' types will be fetched from TupleDesc supplied
;;;                  by fcinfo.)
;;;
(define pl-data-expose
  (lambda (obj)
    (cond
     ;;
	 ;; Is this an array type?
	 ;;
     ((vector? obj) (array-detokenize obj pl-data-expose))

     (else
      (let ((exposers-prod (pl-data-exposers-apply obj pl-data-exposers)))
        (if (not (null? exposers-prod))
            exposers-prod

			;; Object couldn't get realized by exposer functions. Return its
			;; native text representation. (Setting unrecognized values' type to
			;; text.)
            (list (object->string obj) "text")))))))