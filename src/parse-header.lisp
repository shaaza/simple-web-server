;; Functions for parsing the path, header and query parameters. These are called in server.lisp

;; @func get-header
;; @desc Parses header into label and value pairs (list) based on the colon (:) character.

(defun get-header (stream)
                  (let* ((s (read-line stream))
                         (h (let ((i (position #\: s)))
                                 (when i
                                       (cons (string-upcase (subseq s 0 i))
                                             (subseq s (+ i 2)))))))
                        (when h
                              (cons h (get-header stream)))))

;; @func get-content-params
;; @desc Parse the body of the HTTP request, i.e. its content.
;; @example

(defun get-content-params (stream header)
                          (let ((length  (cdr (assoc 'content-length header))))
                               (when length
                                     (let ((content (make-string (parse-integer length))))
                                          (read-sequence content stream)
                                          (parse-params content)))))

;; @func parse-url
;; @desc Parse the URL from the first line of the request header, and call query params parser if they exist.
;; @example (parse-url "GET /jokes.html?extra-funny=yes HTTP/1.1") => ("jokes.html" (EXTRA-FUNNY . "yes"))

(defun parse-url (first-line)
                 (let* ((url (subseq first-line (+ 2 (position #\space first-line))
                                    (position #\space first-line :from-end t)))
                       (x (position #\? url)))

                      (if x
                          (cons (subseq url 0 x) (parse-params (subseq url (1+ x))))
                          (cons url '()))))

;; @func parse-params
;; @desc Parse the query parameters from the request header.
;; @example (parse-params "name=bob&age=25&gender=male") => ((NAME . "bob") (AGE . "25") (GENDER . "male"))

(defun parse-params (querystring)
                    (let ((i1 (position #\= querystring))
                          (i2 (position #\& querystring)))
                          (cond (i1 (cons (cons (intern (string-upcase (subseq querystring 0 i1)))
                                                (decode-params (subseq querystring (1+ i1) i2)))
                                          (and i2 (parse-params (subseq querystring (1+ i2))))))
                                ((equal "" querystring) 'nil)
                                (t querystring))))

;;; Helper Functions

;; @func decode-params
;; @desc Decodes escape code from the query parameters in the request.
;; @example (decode-params "what+is+your+name%3F") => "what is your name?"

(defun decode-params (value)
                     (labels ((f (lst)
                           (when lst
                                 (case (car lst)
                                       (#\% (cons (http-char (cadr lst) (caddr lst))
                                                  (f (cdddr lst))))
                                       (#\+ (cons #\space (f (cdr lst))))
                                       (otherwise (cons (car lst) (f (cdr lst))))))))

                             (coerce (f (coerce value 'list)) 'string)))

;; @func http-char
;; @desc HEX to ASCII character conversion.

(defun http-char (c1 c2 &optional (default #\Space))
                 (let ((code (parse-integer
                                            (coerce (list c1 c2) 'string)
                                            :radix 16
                                            :junk-allowed t)))
                  (if code
                      (code-char code)
                      default)))
