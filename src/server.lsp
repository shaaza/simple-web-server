;;;; Simple-Web Server
;;;; 2 core functions: 

;;;; Server function - serve (request-handler) 

;;;; serve: 1. Listens to 8080 | 2. Parses HTTP request into Lisp-processable data - path, header and params. | 3. Calls request-handler with the 3 arguments and o/ps to socket. 

(defun serve (request-handler)
             (let ((socket (socket-server 8080)))
                  (unwind-protect 
                                 (loop 
                                       (with-open-stream (stream (socket-accept socket))
                                                         (let* ((url (parse-url (read-line stream)))
                                                                (path (car url))
                                                                (header (get-header stream))
                                                                (params (append (cdr url) (get-content-params stream header)))
                                                                (*standard-output* stream))
                                           
                                                               (funcall request-handler path header params))))
                                  (socket-server-close socket))))                                              




;;; get-header: 1. Accept stream as arg. 2. Read first line of stream. 3. Find position of : to separate labels and values in headers. 3.1 cons the label to a value and store as  local var. 4. recursively cons all label-value pairs.

(defun get-header (stream)
                  (let* ((s (read-line stream))
                         (h (let ((i (position #\: s)))
                                 (when i
                                       (cons (string-upcase (subseq s 0 i))
                                             (subseq s (+ i 2)))))))
                        (when h
                              (cons h (get-header stream)))))

;;; get-content-params: 1. Find length of content string from header. 2. If length exists, make an empty string of length 'length'. 3. Fill the string with stream content. 4. Parse parameters from string.

(defun get-content-params (stream header)
                          (let ((length  (cdr (assoc 'content-length header))))
                               (when length 
                                     (let ((content (make-string (parse-integer length))))
                                          (read-sequence content stream)
                                          (parse-params content)))))

;;; parse-url: 1. Accept first line of header as argument. 2.1. Use space delimiters around url to capture url. 2.2. Use question mark position to check for existence of url params. 2.3. If question-mark-position exists, cons the url to the parsed request parameters. 

(defun parse-url (first-line)
                 (let* ((url (subseq first-line (+ 2 (position #\space first-line))
                                    (position #\space first-line :from-end t)))
                       (x (position #\? url)))
                       
                      (if x
                          (cons (subseq url 0 x) (parse-params (subseq url (1+ x))))
                          (cons url '()))))

                 
;;; General Purpose Functions

;;; parse-params

(defun parse-params (querystring)
                    (let ((i1 (position #\= querystring))
                          (i2 (position #\& querystring)))
                          (cond (i1 (cons (cons (intern (string-upcase (subseq querystring 0 i1)))
                                                (decode-params (subseq querystring (1+ i1) i2)))
                                          (and i2 (parse-params (subseq querystring (1+ i2))))))
                                ((equal "" querystring) 'nil)
                                (t querystring))))

;;  decode-params: Coerce the passed parameter value into a string and pass it to a local recursive function which passes a) % and following to ASCII->CHAR converter function b) + to space c) and leaves anything else as is. 

(defun decode-params (value)
                     (labels ((f (lst)
                           (when lst
                                 (case (car lst)
                                       (#\% (cons (http-char (cadr lst) (caddr lst))
                                                  (f (cdddr lst))))
                                       (#\+ (cons #\space (f (cdr lst))))
                                       (otherwise (cons (car lst) (f (cdr lst))))))))
                             
                             (coerce (f (coerce value 'list)) 'string)))
 
;   http-char: Accept 2 characters as arguments, convert the two from hex to ASCII, and convert ASCII into character.

(defun http-char (c1 c2 &optional (default #\Space))
                 (let ((code (parse-integer
                                            (coerce (list c1 c2) 'string)
                                            :radix 16
                                            :junk-allowed t)))
                  (if code
                      (code-char code)
                      default)))

;;;; Request Handler function - request-handler (path header params)
;;;; request-handler: Based on 'path' parameter, generates corresponding HTML content, and uses 'params' for 'dynamic' interaction.a

(defun hello-request-handler (path header params)
                             (if (equal path "greeting")
                                 (let ((name (assoc 'name params)))
                                      (if (not name)
                                          (princ "<html><form>What is your name? <input name='name' /></form></html>")
                                          (format t "<html>Nice to meet you, ~a!</html>" (cdr name))))
                                 (princ "Sorry... I don't know that page.")))