;; The core function that opens a socket and accepts a stream, is housed here.

;; @func serve
;; @desc Listens to port for HTTP requests via a socket, splits the request into path, header and params, and calls the request handler with these variables.
;; @params: takes a request handler function, that outputs the response to the request based on three arguments passed to it: the path, header and query parameters.

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
