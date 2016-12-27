;; The request handler is a function passed into the serve function, where you can tell the server what to do based on the headers, path and query parameters.
;; You can use different request handlers for different functions.

;; @func request-handler
;; @desc Based on 'path' parameter, generates corresponding HTML content, and uses 'params' for 'dynamic' interaction.
;; @example (hello-request-handler "greeting" '() '((name . "Bob"))) => <html>Nice to meet you, Bob!</html>

(defun hello-request-handler (path header params)
                             (if (equal path "greeting")
                                 (let ((name (assoc 'name params)))
                                      (if (not name)
                                          (princ "<html><form>What is your name? <input name='name' /></form></html>")
                                          (format t "<html>Nice to meet you, ~a!</html>" (cdr name))))
                                 (princ "Sorry... I don't know that page.")))
