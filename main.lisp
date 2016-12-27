;; Entry point that runs the server.

;; Load the header parser
(load "src/parse-header.lisp")

;; Load the main server script
(load "src/server.lisp")

;; Load the request handler to be passed to serve function
(load "src/request-handler.lisp")

;; Run the server
(serve #'hello-request-handler)
