## Description
(Under development)
A barebones HTTP (1.1) server in CLISP, meant to be used for different use cases. This was primarily built for educational and hobby purposes. For this reason, no external libraries were used.

## Usage

To run the server, simply load the main.lisp, like this:
```lisp
(load "main.lisp")
```
The server is now running on port 8080.

In case you want to use a separate request handler, then change the request handler loaded in main.lisp to a function of your choice and pass it to the serve function. You can see a template for a request handler in src/request-handler.lisp

For example, if your request handler is called my-handler:

```lisp
;; Load the handler of your choice, say my-handler, in main.lisp
(load 'your/path/handler.lisp')

;; Replace (serve #'hello-request-handler) with line below
(serve #'my-handler)
```


## Background

I do quite a bit of hobby and academic projects in Lisp, which often have a command line interface. This makes it

## Todo

* Specify the port on which the server runs.
* Implement HTTP2
* Serve html using the [xml-generator](https://github.com/shaaza/xml-generator) macro.
* Tests

## Development

I've been too lazy to write tests for these programs, given how this is a hobby project for other hobby projects.
However, I've written stubs for tests by providing examples (with prefixed tag @examples) in the inline comments in many places.

#### Note:

It's very unlikely that I'll develop this a significant amount further. I'd rather contribute to an excellent existing server in CLISP, such as Eitaro Fukumachi's [woo](https://github.com/fukamachi/woo), written in CLISP and C.
