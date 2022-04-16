;; Copyright 2020 Ryan Culpepper
;; SPDX-License-Identifier: Apache-2.0

#lang racket/base
(require racket/match
         racket/list
         racket/contract/base
         net/url-structs
         net/url-string)
(provide (contract-out
          #:unprotected-submodule unchecked
          [build-url
           (->* [(or/c url? string?)]
                [#:query (listof (cons/c symbol? (or/c #f string?)))
                 #:fragment (or/c #f string?)]
                #:rest (listof (or/c path/param? string?))
                url?)]
          [url-trim-final-slash
           (-> (or/c url? string?) url?)]))

(define (build-url base
                   #:query [query-parts null]
                   #:fragment [fragment #f]
                   . path-parts)
  (build-url* base path-parts query-parts fragment))

(define (build-url* base path-parts query-parts fragment)
  (match base
    [(? string?)
     (build-url* (url-trim-final-slash base) path-parts query-parts fragment)]
    [(url scheme user host port path-absolute? path query old-fragment)
     (define path* (append (trim-final-/ path) (map build-path/param path-parts)))
     (define query* (append query query-parts))
     (define fragment* (or fragment old-fragment))
     (url scheme user host port path-absolute? path* query* fragment*)]))

(define (build-path/param p)
  (cond [(path/param? p) p]
        [else (path/param p null)]))

(define (url-trim-final-slash u)
  (match u
    [(? string?)
     (url-trim-final-slash (string->url u))]
    [(url scheme user host port path-absolute? path query fragment)
     (url scheme user host port path-absolute? (trim-final-/ path) query fragment)]))

;; trim-final-/ : (Listof path/param?) -> (Listof path/param?)
;; If pps ends with (path/param "" '()), representing a final "/"
;; after the last path in the url, then drop it.
(define (trim-final-/ pps)
  (cond [(and (pair? pps) (match (last pps) [(path/param "" '()) #t] [_ #f]))
         (drop-right pps 1)]
        [else pps]))
