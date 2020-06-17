#lang eopl

#|-------------------------------------------------------------------------------
 | Name:Sanjana Madhu
 | Pledge: I pledge my honor that I have abided by the Stevens Honor System
 |-------------------------------------------------------------------------------|#


#|-------------------------------------------------------------------------------
 |                               INTRODUCTION
 |-------------------------------------------------------------------------------|#

;; In this lab, we'll be working with undirected graphs.
;; Undirected graphs consist of vertices connected together with edges.
;; The edges in undirected graphs can be traversed in either direction.


;; Three datatypes that will be referred to in this lab
;;   are "vertex", "edge", and "graph".
;;
;; A "vertex" is an integer.
;; An "edge" is a list of two vertices.
;;   All edges in this lab are undirected.
;; A "graph" is a list of edges.
;;   All graphs in this lab are undirected.
;;   Empty graphs (graphs with no edges) are valid input
;;     for all functions throughout the lab.
;;   You MAY assume that a graph does not contain duplicate edges,
;;     including symmetric edges; in other words,
;;     if the edge (a b) is in a graph once,
;;     you can be sure that (a b) will only be listed once,
;;     AND that (b a) will NOT be listed.


;; The ultimate goal of this lab is to determine whether
;;   graphs contain eulerian cycles.
;; An eulerian cycle is a cycle which traverses every edge of a graph exactly once.
;; Given an arbitrary graph G, G will contain an eulerian cycle iff it satisfies two conditions:
;;   1. The degree of every vertex in G is even.
;;   2. G is connected.


;; Here are some example graphs we'll work with for testing:

(define C6 '((1 2) (2 3) (3 4) (4 5) (5 6) (6 1)))
(define K4 '((4 3) (4 2) (4 1) (3 2) (3 1) (2 1)))
(define full5 '((1 1) (1 2) (1 3) (1 4) (1 5)
                      (2 2) (2 3) (2 4) (2 5)
                      (3 3) (3 4) (3 5)
                      (4 4) (4 5) (5 5)))
(define btree '((00 10) (00 11) (10 20) (10 21) (11 22) (11 23)
                        (20 30) (20 31) (21 32) (21 33)
                        (22 34) (22 35) (23 36) (23 37)))
(define triforce '((00 10) (00 11) (10 11) (10 20) (10 21)
                           (20 21) (11 21) (11 22) (21 22)))
(define polygons '((1 2) (2 3) (3 4) (4 1)
                         (5 6) (6 7) (7 5)))
(define reflex '((1 1) (2 2) (3 3)))
(define empty '())


#|-------------------------------------------------------------------------------
 |                               HELPER FUNCTIONS
 |-------------------------------------------------------------------------------|#

;; You may use any/all of the following helper functions in your code.


;; "remove" accepts lists L and R
;;   and returns L filtered such that all elements of L
;;   which are also in R have been removed.
;;
;; Type signature: (remove list list) -> list
(define (remove L R)
  (define (recurse acc L)
    (if (null? L) (reverse acc)
        (recurse (if (member (car L) R)
                     acc
                     (cons (car L) acc))
                 (cdr L))))
  (recurse '() L))


;; "usort" accepts a list of integers L
;;   and returns L sorted from least to greatest
;;   with all duplicate elements removed (hence "unique-sort").
;;
;; Type signature: (usort int-list) -> int-list
(define (usort L)
  (define (recurse acc L)
    (if (null? L) acc
        (let ([m (apply max L)])
          (recurse (cons m acc) (remove L (list m))))))
  (recurse '() L))


;; "dfs" performs a modified depth-first search algorithm.
;; Given a graph, a list of vertices already found through dfs,
;;   and a starting vertex v,
;;   dfs returns a list of all vertices which can be reached from v
;;   through some sequence of edges.
;; The returned list may contain duplicates and may be unsorted.
;; This function won't work without a working implementation of "adjacent",
;;   which you'll write later on in this lab.
;;
;; Type signature: (dfs graph vertex-list vertex) -> vertex-list
(define (dfs G found v)
  (define next-found (cons v found))
  (define adj (remove (adjacent G v) next-found))
  (define (f x) (dfs G next-found x))
  (define res (apply append (map f adj)))
  (cons v res))


#|-------------------------------------------------------------------------------
 |                            IMPLEMENTING CONDITION #1
 |-------------------------------------------------------------------------------|#

;; Here, we'll implement functions to test whether a graph satisfies
;;   the first condition necessary for an eulerian cycle to exist:
;;   is the degree of every vertex even?


;; Implement "get-vertices", which accepts a graph
;;   and returns all vertices in the graph.
;;
;; (Technically a graph can contain a vertex connected to no edges,
;;  in which case we wouldn't know said vertex exists when only
;;  looking at the edges, but we just won't worry about that here.)
;;  
;; Once you've found all the vertices,
;;   run "usort" on the list of vertices and return the result.
;; usort will sort the list from least to greatest
;;   and remove all duplicates.
;; Removing the duplicates will help the efficiency
;;   of some functions you write.
;; The sorting part isn't algorithmically necessary,
;;   but it ensures your output will exactly match the expected output.
;;
;; Examples:
;;   (get-vertices C6) -> (1 2 3 4 5 6)
;;   (get-vertices K4) -> (1 2 3 4)
;;   (get-vertices full5) -> (1 2 3 4 5)
;;   (get-vertices btree) -> (0 10 11 20 21 22 23 30 31 32 33 34 35 36 37)
;;   (get-vertices triforce) -> (0 10 11 20 21 22)
;;   (get-vertices polygons) -> (1 2 3 4 5 6 7)
;;   (get-vertices reflex) -> (1 2 3)
;;   (get-vertices empty) -> ()
;;
;; Type signature: (get-vertices graph) -> vertex-list
(define (get-vertices G)
  (cond ((null? G) '())
        (else (usort(append (list (car(car G)) (car(cdr(car G)))) (get-vertices (cdr G))) ))
       ))




;; Implement "degree", which accepts a graph G and a vertex v,
;;   and returns the degree of v in G.
;; The degree of v in G is the total number of edges pointing
;;   into or out of v.
;; Reflexive edges - edges of the form (v v) - are special,
;;   in that they add 2 to the degree of v.
;;
;; Again, you may assume G does not contain duplicate edges.
;;
;; Examples:
;;   (degree C6 3) -> 2
;;   (degree C6 7) -> 0
;;   (degree K4 4) -> 3
;;   (degree full5 2) -> 6
;;   (degree btree 21) -> 3
;;   (degree triforce 10) -> 4
;;   (degree polygons 6) -> 2
;;   (degree reflex 2) -> 2
;;   (degree empty 1) -> 0
;;
;; Type signature: (degree graph vertex) -> int

(define (degree G v)
  (cond ((null? G) 0)  
        ((and (equal? (car(car G)) v)  (equal? (car (cdr (car G))) v)) (+ 2 (degree (cdr G) v)) )
        ((or (equal? (car(car G)) v)  (equal? (car (cdr (car G))) v)) (+ 1 (degree (cdr G) v)) )
        (else (degree (cdr G) v))
  ))



;; Implement "all-even?", which accepts a graph
;;   and returns whether all of its vertices have even degree.
;;
;; Examples:
;;   (all-even? C6) -> #t
;;   (all-even? K4) -> #f
;;   (all-even? full5) -> #t
;;   (all-even? btree) -> #f
;;   (all-even? triforce) -> #t
;;   (all-even? polygons) -> #t
;;   (all-even? reflex) -> #t
;;   (all-even? empty) -> #t
;;
;; Type signature: (all-even? graph) -> bool
(define (all-even? G)
  (all-even-helper G (get-vertices G)))

(define (all-even-helper G lst)
  (cond ((null? lst) #t) 
        ((equal? (modulo (degree G (car lst)) 2) 0) (all-even-helper G (cdr lst))) 
        (else #f)
    ))



#|-------------------------------------------------------------------------------
 |                            IMPLEMENTING CONDITION #2
 |-------------------------------------------------------------------------------|#

;; Here, we'll implement functions to test whether a graph satisfies
;;   the second condition necessary for an eulerian cycle to exist:
;;   is the graph connected?
;; An undirected graph is connected iff every vertex can be reached
;;   by every other vertex by traversing some sequence of edges.

;; There is technically one exception to this rule which was mentioned before;
;;   if a graph contains more than one vertex, and it contains
;;   a vertex with a degree of 0 (the vertex is "isolated"),
;;   then the graph is not technically connected,
;;   but it could still contain an eulerian cycle.
;; Again, we won't worry about this case because we can't even
;;   know when the graphs we're working with have an isolated vertex anyway.




;; Implement "adjacent", which accepts a graph G and a vertex v
;;   and returns a list of all vertices directly connected to v by an edge.
;; Like with "get-vertices", run the resultant list you find
;;   through "usort" before returning it.
;; This function will need to work for the "dfs" helper function to work.
;;
;; Examples:
;;   (adjacent C6 6) -> (1 5)
;;   (adjacent K4 1) -> (2 3 4)
;;   (adjacent full5 4) -> (1 2 3 4 5)
;;   (adjacent btree 22) -> (11 34 35)
;;   (adjacent triforce 11) -> (0 10 21 22)
;;   (adjacent polygons 3) -> (2 4)
;;   (adjacent reflex 1) -> (1)
;;   (adjacent reflex 4) -> ()
;;   (adjacent empty 5) -> ()
;;
;; Type signature: (adjacent graph vertex) -> vertex-list
(define (adjacent G v)
  (cond ((null? G) '())
        ((equal? (car(car G)) v) (usort(cons (car(cdr(car G))) (adjacent (cdr G) v)) ))
        ((equal? (car(cdr(car G))) v) (usort(cons (car(car G)) (adjacent (cdr G) v)) ))
        (else (adjacent (cdr G) v))                      
    ))
 



;; Implement "connected?", which accepts a graph
;;   and returns whether the graph is connected.
;; Take advantage of the "dfs" helper function,
;;   as it will return all the vertices reachable
;;   from some starting point.
;; If a graph is connected, you should be able
;;   to start from any initial vertex and reach
;;   every other vertex.
;; So, find a way to select any arbitrary vertex
;;   from the input graph, and called "dfs"
;;   with that vertex as the initial vertex,
;;   and an empty list as the initial "found" list of vertices.
;; If the graph is connected, the result of the depth-first search
;;   will contain all the vertices in the graph.
;; Consider how you can easily compare the contents of the dfs output
;;   with the graph's list of vertices.
;;
;; Examples:
;;   (connected? C6) -> #t
;;   (connected? K4) -> #t
;;   (connected? full5) -> #t
;;   (connected? btree) -> #t
;;   (connected? triforce) -> #t
;;   (connected? polygons) -> #f
;;   (connected? reflex) -> #f
;;   (connected? empty) -> #t
;;
;; Type signature: (connected? graph) -> bool
(define (connected? G)
  (connected-helper G (get-vertices G)) )

(define (connected-helper G lst)
  (cond ((equal? G '()) #t)
        ((equal? lst (usort (dfs G '() (car lst)))) #t) 
        (else #f)
        ))


#|-------------------------------------------------------------------------------
 |                            BRINGING IT ALL TOGETHER
 |-------------------------------------------------------------------------------|#

;; Implement "eulerian?", which accepts a graph
;;   and returns whether it is an eulerian graph
;;   (in other words, whether it contains an eulerian cycle).
;; Recall that a graph is eulerian iff
;;   it is connected and all its vertices have even degree.
;;
;; Examples:
;;   (eulerian? C6) -> #t
;;   (eulerian? K4) -> #f
;;   (eulerian? full5) -> #t
;;   (eulerian? btree) -> #f
;;   (eulerian? triforce) -> #t
;;   (eulerian? polygons) -> #f
;;   (eulerian? reflex) -> #f
;;   (eulerian? empty) -> #t
;;
;; Type signature: (eulerian? graph) -> bool
(define (eulerian? G)
  (cond ((and (equal? (connected? G) #t) (equal? (all-even? G) #t)) #t)
        (else #f))
        )