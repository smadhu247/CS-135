#lang eopl

#|-------------------------------------------------------------------------------
 | Name: Sanjana Madhu
 | Pledge: I pledge my honor that I have abided by the Stevens Honor System
 |-------------------------------------------------------------------------------|#


#|-------------------------------------------------------------------------------
 |                               INTRODUCTION
 |-------------------------------------------------------------------------------|#

;; In this lab, we'll implement topological sort,
;;   a sorting algorithm for directed graphs (digraphs).
;; Unlike last lab in which we used undirected graphs,
;;   we will only concern ourselves with digraphs here.


;; Graphs will be represented differently in this lab.
;; We'll use the following "nicknames" for datatypes:
;;
;; A "vertex" can be any datatype.
;;   Consequently, you MUST compare vertices using "equal?".
;;
;; An "edge" is a list of two vertices.
;;   The edge (u v) represents a directed edge from u to v.
;;   The edges (u v) and (v u) are not equivalent.
;;
;; A "graph" is a list containing a list of vertices and a list of edges.
;;   The graph ( (1 2 3) ((1 2) (2 3) (1 1)) ) represents a digraph
;;     with the vertices {1,2,3} and the edges {(1,2), (2,3), (1,1)}.
;;   You may assume that no vertices repeat in the list of vertices,
;;     and that no edges repeat in the list of edges.
;;   You may assume that all vertices referred to in a graph's list of edges
;;     will also be in the graph's list of vertices. However,
;;     a vertex in a graph's list of vertices may not be mentioned in its list of edges.


;; Topological sort is an algorithm which lists the vertices of a graph
;;   such that for every directed edge (u v) in the graph, u is earlier in the list than v.
;; In other words, imagine physically moving the vertices of a graph into a line
;;   so that every edge points in the same direction. The order in which the vertices
;;   are lined up is the "topological sorting" of the graph.
;;
;; Topological sort only works on directed acyclic graphs (dags).
;; Consider how if a graph contains a cycle, it's impossible to align all the vertices
;;   such that all the edges point in the same direction.
;; So, we'll build an error check into our code so that
;;   topological sort fails if we're given a graph that isn't a dag.


;; The topological sort algorithm goes as follows for a graph G:
;;   1. Begin with an empty output list.
;;   2. If G has no vertices, return the output list. Otherwise:
;;   3. Find the indegrees of all the vertices in G.
;;   4. If no vertex has an indegree of zero,
;;      then G contains a cycle and topological sort has failed.
;;      Otherwise:
;;   5. Remove the lowest-indexed vertex with indegree 0 from G,
;;      and add this vertex to the end of the output list.
;;      (Removal of specifically the *lowest-indexed* vertex
;;       isn't algorithmically necessary; it simply creates consistency.
;;       In our case, a vertex's index is its position in a graph's list of vertices.)
;;   6. Go to step 2.
;;
;; Try out this algorithm on a few graphs by hand to get a feel for how it works.
;; Can you see how and why topological sort fails with cyclic graphs?
;; Notice that a dag doesn't have to be connected for topological sort to work.


;; Here are some example digraphs we'll use for testing.
;; Some are dags, some are not.

(define C6 '( (1 2 3 4 5 6)
              ((1 2) (2 3) (3 4) (4 5) (5 6) (6 1)) ))

(define reflex '( (1 2 3)
                  ((1 1) (2 2) (3 3)) ))

(define empty '( () () ))

(define L4 '( (4 3 2 1)
              ((4 3) (1 4) (2 1)) ))

(define btree '( (30 20 31 10 32 21 33 00 34 22 35 11 36 23 37)
                 ((00 10) (00 11) (10 20) (10 21) (11 22) (11 23)
                          (20 30) (20 31) (21 32) (21 33)
                          (22 34) (22 35) (23 36) (23 37)) ))

(define lonely '( ("A cleverly named vertex" "Another vertex" "Vertex number 3")
                  () ))

(define sentence '( (me look hey unscrambled you)
                    ((unscrambled me) (look you) (you unscrambled) (hey look)) ))

(define forest '( (A5 A4 A3 A2 A1 B1 B2 C5 C4 C3 C2 C1 D1 )
                  ((A1 A2) (A3 A2) (A4 A3) (A3 A5) (B2 B1)
                           (C1 C5) (C2 C5) (C3 C5) (C5 C4)) ))

(define ankh '( (1 2 3 4 5 6 7)
                ((1 2) (2 3) (3 4) (4 1) (5 1) (6 1) (7 1))))


#|-------------------------------------------------------------------------------
 |                               HELPER FUNCTIONS
 |-------------------------------------------------------------------------------|#

;; You're encouraged to use all of the following helpers in your code.
;; The first three are trivially simple, but using them helps with "abstraction".


;; "get-vertices" accepts a graph and
;;   returns the list of vertices of said graph.
;; Type signature: (get-vertices graph) -> vertex-list
(define (get-vertices G) (car G))

;; "get-edges" accepts a graph and
;;   returns the list of edges of said graph.
;; Type signature: (dag-edges dag) -> list
(define (get-edges G) (cadr G))

;; "make-graph" accepts a list of vertices and a list of edges
;;   and returns a graph with said vertices and edges.
;; Type signature: (make-graph vertex-list edge-list) -> graph
(define (make-graph Vs Es) (list Vs Es))

;; "tsort-error" raises an error for when topological sort fails
;;   because the provided graph contains a cycle.
;; Call this function with (tsort-error).
(define (tsort-error)
  (eopl:error "Topological sort failure: graph contains a cycle!"))


;; "popv-vertices" accepts a list of vertices and a vertex,
;;   and returns the list of vertices without the given vertex.
;; Type-signature: (popv-vertices vertex-list vertex) -> vertex-list
(define (popv-vertices Vs v)
  (if (null? Vs) '()
      (if (equal? (car Vs) v) (cdr Vs)
          (cons (car Vs)
                (popv-vertices (cdr Vs) v)))))

;; "popv-edges" accepts a list of edges and a vertex,
;;   and returns the list of edges with all edges
;;   to/from the given vertex removed.
;; Type-signature: (popv-edges list vertex) -> list
(define (popv-edges Es v)
  (if (null? Es) '()
      (if (member v (car Es))
          (popv-edges (cdr Es) v)
          (cons (car Es)
                (popv-edges (cdr Es) v)))))




#|-------------------------------------------------------------------------------
 |                               IMPLEMENTATION
 |-------------------------------------------------------------------------------|#


;; Implement "indegree" to accept a list of edges and a vertex,
;;   and return the indegree of the vertex based on the given edges.
;;
;; Recall that the indegree of a vertex is how many edges point *to* it!
;; So ensure you're checking the correct half of each edge.
;; Also recall that you need to use "equal?" to compare vertices,
;;   because they could be any datatype.
;;
;; Examples:
;;   (indegree (get-edges C6) 4) -> 1
;;   (indegree (get-edges reflex) 2) -> 1
;;   (indegree (get-edges empty) "not there") -> 0
;;   (indegree (get-edges L4) 2) -> 0
;;   (indegree (get-edges btree) 35) -> 1
;;   (indegree (get-edges forest) 'C5) -> 3
;;   (indegree (get-edges ankh) 1) -> 4
;;
;; Type signature: (indegree edge-list vertex) -> int
(define (indegree Es v)
  (cond ((null? Es) 0)
        ((equal? v (car (cdr (car Es)))) (+ 1 (indegree (cdr Es) v) ))
        (else (indegree (cdr Es) v) )
        ))



;; Implement "find-top" to accept a graph
;;   and return the "top" of the graph, which is
;;   the lowest-indexed vertex with an indegree of 0.
;;
;; The "index" of a vertex refers to its position
;;   in the graph's list of vertices,
;;   NOT the value of the vertex itself.
;;
;; If no vertex in the graph has indegree 0,
;;   then we cannot find the top of the graph.
;; If this occurs, raise the appropriate exception
;;   by calling (tsort-error).
;; This error will be useful later on because
;;   it will halt the topological sort algorithm
;;   if the provided graph contains a cycle.
;;
;; You may assume the initial input graph has
;;   at least one vertex. We'll only be calling
;;   this function when the graph we're sorting
;;   still has vertices remaining.
;;
;; Examples:
;;   (find-top C6) -> <error>
;;   (find-top reflex) -> <error>
;;   (find-top L4) -> 2
;;   (find-top btree) -> 0
;;   (find-top lonely) -> "A cleverly named vertex"
;;   (find-top sentence) -> hey
;;   (find-top forest) -> A4
;;   (find-top ankh) -> 5
;;
;; Type signature: (find-top graph) -> vertex
(define (find-top G)
  (find-tophelper G (get-vertices G)))

(define (find-tophelper G list)
(cond ((null? list) (tsort-error)) 
        ((equal? 0 (indegree (get-edges G) (car list))) (car list))
        (else (find-tophelper G (cdr list)))
 ))



;; Implement "pop-vertex" to accept a graph G and a vertex v,
;;   and return a subgraph of G where v is removed from G's list of vertices
;;   and all edges to/from v from removed from G's list of edges.
;;
;; Take advantage of the provided helper functions for this!
;;
#| Examples:
     (pop-vertex C6 5) ->
       ( (1 2 3 4 6)
         ((1 2) (2 3) (3 4) (6 1)) )
     (pop-vertex reflex 2) ->
       ( (1 3) ((1 1) (3 3)) )
     (pop-vertex empty 6)
       (() ())
     (pop-vertex lonely "Another vertex")
       ( ("A cleverly named vertex" "Vertex number 3") () )
     (pop-vertex sentence 24)
       ( (me look hey unscrambled you)
         ((unscrambled me) (look you) (you unscrambled) (hey look)) )
     (pop-vertex forest 'A3) ->
       ( (A5 A4 A2 A1 B1 B2 C5 C4 C3 C2 C1 D1)
         ((A1 A2) (B2 B1) (C1 C5) (C2 C5) (C3 C5) (C5 C4)) ) 
     (pop-vertex ankh 4) ->
       ( (1 2 3 5 6 7)
         ((1 2) (2 3) (5 1) (6 1) (7 1)) ) 
|#
;; Type signature: (pop-vertex graph vertex) -> graph
(define (pop-vertex G v)
 (list (popv-vertices (get-vertices G) v) (popv-edges (get-edges G) v)))




;; Implement "tsort" to accept a graph G
;;   and return the topological sorting of G,
;;   which will be a list of G's vertices in a particular order.
;; Remember, the behavior of this function will be
;;   to find the "top" vertex of G, remove said vertex from G,
;;   and repeat with the updated graph until no vertices remain.
;;   
;; Examples:
;;   (tsort C6) -> <error>
;;   (tsort reflex) -> <error>
;;   (tsort empty) -> ()
;;   (tsort L4) -> (2 1 4 3)
;;   (tsort btree) -> (0 10 20 30 31 21 32 33 11 22 34 35 23 36 37)
;;      ^ btree's vertex list is an inorder traversal; tsort produces a preorder traversal!
;;   (tsort lonely) -> ("A cleverly named vertex" "Another vertex" "Vertex number 3")
;;   (tsort sentence) -> (hey look you unscrambled me)
;;   (tsort forest) -> (A4 A3 A5 A1 A2 B2 B1 C3 C2 C1 C5 C4 D1)
;;   (tsort ankh) -> <error>
;;
;; (topological-sort graph) -> vertex-list
(define (tsort G) 
 (tsortHelper G (get-vertices G)))

(define (tsortHelper G lst)
  (cond ((null? lst) '())
        (else (cons (find-top G) (tsortHelper (pop-vertex G (find-top G)) (cdr lst))))
        ))

;;(reverse(cdr (cdr (reverse G))))

;; Congratulations! You've implemented topological sort.
;; Now just wait a little while and you'll get to see it again in CS-385!