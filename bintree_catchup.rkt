;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname bintree_catchup) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
;homework #6

;Exercise 1.31[*]
(define i-node
  (lambda (symbol lson rson)
    (list 'i-node symbol lson rson)))

(define leaf
  (lambda (int)
    (list 'leaf int)))

(define leaf?
  (lambda (bintree)
    (eq? (car bintree) 'leaf)))

(define get-lson
  (lambda (bintree)
    (caddr bintree)))

(define get-rson
  (lambda (bintree)
    (cadddr bintree)))

(define contents-of
  (lambda (bintree)
    (cadr bintree)))

(define bintree
  (i-node 'red
          (i-node 'bar
                  (leaf 26)
                  (leaf 12))
          (i-node 'red
                  (leaf 11)
                  (i-node 'quux
                          (leaf 117)
                          (leaf 14)))))
;Exercise 1.32[*]

(define double-tree
  (lambda (bintree)
    (cond
      ((leaf? bintree) (leaf (* 2 (contents-of bintree))))
      (else (i-node (contents-of bintree) (double-tree (get-lson bintree)) (double-tree (get-rson bintree)))))))

(double-tree bintree)

;Exercise 1.33[**]

(define red-depth-helper
  (lambda (bintree counter)
    (cond
      ((leaf? bintree) (leaf counter))
      ((eq? (contents-of bintree) 'red) (i-node (contents-of bintree)
                                                (red-depth-helper (get-lson bintree) (+ counter 1))
                                                (red-depth-helper (get-rson bintree) (+ counter 1))))
      (else (i-node (contents-of bintree)
                    (red-depth-helper (get-lson bintree) counter)
                    (red-depth-helper (get-rson bintree) counter))))))

(define red-depth
  (lambda (bintree)
    (red-depth-helper bintree 0)))

(red-depth bintree)








     

