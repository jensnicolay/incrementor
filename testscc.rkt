#lang racket

(require "incrementor.rkt")

(define r1 (:- #(n2)    #(n4)))
(define r2 (:- #(n3)    #(n2)))
(define r3 (:- #(n2)    #(n3)))
(define r4 (:- #(n0)    #(n6)))
(define r5 (:- #(n1)    #(n0)))
(define r6 (:- #(n0)    #(n2)))
(define r7 (:- #(n12)   #(n11)))
(define r8 (:- #(n9)    #(n12)))
(define r9 (:- #(n10)   #(n9)))
(define r10 (:- #(n11)  #(n9)))
(define r11 (:- #(n9)   #(n8)))
(define r12 (:- #(n12)  #(n10)))
(define r13 (:- #(n4)   #(n11)))
(define r14 (:- #(n3)   #(n4)))
(define r15 (:- #(n5)   #(n3)))
(define r16 (:- #(n8)   #(n7)))
(define r17 (:- #(n7)   #(n8)))
(define r18 (:- #(n4)   #(n5)))
(define r19 (:- #(n5)   #(n0)))
(define r20 (:- #(n4)   #(n6)))
(define r21 (:- #(n9)   #(n6)))
(define r22 (:- #(n6)   #(n7)))

(define P (set r1 r2 r3 r4 r5 r6 r7 r8 r9 r10 r11 r12 r13 r14 r15 r16 r17 r18 r19 r20 r21 r22))

(printf "stratify ~v\n\n" (stratify P))

