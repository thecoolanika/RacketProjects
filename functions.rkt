#lang racket

(provide alphabetic-only downcase list->table table->list transpose)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; alphabetic-only
; ------------------------------------------------------------------------------
; Strips a list of characters of any non-alphabetic characters
;
; inputs: 
;   chars: A list of characters
; outputs: 
;   The list of characters, filtered to contain only alphabetic characters.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (alphabetic-only chars)
  (filter char-alphabetic? chars)) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; downcase
; ------------------------------------------------------------------------------
; Makes all the characters in a list lowercase(downcase)
;
; inputs: 
;   chars: A list of alphabetic characters
; outputs: 
;   The list of characters, only in downcase
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (downcase chars)
  (map char-downcase chars)) 


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; list->table
; ------------------------------------------------------------------------------
; Given a list of elements, a number of rows r, and a number of columns c,
; returns a list of r lists where each list has c values.
;
; Assumption: the length of the list is r * c.
;
; inputs: 
;   elements: a list of values
;   rows: the number of rows in the table
;   columns: the number of columns in the table
;
; outputs: 
;   A table with the specified number of rows and columns, where the data is
;   in row-major order (i.e., the first c elements of the data become the first
;   row of the table, where c is the number of columns).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (list->table elements rows columns)
  (if (empty? elements)
      empty
      (cons (take elements columns)
            (list->table (drop elements columns) (- rows 1) columns))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; table->list
; ------------------------------------------------------------------------------
; Given a table of elements with a number of rows r, and a number of columns c,
; returns a list of all the elements in row major order.
;
; inputs: 
;   table: a list of values divided into sublists
;
; outputs: 
;   A list of all the values in table in row major order
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (table->list table)
  (if (empty? table) empty (append (first table) (table->list (rest table))) ))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; table->list
; ------------------------------------------------------------------------------
; Given a table with r rows and c columns, return a table with c rows and r columns
; in column-major order
;
; inputs: 
;   table: a list of values divided into sublists with r rows and c columns
;
; outputs: 
;   A list of values divided into sublists with c rows and r columns
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (transpose table)
  (if (empty? (first table))
     empty
     (if (empty? (rest (first table)))
     (list(map (lambda (L) (first L)) table))
     (append (list(map (lambda (L) (first L)) table)) (transpose (map (lambda (L) (rest L)) table))))))
