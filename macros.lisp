;;;; -*- Mode: lisp; indent-tabs-mode: nil -*-
;;;
;;; cl-brainfuck: Brainfuck program macro assembler and simulator
;;;
;;; Copyright (C) 2012,      Charles Lew    <crlf0710(@)gmail.com>
;;;
;;; Permission is hereby granted, free of charge, to any person
;;; obtaining a copy of this software and associated documentation
;;; files (the "Software"), to deal in the Software without
;;; restriction, including without limitation the rights to use, copy,
;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;; of the Software, and to permit persons to whom the Software is
;;; furnished to do so, subject to the following conditions:
;;;
;;; The above copyright notice and this permission notice shall be
;;; included in all copies or substantial portions of the Software.
;;;
;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;; DEALINGS IN THE SOFTWARE.

(in-package #:cl-brainfuck)


(defun cl-brainfuck.macros::inc (&optional (number 1))
  (assert (numberp number))
  (loop repeat number
       collect :inc))

(defun cl-brainfuck.macros::dec (&optional (number 1))
  (assert (numberp number))
  (loop repeat number
     collect :dec))

(defun cl-brainfuck.macros::diff (&rest args)
  (let ((reversed nil))
    (loop for item in args
       if (eql item :reverse)
       do (setf reversed (not reversed))
       else
       do (assert (numberp item))
       and if (not (zerop item))
       collect (if (not reversed)
                   (if (plusp item)
                       `(:inc ,item)
                       `(:dec ,(- item)))
                   (if (plusp item)
                       `(:dec ,item)
                       `(:inc ,(- item)))))))

(defun cl-brainfuck.macros::next (&optional (number 1))
  (assert (numberp number))
  (loop repeat number
       collect :next))

(defun cl-brainfuck.macros::prev (&optional (number 1))
  (assert (numberp number))
  (loop repeat number
     collect :prev))

(defun cl-brainfuck.macros::move (&rest args)
  (let ((reversed nil))
    (loop for item in args
       if (eql item :reverse)
       do (setf reversed (not reversed))
       else
       do (assert (numberp item))
       and if (not (zerop item))
       collect (if (not reversed)
                   (if (plusp item)
                       `(:next ,item)
                       `(:prev ,(- item)))
                   (if (plusp item)
                       `(:prev ,item)
                       `(:next ,(- item)))))))

(defun cl-brainfuck.macros::move-and-begin (&rest args)
  (destructuring-bind
        ((&rest offsets) &rest actions) args
    `((:move ,@offsets)
      ,@actions)))

(defun cl-brainfuck.macros::move-and-return (&rest args)
  (destructuring-bind
        ((&rest offsets) &rest actions) args
    `((:move ,@offsets)
      ,@actions
      (:move :reverse ,@offsets))))

(defun cl-brainfuck.macros::with-action (&rest args)
  (destructuring-bind
        ((name &rest actions) &rest items) args
    (subst `(:begin ,@actions) name items)))

(defun cl-brainfuck.macros::begin (&rest items)
  items)

(defun cl-brainfuck.macros::repeat (number &rest items)
  (assert (numberp number))
  (loop for i from 0 below number
     append items))

(defun cl-brainfuck.macros::var-repeat (number var &rest items)
  (assert (numberp number))
  (loop for i from 0 below number
     append (subst i var items)))

(defun cl-brainfuck.macros::zero (&optional (number 1))
  (assert (numberp number))
  `(:while :dec :loop ,@(loop for i from 1 below number
                           appending '(:next :while :dec :loop))))

#+nil
(defun cl-brainfuck.macros::zero (&optional (number 1))
  (assert (numberp number))
  `((:inc 5) :while :dec :loop ,@(loop for i from 1 below number
                           appending '(:next (:inc 5) :while :dec :loop))))

(defun cl-brainfuck.macros::one (&optional (number 1))
  (assert (numberp number))
  `((:zero) :inc ,@(loop for i from 1 below number
                                appending '(:next (:zero) :inc))))

(defun cl-brainfuck.macros::putch (&optional (number 1) &rest actions)
  (assert (numberp number))
  `(,@actions :putch ,@(loop for i from 1 below number
                          appending `(:next ,@actions :putch))))

(defun cl-brainfuck.macros::getch (&optional (number 1) &rest actions)
  (assert (numberp number))
  `(:getch ,@actions ,@(loop for i from 1 below number
                          appending `(:next :getch ,@actions))))

(defun cl-brainfuck.macros::while-loop (&rest actions)
  `(:while ,@actions :loop))

(defun cl-brainfuck.macros::num (num &optional (number 1))
  (assert (numberp num))
  (assert (numberp number))
  `((:zero) (:inc ,num)
           ,@(loop for i from 1 below number
                appending `(:next (:zero) (:inc ,num)))))


(defun cl-brainfuck.macros::assign-zero (&rest args)
  (destructuring-bind
        ((&rest to-offsets)) args
    `((:move-and-return (,@to-offsets) (:zero)))))

(defun cl-brainfuck.macros::assign (&rest args)
  (destructuring-bind
        ((&rest to-offsets) (&rest from-offsets) (&rest temp0-offsets))
      args
    `((:assign-zero (,@temp0-offsets))
      (:assign-zero (,@to-offsets))
      (:move-and-return (,@from-offsets)
                        (:while-loop (:move :reverse    ,@from-offsets)
                                     (:move-and-return (,@to-offsets)   (:inc))
                                     (:move-and-return (,@temp0-offsets)(:inc))
                                     (:move-and-begin  (,@from-offsets) (:dec))))
      (:move-and-return (,@temp0-offsets)
                        (:while-loop (:move :reverse    ,@temp0-offsets)
                                     (:move-and-return (,@from-offsets) (:inc))
                                     (:move-and-begin  (,@temp0-offsets)(:dec)))))))
                        
      
(defun cl-brainfuck.macros::swap (&rest args)
  (destructuring-bind
        ((&rest x-offsets) (&rest y-offsets) (&rest temp0-offsets))
      args
    `((:assign-zero (,@temp0-offsets))
      (:move-and-return (,@x-offsets)
                        (:while-loop (:move :reverse   ,@x-offsets)
                                     (:move-and-return(,@temp0-offsets) (:inc))
                                     (:move-and-begin (,@x-offsets)     (:dec))))
      (:move-and-return (,@y-offsets)
                        (:while-loop (:move :reverse   ,@y-offsets)
                                     (:move-and-return(,@x-offsets)     (:inc))
                                     (:move-and-begin (,@y-offsets)     (:dec))))
      (:move-and-return (,@temp0-offsets)
                        (:while-loop (:move :reverse   ,@temp0-offsets)
                                     (:move-and-return(,@y-offsets)     (:inc))
                                     (:move-and-begin (,@temp0-offsets) (:dec)))))))

(defun cl-brainfuck.macros::equalf (&rest args)
  (destructuring-bind
        ((&rest x-offsets) (&rest y-offsets)
         (&rest temp0-offsets) (&rest temp1-offsets))
      args
    `((:assign-zero (,@temp0-offsets))
      (:assign-zero (,@temp1-offsets))
      (:move-and-return (,@x-offsets)
                        (:while-loop (:move :reverse   ,@x-offsets)
                                     (:move-and-return(,@temp1-offsets)(:inc))
                                     (:move-and-begin (,@x-offsets)    (:dec)))
                        (:inc))
      (:move-and-return (,@y-offsets)
                        (:while-loop (:move :reverse   ,@y-offsets)
                                     (:move-and-return(,@temp1-offsets)(:dec))
                                     (:move-and-return(,@temp0-offsets)(:inc))
                                     (:move-and-begin (,@y-offsets)    (:dec))))
      (:move-and-return (,@temp0-offsets)
                        (:while-loop (:move :reverse   ,@temp0-offsets)
                                     (:move-and-return(,@y-offsets)    (:inc))
                                     (:move-and-begin (,@temp0-offsets)(:dec))))
      (:move-and-return (,@temp1-offsets)
                        (:while-loop (:move :reverse   ,@temp1-offsets)
                                     (:move-and-return(,@x-offsets)    (:dec))
                                     (:move-and-begin (,@temp1-offsets)(:zero)))))))

(defun cl-brainfuck.macros::not-equalf (&rest args)
  (destructuring-bind
        ((&rest x-offsets) (&rest y-offsets)
         (&rest temp0-offsets) (&rest temp1-offsets))
      args
    `((:assign-zero (,@temp0-offsets))
      (:assign-zero (,@temp1-offsets))
      (:move-and-return (,@x-offsets)
                        (:while-loop (:move :reverse   ,@x-offsets)
                                     (:move-and-return(,@temp1-offsets)(:inc))
                                     (:move-and-begin (,@x-offsets)    (:dec))))
      (:move-and-return (,@y-offsets)
                        (:while-loop (:move :reverse   ,@y-offsets)
                                     (:move-and-return(,@temp1-offsets)(:dec))
                                     (:move-and-return(,@temp0-offsets)(:inc))
                                     (:move-and-begin (,@y-offsets)    (:dec))))
      (:move-and-return (,@temp0-offsets)
                        (:while-loop (:move :reverse   ,@temp0-offsets)
                                     (:move-and-return(,@y-offsets)    (:inc))
                                     (:move-and-begin (,@temp0-offsets)(:dec))))
      (:move-and-return (,@temp1-offsets)
                        (:while-loop (:move :reverse   ,@temp1-offsets)
                                     (:move-and-return(,@x-offsets)    (:dec))
                                     (:move-and-begin (,@temp1-offsets)(:zero)))))))


(defun cl-brainfuck.macros::if (&rest args)
  (destructuring-bind
        ((&rest x-offsets) (&rest temp0-offsets) (&rest temp1-offsets) &rest code)
      args
    `((:assign-zero (,@temp0-offsets))
      (:assign-zero (,@temp1-offsets))
      (:move-and-return (,@x-offsets)
                        (:while-loop (:move :reverse   ,@x-offsets)
                                     (:move-and-return(,@temp0-offsets)(:inc))
                                     (:move-and-return(,@temp1-offsets)(:inc))
                                     (:move-and-begin (,@x-offsets)    (:dec))))
      (:move-and-return (,@temp0-offsets)
                        (:while-loop (:move :reverse   ,@temp0-offsets)
                                     (:move-and-return(,@x-offsets)    (:inc))
                                     (:move-and-begin (,@temp0-offsets)(:dec))))
      (:move-and-return (,@temp1-offsets)
                        (:while-loop (:move :reverse   ,@temp1-offsets)
                                     ,@code
                                     (:move-and-begin (,@temp1-offsets)(:zero)))))))
#+nil
(defun cl-brainfuck.macros::if (&rest args)
  (destructuring-bind
        ((&rest x-offsets) (&rest temp0-offsets) (&rest temp1-offsets) &rest code)
      args
    `((:assign-zero (,@temp0-offsets))
      (:assign-zero (,@temp1-offsets))
      (:move-and-return (,@x-offsets) (:inc 5)
                        (:while-loop (:move :reverse   ,@x-offsets)
                                     (:move-and-return(,@temp0-offsets)(:inc))
                                     (:move-and-return(,@temp1-offsets)(:inc))
                                     (:move-and-begin (,@x-offsets)    (:dec)))
                        (:dec 5))
      (:move-and-return (,@temp0-offsets)
                        (:while-loop (:move :reverse   ,@temp0-offsets)
                                     (:move-and-return(,@x-offsets)    (:inc))
                                     (:move-and-begin (,@temp0-offsets)(:dec))))
      (:move-and-return (,@temp1-offsets) (:dec 5)
                        (:while-loop (:move :reverse   ,@temp1-offsets)
                                     ,@code
                                     (:move-and-begin (,@temp1-offsets)(:zero)))))))


(defun cl-brainfuck.macros::if-zero (&rest args)
  (destructuring-bind
        ((&rest x-offsets) (&rest temp0-offsets) (&rest temp1-offsets) &rest code)
      args
    `((:assign-zero (,@temp0-offsets))
      (:assign-zero (,@temp1-offsets))
      (:move-and-return (,@x-offsets)
                        (:while-loop (:move :reverse   ,@x-offsets)
                                     (:move-and-return(,@temp0-offsets)(:inc))
                                     (:move-and-return(,@temp1-offsets)(:inc))
                                     (:move-and-begin (,@x-offsets)    (:dec))))
      (:move-and-return (,@temp0-offsets)
                        (:while-loop (:move :reverse   ,@temp0-offsets)
                                     (:move-and-return(,@x-offsets)    (:inc))
                                     (:move-and-begin (,@temp0-offsets)(:dec)))
                        (:inc))
      (:move-and-return (,@temp1-offsets)
                        (:while-loop (:move :reverse   ,@temp1-offsets)
                                     (:move-and-return(,@temp0-offsets)(:dec))
                                     (:move-and-begin (,@temp1-offsets)(:zero))))
      (:move-and-return (,@temp0-offsets)
                        (:while-loop (:move :reverse   ,@temp0-offsets)
                                     ,@code
                                     (:move-and-begin (,@temp0-offsets)(:dec)))))))

