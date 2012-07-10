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

#+brainfuck-raw-io
(defcfun ("_getch" %getch) :int)
#+brainfuck-raw-io
(defcfun ("_putch" %putch) :void (ch :int))
#-brainfuck-raw-io
(defun %getch ()  (mod (char-code (read-char)) 256))
#-brainfuck-raw-io
(defun %putch (x) (write-char (code-char x)))

(defparameter *brainfuck-instruction-codes* '((:next  . #\>)
                                              (:prev  . #\<)
                                              (:inc   . #\+)
                                              (:dec   . #\-)
                                              (:putch . #\.)
                                              (:getch . #\,)
                                              (:while . #\[)
                                              (:loop  . #\])
                                              (:debug . #\$)))

(defun brainfuck-make-mem (size)
  (make-array size :element-type '(mod 256) :initial-element 0))

(defmacro byte-incf (form)
  `(setf ,form (mod (+ ,form 1) 256)))

(defmacro byte-decf (form)
  `(setf ,form (mod (+ ,form 256 -1) 256)))

(defun brainfuck-vm (code-list)
  (let ((mem (brainfuck-make-mem 2048))
        (dp  0)
        (ip  0)
        (jmp-pairs nil))
    (loop with jmp-stack = nil
       for i from 0
       for instr across code-list
       do (case instr
            (#\[ (push i jmp-stack))
            (#\] (push `(,(pop jmp-stack) . ,i) jmp-pairs)))
       finally (assert (null jmp-stack) () "Unmatched [ instructions."))
    (loop with code-length = (length code-list)
       while (< ip code-length)
       do (ecase (aref code-list ip)
            (#\> (incf dp))
            (#\< (decf dp))
            (#\+ (byte-incf (aref mem dp)))
            (#\- (byte-decf (aref mem dp)))
            (#\. (%putch (aref mem dp)))
            (#\, (setf (aref mem dp) (%getch)))
            (#\[ (when (zerop (aref mem dp))
                   (setf ip (cdr (assoc ip jmp-pairs)))))
            (#\] (when (not (zerop (aref mem dp)))
                   (setf ip (car (rassoc ip jmp-pairs)))))
            (#\$ (format t "~&Debug: ip=~a dp=~a~&Memory:~a...~%" ip dp (subseq mem 0 50)))
            )         
       do (assert ip () "assertion failed: ~a" jmp-pairs)
       do (incf ip))))

(defun brainfuck-assembler (instruction-list)
  (loop for instr in instruction-list
     collect (let ((ch (cdr (assoc instr *brainfuck-instruction-codes*))))
               (assert ch () "invalid instruction: ~a" instr) ch) into assemble-result
     finally (return (coerce assemble-result 'string))))

(defun brainfuck-disassembler (code-list)
  (loop for code across code-list
     collect (let ((instr (car (rassoc code *brainfuck-instruction-codes*))))
               (assert instr () "invalid code-char: ~a" code) instr) into disassemble-result
     finally (return disassemble-result)))

(defun brainfuck-macroexpand-1 (instruction-list)
  (let ((expanded nil))
    (values 
     (loop
        for instr in instruction-list
        if (consp instr) ;;macro
        append (apply (intern (symbol-name (car instr)) '#:cl-brainfuck.macros) (cdr instr))
        and do (setf expanded t)
        else
        collect instr)
     expanded)))

(defun brainfuck-macroexpand-all (instruction-list)
  (loop with new-code = instruction-list and expanded = t
     while expanded
     do (setf (values new-code expanded) (brainfuck-macroexpand-1 new-code))
     finally (return new-code)))

(defun brainfuck-optimizer (instruction-list)
  (let ((peep-hole-eliminate-list '((:prev :next)
                                    (:next :prev)
                                    (:inc :dec)
                                    (:dec :inc))))
    (loop for item in peep-hole-eliminate-list
       do (loop
               (let ((find-pos (search item instruction-list)))
                 (unless find-pos
                   (return))
                 (setf instruction-list
                       (append (subseq instruction-list 0 find-pos)
                               (subseq instruction-list (+ find-pos
                                                           (length item))))))))
    instruction-list))

(defun brainfuck-macro-assembler (instruction-list)
  (brainfuck-assembler
   (brainfuck-optimizer
    (brainfuck-macroexpand-all instruction-list))))


(defmacro brainfuck-assemble-inline (&rest args)
  `(brainfuck-macro-assembler ',args))

