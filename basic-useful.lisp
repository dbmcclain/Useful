
(in-package :useful-macros)

;; in ML these are referred to as sections
;; these actually correspond to the Dylan operators
;; secr ::= rcurry, secl ::= curry
(defun curry (fn &rest pref-args)
  (lambda (&rest suf-args)
    (apply fn (append pref-args suf-args))))

(defun rcurry (fn &rest suf-args)
  (lambda (&rest pref-args)
    (apply fn (nconc pref-args suf-args))))

(defun make-rubber-vector (&key (length 16) element-type)
  (make-array length
              :fill-pointer 0
              :adjustable t
              :element-type element-type))

  ;; --------------------------------------------

(defun raw-mkstr (&rest args)
  (with-output-to-string (s)
    (dolist (a args) 
      (princ a s))
    ))
  
(defun mkstr (&rest args)
  (with-standard-io-syntax
    (apply 'raw-mkstr args)))

(defun correct-for-symbol-character-case (str)
  ;; a portable way to make symbol strings
  ;; Modern Mode vs ANSI
  (if (eql #\a (char (string :a) 0))
      (string-downcase (string str))
    (string-upcase (string str))))

(defun intern-symbol (str &rest package)
  (apply 'intern (correct-for-symbol-character-case str) package))

(defun symb (&rest args)
  (values (intern-symbol (apply 'mkstr args))))
  

