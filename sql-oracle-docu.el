;;; sql-oracle-docu.el --- SQL documentation helper for sql-oracle and SQL*Plus buffers

;; Copyright (C) 2006, 2017 Dr. Volker Zell <vzell@volkerzell.de>
;; Copyright (C) 1994 Scott Maxwell

;; Author: Dr. Volker Zell
;; Maintainer: Dr. Volker Zell
;; Created: Wed 11 Jul 10:20:46 2006
;; Version: 2.0
;; Keywords: SQL, PL/SQL, SQL*Plus, Oracle, documentation

;; This file is not part of GNU Emacs yet.

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; Inspired by Scott Maxwell's rexx-mode.

;;; ChangeLog:

;; 

;;; Code:

(defvar sql-developing-mode-docs nil
  "This should only be true if you are working on doc/completion tables
and you need them to be rebuilt every time you re-evaluate sql-oracle-mode.")

(defvar sql-additional-doc-files nil
  "*This specifies any additional doc/completion files you would
like loaded.  If you wish to define your own list, you should define
a structure that looks like this in your .emacs file:
  (setq sql-additional-doc-files '(\"sql-ANSI\"
                                   \"sql-SYBASE\"))")

(defvar sql-doc-file "sql-oracle-doc"
  "*Specifies file to use for SQL documentation and completion.\n
This currently defaults to \"SQL-doc\".  The file \"SQL-doc.el\"
should have been included with this packet and contains
fairly complete documentation for SQL internal and external
commands as well as nicer capitalization for many function
names.  If you wish to use stripped down tables to conserve
memory, add
	(setq sql-doc-file \"sql-sml\")
to you .emacs file.  Alternatively, if you wish to replace
this file with your own, add
	(setq sql-doc-file \"my-file\")
or something along those lines to your .emacs.
You can disable this feature altogether with
	(setq sql-doc-file nil)")

(defvar sql-command-auto-upper nil
  "*If this is t, sql-oracle-mode will automatically convert all
SQL command and internal function names to upper case.
If it is 1, it will capitalize.  If 2 or higher, it will
capitalize functions and uppercase commands.")

(defvar sql-external-function-auto-capitilize nil
  "*If non-nil, sql-oracle-mode will automagically fix capitalization
for any external functions it knows about.")

(defvar sql-auto-build-procedure-table nil
  "*If non-nil, sql-oracle-mode will automatically build a local table
of procedures defined in the current buffer.  These are then
added to the completion table for this buffer.")

(defvar sql-super-completion-mode nil
  "*If non-nil, enables command specific completion functions.")

(defvar sql-command-table nil
  "Table of SQL commands for sql-command-auto-upper.")

(defvar sql-external-function-table nil
  "Table of SQL external functions for sql-external-function-auto-capitilize.")

(defconst sql-user-procedure-table nil
  "Table of SQL user procedures defined in the current file.  This is
created automatically for each buffer if sql-auto-build-procedure-table
is non-nil.")

(make-variable-buffer-local 'sql-user-procedure-table)

(if (or (not sql-command-table)
	sql-developing-mode-docs)
    (progn
      (if sql-developing-mode-docs
	  (progn
	    (setq sql-command-table nil)
	    (setq sql-external-function-table nil)
	    (setq sql-command-and-function-table nil)))
      (if sql-doc-file
	  (load sql-doc-file))
      (let ((scan sql-additional-doc-files))
	(while (car scan)
	  (load (car scan))
	  (setq scan (cdr scan))))))

;(load "SQL-doc")

(defconst sql-command-and-function-table nil
  "Combined table of SQL commands and external functions for help.")
(setq sql-command-and-function-table
      (append sql-command-table sql-external-function-table))

(defvar sql-build-eval nil
  "*If this is defined, it is evaluated (executed) just after the
sql-user-procedure-table is cleared and recreated by scanning
the buffer but before it is appended to the command and external
function lists.  I am using this instead of a hook so that this
can be buffer local.

You can use this to add names of your own from some other source
or to change the way the scan works altogether.")

(defun sql-function-at-point ()
  (if (not (or (= (char-syntax (following-char)) ?w)
	       (= (char-syntax (preceding-char)) ?w)))
      nil
    (save-excursion
      (let* ((beg (progn
		    (if (= (char-syntax (preceding-char)) ?w)
			(backward-sexp 1))
		    (while (= (char-syntax (following-char)) ?\')
		      (forward-char 1))
		    (point)))
	     (end (progn (forward-sexp 1) (point)))
	     (pattern (downcase (buffer-substring beg end)))
	     (precap (assoc pattern sql-user-procedure-table)))
	(if precap
	    (if (elt precap 1)
		(elt precap 1)
	      (car precap)))))))

(defun sql-oracle-function-help (function)
  "Display the full documentation of FUNCTION (a symbol)."
  (interactive
   (let ((fn (sql-function-at-point))
	 (enable-recursive-minibuffers t)	     
	 val)
     (setq val
	   (completing-read
	    (if fn
		(format "Describe function (default %s): " fn)
	      "Describe function: ")
	    sql-user-procedure-table nil t))
     (list (if (equal val "")
	       fn val))))
  (with-output-to-temp-buffer "*Help*"
    (princ function)
    (princ ": ")
    (let* ((var (assoc (downcase function) sql-user-procedure-table))
	   (doc (elt var 3))
	   (type (elt var 2)))
      (cond ((assoc (downcase function) sql-command-table)
	     (if type
		 (princ (format "an internal %s\n" type))
	       (princ "an internal command or function\n")))
	    (type
	     (if sql-developing-mode-docs
		 (progn
		   (setq var (assoc (downcase function) sql-external-function-table))
		   (setq doc (elt var 3))
		   (setq type (elt var 2))))
	     (cond ((not (= (char-syntax (string-to-char type)) ?w))
		    (princ (substring type 1))
		    (princ "\n"))
		   ((string-match " " type)
		    (princ type)
		    (princ "\n"))
		   (t
		    (princ (format "an external function from the %s package" type)))))
	    (t
	     (princ "an external command or function\n")))
      (princ "\n")
      (if doc
	  (princ doc)
	(princ "not documented"))
      (switch-to-buffer "*Help*")
      )))

(defun sql-complete-external (desc)
  "Reads the name of an external function name from the minibuffer
with completion."
  (let ((enable-recursive-minibuffers t)	     
	(val
	 (completing-read desc sql-external-function-table nil nil)))
    (sql-capitalize-string val)))

(defun sql-capitalize-string (str)
  "Capitalize string based on sql-external-function-auto-capitilize."
  (if sql-external-function-auto-capitilize
      (let ((ass (assoc (downcase str) sql-user-procedure-table)))
	(if (elt ass 1)
	    (elt ass 1)
	  (capitalize str)))))

(defun sql-clear-procedure-table ()
  "Clears the local procedure table."
  (interactive)
  (setq sql-user-procedure-table sql-command-and-function-table))

(defun sql-build-procedure-table ()
  "Builds the local procedure table."
  (interactive)
  (setq sql-user-procedure-table nil)
  (save-excursion
    (goto-char (point-min))
    (while (re-search-forward "^[a-z][a-z_0-9]*:" nil t)
      (sql-add-to-procedure-table
       (buffer-substring (match-beginning 0) (1- (match-end 0))))))
  (eval sql-build-eval)
  (setq sql-user-procedure-table (append sql-user-procedure-table sql-command-and-function-table)))

(defun sql-add-to-procedure-table (name)
  "Check the function table for the function name.  If it is not
there yet, add it."
  (if (assoc (downcase name) sql-user-procedure-table)
      ()
    (setq sql-user-procedure-table (cons (list (downcase name) name "User procedure") sql-user-procedure-table))))

;; v.z.: Sep 07, 1995
;; This part must be loaded in a hook-function
;(local-set-key "\C-h\C-f" 'sql-oracle-function-help)
;(if sql-auto-build-procedure-table
;    (sql-build-procedure-table)
;  (sql-clear-procedure-table))


(provide 'sql-oracle-docu)

;; End of sql-oracle-docu.el
