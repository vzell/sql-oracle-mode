;;; sql-oracle-src.el --- source code template support for sql-oracle and SQL*Plus buffers

;; Copyright (C) 2006, 2007 Dr. Volker Zell <dr.volker.zell@oracle.com>

;; Author: Dr. Volker Zell <dr.volker.zell@oracle.com>
;; Maintainer: Dr. Volker Zell <dr.volker.zell@oracle.com>
;; Created: Wed 11 Jul 10:20:46 2006
;; Version: 1.0
;; Keywords: SQL, PL/SQL, SQL*Plus, Oracle, template

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

;; See the documentation in sql-oracle-mode.el

;;; ChangeLog:

;; 

;;; Code:

(defun plsql-oracle-declare-block ()
  "Insert complete anonymous PL/SQL block."
  (interactive)
  (progn
    (insert "declare\n\n\n\nbegin\n\n\n\nend;\n\n")
    (previous-line 4)
    (indent-relative)
    (hilit-recenter 9)
    ))

(defun plsql-oracle-declare-block ()
  "Insert complete anonymous PL/SQL block."
  (interactive)
  (let ((block-name (read-input "PL/SQL-Block name: "))
	)

  (progn
    (insert "<<" block-name ">>" "\ndeclare\n\n\n\nbegin\n\n\n\nend " block-name ";\n\n")
    (previous-line 4)
    (indent-relative)
    (hilit-recenter 9)
    )))

(defun plsql-oracle-declare-block ()
  "Insert complete anonymous PL/SQL block."
  (interactive)
  (let ((plsql-oracle-block-indent "  ")
	(block-name (read-input "PL/SQL-Block name: "))
	)

  (progn
    (insert "<<" block-name ">>")
    (newline-and-indent)
    (insert "declare")
    (newline-and-indent)
    (newline-and-indent)
    (newline-and-indent)
    (newline-and-indent)
    (insert "begin")
    (newline-and-indent)
    (newline-and-indent)
    (newline-and-indent)
    (newline-and-indent)
    (insert "end " block-name ";")
    (newline-and-indent)
    (previous-line 3)
    (indent-relative)
    (insert plsql-oracle-block-indent)
    (hilit-recenter 9)
    )))


;--------------------------------------

(defun plsql-oracle-declare-block ()
  "Insert anonymous PL/SQL block with optional DECLARE, EXCEPTION and Block Label.
Indent for the first statement."
  (interactive)
  (let ((plsql-oracle-block-indent "  ")
	(yn-declare (y-or-n-p "Declare"))
	(yn-exception (y-or-n-p "Exception"))
	(yn-block-label (y-or-n-p "PL/SQL Block Label"))
	)

    (progn
      (if yn-block-label
	  (progn
	    (insert "<<" 
		    (setq block-label-name 
			  (read-input "PL/SQL-Block Label Name: ")) ">>")
	    (newline-and-indent)))
      (if yn-declare
	  (progn
	    (insert "declare")
	    (newline-and-indent)
	    (newline-and-indent)
	    (newline-and-indent)
	    (newline-and-indent)))
      (insert "begin")
      (newline-and-indent)
      (newline-and-indent)
      (newline-and-indent)
      (newline-and-indent)
      (if yn-exception
	  (progn
	    (insert "exception")
	    (newline-and-indent)
	    (newline-and-indent)
	    (newline-and-indent)
	    (newline-and-indent)))
      (if yn-block-label
	  (insert "end " block-label-name ";")
	(insert "end;"))
      (newline-and-indent)
      (if yn-exception
	  (previous-line 7)
	(previous-line 3))
      (indent-relative)
      (insert plsql-oracle-block-indent)
      (hilit-recenter 9)
      )))

;-------------------------------------------------------------------------------

(defvar plsql-oracle-indent 2 
  "*Value is the number of columns to indent in PL/SQL source.")
  
(defun plsql-oracle-tabsize (s)
  "Changes spacing used for indentation.
The prefix argument is used as the new spacing."
  (interactive "p")
  (setq plsql-oracle-indent s))

(defun plsql-oracle-newline ()
  "Start new line and indent to current tab stop."
  (interactive)
  (let ((plsql-oracle-cc (current-indentation)))
    (newline)
    (indent-to plsql-oracle-cc)))

(defun plsql-oracle-tab ()
  "Indent to next tab stop."
  (interactive)
  (indent-to (* (1+ (/ (current-indentation) plsql-oracle-indent)) plsql-oracle-indent)))

(defun plsql-oracle-untab ()
  "Delete backwards to previous tab stop."
  (interactive)
  (backward-delete-char-untabify plsql-oracle-indent nil))

(defun plsql-oracle-go-to-this-indent (step indent-level)
  "Move point repeatedly by STEP lines until the current line has
given INDENT-LEVEL or less, or the start or end of the buffer is reached.
Ignore blank lines, statement labels and block or loop names."
  (while (and
	  (zerop (forward-line step))
	  (or (looking-at "^[ \t]*$")
	      (looking-at "^[ \t]*--")
	      (looking-at "^[ \t]*<<[A-Za-z0-9_#$]+>>")
;	      (looking-at "^[A-Za-z0-9_#]+:")
	      (> (current-indentation) indent-level)))
    nil))

(defun plsql-oracle-backward-to-same-indent ()
  "Move point backwards to nearest line with same indentation or less.
If not found, point is left at the top of the buffer."
  (interactive)
  (plsql-oracle-go-to-this-indent -1 (current-indentation))
  (back-to-indentation))

(defun plsql-oracle-forward-to-same-indent ()
  "Move point forwards to nearest line with same indentation or less.
If not found, point is left at the start of the last line in the buffer."
  (interactive)
  (plsql-oracle-go-to-this-indent 1 (current-indentation))
  (back-to-indentation))



(defun plsql-oracle-declare-block ()
  "Insert a block with a declare part.
Indent for the first declaration."
  (interactive)
  (let ((plsql-oracle-block-name (read-string "<<block name>>: ")))
    (insert "declare")
    (cond
     ((not (string-equal plsql-oracle-block-name ""))
;      (beginning-of-line)
      (open-line 1)
      (insert "<<" plsql-oracle-block-name ">>")
      (next-line 1)
      (end-of-line)))
    (plsql-oracle-newline)
    (plsql-oracle-newline)
    (plsql-oracle-newline)
    (plsql-oracle-newline)
    (insert "begin")
    (plsql-oracle-newline)
    (plsql-oracle-newline)
    (plsql-oracle-newline)
    (plsql-oracle-newline)
    (if (string-equal plsql-oracle-block-name "")
	(insert "end;")
      (insert "end " plsql-oracle-block-name ";"))
    )
  (end-of-line -2)
  (plsql-oracle-tab))

(defun plsql-oracle-declare-block ()
  "Insert anonymous PL/SQL block with optional DECLARE, EXCEPTION and Block Label.
Indent for the first statement."
  (interactive)
  (let ((yn-declare (y-or-n-p "Declare"))
	(yn-exception (y-or-n-p "Exception"))
	(yn-block-label (y-or-n-p "PL/SQL Block Label"))
	)
    (progn
      (if yn-block-label
	  (progn
	    (insert "<<" 
		    (setq block-label-name 
			  (read-input "PL/SQL-Block Label Name: ")) ">>")
	    (plsql-oracle-newline)))
      (if yn-declare
	  (progn
	    (insert "declare")
	    (plsql-oracle-newline)
	    (plsql-oracle-newline)
	    (plsql-oracle-newline)
	    (plsql-oracle-newline)))
      (insert "begin")
      (plsql-oracle-newline)
      (plsql-oracle-newline)
      (plsql-oracle-newline)
      (plsql-oracle-newline)
      (if yn-exception
	  (progn
	    (insert "exception")
	    (plsql-oracle-newline)
	    (plsql-oracle-newline)
	    (plsql-oracle-newline)
	    (plsql-oracle-newline)))
      (if yn-block-label
	  (insert "end " block-label-name ";")
	(insert "end;"))
      (plsql-oracle-newline)
      (if yn-exception
	  (previous-line 7)
	(previous-line 3))
      (indent-to (+ (current-indentation) plsql-oracle-indent))
      (hilit-recenter 9)
      )))

(defun plsql-oracle-declare-block ()
  "Insert anonymous PL/SQL block with optional DECLARE, EXCEPTION and Block Label.
Indent for the first statement.
In the case of labeled blocks an additional begin/end pair is inserted."
  (interactive)
  (let ((yn-declare (y-or-n-p "Declare ?"))
	(yn-exception (y-or-n-p "Exception ?"))
	(plsql-oracle-block-label (read-string "<<PL/SQL block label>>: ")))
    (cond ((not (string-equal plsql-oracle-block-label ""))
	   (insert "begin")
	   (newline)
	   (indent-to (+ (current-indentation) plsql-oracle-indent))
	   (insert "<<" plsql-oracle-block-label ">>")
	   (newline)))
    (if yn-declare
	(progn
	  (insert "declare")
	  (plsql-oracle-newline)
	  (plsql-oracle-newline)
	  (plsql-oracle-newline)
	  (plsql-oracle-newline)))
    (insert "begin")
    (plsql-oracle-newline)
    (plsql-oracle-newline)
    (plsql-oracle-newline)
    (plsql-oracle-newline)
    (if yn-exception
	(progn
	    (insert "exception")
	    (plsql-oracle-newline)
	    (plsql-oracle-newline)
	    (plsql-oracle-newline)
	    (plsql-oracle-newline)))
    (if (string-equal plsql-oracle-block-label "")
	(insert "end;")
      (insert "end " plsql-oracle-block-label ";")
      (plsql-oracle-newline)
      (backward-char 2)
      (insert "end;")
      )
    (plsql-oracle-newline)
    (if yn-exception
	(previous-line 8)
      (previous-line 4))
    (indent-to (+ (current-indentation) plsql-oracle-indent))
    (hilit-recenter 9)
    ))

(provide 'sql-oracle-src)

;; End of sql-oracle-src.el
