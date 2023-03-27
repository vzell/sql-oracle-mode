;;; sql-oracle-plsql.el --- plsql helper routines for sql-oracle and SQL*Plus buffers

;; Copyright (C) 2006, 2007 Dr. Volker Zell <dr.volker.zell@oracle.com>

;; Author: Dr. Volker Zell <dr.volker.zell@oracle.com>
;; Maintainer: Dr. Volker Zell <dr.volker.zell@oracle.com>
;; Created: Wed 11 Jul 10:20:46 2006
;; Version: 1.0
;; Keywords: SQL, PL/SQL, SQL*Plus, Oracle

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

(defun plsql-oracle-count-blocks ()
  "Count PL/SQL blocks."
  (interactive)
  (setq begin-list '())
  (goto-char (point-min))
  (while (re-search-forward "begin" nil t)
    (push (match-beginning 0) begin-list))
  (message "No more 'begin' found")
  (setq begin-list (reverse begin-list))
  (princ (format "Number of begin's in file %s: %d" (buffer-name) (length begin-list)))
  (set-buffer (get-buffer-create "test_plsql"))
  (goto-char (point-max))
  (insert "Number of begin's in file " 
	  (buffer-name) ": " (number-to-string (length begin-list)) "\n")
  (while (car begin-list)
    (insert "Position: " (number-to-string (car begin-list)) "\n")
    (setq begin-list (cdr begin-list)))
  (switch-to-buffer "test_plsql")
  )


(defun plsql-oracle-find-begin-end ()
  "Find all starting positions and levels of PL/SQL blocks."
  (interactive)
  (setq begin-list '())
  (goto-char (point-min))
  (while (re-search-forward "begin\\|end[ \t\n]*;" nil t)
    (save-excursion
      (goto-char (match-beginning 0))
      (cond 
       ((looking-at "begin")
	(push (list "begin" (match-beginning 0) 1 1) begin-list))
       ((looking-at "end[ \t\n]*;")
	(push (list "end" (match-beginning 0) 1 1) begin-list))))
    )
  (message "No more 'begin' found")
  (setq begin-list (reverse begin-list))
  (princ (format "Number of begin's in file %s: %d" (buffer-name) (length begin-list)))
  (set-buffer (get-buffer-create "test_plsql"))
  (goto-char (point-max))
  (insert "Number of begin's in file " 
	  (buffer-name) ": " (number-to-string (length begin-list)) "\n")
  (switch-to-buffer "test_plsql")
  )



(defun plsql-oracle-find-begin-end ()
  "Find all starting positions and levels of PL/SQL blocks."
  (interactive)
  (setq found-last "begin")
  (setq begin-list '())
  (setq count-begin 0)
  (setq count-end 0)
  (setq count-begin-level 0)
  (setq count-end-level 0)
  (goto-char (point-min))
  (while (re-search-forward "begin\\|end[ \t\n]*;" nil t)
    (save-excursion
      (goto-char (match-beginning 0))
      (cond 
       ((looking-at "begin")
	(setq count-begin (1+ count-begin))
	(if (string= found-last "begin")
	    (setq count-begin-level (1+ count-begin-level)))
	(push (list "begin" (match-beginning 0) count-begin count-begin-level) begin-list)
	(setq found-last "begin")
	)
       ((looking-at "end[ \t\n]*;")
	(if (string= found-last "end")
	    (setq count-end (1- count-end)) 
	    (setq count-end-level (1- count-end-level)))
	(push (list "end" (match-beginning 0) count-end count-end-level) begin-list)
	(setq found-last "end")
	)
       )
      )
    )
  (message "No more 'begin' found")
  (setq begin-list (reverse begin-list))
  (princ (format "Number of begin's in file %s: %d" (buffer-name) (length begin-list)))
     (set-buffer (get-buffer-create "test_plsql"))
     (goto-char (point-max))
     (insert "Number of begin's in file " 
	     (buffer-name) ": " (number-to-string (length begin-list)) "\n")
     (switch-to-buffer "test_plsql")
     )

;---------------------------------------------------------------------------------

(defun plsql-oracle-forward-to-noncomment (&optional lim ignore-line-labels)
  (or lim (setq lim (point-max)))
  (let (opoint stop)
    (while (not stop)
      (setq opoint (point))
      (skip-chars-forward " \t\n\f;:" lim)
;      (if ignore-line-labels
;	  (while (looking-at "_[a-z_0-9]*:")
;	    (plsql-oracle-forward-sexp)))
      (while (and (<= (point) (+ 2 lim))
		  (looking-at "/\\*"))
	(search-forward "*/" lim 'move))
      (setq stop (or (>= (point) lim)
		     (= (point) opoint)))))
  (point))

(defun plsql-oracle-looking-at-ignore-whitespace (str &optional lim ignore-line-labels)
  (save-excursion
    (plsql-oracle-forward-to-noncomment lim ignore-line-labels)
    (looking-at str)))

(defmacro sign (count)
  (list 'max -1 (list 'min 1 count)))

(defun plsql-oracle-forward-sexp (&optional count noerr)
  "PLSQL mode replacement for forward-sexps so it will recognize 'BEGIN/END;' pairs."
  (interactive "p")
  (or count (setq count 1))
  (if (= count 0)
      (setq count 1))
  (let ((parse-sexp-ignore-comments t)	;always ignore comments
	(dir (sign count))		;dir should be either 1 or -1
	hold)				;this will track the current retval
    (while (/= count 0)			;we have to loop here, not in old func.
      (setq count (- count dir))
      (if (> dir 0)			;pick a direction and scan once
	  (setq hold (plsql-oracle-scan-forward-sexp (point) noerr))
	(setq hold (plsql-oracle-scan-backward-sexp (point) noerr)))
      (if (not hold)			;if we got nil, bail out
	  (setq count 0)))
    (if hold
	(goto-char hold))))

(defun plsql-oracle-backward-sexp (&optional arg noerr)
  "PLSQL mode replacement for forward-sexps so it will recognize 'BEGIN/END;' pairs."
  (interactive "p")
  (or arg (setq arg 1))
  (plsql-oracle-forward-sexp (- arg) noerr))


(defun plsql-oracle-scan-sexps (from count &optional noerr)
  (if noerr
      (condition-case nil
	  (or (scan-sexps from count)
	      (if (> count 0)
		  (save-excursion
		    (goto-char from)
		    (beginning-of-line 2)  ; move forward 2 - 1 lines first
		    (point))
		nil))
	(error
	 (save-excursion
	   (if (> count 0)
	       (re-search-forward "\\(\\s\"\\|\\s\(\\)")
	     (re-search-backward "\\(\\s\"\\|\\s\(\\)"))
	   (point))))
    (or (scan-sexps from count)
	(if (> count 0)
	    (save-excursion
	      (goto-char from)
	      (beginning-of-line 2)
	      (point))
	  nil))))

(defun plsql-oracle-scan-forward-sexp (from &optional noerr)
  ;;get simple value from old func.
  (save-excursion
    (goto-char from)
    (cond ((and (not noerr)
		(plsql-oracle-looking-at-ignore-whitespace "end[ \t\n]*;"))
	   (error "Block ends prematurely"))
	  ((not
	    (plsql-oracle-looking-at-ignore-whitespace "begin\\b"))
	   (plsql-oracle-scan-sexps from 1 noerr)) ;if this isn't 'begin', return scan-sexps
	  ;;if 'begin' skip to matching 'end'
	  (t
	   (let ((depth 1))
	     (while (and (> depth 0)
			 (not (eobp)))
	       (goto-char (plsql-oracle-scan-sexps (point) 1 t))
	       (cond ((plsql-oracle-looking-at-ignore-whitespace "begin\\b")
		      (setq depth (1+ depth)))
		     ((plsql-oracle-looking-at-ignore-whitespace "end[ \t\n]*;")
		      (setq depth (1- depth))))))
	   (if (eobp)
	       (if noerr
		   nil
		 (error "Containing message ends prematurely"))
	     (goto-char (scan-sexps (point) 1))
	     (point))))))

(defun plsql-oracle-scan-backward-sexp (from &optional noerr)
  (save-excursion
    (let (hold last)
      ;;get simple value from old func.
      (setq hold (plsql-oracle-scan-sexps from -1 noerr))
      (if (not hold)			;if old func returned nil, bail out
	  ()
	(goto-char hold)
	(cond
	 ;;are we trying to back out of a sexp illegally
	 ((and (not noerr)
	       (looking-at "begin\\b"))
	  (error "Block ends prematurely"))
	 ;;see if we just skipped over 'end'; if not, return hold
	 ((looking-at "end[ \t\n]*;")
	  ;;if so, skip to matching 'begin'
	  (let ((depth 1))
	    (while (> depth 0)
	      (goto-char (scan-sexps (point) -1))
	      (cond ((looking-at "begin\\b")
		     (setq depth (1- depth)))
		    ((looking-at "end[ \t\n]*;")
		     (setq depth (1+ depth))))))
	  (setq hold (point)))
	;;if we're not looking at anything special, just return hold
	 (t hold))))))


(provide 'sql-oracle-plsql)

;; End of sql-oracle-plsql.el
