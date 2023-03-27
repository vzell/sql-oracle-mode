;;; sql-oracle-imenu.el --- imenu support for sql-oracle and SQL*Plus buffers

;; Copyright (C) 2006, 2017 Dr. Volker Zell <vzell@volkerzell.de>

;; Author: Dr. Volker Zell
;; Maintainer: Dr. Volker Zell
;; Created: Wed 11 Jul 10:20:46 2006
;; Version: 2.0
;; Keywords: SQL, PL/SQL, SQL*Plus, Oracle, imenu

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

(defvar sql-oracle-imenu-generic-expression
      '(
	("Packages" "create\\s-+\\(or\\s-+replace\\s-+\\)?package\\s-+body\\s-+\\(\\(\\w+\\.\\)?\\w+\\)" 2)
	("Procedures" "\\(create\\s-+\\(or\\s-+replace\\s-+\\)?\\)?procedure\\s-+\\(\\(\\w+\\.\\)?\\w+\\)" 3)
	("Functions" "\\(create\\s-+\\(or\\s-+replace\\s-+\\)?\\)?function\\s-+\\(\\(\\w+\\.\\)?\\w+\\)" 3)
	("Views" "create\\s-+\\(or\\s-+replace\\s-+\\)?view\\s-+\\(\\(\\w+\\.\\)?\\w+\\)" 2)
	("Cursors" "cursor\\s-+\\(\\w+\\)" 1)
	("PL/SQL Tables" "type\\s-+\\(\\w+\\)\\s-+is\\s-+table" 1)
	("PL/SQL Records" "type\\s-+\\(\\w+\\)\\s-+is\\s-+record" 1)
	("Tables" "create\\s-+table\\s-+\\(\\(\\w+\\.\\)?\\w+\\)" 1)
	("Indexes" "create\\s-+\\(\\(unique\\|bitmap\\)\\s-+\\)?index\\s-+\\(\\(\\w+\\.\\)?\\w+\\)" 3)
	("Synonyms" "create\\s-+synonym\\s-+\\(\\(\\w+\\.\\)?\\w+\\)" 1)
	("Database Links" "create\\s-+database\\s-+link\\s-+\\(\\(\\w+\\.\\)?\\w+\\)" 1)
	("Tablespaces" "create\\s-+tablespace\\s-+\\(\\w+\\)" 1)
	("User" "create\\s-+user\\s-+\\(\\w+\\)" 1)
	("Profiles" "create\\s-+profile\\s-+\\(\\w+\\)" 1)
	("Constraints" "constraint\\s-+\\(\\w+\\)" 1)
	)

  "Imenu generic expression for SQL mode.  See `imenu-generic-expression'.")

;;;
;;; Regular expression to find SQL-Imenu-Index entries
;;;
;(defvar imenu-example--function-name-regexp-sql
;  (concat
;   "\\(<<[a-z]+[a-z0-9$#_]*>>\\|dbms_[a-z0-9$#_]+\\.\\|cursor\\|type\\|record\\|create[ \t\n]*\\(cluster\\|\\(public[ \t\n]*\\)?\\(database\\|rollback[ \t\n]*segment\\|synonym\\)\\([ \t\n]*link\\)?\\|\\(or[ \t\n]*replace[ \t\n]*\\)?\\(function\\|package\\([ \t\n]*body\\)?\\|procedure\\|trigger\\|\\(\\(force\\|noforce\\)[ \t\n]*\\)?view\\)\\|index\\|profile\\|role\\|schema[ \t\n]*authorization\\|sequence\\|snapshot\\([ \t\n]*log[ \t\n]*on\\)?\\|table\\(space\\)?\\|user\\)\\)"
;   ))

;(defun imenu-example--create-sql-index ()
;  (let ((index-alist '())
;	(index-label-alist '())
;	(index-subroutine-alist '())
;	(index-function-alist '())
;	(index-package-alist '())
;	(index-dbmspackage-alist '())
;	(index-table-alist '())
;	(index-tablespace-alist '())
;	(index-trigger-alist '())
;	(index-cursor-alist '())
;	(index-record-alist '())
;	(index-view-alist '())
;	(index-procedure-alist '())
;	(index-sequence-alist '())
;	(index-synonym-alist '())
;	(index-user-alist '())
;	(index-index-alist '())
;	prev-pos char)
;    (goto-char (point-min))
;    (imenu-progress-message prev-pos 0)
;    ;; Search for the function
;    (save-match-data
;      (while (re-search-forward
;	      imenu-example--function-name-regexp-sql
;	      nil t)
;	(imenu-progress-message prev-pos)
;	(save-excursion
;	  (and (forward-word -1)
;	       (save-excursion
;		 (cond
;		  ((looking-at "[a-z]+[a-z0-9$#_]*>>")
;		   (forward-word 1)
;		   (push (imenu-example--name-and-position)
;			 index-label-alist))
;		  ((looking-at "function")
;		   (forward-word 2)
;		   (push (imenu-example--name-and-position)
;			 index-function-alist))
;		  ((looking-at "package")
;		   (forward-word 2)
;		   (push (imenu-example--name-and-position)
;			 index-package-alist))
;		  ((looking-at "dbms_[a-z0-9$#_]+\\.")
;		   (forward-word 1)
;		   (push (imenu-example--name-and-position)
;			 index-dbmspackage-alist))
;		  ((looking-at "tablespace")
;		   (forward-word 2)
;		   (push (imenu-example--name-and-position)
;			 index-tablespace-alist))
;		  ((looking-at "table")
;		   (forward-word 2)
;		   (push (imenu-example--name-and-position)
;			 index-table-alist))
;		  ((looking-at "record")
;		   (forward-word 2)
;		   (push (imenu-example--name-and-position)
;			 index-record-alist))
;		  ((looking-at "view")
;		   (forward-word 2)
;		   (push (imenu-example--name-and-position)
;			 index-view-alist))
;		  ((looking-at "trigger")
;		   (forward-word 2)
;		   (push (imenu-example--name-and-position)
;			 index-trigger-alist))
;		  ((looking-at "cursor")
;		   (forward-word 2)
;		   (push (imenu-example--name-and-position)
;			 index-cursor-alist))
;		  ((looking-at "procedure")
;		   (forward-word 2)
;		   (push (imenu-example--name-and-position)
;			 index-procedure-alist))
;		  ((looking-at "sequence")
;		   (forward-word 2)
;		   (push (imenu-example--name-and-position)
;			 index-sequence-alist))
;		  ((looking-at "synonym")
;		   (forward-word 2)
;		   (push (imenu-example--name-and-position)
;			 index-synonym-alist))
;		  ((looking-at "user")
;		   (forward-word 2)
;		   (push (imenu-example--name-and-position)
;			 index-user-alist))
;		  ((looking-at "index")
;		   (forward-word 2)
;		   (push (imenu-example--name-and-position)
;			 index-index-alist))
;		  ((looking-at "program")
;		   (forward-word 2)
;		   (push (imenu-example--name-and-position)
;			 index-alist))
;		  ))))))
;    (imenu-progress-message prev-pos 100)
;    (and index-label-alist
;	 (push (cons (imenu-create-submenu-name "Labels") index-label-alist)
;	       index-alist))
;    (and index-function-alist
;	 (push (cons (imenu-create-submenu-name "Functions") index-function-alist)
;	       index-alist))
;    (and index-package-alist
;	 (push (cons (imenu-create-submenu-name "Packages") index-package-alist)
;	       index-alist))
;    (and index-dbmspackage-alist
;	 (push (cons (imenu-create-submenu-name "DBMS Packages") index-dbmspackage-alist)
;	       index-alist))
;    (and index-table-alist
;	 (push (cons (imenu-create-submenu-name "Tables") index-table-alist)
;	       index-alist))
;    (and index-tablespace-alist
;	 (push (cons (imenu-create-submenu-name "Tablespaces") index-tablespace-alist)
;	       index-alist))
;    (and index-record-alist
;	 (push (cons (imenu-create-submenu-name "Records") index-record-alist)
;	       index-alist))
;    (and index-trigger-alist
;	 (push (cons (imenu-create-submenu-name "Triggers") index-trigger-alist)
;	       index-alist))
;    (and index-cursor-alist
;	 (push (cons (imenu-create-submenu-name "Cursor") index-cursor-alist)
;	       index-alist))
;    (and index-view-alist
;	 (push (cons (imenu-create-submenu-name "Views") index-view-alist)
;	       index-alist))
;    (and index-sequence-alist
;	 (push (cons (imenu-create-submenu-name "Sequences") index-sequence-alist)
;	       index-alist))
;    (and index-synonym-alist
;	 (push (cons (imenu-create-submenu-name "Synonyms") index-synonym-alist)
;	       index-alist))
;    (and index-user-alist
;	 (push (cons (imenu-create-submenu-name "Users") index-user-alist)
;	       index-alist))
;    (and index-index-alist
;	 (push (cons (imenu-create-submenu-name "Index") index-index-alist)
;	       index-alist))
;    (and index-procedure-alist
;	 (push (cons (imenu-create-submenu-name "Procedures") index-procedure-alist)
;	       index-alist))
;    (nreverse index-alist)))

(provide 'sql-oracle-imenu)

;; End of sql-oracle-imenu.el
