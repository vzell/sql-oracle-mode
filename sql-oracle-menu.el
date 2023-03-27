;;; sql-oracle-menu.el --- menu support for sql-oracle and SQL*Plus buffers

;; Copyright (C) 2006, 2007, 2017 Dr. Volker Zell <vzell@volkerzell.de>

;; Author: Dr. Volker Zell
;; Maintainer: Dr. Volker Zell
;; Created: Wed 11 Jul 10:20:46 2006
;; Version: 2.0
;; Keywords: SQL, PL/SQL, SQL*Plus, Oracle, menu

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

;; 01.11.2017 vzell: Added region-exists-p to make menu working for emacs

;; 

;;; Code:

;;;
;;; SQL and SQL*Plus Mode Menus
;;;

(require 'easymenu)

(if (not (featurep 'xemacs)) ;; for Emacs compatibiliy
    (defun region-exists-p ()
      "Check if region exists and is NOT empty"
      (not (null mark-active))))

(defun sqlplus-xemacs ()
  (or (string-match "Lucid"  emacs-version)
      (string-match "XEmacs" emacs-version)))

(defun sqlplus-add-sqlplus-menu ()
  "Adds the menu 'SQL*Plus' to the menu-bar in SQL*Plus Mode."
  (easy-menu-define sqlplus-mode-menu sqlplus-mode-map "Menu keymap for SQL*Plus mode."
		    '("SQL*Plus"
		      ["Execute Command"                    sqlplus-execute-command              t]
		      ["Interrupt Subjob"                   sqlplus-interrupt-subjob             t]
		      "---"
		      ["Show Output"                        sql-oracle-show-sqlplus-output       t]
		      ["Goto Output"                        sql-oracle-goto-last-sqlplus-output  t]
		      ["Scroll Up"                          sqlplus-scroll-up                    t]
		      ["Scroll Down"                        sqlplus-scroll-down                  t]
		      ["Scroll Left"                        sqlplus-scroll-left                  t]
		      ["Scroll Right"                       sqlplus-scroll-right                 t]
		      ["Previous Command"                   sqlplus-previous-command             t]
		      ["Next Command"                       sqlplus-next-command                 t]
		      ["Back Command"                       sqlplus-back-command                 t]
		      ["Forward Command"                    sqlplus-forward-command              t]
		      ["End Of Buffer"                      sqlplus-end-of-buffer                t]
		      "---"
		      ["Indent Relative"                    indent-relative                      t]
		      ["Copy Word"                          sqlplus-copy-word                    t]
		      "---"
		      ["Kill Command/Output"                sqlplus-kill-command                 t]
		      ["Reset Buffer"                       sqlplus-reset-buffer                 t]
		      ["Drop Old Lines"                     sqlplus-drop-old-lines               t]
		      ["Save Session"                       sqlplus-save-session                 t]
		      "---"
		      ["Documentation On Word Under Cursor" sql-oracle-w3m-help   (current-word t)]
                      ["SQL Syntax Help"                    sql-oracle-function-help             t]
		      ["Display Message For ORA-Error"      sqlplus-display-oracle-error-message t]
		      ))
  (if (sqlplus-xemacs) 
      (progn
	(easy-menu-add sqlplus-mode-menu)
	(setq mode-popup-menu sqlplus-mode-menu)
	)))

(defun sql-oracle-add-sql-menu ()
  "Adds the menu 'SQL' to the menu-bar in SQL Oracle Mode."
  (easy-menu-define sql-oracle-mode-menu sql-oracle-mode-map "Menu keymap for SQL Oracle Mode."
		    '("SQL Mode"
		      ["Mark Backward Codeblock"                  sql-oracle-backward-codeblock
		       :help "Move to beginning of current codeblock and mark it"]
		      ["Mark Forward Codeblock"                   sql-oracle-forward-codeblock
		       :help "Move to end of current codeblock and mark it"]
		      ["Mark Current Codeblock"                   sql-oracle-mark-codeblock       t]
		      ["Indent Relative"                          indent-relative                 t]
		      "---"
		      ["Send Buffer To SQL*Plus"                  sql-oracle-send-buffer          t]
		      ["Send Region To SQL*Plus"                  sql-oracle-send-region
                       :active (region-exists-p)]
		      ["Send Region To Shell"                     sql-oracle-send-region-to-shell
                       :active (region-exists-p)]
		      "---"
                      ("SQL*Plus Buffer"
		       ["Scroll Up"                      	  sqlplus-scroll-up-other-window               t]
		       ["Scroll Down"                    	  sqlplus-scroll-down-other-window             t]
		       ["Scroll Left"                    	  sqlplus-scroll-left-other-window             t]
		       ["Scroll Right"                   	  sqlplus-scroll-right-other-window            t]
		       ["Recenter Window To Output"      	  sql-oracle-show-sqlplus-output-other-window  t]
		       ["Back Command"                   	  sqlplus-back-command-other-window            t]
		       ["Forward Command"                	  sqlplus-forward-command-other-window         t]
		       ["End Of Buffer"                  	  sqlplus-end-of-buffer-other-window           t]
		       ["Kill Command"                   	  sqlplus-kill-command-other-window            t]
		       ["Reset Buffer"                   	  sqlplus-reset-buffer-other-window            t]
		       )
                      ("Settings"
                       ["Set Server, User, and Password..."       sql-oracle-set-server		  t]
                       ["Set User And Password..."	          sql-oracle-set-user	          t]
                       ["Set Password..."			  sql-oracle-set-password	  t]
                       )
                      ("Actions"
                       ["Clear Cached Data"		          sql-oracle-sql-clear-cached-data	     t]
                       ("Clear Specific Cache"
                        ["Clear Table List"		          sql-oracle-sql-clear-cached-table-data     t]
                        ["Clear Column List"		          sql-oracle-sql-clear-cached-column-data    t]
                        ["Clear Value List"                       sql-oracle-sql-clear-cached-value-data     t]
                        ["Clear Stored Procedure List"
                         sql-oracle-sql-clear-cached-stored-procedure-data                                       t]
                        ["Clear Database List"      	          sql-oracle-sql-clear-cached-database-data  t]
                        ["Clear User List"      	          sql-oracle-sql-clear-cached-user-data      t]
                        )
                       ["Load Cached Data"		          sql-oracle-sql-load-cache-data	     t]
                       ["Save Cached Data"		          sql-oracle-sql-save-cache-data
                        sql-oracle-sql-modified-cache]
                       )
		      "----"
		      ["Create Column List For SELECT"            sql-oracle-describe-to-cols
                       (save-excursion
                         (if (= (point) (point-min))
                             nil
                           (progn
                             (skip-chars-backward " \t\r\n\f")
                             (backward-char)
                             (if (looking-at "/")
                                 t))))]
		      ["Insert ACCEPT-WHERE Clause"               sql-oracle-insert-accept-where
                       (save-excursion
                         (if (= (point) (point-min))
                             nil
                           (progn
                             (skip-chars-backward " \t\r\n\f")
                             (backward-char)
                             (if (looking-at "/")
                                 t))))]
		      "---"
		      ["Beautify SQL Statement In Region"         sql-oracle-beautify-sql
                       :active (region-exists-p)]
		      ["Reformat COL Statements"                  sql-oracle-reformat-cols
                       (save-excursion
                         (beginning-of-line)
                         (if (looking-at "col")
                             t))]
		      ["Reformat SELECT Statement"                sql-to-select
                       (save-excursion
                         (let ((pos (point)))
                           (beginning-of-line)
                           (while (and (not (looking-at "^/"))
                                       (< (point-min) pos))
                             (forward-line -1)
                             (setq pos (point))
                             )
                           (forward-line)
                           (setq pos (point))
                           (while (and (looking-at "^$\\|^\\s-+\\|col")
                                       (> (point-max) pos))
                             (forward-line))
                           (if (looking-at "select")
                               t)))]
		      ["SQL Rewrite"                              sql-rewrite
		       :help "Rewrite current SQL statement with proper indentation"]
                      ("SQL Transform"
                       ["SQL To SELECT"                           sql-to-select                   t]
                       ["SQL To INSERT"                           sql-to-insert                   t]
                       ["SQL To UPDATE"                           sql-to-update                   t]
                       ["SQL To DELETE"                           sql-to-delete                   t]
                       )
		      "---"
		      ["Documentation On Word Under Cursor"       sql-oracle-w3m-help
                       (current-word t)]
                      ["SQL Syntax Help"                          sql-oracle-function-help        t]
                      ["Documentation Access FILE"
                       (customize-set-variable 'sql-oracle-db-documentation-access "file")
                       :style radio
                       :selected (and (boundp 'sql-oracle-db-documentation-access)
                                      (string-equal sql-oracle-db-documentation-access "file"))
                       :active (boundp 'sql-oracle-db-documentation-access)]
                      ["Documentation Access HTTP"
                       (customize-set-variable 'sql-oracle-db-documentation-access "http")
                       :style radio
                       :selected (and (boundp 'sql-oracle-db-documentation-access)
                                      (string-equal sql-oracle-db-documentation-access "http"))
                       :active (boundp 'sql-oracle-db-documentation-access)]
                      ["Documentation Version 10.2"
                       (progn
                         (customize-set-variable 'sql-oracle-db-documentation-version "10.2")
                         (customize-set-variable 'sql-oracle-db-documentation-directory
                                                 "/opt/oracle/docs/db")
                         (if (string-equal sql-oracle-db-documentation-access "file")
                             (customize-set-variable 'sql-oracle-db-documentation-url
                                                     (concat "file://" sql-oracle-db-documentation-directory "/"))
                           (customize-set-variable 'sql-oracle-db-documentation-url
                                                   "http://www.oracle.com/pls/db102/")))
                       :style radio
                       :selected (and (boundp 'sql-oracle-db-documentation-version)
                                      (string-equal sql-oracle-db-documentation-version "10.2"))
                       :active (boundp 'sql-oracle-db-documentation-version)]
                      ["Documentation Version 11.1"
                       (progn
                         (customize-set-variable 'sql-oracle-db-documentation-version "11.1")
                         (customize-set-variable 'sql-oracle-db-documentation-directory
                                                 (concat "/opt/oracle/docs/db" sql-oracle-db-documentation-version))
                         (if (string-equal sql-oracle-db-documentation-access "file")
                             (customize-set-variable 'sql-oracle-db-documentation-url
                                                     (concat "file://" sql-oracle-db-documentation-directory "/"))
                           (customize-set-variable 'sql-oracle-db-documentation-url
                                                   "http://www.oracle.com/pls/db111/")))
                       :style radio
                       :selected (and (boundp 'sql-oracle-db-documentation-version)
                                      (string-equal sql-oracle-db-documentation-version "11.1"))
                       :active (boundp 'sql-oracle-db-documentation-version)]
                      ["Documentation Version 11.2"
                       (progn
                         (customize-set-variable 'sql-oracle-db-documentation-version "11.2")
                         (customize-set-variable 'sql-oracle-db-documentation-directory
                                                 (concat "/opt/oracle/docs/db" sql-oracle-db-documentation-version))
                         (if (string-equal sql-oracle-db-documentation-access "file")
                             (customize-set-variable 'sql-oracle-db-documentation-url
                                                     (concat "file://" sql-oracle-db-documentation-directory "/"))
                           (customize-set-variable 'sql-oracle-db-documentation-url
                                                   "http://www.oracle.com/pls/db112/")))
                       :style radio
                       :selected (and (boundp 'sql-oracle-db-documentation-version)
                                      (string-equal sql-oracle-db-documentation-version "11.2"))
                       :active (boundp 'sql-oracle-db-documentation-version)]
                      ["Documentation Version 12.1"
                       (progn
                         (customize-set-variable 'sql-oracle-db-documentation-version "12.1")
                         (customize-set-variable 'sql-oracle-db-documentation-directory
                                                 (concat "/opt/oracle/docs/db" sql-oracle-db-documentation-version))
                         (if (string-equal sql-oracle-db-documentation-access "file")
                             (customize-set-variable 'sql-oracle-db-documentation-url
                                                     (concat "file://" sql-oracle-db-documentation-directory "/"))
                           (customize-set-variable 'sql-oracle-db-documentation-url
                                                   "http://www.oracle.com/pls/db121/")))
                       :style radio
                       :selected (and (boundp 'sql-oracle-db-documentation-version)
                                      (string-equal sql-oracle-db-documentation-version "12.1"))
                       :active (boundp 'sql-oracle-db-documentation-version)]
                      ["Documentation Version 12.2"
                       (progn
                         (customize-set-variable 'sql-oracle-db-documentation-version "12.2")
                         (customize-set-variable 'sql-oracle-db-documentation-directory
                                                 (concat "/opt/oracle/docs/db" sql-oracle-db-documentation-version))
                         (if (string-equal sql-oracle-db-documentation-access "file")
                             (customize-set-variable 'sql-oracle-db-documentation-url
                                                     (concat "file://" sql-oracle-db-documentation-directory "/"))
                           (customize-set-variable 'sql-oracle-db-documentation-url
                                                   "http://www.oracle.com/pls/db122/")))
                       :style radio
                       :selected (and (boundp 'sql-oracle-db-documentation-version)
                                      (string-equal sql-oracle-db-documentation-version "12.2"))
                       :active (boundp 'sql-oracle-db-documentation-version)]
                      ["SQL Oracle Mode Manual"                   sql-oracle-goto-info-page       t]
                      ))
  (if (sqlplus-xemacs)
      (progn
	(easy-menu-add sql-oracle-mode-menu)
	(setq mode-popup-menu sql-oracle-mode-menu)
	))
  )

(provide 'sql-oracle-menu)

;; End of sql-oracle-menu.el
