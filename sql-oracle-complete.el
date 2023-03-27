;;; sql-oracle-complete.el --- completion for sql-oracle and SQL*Plus buffers

;; Copyright (C) 2006, 2017 Dr. Volker Zell <vzell@volkerzell.de>

;; Author: Dr. Volker Zell
;; Maintainer: Dr. Volker Zell
;; Created: Wed 11 Jul 10:20:46 2006
;; Version: 2.0
;; Keywords: SQL, PL/SQL, SQL*Plus, Oracle, completion

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

;; Inspired by the completion code from Peter D. Pezaris's sql-mode.

;;; ChangeLog:
;;   23-Apr-16 (vzell)  - Added emacs support (by fixing regexp for procedure member list
;;   03-Nov-17 (vzell)  - Added emacs support for dynamic context popup menu completion
;; 

;;; Code:

(defvar sql-oracle-sql-package-member-list nil
  "Procedure (p) and function (f) list per package with the following element structure:
\(package-name (member-name1 . \"p\") (membername2 . \"f\") ... (membernameN .\"p\"))")
(defvar sql-oracle-sql-member-parameter-list nil
  "Parameter list per member with the following structure:
  (member-name member-type (parameter-name data-type mode default) ... (parameter-name data-type mode default))")
(defvar sql-oracle-sql-initialization-parameter-list nil
  "Initialization parameter list.")
(defvar sql-oracle-sqlplus-datatypemappings '(("binary_integer" . "number"))
  "Alist of database datatypes and corresponding SQL*Plus datatypes.")
(defvar sql-oracle-characterdatatypes '("varchar2" "char" "clob" "date" "nvarchar2" "nchar" "nclob")
  "List of Oracle database datatypes which have character syntax when used as input.")

(defun sql-oracle-set-server ()
  "Change the server, user, and password in the current sql-oracle-mode buffer."
  (interactive)
  (let* ((new-server (completing-read "Server Name: " sql-oracle-sql-server-table))
	 (new-user (completing-read (format "User Name on Server %s: "
					    new-server)
				    sql-oracle-sql-user-table))
	 (new-password (let ((prompt (format "Password for %s on %s: " 
					     new-user new-server)))
			 (if sql-oracle-sql-secure-passwords
			     (sql-oracle-sql-read-password prompt)
			   (read-string prompt)))))
    (progn
      (setq sql-oracle-sql-server new-server)
      (setq sql-oracle-sql-user new-user)
      (setq sql-oracle-sql-password new-password)
      (sql-oracle-sql-clear-cached-data)
      (and sql-oracle-sql-always-load-cache (sql-oracle-sql-load-cache-data)))))

(defun sql-oracle-set-user ()
  "Change the user and password in the current sql-oracle-mode buffer."
  (interactive)
  (let* ((new-user (completing-read "User Name: " sql-oracle-sql-user-table))
	 (new-password (let ((prompt (format "Password for %s on %s: " 
					     new-user sql-oracle-sql-server)))
			 (if sql-oracle-sql-secure-passwords
			     (sql-oracle-sql-read-password prompt)
			   (read-string prompt)))))
    (setq sql-oracle-sql-user new-user)))

(defun sql-oracle-set-password ()
  "Change the password in the current sql-oracle-mode buffer."
  (interactive)
  (let ((new-password (let ((prompt (format "Password for %s on %s: " 
					    sql-oracle-sql-user sql-oracle-sql-server)))
			(if sql-oracle-sql-secure-passwords
			    (sql-oracle-sql-read-password prompt)
			  (read-string prompt)))))
    (setq sql-oracle-sql-password new-password)))

(defun sql-oracle-display-completion-context ()
  "Display the completion context at point."
  (interactive)
  (message (symbol-name (sql-oracle-get-completion-context))))

(defun sql-oracle-get-completion-context (&optional location)
  "Get the completion context at point, or at optional LOCATION."
  (save-excursion
    (and location
	 (goto-char location))
    (let ((previous (sql-oracle-sql-previous-word)))
      (and previous
	   (cond
	    ((or (string-equal (downcase previous) "select"))
	     'column)
	    ((or
              (string-equal (downcase previous) "into")
              (string-equal (downcase previous) "from")
              (string-equal (downcase previous) "update")
              (string-equal (downcase previous) "delete")
              )
	     'table)
	    ((or (string-equal (downcase previous) "where")
		 (string-equal (downcase previous) "set"))
	     'column)
	    ((and (string-equal (downcase previous) "by")
		  (string-equal (downcase (sql-oracle-sql-previous-word 2)) "order"))
	     'column)
	    ((and (string-equal (downcase previous) "by")
		  (string-equal (downcase (sql-oracle-sql-previous-word 2)) "group"))
	     'column)
	    ((and (string-equal (downcase previous) "table")
		  (or
                   (string-equal (downcase (sql-oracle-sql-previous-word 2)) "alter")
                   (string-equal (downcase (sql-oracle-sql-previous-word 2)) "drop")
                   (string-equal (downcase (sql-oracle-sql-previous-word 2)) "truncate")
                   ))
	     'table)
	    ((or
              (string-equal (downcase previous) "exec")
              (string-equal (downcase previous) "execute")
              )
	     (save-excursion
               (beginning-of-line)
               (cond
                ((looking-at "exec\\(ute\\)?\\s-+[a-zA-Z0-9#_$]+\\.")
                 'package)
                ((looking-at "exec\\(ute\\)?\\s-+[a-zA-Z0-9#_$]+\\.[a-zA-Z0-9#_$]+\\s-+(")
                 'parameter)
                (t
                 'stored-procedure))))
	    ((or
              (string-equal (downcase previous) "to"))
	     'user)
	    ((or (string-equal (downcase previous) "and")
		 (string-equal (downcase previous) "or"))
	     (save-excursion
	       (re-search-backward sql-oracle-sql-keyword-regexp nil t 2)
	       (forward-word 1)
	       (forward-char 1) ; v.z.: this was added without blank to (forward-char 1): lookup-region-full-screen
	       (sql-oracle-get-completion-context)))
	    ((string-equal (downcase previous) ",")
	     (save-excursion
	       (re-search-backward sql-oracle-sql-keyword-regexp nil t 1)
	       (forward-word 2)
	       (sql-oracle-get-completion-context)))
	    ((string-equal (downcase previous) ".")
             'package
             )
	    ((string-equal (downcase previous) "(")
             'parameter
             )
	    ((member (downcase previous) sql-oracle-sql-operators)
;	     (string-equal (downcase previous) "=")
	     'value)
	    ((null previous)
	     'keyword)
	    ((string-equal (downcase (sql-oracle-sql-previous-word 2)) "where")
	     'operator)
	    ((and (or (string-equal (downcase previous) "parameter")
                      (string-equal (downcase previous) "parameters"))
                  (or (string-equal (downcase (sql-oracle-sql-previous-word 2)) "sho")
                      (string-equal (downcase (sql-oracle-sql-previous-word 2)) "show")))
	     'initialization-parameter)
	    ((save-excursion
               (backward-char)
               (if (looking-at "@")
                   't)
               )
	     'script)

;            ((save-excursion
;               (re-search-backward "("
;                                   (save-excursion
;                                     (re-search-backward "^exec\\(ute\\)?\\|)" nil t)) t))
;             'parameter)
            
;            ((setq completion-context (completing-read "Completion context: " '(("package")("parameter")("column"))))
;             (cond
;              ((string-equal completion-context "package")
;               'package)
;              ((string-equal completion-context "parameter")
;               'parameter)
;              ((string-equal completion-context "column")
;               'column)
;              ))
            
	    (t
	     (if sql-oracle-sql-lucid
		 (or (buffer-syntactic-context) 'keyword)
	       'keyword))
            )))))

(defun sql-oracle-complete (completion-table)
  "Complete the word at point based on COMPLETION-TABLE."
  (let* ((end (point))
	 (begin (save-excursion (re-search-backward " \\|\\.\\|\n\\|(" nil t) ; v.z.: added (
				(+ 1 (point))))
;	 (partial (current-word))
	 (partial (if (or (eq (preceding-char) ?,)
			  (eq (preceding-char) ?=)
			  (eq (preceding-char) ?\()
                          )
                      ""
                    (buffer-substring begin end)))
         (complete (try-completion partial completion-table))
         (all-complete (all-completions partial completion-table)))
    (cond
     ((eq complete t)
;      (insert " ")
      (message "Sole completion."))
     ((eq complete nil)
      (message "No match.")
      (and sql-oracle-sql-noisy
           sql-oracle-sql-xemacs
           (assoc 'no-completion sound-alist)
           (play-sound 'no-completion)))
     (t
      (if (string-equal complete partial)
          (sql-oracle-sql-dynamic-list-completions all-complete)
;	(delete-region begin end)
        (cond
         ((eq (preceding-char) ?\()
          nil)
         ((eq (preceding-char) ?\")
          (delete-char -1))
         ((not (eq (preceding-char) ? ))
          (backward-kill-word 1)
          (if (eq (preceding-char) ?\")
              (delete-char -1)))
         (t
          nil))
        (insert complete)
        (if (equal 1 (length all-complete))
            (progn
;              (insert " ")
              (message "Sole completion."))
          (if (assoc complete completion-table)
              (message "Complete, but not unique.")
            (message "Partial Completion."))))))))

(defun sql-oracle-complete-word-maybe (arg)
  "Complete the word at point, or insert a tab.
If there is only whitespace between the beginning of the line and
point, insert a tab character (or whatever key this function is mapped
to), otherwise complete the word."
  (interactive "P")
  (sql-oracle-set-connect-string)
  (let ((complete-it nil)
	(first-point (point)))
    (save-excursion
      (back-to-indentation)
      (if (< (point) first-point)
	  (setq complete-it t)))
    (if complete-it
	(let ((context (sql-oracle-get-completion-context)))
	  (cond
	   ((eq context 'keyword)
	    (if (not sql-oracle-sql-keyword-list)
		(sql-oracle-sql-get-keywords))
	    (sql-oracle-complete sql-oracle-sql-keyword-list))
	   ((eq context 'table)
	    (if (not sql-oracle-sql-table-list)
		(sql-oracle-sql-get-tables))
	    (sql-oracle-complete sql-oracle-sql-table-list))
	   ((eq context 'column)
	    (let* ((alias (sql-oracle-sql-get-column-alias))
		   (table-name (sql-oracle-sql-get-table-name alias)))
	      (if (not (assoc table-name sql-oracle-sql-column-list))
		  (sql-oracle-sql-get-columns table-name))
	      (sql-oracle-complete (cdr (assoc table-name sql-oracle-sql-column-list)))))
	   ((eq context 'package)
	    (let* ((package-name (sql-oracle-sql-get-package-name)))
	      (if (not (assoc package-name sql-oracle-sql-package-member-list))
		  (sql-oracle-sql-get-package-members-list package-name))
	      (sql-oracle-complete (cdr (assoc package-name sql-oracle-sql-package-member-list)))))
           
	   ((eq context 'parameter)
	    (let* ((member-name (sql-oracle-sql-get-member-name))
                   (package-name (sql-oracle-sql-get-package-name)))
	      (if (not (assoc member-name sql-oracle-sql-member-parameter-list))
		  (sql-oracle-sql-get-package-members-list package-name))
              ;; FIXME: member-name could be overloaded
              ;;        Right now only the first occurence of member-name is taken into account
              (let* ((member-parameter-list (assoc member-name sql-oracle-sql-member-parameter-list))
                     (param-list (nthcdr 3 member-parameter-list))
                     (member-type (nth 1 member-parameter-list))
                     (return-type (nth 2 member-parameter-list))
                     (list param-list)
                     out-list
                     out-list-save
                     )

                ;; Create a list of OUT parameters with their datatypes
                (while list
                  (if (or (string-equal (nth 2 (car list)) "out")
                          (string-equal (nth 2 (car list)) "in/out"))
                      (setq out-list-save (cons (cons (nth 0 (car list)) (nth 1 (car list))) out-list-save)))
                  (setq list (cdr list)))
                (setq out-list out-list-save)
                
                ;; Format the exec[ute] package.member(...); statement to an equivalent
                ;; anonymous block depending on the member type (procedure or function)
                (beginning-of-line)
                (sql-oracle-kill-entire-line)
                (if (string-equal member-type "p")
                    ;; procedure
                    (progn
                      ;; OUT list
;                      (when out-list
;                        (insert "declare\n")
;                        )
                      (while out-list
;                        (insert (concat "  " (car (car out-list)) "  " (cdr (car out-list))) ";\n")
                        (insert (concat "variable "
                                        (car (car out-list)) ; variable name
                                        "  "
                                        (sql-oracle-convert-to-sqlplus-datatype (cdr (car out-list)) sql-oracle-sqlplus-datatypemappings) ; variable type
                                        "\n"))
                        (setq out-list (cdr out-list))
                        )
                      (insert "begin\n")
                      (insert (concat "  " package-name "." member-name "("))
                      )
                  ;; function
                  (progn
;                    (insert "declare\n")
                    ;; RETURN type
;                    (insert (concat "  ret " return-type ";\n"))
                    (insert (concat "variable rc "
                                    (sql-oracle-convert-to-sqlplus-datatype return-type sql-oracle-sqlplus-datatypemappings)
                                    ";\n"))
                    ;; OUT list
                    (while out-list
;                      (insert (concat "  " (car (car out-list)) "  " (cdr (car out-list)) ";\n"))
                        (insert (concat "variable "
                                        (car (car out-list)) ; variable name
                                        "  "
                                        (sql-oracle-convert-to-sqlplus-datatype (cdr (car out-list)) sql-oracle-sqlplus-datatypemappings) ; variable type
                                        "\n"))
                        (setq out-list (cdr out-list))
                        )
                    (insert "begin\n")
;                    (insert "  rc := \n")
                    (insert "  :rc := \n")
                    (insert (concat "  " package-name "." member-name "("))
                    )
                  )
                (sql-oracle-insert-package-member-paramlist param-list)
                (insert "  );\n")
                (insert "end;\n/\n")
                (if (string-equal member-type "f")
                    (insert (concat "print rc\n")))
                (setq out-list out-list-save)
                ;; insert PRINT list for OUT parameters
                (while out-list
                  (insert (concat "print " (car (car out-list))) "\n")
                  (setq out-list (cdr out-list))
                  )
                )))
           
	   ((eq context 'stored-procedure)
	    (if (not sql-oracle-sql-stored-procedure-list)
		(sql-oracle-sql-get-stored-procedures))
	    (sql-oracle-complete sql-oracle-sql-stored-procedure-list))
	   ((eq context 'user)
	    (if (not sql-oracle-sql-user-list)
		(sql-oracle-sql-get-users))
	    (sql-oracle-complete sql-oracle-sql-user-list))
	   ((eq context 'initialization-parameter)
	    (if (not sql-oracle-sql-initialization-parameter-list)
		(sql-oracle-sql-get-initialization-parameters))
	    (sql-oracle-complete sql-oracle-sql-initialization-parameter-list))
	   ((eq context 'operator)
	    (if (not sql-oracle-sql-operator-list)
		(sql-oracle-sql-get-operators))
	    (sql-oracle-complete sql-oracle-sql-operator-list))
	   ((eq context 'value)
	    (let ((table-name (sql-oracle-sql-get-table-name))
		  (column-name (sql-oracle-sql-get-column-name)))
	      (if (not (assoc (cons table-name column-name) sql-oracle-sql-value-list))
		  (sql-oracle-sql-get-values table-name column-name))
	      (sql-oracle-complete (cdr (assoc (cons table-name column-name)
					sql-oracle-sql-value-list)))))
	   ((eq context 'script)
	    (insert (completing-read "Filename: "
                                     (sql-oracle-create-alist-from-list
                                      (cdr (cdr (directory-files sql-oracle-script-directory)))))))
;	    (message "Unable to complete (value)."))
	   (t
	    (message "Unknown context."))))
                                        ; insert a tab
      (self-insert-command (prefix-numeric-value arg)))))


(defun sql-oracle-insert-package-member-paramlist (paramlist)
  ;; Inserts a formatted PARAMLIST in the buffer.
  (interactive)
  (let ((i (length paramlist))
        ;; FIXME: Clean up variables used
        (parlen 0)
        (typlen 0)
        (temp 0)
        (inp nil)
        (outp nil)
        (column nil)
        (orgpoint 0)
        (firstcol nil))

    ;;
    ;; loop until last parameter
    ;;
    (while (not (zerop i))
      (setq i (1- i))
      ;;
      ;; get max length of parameter-name
      ;;
      (setq parlen
            (if (<= parlen (setq temp
                                 (length (nth 0 (nth i paramlist)))))
                temp
              parlen))
      )

    (newline)

    (setq firstcol (current-column))
    (setq i (length paramlist))

    ;;
    ;; loop until last parameter
    ;;
    (while (not (zerop i))
      (setq i (1- i))
      (setq column firstcol)
      ;;
      ;; comment if parameter has a default value
      ;;
      (if (string-equal (nth 3 (nth i paramlist)) "default")
          (insert "--"))
      ;;
      ;; in case of IN or IN OUT parameter insert parameter-name, => and data-type
      ;; in case of OUT parameter insert parameter-name
      ;;
      ;; FIXME: Maybe use tab-stop-list parameter as a local mode parameter
      (insert "    ")
;      (tab-to-tab-stop)
      (insert (concat
               ;;
               ;; check if it is the first parameter
               ;;
               (if (= i (1- (length paramlist)))
                   ;; yes => do not insert ',' before parameter-name
                   "  "
                 ", ")
               (nth 0 (nth i paramlist))))  ; parameter-name
      (indent-to (+ tab-width column parlen 2))

      (if (string-equal (substring (nth 2 (nth i paramlist)) 0 2) "in")
          ;; IN or IN/OUT
          (let ((datatype (nth 1 (nth i paramlist))))
            (insert " =>")
            (if (member datatype sql-oracle-characterdatatypes)
                (insert (concat " '" datatype "'"))
              (insert (concat " " datatype)))
            )
        ;; OUT parameter
        ;; insert : for PL/SQL bind variable befor OUT parameter
        (progn
          (beginning-of-line)
          (re-search-forward "[a-zA-Z#$]")
          (backward-char)
          (insert ":")
          (end-of-line)
          )
;        (progn
;          (insert "    --  <--")
;          (insert (concat " " (nth 1 (nth i paramlist)))) ; data-type
;          )
        )
      (newline))
    ))

(defun sql-oracle-sql-get-package-name ()
  "Get the name of the package to do the completion."
  (save-excursion
    (re-search-backward "\\." nil t)
    (setq package-name (current-word))
    ))

(defun sql-oracle-sql-get-member-name ()
  "Get the name of the procedure or function to do the completion."
  (save-excursion
    (re-search-backward "\\." nil t)
    (forward-char)
    (setq member-name (current-word))
    ))

(defun sql-oracle-sql-get-package-members-list (&optional package-name)
  "Return list of procedures and/or functions in package after point"
  (interactive)
  (cond
   ((string-match "GNU" (emacs-version))
    (setq member-regexp (concat "\\("
				;; parameter name
				sql-oracle-sql-word-regexp
				"\\)[ \t]+"
				;; datatype (for example: table of number(38))
				"\\([a-z0-9_]+\\)"
				;; in/out mode and optional default
				"[ \t]+\\(in/out\\|out\\|in\\)\\([ \t]+\\(default\\)\\)?")))
   ((string-match "XEmacs" (emacs-version))
    (setq member-regexp (concat "\\("
				;; parameter name
				sql-oracle-sql-word-regexp
				"\\)[ \t]+"
				;; datatype (for example: table of number(38))
				"\\([az-A-Z][-_.*$a-zA-Z0-9#() ]+[az-A-Z0-9)]\\)"
				;; in/out mode and optional default
				"[ \t]+\\(in/out\\|out\\|in\\)\\([ \t]+\\(default\\)\\)?")))
   )
  (setq sql-oracle-sql-modified-cache t)
  (message "Creating package member completion list... (querying database...)")
  (let* ((the-old-window (selected-window))
	 (the-old-buffer (current-buffer))
	 (the-old-point (point))
	 (temp-buffer (get-buffer-create " SQL-TEMP"))
	 (package-name (or package-name (sql-oracle-sql-get-package-name)))
	 (command (concat "describe " package-name))
	 (sql-oracle-sql-getting-completions t)
	 (sql-oracle-sql-completion-buffer temp-buffer)
	 (sql-oracle-sql-evaluation-method 'foreground))
    (setq sql-old-point (point))
    (set-buffer temp-buffer)
    (erase-buffer)
    (set-buffer the-old-buffer)
    (sql-oracle-sql-evaluate command sql-oracle-sql-server sql-oracle-sql-user sql-oracle-sql-password sql-oracle-sql-batch-command-switches
		  sql-oracle-sql-matching-buffer t)
    (message "Creating package member completion list... (parsing results...)")
    (set-buffer temp-buffer)
    (goto-char (point-min))
    (and (looking-at sql-oracle-sql-error-regexp)
         (forward-line)
         (let* ((begin (point-at-bol))
                (end (point-at-eol))
                (message (buffer-substring begin end)))
           (kill-buffer temp-buffer)
           (error (concat "Error parsing columns: " message))))
    (save-excursion
      (let ((begin (point))
            (end (goto-char (point-max))))
        (downcase-region begin end)
        ))
    (let ((new-package-member-list nil))
      (save-excursion
        (while (re-search-forward "^ Argument Name" nil t)
          (beginning-of-line)
          (kill-line 2)))
      (while (re-search-forward (concat "^\\(PROCEDURE\\|FUNCTION\\) \\(" sql-oracle-sql-word-regexp "\\)\\( RETURNS \\(.*\\)\\)?$") nil t)
        (let ((new-member-parameter-list nil)
              (member-type (match-string 1))
              (procedure-name (match-string 2))
              ;; FIXME: add record-member-list
              (return-type (match-string 4)))
;          (re-search-forward sql-oracle-sql-word-regexp nil t)
;          (setq procedure-name (match-string 0))
          (setq new-package-member-list (cons (cons procedure-name (substring member-type 0 1)) new-package-member-list))
          ;; FIXME: search for exactly one SPACE
          ;;        When there are more spaces, the datatype of the previous member is RECORD
          ;; FIXME: this while loop must be expressed with three loops with looking-at statements
          ;;        looping over procedure/function
          ;;        looping over members
          ;;        if member = record then looping over record members
          (while (re-search-forward "^ [^ ]" (save-excursion (re-search-forward "^\\(PROCEDURE\\|FUNCTION\\) " nil t)) t)
            (backward-char)
            (re-search-forward member-regexp nil t)
            (setq member-parameter-name (match-string 1))
            (setq type    (match-string 2))
            (setq inout   (match-string 3))
            (setq default (match-string 5))
            (setq new-member-parameter-list (cons (list member-parameter-name type inout default) new-member-parameter-list))
            )
          (setq sql-oracle-sql-member-parameter-list (cons (cons procedure-name
                                                      (cons (substring member-type 0 1)
                                                            (cons return-type
                                                                  new-member-parameter-list)))
                                                sql-oracle-sql-member-parameter-list))
          ))
      (kill-buffer temp-buffer)
      (set-buffer the-old-buffer)
      (setq sql-oracle-sql-package-member-list (cons (cons package-name new-package-member-list) sql-oracle-sql-package-member-list)))
    (select-window the-old-window)
    (goto-char the-old-point)
    (message "Creating package member completion list... done")))

(defun sql-oracle-sql-complete-initialization-parameter-list (string ignore action)
  (let ((old-buffer (current-buffer)))
    (set-buffer sql-completion-saved-buffer)
    (or sql-oracle-sql-initialization-parameter-list (sql-oracle-sql-get-initialization-parameters))
    (let ((initialization-parameters sql-oracle-sql-initialization-parameter-list))
      (set-buffer old-buffer)
      (cond ((null action)
	     (try-completion string initialization-parameters))
	    ((eq action 'lambda)
	     (member string (mapcar 'car initialization-parameters)))
	    (t
	     (all-completions string initialization-parameters))))))

(defun sql-oracle-sql-get-initialization-parameters ()
  "Set the variable `sql-oracle-sql-initialization-parameter-list' to the initialization parameters
in the current database."
  (interactive)
  (setq sql-oracle-sql-modified-cache t)
  (message "Creating initialization parameter completion list... (querying database...)")
  (let* ((the-old-window (selected-window))
	 (the-old-buffer (current-buffer))
	 (the-old-point (point))
	 (temp-buffer (get-buffer-create " SQL-TEMP"))
	 (command (concat (if sql-oracle-sql-database (concat "use " sql-oracle-sql-database "\ngo\n") "")
			  sql-oracle-sql-get-initialization-parameters-command))
	 (sql-oracle-sql-getting-completions t)
	 (sql-oracle-sql-completion-buffer temp-buffer)
	 (sql-oracle-sql-evaluation-method 'foreground))
    (setq sql-old-point (point))
    (set-buffer temp-buffer)
    (erase-buffer)
    (set-buffer the-old-buffer)
    (sql-oracle-sql-evaluate command sql-oracle-sql-server sql-oracle-sql-user sql-oracle-sql-password sql-oracle-sql-batch-command-switches
		  sql-oracle-sql-matching-buffer t)
    (message "Creating initialization parameter completion list... (parsing results...)")
    (set-buffer temp-buffer)
    (goto-char (point-min))
    (and (looking-at sql-oracle-sql-error-regexp)
	 (progn
	   (kill-buffer temp-buffer)
	   (error "Error parsing initialization parameters.")))
    (goto-char (point-max))
    (cond
     ((eq sql-oracle-sql-dataserver-type 'oracle)
      (goto-char (point-min))
      (kill-line 3)
      (save-excursion
        (let ((begin (point))
              (end (goto-char (point-max))))
          (downcase-region begin end)
          )))
     (t
      (forward-line -2)
      (kill-line 3)
      (goto-char (point-min))
      (kill-line 2)))
    (let ((new-initialization-parameter-list nil))
      (while (re-search-forward sql-oracle-sql-word-regexp nil t)
	(setq new-initialization-parameter-list
	      (cons (cons (match-string 0) "1") new-initialization-parameter-list)))
      (kill-buffer temp-buffer)
      (set-buffer the-old-buffer)
      (setq sql-oracle-sql-initialization-parameter-list new-initialization-parameter-list))
    (select-window the-old-window)
    (goto-char the-old-point)
    (message "Creating initialization parameter completion list... done")))

(defun sql-oracle-sql-popup-initialization-parameter-list ()
  "Pop up a menu displaying the initialization parameters to choose from."
  (interactive)
  (or sql-oracle-sql-initialization-parameter-list
      (sql-oracle-sql-get-initialization-parameters))
  (sql-oracle-sql-popup-continuation-menu sql-oracle-sql-initialization-parameter-list "Initialization parameters"))

(defun sql-oracle-dynamic-popup-menu (event)
  "Pop up a menu depending on the context of point.
Possibilities include associations, or completion lists of tables, column,
stored procedures or keywords."
  (interactive "@e")
  (if (featurep 'xemacs) ;; for XEmacs
      (let ((pos (event-point event))
	    (context))
	(save-excursion
	  (goto-char (or pos (point-max)))
	  (setq context (and pos (sql-oracle-get-completion-context)))
	  (cond
	   ((eq context 'table)
	    (sql-oracle-sql-popup-table-list))
	   ((eq context 'column)
	    (sql-oracle-sql-popup-column-list))
	   ((eq context 'stored-procedure)
	    (sql-oracle-sql-popup-stored-procedure-list))
	   ((eq context 'keyword)
	    (sql-oracle-sql-popup-keyword-list))
	   ((eq context 'value)
	    (sql-oracle-sql-popup-value-list))
	   ((eq context 'operator)
	    (sql-oracle-sql-popup-operator-list))
	   ((eq context 'user)
	    (sql-oracle-sql-popup-user-list))
	   ((eq context 'initialization-parameter)
	    (sql-oracle-sql-popup-initialization-parameter-list))
	   (t
	    ;; FIXME: change to call sql-oracle-popup-menu
	    ;;        remove the code from sql-mode
	    (sql-oracle-sql-popup-association-menu))))
	(and pos (goto-char pos)))
    (let ((pos (posn-point (event-start event)))
	  (context))
      (save-excursion
	(goto-char (or pos (point-max)))
	(setq context (and pos (sql-oracle-get-completion-context)))
	(cond
	 ((eq context 'table)
	  (sql-oracle-sql-popup-table-list))
	 ((eq context 'column)
	  (sql-oracle-sql-popup-column-list))
	 ((eq context 'stored-procedure)
	  (sql-oracle-sql-popup-stored-procedure-list))
	 ((eq context 'keyword)
	  (sql-oracle-sql-popup-keyword-list))
	 ((eq context 'value)
	  (sql-oracle-sql-popup-value-list))
	 ((eq context 'operator)
	  (sql-oracle-sql-popup-operator-list))
	 ((eq context 'user)
	  (sql-oracle-sql-popup-user-list))
	 ((eq context 'initialization-parameter)
	  (sql-oracle-sql-popup-initialization-parameter-list))
	 (t
	  ;; FIXME: change to call sql-oracle-popup-menu
	  ;;        remove the code from sql-mode
	  (sql-oracle-sql-popup-association-menu))))
      (and pos (goto-char pos)))
    ))

(defun sql-oracle-convert-to-sqlplus-datatype (db-datatype datatypealist)
  "Convert database datatype to corresponding SQL*Plus datatype."
  (let ((sqlplus-datatype (cdr (assoc db-datatype datatypealist))))
    (if sqlplus-datatype
        sqlplus-datatype
      db-datatype
      )
    )
  )

(defun sql-oracle-create-alist-from-list (the-list)
  (mapcar 'list the-list))

(provide 'sql-oracle-complete)

;; End of sql-oracle-complete.el
