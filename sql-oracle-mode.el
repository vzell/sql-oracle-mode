;; sql-oracle-mode.el --- Oracle SQL*Plus/SQL-Mode interface

;; Copyright (C) 2006, 2007, 2009, 2011, 2012, 2016, 2017  Dr. Volker Zell <vzell@volkerzell.de>
;; Copyright (C) 2005 Free Software Foundation, Inc., Jim Lange

;; Author: Jim Lange, Oracle Corporation 
;; Author: Dr. Volker Zell
;; Maintainer: Dr. Volker Zell
;;             since 15-Nov-2005
;; Created: 27-MAR-90
;; Version: 1.0
;; Keywords: SQL, PL/SQL, SQL*Plus, Oracle

;; This file is not part of GNU Emacs. It is derived from 18.55's shell.el.

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

;;; ChangeLog:

;;   19-APR-90 (jlange) - Trap EXIT/QUIT and terminate session. 
;;                      - Trap EDIT, but just print message. 
;;                      - Allow multiple sqlplus sessions by renaming current 
;;                        buffer to new name and executing sqlplus again. 
;;                      - Set left-margin to 5 in sqlplus-mode so that 
;;                        newline-and-indent (C-j) will indent. 
;;                      - Added (accept-process-output process) in  
;;                        sqlplus-kill-command to prevent occasional scrambled 
;;                        lines when command is executed. 
;;                      - Added sqlplus-reset-buffer. 
;;   25-APR-90 (jlange) - Treat GET like LIST and RUN--ignore in  
;;                        sqlplus-get-command. 
;;                      - Add sqlplus-drop-old-lines. 
;;   04-MAY-90 (jlange) - Add sqlplus-copy-word (C-c C-w). 
;;                      - Enhance sqlplus-kill-command to delete command or 
;;                        command output, depending on location of point. 
;;   11-MAY-90 (jlange) - In sql-oracle-send-region, detect imbedded SQL statement  
;;                        format (used in SQL*FORMS, SQL*REPORT, PRO*C, etc.)  
;;                        and convert to standard SQL before executing (remove 
;;                        INTO clause and convert :WORD to &WORD). 
;;                      - Automatically load and save session history based 
;;                        on the variable sqlplus-keep-history. 
;;   05-JUN-90 (jlange) - Delete ~/sqlplus.buf when exiting. 
;;                      - In sqlplus-send-region, when performing substitutions 
;;                        in statements, add "/" at end if not present. 
;;   12-JUN-90 (jlange) - In sqlplus-send-region, look for :text.text and convert 
;;                        to &text_text (used in SQL*Forms for block.field). 
;;   07-SEP-90 (jlange) - Removed process argument from accept-process-output in 
;;                        sql-send-line to prevent apparent lockup after issuing 
;;                        complex SQL statement. 
;;                      - Trap EDIT command and open new emacs buffer containing 
;;                        command text.  (new revision 1.2) 
;;   08-MAY-91 (jlange) - In sql-oracle-send-region, truncate &-variables to 30 characters 
;;                        before executing. 
;;   28-Sep-96 (vzell)  - a lof of changes before this entry (I can't remember)
;;   29-Sep-96 (vzell)  - Changed call to SQL*Plus from sqlplus to plus33
;;                        otherwise a new session is created (WindowsNT)
;;   27-Dec-96 (vzell)  - incorporated Server Manager stuff
;;   17-Apr-05 (vzell)  - Removed obsolete Server Manager stuff
;;   17-Nov-05 (vzell)  - Major rewrite, renamed sql-mode to sql-oracle-mode
;;   06-Jul-07 (vzell)  - Vertical select list display
;;   10-Jul-07 (vzell)  - Renamed files to use 'sql-oracle-' prefix
;;   28-Jul-07 (vzell)  - Changed authentication handling
;;                        Added customization
;;   31-Jul-07 (vzell)  - Removed process from (accept-process-output process) in
;;                        sqlplus-kill-command
;;                        On Windows sqlplus hangs
;;   03-Mar-09 (vzell)  - Added 10.2 and 11.1 documentation customization
;;                        Added automatic M-x sqlplus handling when doing C-c d
;;                        Added menu support for choosing documentation version
;;   26-May-11 (vzell)  - Added 11.2 documentation customization
;;                        Changed sql-oracle-send-region-to-shell
;;
;;   07-Feb-12 (vzell)  - Added F4 as shorcur for invoking sql-oracle-send-region
;;
;;   02-Oct-12 (vzell)  - Changed documentation directory computation
;;                        Changed documentation urls to match latest 11.2 release
;;   23-Apr-16 (vzell)  - Added 12.1 documentation customization
;;   04-Oct-16 (vzell)  - Simplified documentation directory variable
;;   30-Oct-17 (vzell)  - Changed recentering buffer so it also works for PL/SQL blocks
;;   31-Oct-17 (vzell)  - Added emacs compatibility for browsing PL/SQL errors in sqlplus buffer 
;;                      - Fixed mouse click to select errors in PL/SQL source code
;;                      - Handle recenter cases when sqlplus starts up initially
;;   08-Nov-17 (vzell)  - Fix browse-url under cygwin (make file URL drive letter agnostic)

;;; Documentation

;; Installation information and documentation is provided in the README file.

;;; Code:
 
;;; The custom variables

(defgroup sql-oracle nil
  "Major mode for editing SQL*Plus batch files."
  :version "3.14"
  :tag "SQL Oracle Mode"
  :group 'SQL
  :group 'processes)

(defgroup sqlplus nil
  "Major mode for interacting with Oracle SQL*Plus."
  :version "1.0"
  :tag "SQL*Plus Mode"
  :group 'processes)

(defcustom sqlplus-keep-history nil
  "If non-nil, save current session in file $HOME/.sqlhist when exiting."
  :type 'boolean
  :group 'sqlplus)

(defcustom sqlplus-lines-to-keep 1000
  "Number of lines to keep in a SQL*Plus buffer when
\\[sqlplus-drop-old-lines] is executed."
  :type 'number
  :group 'sqlplus)

(defcustom sqlplus-executable "sqlplus"
  "Executable called by SQL*Plus mode."
  :type 'string
  :group 'sqlplus)

(defcustom sqlplus-source-context-lines 3
  "Number of context lines to show around error line in PL/SQL source code."
  :type 'number
  :group 'sqlplus)

(defcustom sql-oracle-db-documentation-version "12.2"
  "Oracle database documentation version."
  :type '(choice
          (const :tag "10.2" "10.2")
          (const :tag "11.1" "11.1")
          (const :tag "11.2" "11.2")
          (const :tag "12.1" "12.1")
          (const :tag "12.2" "12.2")
          )
  :group 'sql-oracle)

(defcustom sql-oracle-db-documentation-access "file"
  "Oracle database documentation URL access method"
  :type '(choice (const :tag "file" "file")
                 (const :tag "http" "http"))
  :group 'sql-oracle)

(defcustom sql-oracle-db-documentation-base-directory "/opt/oracle/docs/db"
  "Oracle database base documentation directory."
  )

(defcustom sql-oracle-db-documentation-drive
  (if (eq system-type 'cygwin)
      (if (or (string-equal (system-name) "vzell-lap")
	      (string-equal (system-name) "NB-2483"))
	  "D:"
	"E:"))
  "Oracle database base documentation Windows drive letter.
This variable only makes sense on Windows operating systems."
  )

(defcustom sql-oracle-db-documentation-directory
  (concat sql-oracle-db-documentation-base-directory "/" sql-oracle-db-documentation-version)
  "Directory where the Oracle database documentation is unpacked. Right now only the
following versions are supported:

 o 10.2 - http://download.oracle.com/docs/cds/B19306_01.zip
 o 11.1 - http://download.oracle.com/docs/cds/B28359_01.zip
 o 11.2 - http://download.oracle.com/docs/cds/E11882_01.zip
 o 12.1 - http://download.oracle.com/docs/cds/E50529-01.zip
 o 12.2 - http://download.oracle.com/docs/cds/E66230_01.zip
"
  :type 'string
  :group 'sql-oracle)

(defcustom sql-oracle-db-http-documentation-directory
  (cond
   ((string-equal sql-oracle-db-documentation-version "10.2")
    "http://download.oracle.com/docs/cd/B19306_01"
    )
   ((string-equal sql-oracle-db-documentation-version "11.1")
    "http://download.oracle.com/docs/cd/B28359_01"
    )
   ((string-equal sql-oracle-db-documentation-version "11.2")
    "http://download.oracle.com/docs/cd/E11882_01"
    )
   ((string-equal sql-oracle-db-documentation-version "12.1")
    "http://download.oracle.com/docs/cd/E50529-01"
    )
   ((string-equal sql-oracle-db-documentation-version "12.2")
    "http://download.oracle.com/docs/cd/E66230_01"
    )
   )
  "URL where the Oracle database documentation is located. Right now only the
following versions are supported:

 o 10.2 - http://download.oracle.com/docs/cds/B19306_01.zip
 o 11.1 - http://download.oracle.com/docs/cds/B28359_01.zip
 o 11.2 - http://download.oracle.com/docs/cds/E11882_01.zip
 o 12.2 - http://download.oracle.com/docs/cds/E66230_01.zip
 o 12.1 - http://download.oracle.com/docs/cds/E66230_01.zip
"
  :type 'string
  :group 'sql-oracle)

(defcustom sql-oracle-db-documentation-url (concat "file://" sql-oracle-db-documentation-directory "/")
  "URL of Oracle database documentation. On Windows systems you can use the following
URL types: file:///D:/opt/oracle/docs/db/"
  :type 'string
  :group 'sql-oracle)

(defcustom sql-oracle-db-documentation-help
  (concat "Please install Oracle Database documentation from

 o 10.2 - http://download.oracle.com/docs/cds/B19306_01.zip
 o 11.1 - http://download.oracle.com/docs/cds/B28359_01.zip
 o 11.2 - http://download.oracle.com/docs/cds/E11882_01.zip
 o 12.1 - http://download.oracle.com/docs/cds/E50529-01.zip
 o 12.2 - http://download.oracle.com/docs/cds/E66230_01.zip

to " sql-oracle-db-documentation-base-directory "

See also the lisp variable 'sql-oracle-db-documentation-directory'")
  "Help string for display when documentation was not found"
  :type 'string
  :group 'sql-oracle)

(defcustom sql-oracle-script-directory "/opt/oracle/admin/sql/dd"
  "Directory which stores ready to run SQL scripts for completion with @"  
  :type 'string
  :group 'sql-oracle)

(defcustom sqlplus-output-message "Output from buffer"
  "Message which will be prepended to output in sqlplus buffers."
  :type 'string
  :group 'sqlplus)

(defcustom sqlplus-buffer-temp-filename ".sqlplus.buf"
  "Filename for storing SQL buffer before executing."
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defcustom sql-oracle-sql-secure-passwords t
  "*Make password entry invisible if non-nil."
  :type 'boolean
  :group 'sql-oracle)

(defcustom sql-oracle-sql-get-tables-command
  "set feedback off\nset pagesize 20000\nselect object_name from all_objects where object_type in ('TABLE', 'VIEW') union select name from v$fixed_table;\n"
  "Command that will return the list of tables in the current database.
sybase: \"select name from sysobjects where type in (\"U\", \"S\") order by name\\ngo\\n\"
oracle: \"set feedback off\\nset pagesize 20000\\nselect object_name from all_objects where object_type in ('TABLE', 'VIEW') union select name from v$fixed_table;\\n\""
  :type 'string
  :group 'sql-oracle)

(defcustom sql-oracle-sql-get-columns-command-prefix
  "set feedback off\nset pagesize 10000\nselect column_name, data_type from all_tab_columns where table_name = '"
  "Command that will return the list of columns in the current database.
sybase: \"select name, type from syscolumns where id = object_id(\"\"
oracle: \"set feedback off\\nset pagesize 10000\\nselect column_name, data_type from all_tab_columns where table_name = '\""
  :type 'string
  :group 'sql-oracle)

(defcustom sql-oracle-sql-get-columns-command-suffix
  "';\n"
  "Command that will return the list of tables in the current database.
sybase: \"\")\\ngo\\n\"
oracle: \"';\\n\""
  :type 'string
  :group 'sql-oracle)

(defcustom sql-oracle-sql-get-values-command-prefix
  "set feedback off\nset pagesize 20000\nselect distinct "
  "Command that will return the list of values in the current database.
The \"set rowcount\" string limits the number of values that are resturned
from the dataserver when doing value-based completion.  If you remove this
part of the string, there will be no limit to the number of rows returned.
sybase: \"set rowcount 1000\\nselect distinct \"
oracle: \"set feedback off\\nset pagesize 20000\\nselect distinct \""
  :type 'string
  :group 'sql-oracle)

(defcustom sql-oracle-sql-get-values-command-middle
  " from "
  "Command that will return the list of values in the current database.
sybase: \" from \"
oracle: \" from \""
  :type 'string
  :group 'sql-oracle)

(defcustom sql-oracle-sql-get-values-command-suffix
  ";\n"
  "Command that will return the list of values in the current database.
sybase: \"\\ngo\\n\"
oracle: \";\\n\""
  :type 'string
  :group 'sql-oracle)

;; FIXME: synonyms which point to packages are not used yet
;; FIXME: standalone procedures are not used yet
(defcustom sql-oracle-sql-get-stored-procedures-command
  "set feedback off\nset pagesize 20000\nselect object_name from all_objects where object_type = 'PACKAGE' order by object_name;\n"
  "Command that will return the list of stored procedures in the current database.
sybase: \"select name from sysobjects where type = \"P\" order by name\\ngo\\n\"
oracle: \"set feedback off\\nset pagesize 20000\\nselect object_name from all_objects where object_type = 'PACKAGE' order by object_name;\\n\""
  :type 'string
  :group 'sql-oracle)

(defcustom sql-oracle-sql-get-initialization-parameters-command
  "set feedback off\nset pagesize 20000\nselect name from v$parameter order by name;\n"
  "Command that will return the list of instance initialization parameters in the server.
sybase: NOT USED
oracle: \"set feedback off\\nset pagesize 20000\\nselect name from v$parameter order by name;\\n\""
  :type 'string
  :group 'sql-oracle)

(defcustom sql-oracle-sql-get-users-command
  "set feedback off\nset pagesize 20000\nselect username from all_users order by username;\n"
  "Command that will return the list of users in the server.
sybase: \"select name from sysusers\\ngo\\n\"
oracle: \"set feedback off\\nset pagesize 20000\\nselect username from all_users order by username;\\n\""
  :type 'string
  :group 'sql-oracle)

(defcustom sql-oracle-sql-batch-command-switches "-S"
  "*Switches to concatenate to the sql-oracle-sql-command in sql-batch-mode.
For Oracle this should be set to \"-S\""
  :type 'string
  :group 'sql-oracle)

(defcustom sql-oracle-sql-evaluation-method 'foreground
  "*The preferred way to call sql-evaluate-buffer.
Possible options are currently 'foreground, which will block until the
query returns, and 'background, which will evaluate the query in the
background.

The toolbar GO button pays attention to this variable."
  :type '(choice (const :tag "Foreground" foreground)
		 (const :tag "Background" background))
  :group 'sql-oracle)

(defcustom sql-oracle-sql-stay-logged-in nil
  "*Keep an open connection to the dataserver if non-nil."
  :type 'boolean
  :group 'sql-oracle)

(defcustom sql-oracle-sql-dataserver-type 'oracle
  "Type of database you are using.  Recognized options are 'sybase,
'oracle 'mSQL and 'postgress.  Setting this variable will change the default
value of a few variables, as well as the behavior of SQL Mode.
Oracle and mSQL support should be considered limited compared to Sybase
support.  In fact, support for vendors other than Sybase is just about
nil right now, although it is high on my priority list for SQL Mode.
If you would like to help with this support, *please* let me know.

Do not set the value of this variable directly.  Instead use the function
`sql-oracle-sql-set-dataserver-type'."
  :type '(choice (const :tag "MySQL" mSQL)
		 (const :tag "Oracle" oracle)
		 (const :tag "PostGres" postgress)
		 (const :tag "Sybase" sybase))
  :group 'sql-oracle)

(defcustom sql-oracle-sql-error-regexp "[a-zA-Z][a-zA-Z][a-zA-Z0-9]-[0-9]+\\|ERROR:"
  "*Regular expression to match error lines.
sybase: \"Msg .* Level .* State .*\\nServer .* Line \"
oracle: \"[a-zA-Z][a-zA-Z][a-zA-Z0-9]-[0-9]+\\|ERROR:\""
  :type 'string
  :group 'sql-oracle)

(defcustom sql-oracle-sql-word-regexp "[-_.*$a-zA-Z0-9#]+" 
  "Regexp matching a word in a buffer."
  :type 'string
  :group 'sql-oracle)

(defcustom sql-oracle-sql-noisy t
  "*Make sounds in certain situations if non-nil."
  :type 'boolean
  :group 'sql-oracle)

(defcustom sql-oracle-sql-table-prefix-regexp "\\<\\(from\\|delete\\|update\\|into\\|truncate\\|FROM\\|DELETE\\|UPDATE\\|INTO\\|TRUNCATE\\)[\t\n ]"
  "Regular expression matching SQL code that will preceed a table name."
  :type 'string
  :group 'sql-oracle)

(defcustom sql-oracle-sql-line-regexp "[ \t]*\\(.*[-_.*$a-zA-Z0-9#]\\)"
  "Regexp matching a line in a buffer."
  :type 'string
  :group 'sql-oracle)

(defcustom sql-oracle-sql-string-column-types
  '("39" "45" "47" "58" "61")
  "*A list of column types that require quotes."
  :type 'list
  :group 'sql-oracle)

(defcustom sql-oracle-sql-ignore-column-types
  '("34" "35" "37" "111")
  "*A list of column types that should be ignored when doing an update."
  :type 'list
  :group 'sql-oracle)

(defcustom sql-oracle-sql-always-load-cache t
  "Non-nil causes SQL Mode to automatically load the cache information if
it exists when you enter a sql-batch-mode buffer."
  :type 'boolean
  :group 'sql-oracle)

(defcustom sql-oracle-sql-cache-data-file-name
  (concat (getenv "HOME") "/.sql-oracle-cache-dir/sql-cache")
  "Base file name in which to load save cached information.
The server and database are appended to this base name to get the full name."
  :type 'string
  :group 'sql-oracle)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define variables

(defvar sql-oracle-sql-command
  (cond ((eq sql-oracle-sql-dataserver-type 'sybase)
	 "isql")
	((eq sql-oracle-sql-dataserver-type 'oracle)
	 "sqlplus")
	((eq sql-oracle-sql-dataserver-type 'mSQL)
	 "mSQL")
	((eq sql-oracle-sql-dataserver-type 'postgress)
	 "psql")
	(t
	 "isql"))
  "*Command to invoke sql.
This should be just the program name, NOT path/command.  The command
should be in your path.  If you need to add switches, see the variables
`sql-oracle-sql-batch-command-switches' and `sql-interactive-command-switches'.
The value of this variable will have a default based on the value of
the variable `sql-dataserver-type'.  If you are changing the value of this
variable you should first see the variable `sql-oracle-sql-dataserver-type' and
the function `sql-oracle-sql-set-dataserver-type'.")

(defvar sql-oracle-sql-user nil)

(defvar sql-oracle-sql-user-table nil)

(defvar sql-oracle-sql-password nil)

(defvar sql-oracle-sql-server nil)

(defvar sql-oracle-sql-server-table nil)

(defvar sql-oracle-sql-database nil)

(defvar sql-oracle-sql-database-list nil)

(defvar sql-oracle-sql-table-list nil)

(defvar sql-oracle-sql-column-list nil)

(defvar sql-oracle-sql-value-list nil)

(defvar sql-oracle-sql-user-list nil)

(defvar sql-oracle-sql-stored-procedure-list nil)

(defvar sql-oracle-sql-getting-completions nil)

(defvar sql-oracle-sql-matching-buffer nil)

(defvar sql-oracle-sql-modified-cache nil)

(defvar sql-oracle-sql-operators '("=" "like" "in" "<" ">" "!=" "<=" ">="))

(defvar sql-oracle-sql-operator-regexp "\\(=\\|like\\|in\\|<\\|>\\|!=\\|<=\\|>=\\)")

(defvar sql-oracle-sql-keywords '("to" "select" "from" "where" "tran" "transaction" "commit" "group" "exec" "execute" "rollback" "compute" "union" "by" "order" "having" "set" "update" "delete" "insert" "into" "values" "use" "null" "begin" "end" "else" "if" "goto" "break" "continue" "raiserror" "waitfor" "and" "or" "not" "in" "is" "declare" "print" "return" "exists" "like" "sum" "avg" "count" "max" "min" "all" "distinct" "alter" "table" "database" "create" "disk" "nonclustered" "reconfigure" "revoke" "override" "procedure" "proc" "checkpoint" "dump" "drop" "index" "fillfactor" "rule" "shutdown" "tape" "view" "truncate" "kill" "load" "clustered" "dbcc" "grant" "as" "with" "nowait" "no_log" "refit" "reinit" "init" "mirror" "unmirror" "remirror" "default" "statistics"))

(defvar sql-oracle-sql-keyword-regexp "\\(to\\|select\\|from\\|where\\|tran\\|transaction\\|commit\\|group\\|exec\\|execute\\|readtext\\|rollback\\|compute\\|union\\|by\\|order\\|having\\|set\\|update\\|delete\\|insert\\|into\\|writetext\\|values\\|go\\|use\\|begin\\|end\\|else\\|if\\|goto\\|break\\|continue\\|raiserror\\|waitfor\\|and\\|or\\|not\\|in\\|is\\|declare\\|print\\|return\\|exists\\|like\\|sum\\|avg\\|count\\|max\\|min\\|all\\|distinct\\|alter\\|table\\|database\\|create\\|disk\\|nonclustered\\|reconfigure\\|revoke\\|override\\|procedure\\|proc\\|checkpoint\\|dump\\|drop\\|index\\|fillfactor\\|rule\\|shutdown\\|tape\\|view\\|truncate\\|kill\\|load\\|clustered\\|dbcc\\|grant\\|as\\|with\\|nowait\\|no_log\\|refit\\|reinit\\|init\\|mirror\\|unmirror\\|remirror\\|default\\|sp_[a-zA-Z]*\\|statistics\\TO|\\|SELECT\\|FROM\\|WHERE\\|TRAN\\|TRANSACTION\\|COMMIT\\|GROUP\\|EXEC\\|EXECUTE\\|READTEXT\\|ROLLBACK\\|COMPUTE\\|UNION\\|BY\\|ORDER\\|HAVING\\|SET\\|UPDATE\\|DELETE\\|INSERT\\|INTO\\|WRITETEXT\\|VALUES\\|GO\\|USE\\|BEGIN\\|END\\|ELSE\\|IF\\|GOTO\\|BREAK\\|CONTINUE\\|RAISERROR\\|WAITFOR\\|AND\\|OR\\|NOT\\|IN\\|IS\\|DECLARE\\|PRINT\\|RETURN\\|EXISTS\\|LIKE\\|SUM\\|AVG\\|COUNT\\|MAX\\|MIN\\|ALL\\|DISTINCT\\|ALTER\\|TABLE\\|DATABASE\\|CREATE\\|DISK\\|NONCLUSTERED\\|RECONFIGURE\\|REVOKE\\|OVERRIDE\\|PROCEDURE\\|PROC\\|CHECKPOINT\\|DUMP\\|DROP\\|INDEX\\|FILLFACTOR\\|RULE\\|SHUTDOWN\\|TAPE\\|VIEW\\|TRUNCATE\\|KILL\\|LOAD\\|CLUSTERED\\|DBCC\\|GRANT\\|AS\\|WITH\\|NOWAIT\\|NO_LOG\\|REFIT\\|REINIT\\|INIT\\|MIRROR\\|UNMIRROR\\|REMIRROR\\|DEFAULT\\|SP_[a-zA-Z]*\\|STATISTICS\\)[\t\n ]")

(defvar sql-oracle-sql-lucid (string-match "Lucid" (emacs-version)))

(defvar sql-oracle-sql-xemacs (string-match "XEmacs" (emacs-version)))

(defvar sql-oracle-sql-xemacs-19-12 (and sql-oracle-sql-xemacs
			      (or (> emacs-major-version 19)
				  (and (eq emacs-major-version 19)
				       (> emacs-minor-version 11)))))

(defvar sql-oracle-sql-keyword-list nil)

(defvar sql-oracle-sql-query-in-progress nil)

(defvar sql-oracle-sql-last-table nil)

(defvar sql-oracle-sql-association-alist nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defvar sqlplus-startup-message
  (concat
   "##########################################################################################\n"
   "# Emacs SQL*Plus Interpreter:  by Jim Lange of Oracle Corporation                        #\n"
   "#                              modified heavily by Dr. Volker Zell of Oracle Corporation #\n"
   "# Revision: 3.0                                                                          #\n"
   "# Copyright (c) 2005-2017 Free Software Foundation, Inc., Jim Lange and Dr. Volker Zell  #\n"
   "##########################################################################################\n"
;   (format (concat sqlplus-output-message " '%s':\n") "SQL buffer")
   )
  "Message displayed when \\[sqlplus] is executed.")
(defvar last-output-start nil
  "In a sqlplus-mode buffer, marker for beginning of last batch of output.")
(defvar sqlplus-prompt nil
  "In a sqlplus-mode buffer, string containing prompt text.")
(defvar sqlplus-continue-pattern nil
  "In a sqlplus-mode buffer, regular expression for continuation line prompt.")
(defvar sqlplus-username-password nil
  "The username/password[@NetServiceName] to use when starting SQL*Plus.")
(defvar sqlplus-stack-pointer 0
  "Current command recalled from history of commands.")
(defvar sqlplus-mode-map nil)
(defvar sql-oracle-mode-map nil)
(defvar sqlplus-line-number-keymap nil
  "Keymap used while over highlighted error line numbers.")
(defvar sql-oracle-mode-syntax-table nil
  "Syntax table used while in SQL and SQL*Plus modes.")
(defvar sql-oracle-mode-temp-syntax-table nil
  "Syntax table used while in temporary SQL*Plus mode.")
(defvar sql-oracle-mode-abbrev-table nil
  "Abbrev table used in SQL and SQL*Plus modes.")
(defvar sql-oracle-syntax-errors-found nil
  "Will be set to found when syntax errors are found.")

;; Face Names
(defvar sql-oracle-fixme-face 'sql-oracle-fixme-face
  "Face name to use for output message.")

(defvar sqlplus-error-sourceline-face 'sqlplus-error-sourceline-face
  "Face name to use for output message.")
(defvar sqlplus-output-from-buffer-face	'sqlplus-output-from-buffer-face
  "Face name to use for output message.")
(defvar sqlplus-type-face 'sqlplus-type-face
  "Face name to use for output message.")
(defvar sqlplus-variable-name-face 'sqlplus-variable-name-face
  "Face name to use for output message.")
(defvar sqlplus-builtin-face 'sqlplus-builtin-face
  "Face name to use for output message.")
(defvar sqlplus-function-name-face 'sqlplus-function-name-face
  "Face name to use for output message.")
(defvar sqlplus-keyword-face 'sqlplus-keyword-face
  "Face name to use for keywords.")
(defvar sqlplus-reference-face 'sqlplus-reference-face
  "Face name to use for output message.")
(defvar sqlplus-warning-face 'sqlplus-warning-face
  "Face name to use for output message.")
(defvar sqlplus-preprocessor-face 'sqlplus-preprocessor-face
  "Face name to use for output message.")
(defvar sqlplus-string-face 'sqlplus-string-face
  "Face name to use for output message.")
(defvar sqlplus-comment-face 'sqlplus-comment-face
  "Face name to use for output message.")
(defvar sqlplus-constant-face 'sqlplus-constant-face
  "Face name to use for output message.")
(defvar sqlplus-semicolon-face 'sqlplus-semicolon-face
  "Face name to use for output message.")
(defvar sqlplus-send-to-interpreter-face 'sqlplus-send-to-interpreter-face
  "Face name to use for output message.")
(defvar sqlplus-special-functions-face 'sqlplus-special-functions-face
  "Face name to use for output message.")
(defvar sqlplus-pseudo-columns-face 'sqlplus-pseudo-columns-face
  "Face name to use for output message.")

;; Face Definitions
(defface sql-oracle-fixme-face '((t (:background  "red4")))
  "Face used for highlighting of FIXME comments."
  :group 'sql-oracle)

(defface sqlplus-error-sourceline-face '((t (:background  "orange")))
  "Face used for highlighting of error lines in PL/SQL source code."
  :group 'sqlplus)

(defface sqlplus-output-from-buffer-face '((t (:background  "orange")))
  "Face used for highlighting of '\\[sqlplus-output-message]'."
  :group 'sqlplus)

(defface sqlplus-type-face '((t (:foreground  "green4" :weight bold)))
  "Face used for highlighting of keywords'."
  :group 'sqlplus)

(defface sqlplus-variable-name-face '((t (:foreground  "magenta4")))
  "Face used for highlighting of keywords'."
  :group 'sqlplus)

(defface sqlplus-builtin-face '((t (:foreground  "Purple")))
  "Face used for highlighting of keywords'."
  :group 'sqlplus)

(defface sqlplus-function-name-face '((t (:foreground  "brown4")))
  "Face used for highlighting of keywords'."
  :group 'sqlplus)

(defface sqlplus-keyword-face '((t (:foreground  "red4" :weight bold)))
  "Face used for highlighting of keywords'."
  :group 'sqlplus)

(defface sqlplus-reference-face '((t (:foreground  "red3")))
  "Face used for highlighting of keywords'."
  :group 'sqlplus)

(defface sqlplus-warning-face '((t (:foreground  "Red")))
  "Face used for highlighting of keywords'."
  :group 'sqlplus)

(defface sqlplus-preprocessor-face '((t (:foreground  "blue3")))
  "Face used for highlighting of keywords'."
  :group 'sqlplus)

(defface sqlplus-string-face '((t (:foreground  "green4")))
  "Face used for highlighting of keywords'."
  :group 'sqlplus)

(defface sqlplus-comment-face '((t (:foreground  "blue4")))
  "Face used for highlighting of keywords'."
  :group 'sqlplus)

(defface sqlplus-constant-face '((t (:foreground  "CadetBlue")))
  "Face used for highlighting of keywords'."
  :group 'sqlplus)

(defface sqlplus-semicolon-face '((t (:foreground  "red4" :weight bold)))
  "Face used for highlighting of keywords'."
  :group 'sqlplus)

(defface sqlplus-send-to-interpreter-face '((t (:foreground "black" :background "sandybrown")))
  "Face used for highlighting of keywords'."
  :group 'sqlplus)

(defface sqlplus-special-functions-face '((t (:foreground  "CadetBlue" :slant italic)))
  "Face used for highlighting of keywords'."
  :group 'sqlplus)

(defface sqlplus-pseudo-columns-face '((t (:foreground  "CadetBlue" :slant italic)))
  "Face used for highlighting of keywords'."
  :group 'sqlplus)

;; Requires
(require 'sql-oracle-complete)
(require 'sql-oracle-fontlock)
(require 'sql-oracle-src)
(require 'sql-oracle-menu)
(require 'sql-oracle-docu)
(require 'sql-oracle-imenu)
(require 'sql-oracle-plsql)
(require 'sql-oracle-transform)

;; initialize syntax tables
(if sql-oracle-mode-syntax-table
    () 
  (setq sql-oracle-mode-syntax-table (make-syntax-table)) 
  (modify-syntax-entry ?/ ". 14" sql-oracle-mode-syntax-table)   ; comment start 
  (modify-syntax-entry ?* ". 23" sql-oracle-mode-syntax-table) 
  (modify-syntax-entry ?+ "." sql-oracle-mode-syntax-table) 
  (modify-syntax-entry ?- "." sql-oracle-mode-syntax-table) 
  (modify-syntax-entry ?= "." sql-oracle-mode-syntax-table) 
;  (modify-syntax-entry ?% "w" sql-oracle-mode-syntax-table) 
  (modify-syntax-entry ?< "." sql-oracle-mode-syntax-table) 
  (modify-syntax-entry ?> "." sql-oracle-mode-syntax-table) 
;  (modify-syntax-entry ?& "w" sql-oracle-mode-syntax-table) 
  (modify-syntax-entry ?| "." sql-oracle-mode-syntax-table) 
  (modify-syntax-entry ?_ "w" sql-oracle-mode-syntax-table)     ; make _ part of words 
  (modify-syntax-entry ?\' "\"" sql-oracle-mode-syntax-table)) 

(if sql-oracle-mode-temp-syntax-table 
    () 
  (setq sql-oracle-mode-temp-syntax-table (make-syntax-table)) 
  (modify-syntax-entry ?/ ". 14" sql-oracle-mode-temp-syntax-table)   ; comment start 
  (modify-syntax-entry ?* ". 23" sql-oracle-mode-temp-syntax-table) 
  (modify-syntax-entry ?+ "." sql-oracle-mode-temp-syntax-table) 
  (modify-syntax-entry ?- "." sql-oracle-mode-temp-syntax-table) 
  (modify-syntax-entry ?= "." sql-oracle-mode-temp-syntax-table) 
  (modify-syntax-entry ?% "w" sql-oracle-mode-temp-syntax-table) 
  (modify-syntax-entry ?< "." sql-oracle-mode-temp-syntax-table) 
  (modify-syntax-entry ?> "." sql-oracle-mode-temp-syntax-table) 
  (modify-syntax-entry ?& "w" sql-oracle-mode-temp-syntax-table) 
  (modify-syntax-entry ?| "." sql-oracle-mode-temp-syntax-table) 
  (modify-syntax-entry ?_ "w" sql-oracle-mode-temp-syntax-table)     ; make _ part of words 
  (modify-syntax-entry ?\' "\"" sql-oracle-mode-temp-syntax-table)) 

;; initialize abbreviations 
(if sql-oracle-mode-abbrev-table 
    nil 
  (define-abbrev-table 'sql-oracle-mode-abbrev-table ()) 
  (let ((abbrevs-changed nil)) 
;; Abbreviations for SQL
    (define-abbrev sql-oracle-mode-abbrev-table  "s"   "select"       nil)
    (define-abbrev sql-oracle-mode-abbrev-table  "f"   "from"         nil)
    (define-abbrev sql-oracle-mode-abbrev-table  "w"   "where"        nil)
    (define-abbrev sql-oracle-mode-abbrev-table  "g"   "group by"     nil)
    (define-abbrev sql-oracle-mode-abbrev-table  "h"   "having"       nil)
    (define-abbrev sql-oracle-mode-abbrev-table  "o"   "order by"     nil)
    (define-abbrev sql-oracle-mode-abbrev-table  "l"   "like"         nil)
    (define-abbrev sql-oracle-mode-abbrev-table  "up"  "update"       nil)
    (define-abbrev sql-oracle-mode-abbrev-table  "del" "delete"       nil)
    (define-abbrev sql-oracle-mode-abbrev-table  "ins" "insert into"  nil)
;; v.z.: Sep 06, 1995
;; Abbreviations for PL/SQL
    (define-abbrev sql-oracle-mode-abbrev-table  "exc"  "exception"   nil)
    (define-abbrev sql-oracle-mode-abbrev-table  "dop"  "dbms_output.put_line('');"   nil)
;; v.z.: Dec 27, 1996
;; Abbreviations for SQL*Plus
    (define-abbrev sql-oracle-mode-abbrev-table  "d"   "describe"     nil)
    (define-abbrev sql-oracle-mode-abbrev-table  "fo"  "format a"     nil)
    (define-abbrev sql-oracle-mode-abbrev-table  "he"  "heading \"\"" nil)
    (define-abbrev sql-oracle-mode-abbrev-table  "ac"  "accept string char format a30 prompt ' ->'" nil)
    (define-abbrev sql-oracle-mode-abbrev-table  "sp"  "show parameter" nil)
    (define-abbrev sql-oracle-mode-abbrev-table  "ex"  "execute" nil)
    )
  )

(defun sql-oracle-mode () 
  "Major mode for editing SQL*Plus batch files. 
\\{sql-oracle-mode-map} 
sql-oracle-send-buffer and sql-oracle-send-region are commands that will send SQL*Plus 
commands defined in the current buffer to SQL*Plus to be executed.  Output 
is displayed in the *sqlplus* buffer (which will open as separate window 
if it does not already exist).  (use '\\[describe-mode]' while in *sqlplus* 
buffer for information on sqlplus-mode.) 
 
Entry to this mode calls the value of sqlplus-mode-hook with no args, 
if that value is non-nil.  Abbrev-mode is also enabled with the following 
abbreviations available by default: 
 
        s  ->  Select 
        f  ->  From 
        w  ->  Where 
        o  ->  Order By 
 
Use \\[list-abbrevs] for a full list. 
 
\\[sql-oracle-function-help] displays help for SQL commands and functions.

If the SQL statements to be executed contain variables prefixed with colons 
or INTO clauses, the colons are converted into ampersands and the INTO clauses 
are removed before being sent to SQL*Plus.  This provides compatibility with 
Pro*C, SQL*Report, and SQL*Forms (.inp files).  For example, 
 
     SELECT SYSDATE + :days_added INTO :variable FROM SYSTEM.DUAL 
 
is converted to 
 
     SELECT SYSDATE + &days_added FROM SYSTEM.DUAL 
 
and the user is prompted to enter the value of days_added." 
  (interactive)
  (kill-all-local-variables)
  (setq major-mode 'sql-oracle-mode) 
  (setq mode-name "SQL Oracle") 
  (use-local-map sql-oracle-mode-map) 
  (set-syntax-table sql-oracle-mode-syntax-table) 
  (setq local-abbrev-table sql-oracle-mode-abbrev-table) 
  (abbrev-mode 1) 
  (setq abbrev-all-caps 1) 
  (make-local-variable 'comment-start)
  (setq comment-start "/* ")
  (make-local-variable 'comment-end)
  (setq comment-end " */")
  (make-local-variable 'comment-column)
  (setq comment-column 32)
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "/\\*+ *")
  (setq require-final-newline t)
  ;;  (make-local-variable 'indent-line-function)     ;  v.z.: 10/24/95.
  ;;  (setq indent-line-function 'c-indent-line)      ;  v.z.: 10/24/95.
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(sql-oracle-font-lock-keywords nil t nil nil (font-lock-comment-start-regexp . "/[*/]")))
;  (setq font-lock-defaults '(sql-oracle-font-lock-keywords nil t))
  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression sql-oracle-imenu-generic-expression)
  (imenu-add-to-menubar "Create-Statements")
  (sql-oracle-add-sql-menu)
  (run-hooks 'sql-oracle-mode-hook) 
  )
 
(if sql-oracle-mode-map 
    nil 
  (setq sql-oracle-mode-map (make-sparse-keymap)) 
  (define-key sql-oracle-mode-map [(control meta return)]  'sql-oracle-send-buffer)
  (define-key sql-oracle-mode-map "\e\C-m"      	   'sql-oracle-send-region)     ; ESC Return/ALT Return
  (define-key sql-oracle-mode-map [(f4)]        	   'sql-oracle-send-region)     ; v.z.: Feb 07, 2012
  (define-key sql-oracle-mode-map "\C-cp"    	           'sql-rewrite)
  (define-key sql-oracle-mode-map "\C-cs"    	   	   'sql-to-select)
  (define-key sql-oracle-mode-map "\C-ci"    	   	   'sql-to-insert)
  (define-key sql-oracle-mode-map "\C-cu"    	   	   'sql-to-update)
  (define-key sql-oracle-mode-map "\C-cd"    	   	   'sql-to-delete)
  (define-key sql-oracle-mode-map [(tab)]       	   'sql-oracle-complete-word-maybe)
  (define-key sql-oracle-mode-map [(f1)]                   'sql-oracle-w3m-help)
  (define-key sql-oracle-mode-map [(control return)]       'sql-oracle-send-region-to-shell)
  (define-key sql-oracle-mode-map "\C-cc"                  'sql-oracle-describe-to-cols) 
  (define-key sql-oracle-mode-map "\C-ca"                  'sql-oracle-insert-accept-where)
;  (define-key sql-oracle-mode-map "\C-cb"       	   'sql-oracle-beautify-sql)
;  (define-key sql-oracle-mode-map "\C-ci"       	   'sql-oracle-goto-info-page)
  (define-key sql-oracle-mode-map [(meta up)]   	   'sql-oracle-backward-codeblock)
  (define-key sql-oracle-mode-map [(meta down)] 	   'sql-oracle-forward-codeblock)
  (define-key sql-oracle-mode-map "\C-cm"    	           'sql-oracle-mark-codeblock)
  (define-key sql-oracle-mode-map "\C-cr"     	           'sql-oracle-show-sqlplus-output-other-window)
  (define-key sql-oracle-mode-map "\C-ch"                  'sql-oracle-function-help)
  (define-key sql-oracle-mode-map [(f8)]        	   'sqlplus-scroll-right-other-window)
  (define-key sql-oracle-mode-map [(f9)]        	   'sqlplus-scroll-left-other-window)
  (define-key sql-oracle-mode-map [(f11)]       	   'sqlplus-scroll-down-other-window)
  (define-key sql-oracle-mode-map [(f12)]       	   'sqlplus-scroll-up-other-window)
  (define-key sql-oracle-mode-map "\C-cb"    	   	   'sqlplus-back-command-other-window)      
  (define-key sql-oracle-mode-map "\C-cf"    	   	   'sqlplus-forward-command-other-window)   
  (define-key sql-oracle-mode-map "\C-ce"    	   	   'sqlplus-end-of-buffer-other-window)
  (define-key sql-oracle-mode-map "\C-ck"    	   	   'sqlplus-kill-command-other-window)
  (define-key sql-oracle-mode-map "\C-cx"    	   	   'sqlplus-reset-buffer-other-window)
  (if (featurep 'xemacs) ;; for XEmacs
      (define-key sql-oracle-mode-map [(control shift button3)] 'sql-oracle-dynamic-popup-menu)
    (define-key sql-oracle-mode-map [(mouse-3)] 'sql-oracle-dynamic-popup-menu))
  (define-key sql-oracle-mode-map [(backspace)] 	   'delete-backward-char)
  (define-key sql-oracle-mode-map "\C-c\t"                 'indent-relative)
  )

(defun sqlplus-mode () 
  "Major mode for interacting with Oracle SQL*Plus. 
Return at end of buffer sends line as input. 
Return not at end copies SQL statement to end and executes it.   
\\{sqlplus-mode-map} 
This mode is normally invoked by 'M-x sqlplus' (not 'M-x sqlplus-mode'). 
You will be prompted to enter a username/password combination 
to access the Oracle database.  This can be prevented by setting the  
variable sqlplus-username-password in your .emacs file as follows: 
 
     (setq sqlplus-username-password \"myname/mypassword\") 
 
\\[sql-oracle-function-help] displays help for SQL commands and functions.

There are two ways of editing and re-executing prior commands. 
'\\[sqlplus-back-command]' and '\\[sqlplus-forward-command]' will move to the 
location  
in the buffer of the previous or next SQL statement, respectively 
based on the command prompt).  The command can then be edited 
normally and re-executed by pressing Return.  To insert a newline, 
you may press '\\[newline-and-indent]'.  '\\[sqlplus-next-command]' and 
'\\[sqlplus-previous-command]'  
are similar except the next or previous SQL statement is inserted at 
the end of the buffer.  Repeating these commands will clear the 
current statement and recall the next or previous statement from the 
stack.  For additional information on command execution use  
'\\[describe-key] RTN'. 

'\\[sql-oracle-show-sqlplus-output]' will move to the beginning of the last output 
generated by SQL*plus.  This is useful for reviewing the results of  
the last statement.  '\\[sqlplus-end-of-buffer]' moves to the end of the 
buffer,  
but unlike '\\[end-of-buffer]' (end-of-buffer), it does not set the mark. 

'\\[sqlplus-kill-command]' deletes either the current command being entered or 
the last output generated by SQL*Plus, depending on when it is used.   
If executed while the cursor is within a SQL statement, the statement and  
any text after it are deleted.  If the cursor is within or at the end of 
output generated by SQL*Plus, the output is deleted and the cursor is  
positioned at the end of the SQL statement that generated the ouput.   
'\\[sqlplus-kill-command]' can be used like an undo command to alternately 
delete commands and output from the end of the buffer. 

The commands sql-oracle-send-region and sql-send-buffer can be executed from 
another buffer to execute the SQL statements defined by the current 
region or entire buffer, respectively.  Output from these commands is 
displayed in the *sqlplus* buffer.  The major mode called sql-oracle-mode has 
these functions bound to key sequences.

Entry to this mode calls the value of sqlplus-mode-hook with no args, 
if that value is non-nil.  Abbrev-mode is also enabled with the following 
abbreviations available by default: 

        s  ->  Select 
        f  ->  From 
        w  ->  Where 
        o  ->  Order By 

Use '\\[list-abbrevs]' for a full list.   

If the variable sqlplus-keep-history is non-nil, the current session is 
saved in the file ~/.sqlhist and recalled automatically the next time 
sqlplus is executed.  The session will only be saved if the EXIT or QUIT 
command is used to terminate the SQL*Plus session.  The maximum number of  
lines saved can be set with the variable sqlplus-lines-to-keep which defaults 
to 1000.  The command '\\[sqlplus-drop-old-lines]' will truncate the buffer  
to this length at any time.  sqlplus-keep-history and sqlplus-lines-to-keep 
should be set in your .emacs file or via customize.

If the *sqlplus* buffer is killed with '\\[kill-buffer]', the SQL*Plus 
process will automatically be terminated, but the session will not be saved, 
even if sqlplus-keep-history is non-nil. 

'\\[sqlplus-reset-buffer]' will delete all output lines from the buffer, leaving
only commands.  This will significantly shrink the buffer, but retain a full 
history of commands for re-execution."
  (interactive) 
  (kill-all-local-variables)
  (setq major-mode 'sqlplus-mode) 
  (setq mode-name "SQL*Plus") 
  (setq mode-line-process '(": %s")) 
  (use-local-map sqlplus-mode-map) 
  (set-syntax-table sql-oracle-mode-syntax-table) 
  (setq local-abbrev-table sql-oracle-mode-abbrev-table) 
  (make-local-variable 'last-output-start) 
  (setq last-output-start (make-marker)) 
  (make-local-variable 'font-lock-defaults)
  (setq font-lock-defaults '(sqlplus-oracle-font-lock-keywords t t nil nil (font-lock-comment-start-regexp . "/[*/]")))
  (make-local-variable 'sqlplus-prompt) 
  (setq sqlplus-prompt "^\\(SQL> \\)+")     ;* allows "SQL> SQL> ..." 
  (make-local-variable 'sqlplus-continue-pattern) 
  (setq sqlplus-continue-pattern "^[ 0-9][ 0-9][0-9][* \t][ \t]\\|     ") 
  (setq indent-tabs-mode nil) 
  (setq left-margin 5) 
  (abbrev-mode 1) 
  (setq abbrev-all-caps 1)
  (make-local-variable 'imenu-generic-expression)
  (setq imenu-generic-expression sql-oracle-imenu-generic-expression)
  (imenu-add-to-menubar "Create-Statements")
  (sqlplus-add-sqlplus-menu)
  (run-hooks 'sqlplus-mode-hook) 
) 
 
(if sqlplus-mode-map 
    nil 
  (setq sqlplus-mode-map (make-sparse-keymap)) 
  (define-key sqlplus-mode-map "\C-m"           'sqlplus-execute-command)
  (define-key sqlplus-mode-map "\C-cc"          'sqlplus-interrupt-subjob)
  (define-key sqlplus-mode-map "\C-cp"          'sqlplus-previous-command)
  (define-key sqlplus-mode-map [(control up)]   'sqlplus-previous-command)
  (define-key sqlplus-mode-map "\C-cn"          'sqlplus-next-command)      
  (define-key sqlplus-mode-map [(control down)] 'sqlplus-next-command)
  (define-key sqlplus-mode-map "\C-cb"       	'sqlplus-back-command)      
  (define-key sqlplus-mode-map "\C-cf"       	'sqlplus-forward-command)   
  (define-key sqlplus-mode-map "\C-ce"       	'sqlplus-end-of-buffer)     
  (define-key sqlplus-mode-map "\C-ck"       	'sqlplus-kill-command)      
  (define-key sqlplus-mode-map "\C-cx"       	'sqlplus-reset-buffer)      
  (define-key sqlplus-mode-map "\C-cd"       	'sqlplus-drop-old-lines)    
  (define-key sqlplus-mode-map "\C-cw"       	'sqlplus-copy-word)         
  (define-key sqlplus-mode-map "\C-cs"       	'sqlplus-save-session)      
  (define-key sqlplus-mode-map "\C-co"       	'sqlplus-display-oracle-error-message)
  (define-key sqlplus-mode-map [(f8)]           'sqlplus-scroll-right)
  (define-key sqlplus-mode-map [(f9)]           'sqlplus-scroll-left)
  (define-key sqlplus-mode-map [(f11)]          'sqlplus-scroll-down)
  (define-key sqlplus-mode-map [(f12)]          'sqlplus-scroll-up)
  (define-key sqlplus-mode-map [(tab)]          'sql-oracle-complete-word-maybe)
  (define-key sqlplus-mode-map "\C-cr"          'sql-oracle-show-sqlplus-output)       
  (define-key sqlplus-mode-map "\C-ch"          'sql-oracle-function-help)
  (define-key sqlplus-mode-map [(f1)]           'sql-oracle-w3m-help)
  (define-key sqlplus-mode-map [(backspace)]    'delete-backward-char)
  (define-key sqlplus-mode-map "\C-c\t"         'indent-relative)                 
  )

(defun sqlplus ()
  "Start up an interactive SQL*Plus session in a new buffer.
If an active SQL*Plus process already exists, will switch to that
buffer."
  (interactive)
  (let ((process (sqlplus-start)))
    (switch-to-buffer "*sqlplus*")
    (if (and sqlplus-keep-history
	     (file-readable-p (expand-file-name "~/.sqlhist")))
	(progn
	  (sit-for 1)
	  (while (accept-process-output) (sleep-for 1))
	  (insert-file-contents (expand-file-name "~/.sqlhist") nil)
	  (goto-char (point-max))
	  (set-marker (process-mark process) (point))
	  (message ".sqlhist loaded")
          ))
    (if (eq system-type 'cygwin)
	(save-excursion
	  (let (
		(beg (point-min))
		)
	    (goto-char beg)
	    (while (re-search-forward "\r+$" (point-max) t)
	      (replace-match "" t t)))))
    ))

(defun sqlplus-start ()
  "Start up an interactive SQL*Plus session in a new buffer."
  (let ((sqlplus-buffer (get-buffer-create "*sqlplus*"))
        process)
;    (sql-oracle-set-connect-string)
;    (setq sqlplus-username-password (concat sql-oracle-sql-user "/" sql-oracle-sql-password "@" sql-oracle-sql-server))
    (setq process			; set process
	  (or				; to the first that is non-nil:
	   (get-buffer-process sqlplus-buffer)    ; current process
	   (progn			          ; or new process
	     (set-buffer sqlplus-buffer)
	     (insert sqlplus-startup-message)
;	     (start-process "SQL*Plus" sqlplus-buffer sqlplus-executable sqlplus-username-password)
	     (start-process "SQL*Plus" sqlplus-buffer sqlplus-executable "/nolog")
;		(or sqlplus-username-password
;		    (setq sqlplus-username-password
;			  (read-string "Enter SQL*Plus username/password[@NetServiceName]: ")))
             )))
    (set-buffer sqlplus-buffer)
    (set-marker (process-mark process) (point))
    (process-send-string process (concat
				  "set feedback off\n"
				  "set heading off\n"
				  "set linesize 2000\n"
				  "set pagesize 2000\n"
				  "set feedback on\n"
				  "set heading on\n"
;				  "prompt Welcome to SQL*Plus SQL> SQL> SQL> SQL> SQL> SQL> SQL>\n"
				  ))
    (goto-char (point-max))
    (set-marker (process-mark process) (point))
    (sqlplus-mode)
    process             ; return process
    )
  )

(defun sql-oracle-set-connect-string ()
  "Prompt user for NetServiceName, database user and password."
  (setq sql-oracle-sql-server (or sql-oracle-sql-server
                       (completing-read "Enter value for NetServiceName for completion and execution: "
                                        sql-oracle-sql-server-table)))
  (setq sql-oracle-sql-user (or sql-oracle-sql-user
                     (completing-read (format "Enter value for User Name on Server %s: "
                                              sql-oracle-sql-server)
                                      sql-oracle-sql-user-table)))
  (setq sql-oracle-sql-password (or sql-oracle-sql-password
			 (let ((prompt (format "Enter value for Password for %s on %s: "
					       sql-oracle-sql-user sql-oracle-sql-server)))
			   (if sql-oracle-sql-secure-passwords
			       (sql-oracle-sql-read-password prompt)
			     (read-string prompt)))))
  (if (string-equal (downcase sql-oracle-sql-user) "sys")
      (if (not (string-match (downcase "as\\s-sysdba$") sql-oracle-sql-server))
          (setq sql-oracle-sql-server (concat sql-oracle-sql-server " as sysdba")))))

(defun sqlplus-execute-command () 
  "When executed at end of buffer, sends text entered since last  
output from SQL*Plus.  When executed while positioned within another 
valid command in the buffer, copies command to end of buffer and  
re-executes it.  If point is within a multi-line statement at the end 
of the buffer (such as after '\\[sqlplus-previous-command]'), the entire 
statement will be cleared and re-entered one line at a time. 
 
Multi-line statements are recognized by the continuation prompt displayed 
by SQL*Plus.  This is controlled by the variable sqlplus-continue-pattern 
which defaults to recognize either a right-justified number padded to four  
characters followed by a space or asterisk, or simply five spaces.  A line 
ending with \";\" or \" /\" is also considered the end of a statement. 
A new line inserted into a prior statement must be indented at least five 
spaces to be included when the statement is re-executed.  
 
The output from a List command is also recognized as a valid SQL*Plus 
statement; the 'List' command itself is stripped out (as are 'Get' and 'Run'). 
 
When a complex SQL statement is executed, it may take a long time before 
the output is generated.  Emacs may appear to be hung since no keystrokes 
are accepted until the first character of output arrives.  In this situation 
'\\[keyboard-quit]' may be used to force emacs to stop waiting for output. 
You may then switch to another buffer to perform other work while you wait 
or press '\\[sqlplus-interrupt-subjob]' to cancel the current SQL command." 
  (interactive)
;  (setq sqlplus-last-output-start (point)) ;; v.z.: This for stripping ^M
  (let ((process (get-buffer-process (current-buffer))))
    (if (not process)
	(error "Current buffer has no process.  Use 'M-x sqlplus' to start SQL*Plus process.")
      )
    
    (cond 
     ;; last line of buffer and only one input line 
     ((and (save-excursion (end-of-line) (eobp))  
	   (<= (count-lines (process-mark process) (point)) 1)) 
      (end-of-line) 
      (sqlplus-send-line process) 
      ) 
     
     ;; within last multi-line command of buffer  
     ((not (save-excursion (re-search-forward sqlplus-prompt (point-max) t))) 
      (let ((command-lines (sqlplus-get-command))) 
	(sqlplus-kill-command t)        ; clear existing command lines 
	(while command-lines            ; send command lines 
	  (insert (car command-lines)) 
	  (sqlplus-send-line process) 
	  (setq command-lines (cdr command-lines)) 
	  ) 
	) 
      ) 
     ;; otherwise - prior command in buffer 
     (t                                  
      (or (save-excursion 
	    (beginning-of-line) 
	    (looking-at (concat sqlplus-prompt "\\|" sqlplus-continue-pattern))) 
	  (error "This is not a valid SQL*Plus command.")) 
      (let ((command-lines (sqlplus-get-command))) 
	(goto-char (point-max)) 
	(sqlplus-kill-command t)     ; clear pending command (if any) 
	(while command-lines 
	  (insert (car command-lines)) 
	  (sqlplus-send-line process) 
	  (setq command-lines (cdr command-lines)) 
	  ) 
	) 
      ) 
     ) ; end cond 
    ) ; end let
  (if (eq system-type 'cygwin)
      (if (get-buffer "*sqlplus*")
  	  (sqlplus-strip-ctrl-m))) ;; v.z.: This for stripping ^M
  (setq sqlplus-stack-pointer 0)
  )					; end defun

(defun sqlplus-send-line (process)      ; called from sqlplus-execute-command
  (insert ?\n)
  (setq sqlplus-last-output-start (point)) ;; v.z.: This for stripping ^M
  (let ((command (buffer-substring (process-mark process) (point)))
	(temp-file (expand-file-name sqlplus-buffer-temp-filename)))
    (move-marker last-output-start (point))
    ; trap EDIT command - must be the only word on the line
    (if (string-match "^ *edit\\s-*\\(\\w*\\)[ ;]*$" command)
	(let (command-lines
	      (edit-file-name (save-excursion (and
			       (re-search-backward "edit\\s-+\\([^ \t\n;]+\\)"
						   (process-mark
						    process) t)
			       (buffer-substring
				(match-beginning 1) (match-end 1))
			       )))
	      )
	  (sit-for 0)
	  (set-marker (process-mark process) (point))
	  (process-send-string process "LIST\n")
	  (accept-process-output process) ; wait for process to respond
	  (sleep-for 1)
	  (forward-line -1)
	  (setq command-lines (sqlplus-get-command)) ; capture command
	  (delete-region last-output-start (point)) ; delete listed lines
	  (goto-char (point-max))
	  (switch-to-buffer-other-window (get-buffer-create (or edit-file-name
								"*sqlplus-temp*")))
	  (if edit-file-name
	      (setq buffer-offer-save t)
	    )
	  (delete-region (point-min) (point-max)) ; clear buffer
	  (while command-lines		; insert command lines
	    (insert (car command-lines) "\n")
	    (setq command-lines (cdr command-lines))
	    )
	  (insert "/\n")
	  (goto-char (point-min))
	  (sql-oracle-mode)			; turn on sql-oracle-mode
	  )
					; else
					; execute command line
      (process-send-string process command)
      (goto-char (point-max))
      (set-marker (process-mark process) (point))
      (sit-for 0)			; force display update
      (accept-process-output)		; wait for process to respond
      )
					; trap QUIT command
    (if (string-match "^ *\\(exit\\|quit\\)[ ;]*$" command)
	(progn
	  (if sqlplus-keep-history
	      (let ((lines-to-keep (or sqlplus-lines-to-keep 1000)))
		(and (> (count-lines (point-min) (point-max)) lines-to-keep)
		     (y-or-n-p
		      (format "Current session is longer than %d lines.  Ok to truncate? " lines-to-keep))
		     (sqlplus-drop-old-lines lines-to-keep)
		     )
		(sqlplus-save-session "~/.sqlhist")
		)
	    )
	  (while (get-buffer-process (current-buffer))
	    (sit-for 1))		; wait for process to die
	  (kill-buffer (current-buffer))
	  (and (file-exists-p temp-file) ; if buffer 'sqlplus-buffer-temp-filename exists, delete it
	       (delete-file temp-file))
	  )				; end progn
      )					; end if
    )					; end let
  )

(defun sqlplus-kill-command (command-only-flag) 
  "Delete the current SQL command or output generated by last SQL command. 
When used at the end of the buffer, serves as an undo command. 
 
If point is within a valid SQL statement, delete region from prompt  
before point to end of buffer, otherwise delete all text between the end  
of the prior SQL statement and the end of the buffer." 
  (interactive "P") 
  (let ((process (get-buffer-process (current-buffer)))) 
    (if (or command-only-flag 
	    (save-excursion 
	      (beginning-of-line) 
	      (looking-at (concat sqlplus-prompt ".+\\|" sqlplus-continue-pattern)) 
	    ) 
	 ) 
	; then - delete command and everything beyond 
	(progn 
	  (delete-region (progn  
			   (re-search-backward sqlplus-prompt (point-min) t)  
			   (point)) 
			 (point-max)) 
	  (process-send-string process "\n")	; generate new prompt 
	  (goto-char (point-max)) 
	  (set-marker (process-mark process) (point)) 
	  (sit-for 0)				; update display
	  (accept-process-output)	; wait for prompt
	)
      ; else - delete output from prior command, leaving cursor at end of command
      (beginning-of-line)
      (or (re-search-backward sqlplus-prompt (point-min) t)
	  (error "Nothing to kill"))
      (set-marker (process-mark process) (match-end 0))
      (sqlplus-get-command)		; convenient way to find end of command
      (forward-char -1)			; back up one character
      (delete-region (point) (point-max))
    ) ; end if
  )
)

(defun sqlplus-kill-command-other-window (command-only-flag) 
  "Delete the current SQL command or output generated by last SQL command in the SQL*Plus buffer. 
When used at the end of the buffer, serves as an undo command. 
 
If point is within a valid SQL statement, delete region from prompt  
before point to end of buffer, otherwise delete all text between the end  
of the prior SQL statement and the end of the buffer." 
  (interactive "P")
  (let ((this-buffer))
    (setq this-buffer (current-buffer))
    (switch-to-buffer-other-window "*sqlplus*")
    (sqlplus-kill-command command-only-flag)
    (switch-to-buffer-other-window this-buffer)
    ))

(defun sqlplus-get-command () 
  (interactive) 
  (let ((line "") command-lines) 
    (end-of-line) 
    (or (re-search-backward sqlplus-prompt (point-min) t) 
	(error "Unable to execute this command")) 
    (goto-char (match-end 0))       ; skip past prompt 
    (setq command-lines             ; initialize command-lines list 
        (if (looking-at "l$\\|list$\\|r$\\|run$\\|get .*\\|edit") ; ignore LIST, RUN, GET, EDIT 
	    nil 
	  (list (setq line  
		      (buffer-substring (point) (progn (end-of-line) (point))))) 
	  ) 
	) 
    (forward-line) 
    (while (and                                      ; while previous line  
	    (not (string-match "^\\(.*;$\\| */\\)$" line)) ; does not end in / or ; 
	    (looking-at sqlplus-continue-pattern))       ; and this is a cont. line 
         (goto-char (match-end 0))                   ; skip over prompt 
	 (setq line (buffer-substring (point) (progn (end-of-line) (point)))) 
	 (setq command-lines (append command-lines (list line))) 
	 (forward-line) 
	 ) 
    command-lines          ; return command-lines as value of function 
    ))

(defun sqlplus-interrupt-subjob () 
  "Interrupt this shell's current subjob." 
  (interactive) 
  (interrupt-process nil t)) 

(defun sql-oracle-goto-last-sqlplus-output ()
  "Display most recent batch of output at top of window.
Also put cursor there."
  (interactive)
  (goto-char last-output-start)
  )

(defun sql-oracle-show-sqlplus-output () 
  "Display most recent batch of output at top of window.
Also put cursor there." 
  (interactive) 
  (sqlplus-back-command 1)
  (forward-line 1)
  (recenter 0)
  (if (looking-at "SQL> ")
      (progn
       (re-search-backward sqlplus-output-message)
       (recenter 0)
       ))
  )

(defun sql-oracle-show-sqlplus-output-other-window () 
  "Display most recent batch of output at top of SQL*Plus window.
Also put cursor there." 
  (interactive)
  (let ((this-buffer))
    (setq this-buffer (current-buffer))
    (switch-to-buffer-other-window "*sqlplus*")
    (sql-oracle-show-sqlplus-output)
    (switch-to-buffer-other-window this-buffer)
  ))

(defun sql-oracle-send-buffer (prefix-arg) 
  "Execute all SQL*Plus commands defined in current buffer. 
Output is displayed in the *sqlplus* buffer." 
  (interactive "P") 
  (sql-oracle-send-region (point-min) (point-max) prefix-arg) 
) 

(defun sql-oracle-send-region (start end prefix-arg)
  "Execute all SQL*Plus commands defined between point and mark in current buffer.
Output is displayed in the *sqlplus* buffer."
  (interactive "r\nP")
  (let (process
	this-buffer
	temp-file
	imbedded-variables
	(temp-buffer nil)
	(command (buffer-substring-no-properties start end))
	)
    (setq this-buffer (current-buffer))
    (or (setq process (get-buffer-process "*sqlplus*")) ; look for process
        (setq process (sqlplus-start)) ; or create process
        (error "Unable to create SQL*Plus session."))  
    ;;    (setq temp-file (format "/tmp/sqlbuf.%d" (process-id process)))
    (if (and (eq system-type 'cygwin)
	     (featurep 'xemacs))
	(setq temp-file (concat "D:\\temp\\" sqlplus-buffer-temp-filename))
      (setq temp-file (expand-file-name sqlplus-buffer-temp-filename)))
    (set-buffer this-buffer)
    ;; FIXME: Error: There is no region now, when executing with a prefix argument
    ;;    (if (and (null prefix-arg)		; if no prefix argument 
    (if (and prefix-arg		        ; if prefix argument 
	     (save-excursion		; look for 'INTO :' or ':variable' 
	       (goto-char start)
	       (re-search-forward "\\binto\\s-+:\\|\\s-:\\w+" end t)))
	  (progn 
	    (setq temp-buffer (get-buffer-create "*sql-temp*")) 
	    (set-buffer temp-buffer) 
	    (set-syntax-table sql-oracle-mode-temp-syntax-table) ; important for regular expressions v.z.: 24.03.2005
	    (erase-buffer) 
	    (insert-buffer-substring this-buffer start end) ; insert region 
	    (skip-chars-backward "\n\t ")	; trim trailing whitespace 
	    (if (save-excursion 
		  (forward-char -1)	; back up one 
		  (not (looking-at ";\\|/")) ; last character is not ; or / 
		  ) 
		(insert "\n/\n")		; add "/" so statement is executed 
	      ) 
	    (goto-char (point-min))	; delete INTO clause 
	    (if (re-search-forward "\\binto\\s-+:" (point-max) t) 
		(delete-region (match-beginning 0) ; delete between INTO & FROM 
			       (progn 
				 (re-search-forward "\\bfrom\\b" (point-max) t) 
				 (match-beginning 0) 
				 ) 
			       ) 
	      ) ; endif 
	    (goto-char (point-min))	; convert all ":block.field" to "&block_field" 
	    (replace-regexp ":\\(\\w+\\)\\." "&\\1_" nil)
	    (goto-char (point-min))	; convert all remaining ":" to "&"
	    (replace-string ":" "&" nil)
	    (goto-char (point-min))
	    (while (re-search-forward "&\\w+" (point-max) t)
	      (let ( (wbeg (match-beginning 0)) (wend (match-end 0)) )
		(if (> (- wend wbeg) 30)	; if word > 30 characters
		    (delete-region (+ wbeg 1) (- wend 30)) ; truncate to 30
		  )
		)
	      )
	    (setq start (point-min))	; reset start & end for write-region
	    (setq end (point-max))
	    ) ; end progn
	) ; endif prefix argument
      (setq imbedded-variables (save-excursion     ; look for &, accept, acc
				 (goto-char start)
				 (re-search-forward "&\\|\\bacc\\(ept\\)?\\b" end t)))
      (write-region start end temp-file nil 0) ; write temp file
      (switch-to-buffer-other-window "*sqlplus*")
      (goto-char (point-max))
      (recenter 0)
      (insert (format (concat "\n" sqlplus-output-message " '%s':\n")
		      (buffer-name this-buffer)))
      (set-marker (process-mark process) (point))
      (sit-for 0)				; update display
      (process-send-string process (concat "@" temp-file "\n"))
      (if temp-buffer (kill-buffer temp-buffer))
	
      (if (get-buffer-process "*sqlplus*")
	  (if imbedded-variables		; stay in *sqlplus* buffer to
	      (goto-char (point-max))		; allow entry of variables
	    (progn
		(switch-to-buffer-other-window this-buffer)
		(save-excursion
		  (setq sql-oracle-syntax-errors-found nil)
		  ;; FIXME: Only when there is no prompt
		  (sqlplus-check-for-errors this-buffer process)
		  )
		(if (not sql-oracle-syntax-errors-found)
		    (progn
		      (set-buffer "*sqlplus*")
		      ;; Recenter output at line 0
		      (sql-oracle-show-sqlplus-output)

		      (if (eq system-type 'cygwin)
			  (save-excursion
			    (let (
				  (beg (point))
				  )
			      (while (re-search-forward "\r+$" (point-max) t)
				(replace-match "" t t)))))
		      
		      (pop-to-buffer this-buffer))))))
      
      (if (string-match "^ *\\(exit\\|quit\\)[ ;]*$" command)
	  (progn
	    (if sqlplus-keep-history
		(let ((lines-to-keep (or sqlplus-lines-to-keep 1000)))
		  (and (> (count-lines (point-min) (point-max)) lines-to-keep)
		       (y-or-n-p
			(format "Current session is longer than %d lines.  Ok to truncate? " lines-to-keep))
		       (sqlplus-drop-old-lines lines-to-keep)
		       )
		  (sqlplus-save-session "~/.sqlhist")))
	    ;;	      (while (get-buffer-process "*sqlplus*")
	    ;;		(sit-for 1))		; wait for process to die
	    (kill-buffer "*sqlplus*")
	    (and (file-exists-p temp-file) ; if buffer 'sqlplus-buffer-temp-filename exists, delete it
		 (delete-file temp-file))
	    )				; end progn
	)					; end if
      
      )
  )

(defun sqlplus-strip-ctrl-m (&optional _string)
  "Strip trailing `^M' characters from the current output group."
  (interactive)
  (let (
	(pmark (process-mark (get-buffer-process (current-buffer))))
	)
    (save-excursion
      (condition-case nil
	  (goto-char
	   (if (called-interactively-p 'interactive)
	       comint-last-input-end sqlplus-last-output-start))
	(error nil))
      (while (re-search-forward "\r+$" pmark t)
	(replace-match "" t t)))))

(defun sqlplus-check-for-errors (buffer process)
  "After command execution check for ORA or similar errors and compilation warnings
and react upon."
  (interactive)
;  (or (setq process (get-buffer-process "*sqlplus*")) ; look for process
;      (setq process (sqlplus-start)) ; or create process
;      (error "Unable to create SQL*Plus session."))
  (setq sql-oracle-old-window-configuration (current-window-configuration))
  (switch-to-buffer-other-window "*sqlplus*")
  (accept-process-output)
  (sit-for 1)
  (goto-char (point-max))
  (let ((beg (save-excursion (re-search-backward sqlplus-prompt nil t 2)))
        msg)
    (save-excursion
        (let (error-list)
          (while (re-search-backward "^\\([A-Z][A-Z][A-Z0-9]\\)-\\([0-9]?[0-9]?[0-9]?[0-9]?[0-9]\\)" beg t)
            (if (not (equal (match-string 2) "06512"))
                (add-to-list 'error-list (cons (match-string 1) (match-string 2)))))
          (while error-list
            (shell-command (concat "oerr " (car (car error-list)) " " (cdr (car error-list))))
            ;; FIXME: Only call set-buffer when there really is a *Shell Command Output* buffer
            ;; Some oerr commands return 0, and do not trigger buffer creation.
            (set-buffer "*Shell Command Output*")
            (setq msg (concat msg (buffer-substring (point-min) (point-max)) "\n"))
            (setq error-list (cdr error-list))
            )
          (if msg
              (progn
                (get-buffer-create "OERR-Messages")
                (set-buffer "OERR-Messages")
                (goto-char (point-min))
                (insert msg)
                (kill-buffer "*Shell Command Output*")
                (pop-to-buffer "OERR-Messages")
                (view-mode)
                (local-set-key "q" 'sql-oracle-quit-buffer)
                (message "Type SPACE to scroll, q to quit.")
		(setq sql-oracle-syntax-errors-found t)
                )
            )
          )
        )
      (save-excursion
        (if (re-search-backward "^Warning: .* created with compilation errors." beg t)
            (progn
              (goto-char (point-max))
              (insert ?\n)
              (move-marker last-output-start (point))
              (process-send-string process "show errors\n")
              (set-marker (process-mark process) (point))
              (sit-for 1)				; update display
              (sqlplus-parse-error-lines process)
              (accept-process-output)
              (setq sql-oracle-syntax-errors-found t)
              )))
      (if (and (not msg)
	       sql-oracle-syntax-errors-found)
	  ;; position cursor on first error line reference
	  (forward-line 5)
        )
      )
  )

(defun sql-oracle-quit-buffer ()
  "Kill the current buffer and restore the old window configuration."
  (interactive)
  (kill-buffer nil)
  (set-window-configuration sql-oracle-old-window-configuration)
  )

(defun sqlplus-parse-error-lines (process)
  "Parse line numbers of errors."
  (let ((line-numbers ())
        (object-type (progn
                       (save-excursion (re-search-backward "^Errors for \\(.*\\) \\(.*\\):"))
                       (match-string 1)))
        (object-name (progn
                       (save-excursion (re-search-backward "^Errors for \\(.*\\) \\(.*\\):"))
                       (match-string 2)))
        (beg (save-excursion (re-search-backward "^-"))))
    (while (re-search-backward "^\\([0-9]+\\)/" beg t)
      (if (featurep 'xemacs) ;; for XEmacs
	  (let ((line-number (make-extent (match-beginning 1) (match-end 1))))
	    (set-extent-face line-number 'red)
	    (set-extent-property  line-number 'highlight t)
	    (set-extent-property  line-number 'balloon-help "mouse-1: to go to source line")
	    (set-extent-property  line-number 'help-echo "mouse-1: to go to source line")
	    (set-extent-property  line-number 'keymap sqlplus-line-number-keymap)
	    )
	(let ((line-number (make-overlay (match-beginning 1) (match-end 1))))
	  (overlay-put line-number 'face 'sqlplus-error-sourceline-face)
	  (overlay-put line-number 'highlight t)
	  (overlay-put line-number 'balloon-help "mouse-1: to go to source line")
	  (overlay-put line-number 'help-echo "mouse-1: to go to source line")
	  (overlay-put line-number 'keymap sqlplus-line-number-keymap)
	  ))
      (add-to-list 'line-numbers (match-string 1))
      )
    (process-send-string process (concat "set feedback off\nset heading off\ncol line format 99999\ncol text format a200\n"))
    (let ((lines line-numbers))
      (while lines
        (process-send-string process (concat "select line, text from user_source where upper(name) = upper('" object-name "') and type = '" object-type "' and line between " (number-to-string (- (string-to-number (car lines)) sqlplus-source-context-lines)) " and " (number-to-string (+ (string-to-number (car lines)) sqlplus-source-context-lines)) " order by line;\n"))
        (setq lines (cdr lines))
        ))
    (process-send-string process (concat "set feedback on\nset heading on\n"))
    (sit-for 1)				; update display
    (accept-process-output)
    (let ((lines line-numbers))
      (while lines
        (re-search-forward (concat "^ +" (car lines) "[ \t]"))
        (save-excursion
          (let* ((beg (progn (beginning-of-line) (point)))
                 (end (progn (end-of-line) (point)))
		 (error-line 
		  (if (featurep 'xemacs)
		      (make-extent beg end)
		    (make-overlay beg end))))
	    (if (featurep 'xemacs)
		(set-extent-face error-line 'sqlplus-error-sourceline-face)
	      (overlay-put error-line 'face 'sqlplus-error-sourceline-face)
	      )
	    ))
	(setq lines (cdr lines))
	))))

(defun sqlplus-mouse-click-to-sourceline (@click)
  "Click to select PL/SQL source line"
  (interactive "e")
  (let ((p1 (posn-point (event-start @click))))
    (goto-char p1)
    (sqlplus-goto-source-line)))

(if sqlplus-line-number-keymap
    nil
  (setq sqlplus-line-number-keymap (make-sparse-keymap))
  (if (featurep 'xemacs)
      (define-key sqlplus-line-number-keymap [(button2)] 'sqlplus-goto-source-line)
    (define-key sqlplus-line-number-keymap [(mouse-1)] 'sqlplus-mouse-click-to-sourceline))
  (define-key sqlplus-line-number-keymap [(return)] 'sqlplus-goto-source-line)
  )

(defun sqlplus-goto-source-line ()
  "Goto source line."
  (interactive)
  ;; FIXME: works only when point is over extent
  ;;        how do I get the mouse click point ?
  (if (featurep 'xemacs) ;; for XEmacs
      (let* ((e (extent-at (point)))
	     (line-number (buffer-substring (extent-start-position e) (extent-end-position e))))
	(re-search-forward sqlplus-prompt)
	(re-search-forward (concat "^ *" line-number))
	)
    (let* ((o (car (overlays-at (point))))
	   (line-number (buffer-substring (overlay-start o) (overlay-end o))))
      (re-search-forward sqlplus-prompt)
      (re-search-forward (concat "^ *" line-number))
      )))

(defun sqlplus-back-command (arg) 
  "Move to the SQL*Plus command before current position. 
With prefix argument, move to ARG'th previous command." 
  (interactive "p") 
  (if (save-excursion  
	(beginning-of-line) 
	(re-search-backward sqlplus-prompt (point-min) t arg) 
      ) 
      (goto-char (match-end 0)) 
    (error "No previous SQL*Plus command.") 
    ) 
  ) 
   
(defun sqlplus-forward-command (arg) 
  "Move to the SQL*Plus command after current position. 
With prefix argument, move to ARG'th previous command." 
  (interactive "p") 
  (if (re-search-forward sqlplus-prompt (point-max) t arg) 
      nil 
    (error "No next SQL*Plus command.") 
  ) 
) 
 
(defun sqlplus-back-command-other-window (arg) 
  "Move to the SQL*Plus command before current position in SQL*Plus buffer. 
With prefix argument, move to ARG'th previous command." 
  (interactive "p") 
  (let ((this-buffer))
    (setq this-buffer (current-buffer))
    (switch-to-buffer-other-window "*sqlplus*")
    (sqlplus-back-command arg)
    (switch-to-buffer-other-window this-buffer)
    ))
   
(defun sqlplus-forward-command-other-window (arg) 
  "Move to the SQL*Plus command after current position in SQL*Plus buffer. 
With prefix argument, move to ARG'th previous command." 
  (interactive "p") 
  (let ((this-buffer))
    (setq this-buffer (current-buffer))
    (switch-to-buffer-other-window "*sqlplus*")
    (sqlplus-forward-command arg)
    (switch-to-buffer-other-window this-buffer)
    ))
 
(defun sqlplus-previous-command (arg) 
  "Recall the previous SQL*Plus command from the command stack. 
With prefix argument, recall the command ARG commands before the current 
stack pointer." 
  (interactive "p") 
; - clear current pending command 
  (goto-char (process-mark (get-buffer-process (current-buffer)))) 
  (delete-region (point) (point-max)) 
 
; - increment stack pointer by arg 
  (setq sqlplus-stack-pointer (+ sqlplus-stack-pointer arg)) 
  (if (< sqlplus-stack-pointer 0) 
      (progn (setq sqlplus-stack-pointer 0) 
	     (error "At last command.")) 
  ) 
  ;if there is a prior command     
  (if (re-search-backward (concat sqlplus-prompt ".+")  ; skip empty prompts 
			  (point-min) t sqlplus-stack-pointer) 
  ;then 
      (let ((command-lines (sqlplus-get-command)) col) 
	(goto-char (point-max)) 
	(setq col (current-column)) 
	(while command-lines 
	  (indent-to col) 
	  (insert (car command-lines)) 
	  (setq command-lines (cdr command-lines)) 
	  (if command-lines (insert ?\n)) 
	) 
        (message (if (> sqlplus-stack-pointer 0) 
		     (format "#%d" sqlplus-stack-pointer) 
		   "")) 
      ) 
  ;else 
    (setq sqlplus-stack-pointer (- sqlplus-stack-pointer arg)) ; reset 
    (error "No previous SQL*Plus command.") 
    ) 
  ) 

(defun sqlplus-next-command (arg) 
  "Recall the next SQL*Plus command from the command stack. 
With prefix argument, recall the command ARG commands after the current 
stack pointer." 
  (interactive "p") 
  (sqlplus-previous-command (- arg)) 
  ) 
 
(defun sqlplus-end-of-buffer () 
  "Move cursor to end of buffer." 
  (interactive) 
  (goto-char (point-max)) 
  ) 

(defun sqlplus-end-of-buffer-other-window () 
  "Move cursor to end of buffer in SQL*Plus buffer." 
  (interactive)
  (let ((this-buffer))
    (setq this-buffer (current-buffer))
    (switch-to-buffer-other-window "*sqlplus*")
    (sqlplus-end-of-buffer)
    (switch-to-buffer-other-window this-buffer)
    ))

(defun sqlplus-reset-buffer () 
  "Reset SQL*Plus buffer to contain only command history, not output. 
Commands of one or fewer characters (/, l, r, etc.) are not retained." 
  (interactive) 
  (and (y-or-n-p  
	(format "Delete all output lines from buffer %s? " (buffer-name))) 
       (let ((line "") (process (get-buffer-process (current-buffer))) start) 
	 (message "Deleting output lines...") 
	 (goto-char (point-min)) 
	 (setq start (point)) 
	 (while (re-search-forward (concat sqlplus-prompt "..+") (point-max) t) 
	   (goto-char (match-end 1)) 
	   (setq line (buffer-substring (point) (progn (end-of-line) (point)))) 
	   (beginning-of-line) 
	   (delete-region start (point)) 
	   (forward-line) 
	   (while (and			; skip past SQL statement 
		   (not (string-match "^\\(.*;$\\| */\\)$" line)) 
		   (looking-at sqlplus-continue-pattern)) ; and this is a cont. line 
	     (goto-char (match-end 0))	; skip over prompt 
	     (setq line (buffer-substring (point) (progn (end-of-line) 
							 (point)))) 
	     (forward-line) 
	     ) 
	   (setq start (point)) 
	   ) 
	 (goto-char (point-max)) 
	 (delete-region start (point)) 
	 (process-send-string process "\n") ; generate new prompt 
	 (goto-char (point-max)) 
	 (set-marker (process-mark process) (point)) 
	 (sit-for 0)			; update display
	 (accept-process-output)	; wait for prompt
	 (message "Deleting output lines...Done.")
	 )
       )
  )

(defun sqlplus-reset-buffer-other-window () 
  "Reset SQL*Plus buffer to contain only command history, not output. 
Commands of one or fewer characters (/, l, r, etc.) are not retained." 
  (interactive)
  (let ((this-buffer))
    (setq this-buffer (current-buffer))
    (switch-to-buffer-other-window "*sqlplus*")
    (sqlplus-reset-buffer)
    (switch-to-buffer-other-window this-buffer)
    ))


(defun sqlplus-drop-old-lines (lines-to-keep) 
  "Delete old lines from current buffer. 
Number of lines to keep is determined by the variable sqlplus-lines-to-keep. 
With prefix argument, keep the last ARG lines." 
  (interactive "P") 
  (delete-region (save-excursion 
		   (goto-char (point-min)) 
		   (point)) 
		 (save-excursion 
		   (goto-char (point-max)) 
		   (forward-line (- (or lines-to-keep  
					sqlplus-lines-to-keep  
					1000))) 
		   (point))) 
  ) 

(defun sqlplus-save-session (filename) 
"Save current SQL*Plus session to FILENAME." 
  (interactive "FFile to save session to: ")	     
  (save-excursion 
    (if (or (null filename) (string= filename "")) 
	(setq filename "~/sqlhist")) 
    (message (format "Saving %s..." filename)) 
    (write-region (progn 
		    (goto-char (point-min)) 
		    (re-search-forward sqlplus-prompt (point-max) t) 
		    (match-end 0)) 
		  (progn 
		    (goto-char (point-max)) 
		    (re-search-backward sqlplus-prompt (point-min) t) 
		    (match-end 0)) 
		  (expand-file-name filename) nil 0) 
    (message (format "Saving %s...done" filename)) 
    ) 
  )

(defun sqlplus-copy-word () 
  "Copy current word to the end of buffer, inserting SELECT keyword  
or commas if appropriate." 
  (interactive) 
  (let (word preceding-word) 
    (save-excursion 
      (setq word (buffer-substring	; extract word 
		  (progn (forward-char 1) (backward-word 1) (point)) 
		  (progn (forward-word 1) (point)))) 
      (goto-char (point-max))		; goto end of buffer 
      (cond 
       ; sitting at empty command line 
       ((save-excursion (beginning-of-line)  
			(looking-at (concat sqlplus-prompt "$"))) 
	(insert "select ") 
       ) 
       ; on same line as SELECT or ORDER BY, but other words already inserted 
       ((save-excursion (re-search-backward " select .+\\| order by .+"  
			  (save-excursion (beginning-of-line) (point)) t)) 
	(insert ", ") 
       ) 
       ; Otherwise 
       (t 
	(if (eq (preceding-char) ? )	; if preceding character = space 
	    nil 
	  (insert " ") 
	) 
       ) 
      ) ;end case 
      (insert word) 
      (message (format "\"%s\" copied" word)) 
      ) 
    ) 
  ) 

(defun sqlplus-display-oracle-error-message ()
  "Display cause and action for last Oracle error message before point."
  (interactive)
  (save-excursion
    (let* ((oracle-error-regexp "^[A-Z][A-Z][A-Z0-9]-[0-9]?[0-9]?[0-9]?[0-9]?[0-9]")
           (start (search-backward-regexp oracle-error-regexp))
           (oracle-message (buffer-substring start (+ start 9)))
           (oracle-message-type (substring oracle-message 0 3))
           (oracle-message-number (substring oracle-message 4))
           (oracle-message-string (concat oracle-message-type " " oracle-message-number)))
      (with-output-to-temp-buffer "*Help*"   
	(shell-command (concat "oerr " oracle-message-string) "*Help*")))
    (pop-to-buffer "*Help*")
    ))

(defun sql-oracle-cols-to-selectlist ()
  "Columns to select list."
  (interactive)
  (let (
        (select-list nil)
        )
    (progn
      (re-search-forward "select\\(\\s-+\\*\\s-*\\)")
      (replace-match "" nil nil nil 1)
      (newline)
      (save-excursion
        (or (re-search-backward "^/" nil t)
            (beginning-of-buffer))
        (progn
          (re-search-forward "^col \\([a-zA-Z0-9#$_]+\\)")
          (setq select-list (concat "\t  " (match-string 1)))
          (forward-line)
          )
        (while (not (looking-at "^$"))
          (progn
            (re-search-forward "^col \\([a-zA-Z0-9#$_]+\\)")
            (setq select-list (concat select-list "\n\t, " (match-string 1)))
            (forward-line)
            )
          )
        )
      (insert select-list)
      (newline)
;      (re-search-forward "\\s-+")
;      (newline)
;      (insert "\t  ")
      (if (re-search-forward "[ \t]+" (save-excursion (end-of-line) (point)) t)
          (progn
            (delete-backward-char 1)
            (newline)
            (insert "\t  "))
        )
      )
    )
  )

(defun sql-oracle-cols-to-selectlist-vertical ()
  "Translate column list to a select list in the format
	'Alias1 : ' || column1 || chr(10) ||
	'Alias2 : ' || column2 || chr(10) ||
	'...... : ' || ....... || chr(10) ||
	'AliasN : ' || columnN"
  (interactive)
  (let (
        (select-list nil)
        )
    (progn
      (re-search-forward "select\\(\\s-+\\*\\s-*\\)")
      (replace-match "" nil nil nil 1)
      (newline)
      (save-excursion
        (or (re-search-backward "^/" nil t)
            (beginning-of-buffer))
        (while (not (looking-at "^$"))
          (progn
            (re-search-forward "^col \\([a-zA-Z0-9#$_]+\\)")
            (setq select-list (concat select-list "\t '" (capitalize (match-string 1))
                                      (make-string (- max-column-length (length (match-string 1)) 4) ? )
                                      ": ' || " (match-string 1) " || chr(10) ||\n"))
            (forward-line)
            )
          )
        )
      (insert select-list)
      ;; kill the last || chr(10) ||
      (delete-backward-char 15)
      (newline)
      (insert "           as \"Rows in block format\"")
      (newline)
      ;; indent table name
      (if (re-search-forward "[ \t]+" (save-excursion (end-of-line) (point)) t)
          (progn
            (delete-backward-char 1)
            (newline)
            (insert "\t  "))
        )
      )
    )
  )

(defun sql-oracle-to-col ()
  "To col."
  (interactive)
  (while (not (looking-at "^$"))
    (progn
      (beginning-of-line)
      (if (looking-at " ")
          (delete-char 1))
      (insert "col ")
      (let
          ((start (point))
           (end (re-search-forward "[a-zA-Z0-9#$_]+"))
           (column (match-string 0))
           (column-length (length (match-string 0)))
           )
        (downcase-region start end)
        (re-search-forward "\\s-+\\(not null \\)?")
        (replace-match " format " t)
;        (re-search-forward "\\(n?\\(var\\)?char2?(?\\|number(?\\|date\\|long\\|rowid\\|n?clob\\|raw(\\|anydata\\|re$nv_list\\|xmltype\\|interval \\(day\\|year\\)(\\|timestamp(\\)\\([0-9]+\\)?" (save-excursion (end-of-line) (point)))
        (re-search-forward "\\(n?\\(var\\)?char2?(?\\|number(?\\|date\\|long\\|rowid\\|n?clob\\|raw(\\|anydata\\|re$nv_list\\|xmltype\\|interval \\(day\\|year\\)(\\|timestamp(\\|[a-zA-Z$_]+\\)\\([0-9]+\\)?" (save-excursion (end-of-line) (point)))
        (let ((mb (match-beginning 4))
              (me (match-end 4)))
          (if (or mb me)
              (setq char-length (string-to-number (buffer-substring mb me))))
          (cond
            ((or
              (string-equal (match-string 1) "VARCHAR2(")
              (string-equal (match-string 1) "CHAR(")
              (string-equal (match-string 1) "NVARCHAR2(")
              (string-equal (match-string 1) "NCHAR(")
              (string-equal (match-string 1) "RAW(")
              )
             (progn
               (if (>= char-length 128)
                   (setq char-length 60))
               (replace-match (concat "a" (if (< column-length char-length)
                                              (int-to-string char-length)
                                            (int-to-string column-length))) t)
               (kill-line)
               )
             )
            ((string-equal (match-string 1) "VARCHAR2")
             (replace-match (concat "a" (int-to-string column-length)) t))
            ((or
              (string-equal (match-string 1) "TIMESTAMP(")
              (string-equal (match-string 1) "INTERVAL DAY(")
              (string-equal (match-string 1) "INTERVAL YEAR(")
              )
             (progn
               (if (>= char-length 128)
                   (setq char-length 60))
               (replace-match (concat "a" (if (< column-length 40)
                                              "40"
                                            (int-to-string column-length))) t))
             (kill-line)
             )
            ((or
              (string-equal (match-string 1) "LONG")
              (string-equal (match-string 1) "CLOB")
              (string-equal (match-string 1) "NCLOB")
              (string-equal (match-string 1) "RE$NV_LIST")
              (string-equal (match-string 1) "ANYDATA")
              (string-equal (match-string 1) "XMLTYPE")
              )
             (replace-match (concat "a" (if (< column-length 100)
                                            "100"
                                          (int-to-string column-length))) t))
            ((string-equal (match-string 1) "DATE")
             (replace-match (concat "a" (if (< column-length 21)
                                            "21"
                                          (int-to-string column-length))) t))
            ((string-equal (match-string 1) "ROWID")
             (replace-match (concat "a" (if (< column-length 18)
                                            "18"
                                          (int-to-string column-length))) t))
            ((string-equal (match-string 1) "NUMBER")
             (replace-match (make-string column-length ?9) t))
            ((string-equal (match-string 1) "NUMBER(")
             (progn
;               (replace-match (make-string column-length ?9) t)
               (replace-match (make-string (if (< column-length char-length)
                                               char-length
                                             column-length) ?9) t)
               (kill-line)
               )
             )
            (t
             (replace-match "a100" t))
            ))
        (end-of-line)
        (insert (concat " heading \"" (capitalize column) "\""))
        (re-search-backward "\"[a-zA-Z0-9#$_]+\"")
        (while (search-forward "_" (save-excursion (end-of-line) (point)) t)
          (replace-match " "))
        (forward-line)
        ))))

(defun sql-oracle-describe-to-cols (prefix-arg)
  "Insert output of SQL*Plus Describe command on table name to a series of col commands and
an indented select list. With prefix argument output column list verticaly."
  (interactive "P")
  (save-excursion
    (or (get-buffer "*sqlplus*")
        (let ((this-buffer))
          (setq this-buffer (current-buffer))
          (sqlplus)
          (switch-to-buffer this-buffer)))
    (re-search-backward "^select\\s-+")
    (newline)
    (forward-line -1)
    (save-excursion
;    (re-search-forward "from\\s-+\\(\\w+\\)")
      (re-search-forward (concat "from\\s-+\\(" sql-oracle-schema-object-name "\\)"))
      (let ((tablename (match-string 1)))
        (save-excursion
          (set-buffer "*sqlplus*")
          (goto-char (point-max))
          (insert (concat "describe " tablename))
          (save-excursion
            (sqlplus-execute-command)
            )
          (sleep-for 1)
          ;; FIXME: Check for Oracle errors (e.g. Not connected) before parsing
          (forward-line 3)
          (let ((br (point))
                (er (re-search-forward "^$")))
            (setq desc (buffer-substring br er))
            ))))
    (save-excursion
      (insert desc))
    (sql-oracle-to-col)
    (forward-line -1)
    (sql-oracle-reformat-cols)
    (if prefix-arg
        (sql-oracle-cols-to-selectlist-vertical)
      (sql-oracle-cols-to-selectlist))
    ))

(defun sql-oracle-reformat-cols ()
  "Reformat SQL*Plus column statements"
  (interactive)
  (or
   (progn
     (and (re-search-backward "^$" nil t)
          (forward-line)))
   (beginning-of-buffer))
  (setq max-column-length 0)
  (save-excursion
    (while
        (re-search-forward "^\\s-*col\\(umn\\)?\\s-+\\(\"[^\"]*\"\\|[a-zA-Z0-9#$_]+\\)" (save-excursion (re-search-forward "^$")) t)
      (let
          (
           (column-length (length (match-string 0)))
           )
        (if (> column-length max-column-length)
            (setq max-column-length column-length)
          )
        )
      )
    )
  (save-excursion
    (while
        (re-search-forward "^\\s-*col\\(umn\\)?\\s-+\\(\"[^\"]*\"\\|[a-zA-Z0-9#$_]+\\)\\s-+format" (save-excursion (re-search-forward "^$")) t)
      (search-backward "format")
      (fixup-whitespace)
      (forward-char)
      (indent-to-column (+ max-column-length 1))
      )
    )
  (save-excursion
    (let ((begin (point))
	  (end (re-search-forward "^$")))
      (untabify begin end)
      ))
  (setq max-format-length 0)
  (save-excursion
    (while
        (re-search-forward "^\\s-*col\\(umn\\)?\\s-+\\(\"[^\"]*\"\\|[a-zA-Z0-9#$_]+\\)\\s-+format\\s-+\\w+" (save-excursion (re-search-forward "^$")) t)
      (let
          (
           (format-length (length (match-string 0)))
           )
        (if (> format-length max-format-length)
            (setq max-format-length format-length)
          )
        )
      )
    )
  (message (concat "Max format length " (int-to-string max-format-length)))
  (save-excursion
    (while
        (re-search-forward "^\\s-*col\\(umn\\)?\\s-+\\(\"[^\"]*\"\\|[a-zA-Z0-9#$_]+\\)\\s-+format\\s-+\\w+\\s-+heading" (save-excursion (re-search-forward "^$")) t)
      (search-backward "heading")
      (fixup-whitespace)
      (forward-char)
      (indent-to-column (+ max-format-length 1))
      )
    )
  )

(defun sql-oracle-insert-accept-where (column-name)
  "Insert sqlplus ACCEPT statement for COLUMN-NAME and corresponding sql WHERE clause."
  (interactive "sEnter value for column name: ")
  (let ((beg (save-excursion
               (re-search-backward "^\\s-*select\\s-+")))
        (end (save-excursion
               (re-search-backward "^/")))
        (where-clause (concat "\n\t  upper(" column-name ") like upper('%&s_" column-name "%')")))
    (save-excursion
      (goto-char end)
      (if (re-search-backward "^where" beg t)
          (progn
            (goto-char end)
            (insert (concat "and" where-clause)))
        (insert (concat "where" where-clause)))
      (newline)
      (goto-char beg)
      (forward-line -1)
      (if (not (looking-at "^$"))
          (progn
            (forward-line 1)
            (open-line 1)
            ))
      (insert (concat "accept s_" column-name " char format a30 prompt ' Enter value for: " (upcase-initials column-name) " -> '\n"))
      (forward-line -2)
      (if (not (looking-at "^accept"))
          (progn
            (forward-line 1)
            (open-line 1)
            ))
      )
    )
  )

(defun sql-oracle-insert-ddview ()
  "Insert DD view at point."
  (interactive)
  (insert 
   (completing-read "Data Dictionary view: " sql-oracle-ddview-alist nil t))
 )

(defun sql-oracle-create-sql-files ()
  "Create files from Data Dictionary view names"
  (interactive)
  (setq directory (concat (getenv "TMP") "/sql/"))
  (find-file (concat directory "ddviews"))
  (while (re-search-forward "^\\(\\(all\\|dba\\|user\\)_\\|v\\$\\)?\\([a-zA-Z0-9#$_]+\\)" nil (save-excursion (re-search-forward "^$")))
    (let
        (
         (tablename (match-string 0))
         (filename
          (cond
           ((string-equal (match-string 2) "all")
            (concat "a" (match-string 3)))
           ((string-equal (match-string 2) "dba")
            (match-string 3))
           ((string-equal (match-string 2) "user")
            (concat "u" (match-string 3)))
           ((string-equal (match-string 1) "v$")
            (match-string 3))
           ((string-equal (match-string 1) nil)
            (match-string 3))
           )
          )
         )
      (message (concat "Working on: " tablename " -> " filename))
      (save-excursion
        (let ((sqlbuffer (find-file-noselect (concat directory filename ".sql") t)))
          (set-buffer sqlbuffer)
          (insert (concat "select * from " tablename "\n/\n"))
	  (sql-oracle-describe-to-cols nil)
          (beginning-of-buffer)
          (insert (concat "prompt " (upcase tablename) ":\n\n"))
          (save-buffer)
          (kill-buffer nil))
        )
      )
    )
  (kill-buffer nil)
  )

(cond ((fboundp 'with-displaying-help-buffer) ; for XEmacs
       (defun sql-oracle-show-info (info) 
         (with-displaying-help-buffer
          (lambda () 
	    (princ info)
	    "Help"))))
      (t                                ; for Emacs
       (defun sql-oracle-show-info (info)
	 (let ((b (get-buffer-create "Help")))
	   (display-buffer b)
	   (with-current-buffer b
	     (buffer-disable-undo)
	     (erase-buffer)
	     (insert info)
	     (goto-char 1)))
	 info)))

;; FIXME: fix the whole directory stuff and parse directory out of string
(defun sql-oracle-w3m-help (prefix-arg)
  "Display help for word under point in default browser. With prefix argument
use w3m inside of XEmacs instead."
  (interactive "P") 
;  (let ((toc (list 
;               (concat sql-oracle-db-documentation-directory "/server.101/b10759/toc.htm")
;               (concat sql-oracle-db-documentation-directory "/server.101/b10755/toc.htm")
;               (concat sql-oracle-db-documentation-directory "/appdev.101/b10802/toc.htm")
;               (concat sql-oracle-db-documentation-directory "/server.101/b12170/toc.htm")))
;        (dir '("server.101/b10759" "server.101/b10755" "appdev.101/b10802" "server.101/b12170"))
  (let
      (
       (toc
        (cond ((string-equal sql-oracle-db-documentation-version "10.2")
               (list (concat sql-oracle-db-documentation-directory "/server.102/b14237/toc.htm") ;;  Reference (init.ora parameter and DD views)      
                     (concat sql-oracle-db-documentation-directory "/server.102/b14200/toc.htm") ;;  SQL Reference                                    
                     (concat sql-oracle-db-documentation-directory "/server.102/b14227/toc.htm") ;;  Advanced Replication Management API Reference    
                     (concat sql-oracle-db-documentation-directory "/appdev.102/b14258/toc.htm") ;;  PL/SQL Packages and Types Reference              
                     (concat sql-oracle-db-documentation-directory "/server.102/b14357/toc.htm") ;;  SQL*Plus® User's Guide and Reference             
                     (concat sql-oracle-db-documentation-directory "/appdev.102/b14253/toc.htm") ;;  Application Developer's Guide - Workspace Manager
                     (concat sql-oracle-db-documentation-directory "/appdev.102/b14255/toc.htm") ;;  Spatial User's Guide and Reference               
                     (concat sql-oracle-db-documentation-directory "/text.102/b14218/toc.htm")   ;;  Text Reference                                    
                     ))
              ((string-equal sql-oracle-db-documentation-version "11.1")
               (list (concat sql-oracle-db-documentation-directory "/server.111/b28320/toc.htm") ;;  Reference (init.ora parameter and DD views)      
                     (concat sql-oracle-db-documentation-directory "/server.111/b28286/toc.htm") ;;  SQL Reference                                    
                     (concat sql-oracle-db-documentation-directory "/server.111/b28327/toc.htm") ;;  Advanced Replication Management API Reference    
                     (concat sql-oracle-db-documentation-directory "/appdev.111/b28419/toc.htm") ;;  PL/SQL Packages and Types Reference              
                     (concat sql-oracle-db-documentation-directory "/server.111/b31189/toc.htm") ;;  SQL*Plus® User's Guide and Reference             
                     (concat sql-oracle-db-documentation-directory "/appdev.111/b28396/toc.htm") ;;  Application Developer's Guide - Workspace Manager
                     (concat sql-oracle-db-documentation-directory "/appdev.111/b28400/toc.htm") ;;  Spatial User's Guide and Reference               
                     (concat sql-oracle-db-documentation-directory "/appdev.111/b28398/toc.htm") ;;  Oracle® Spatial GeoRaster Developer's Guide
                     (concat sql-oracle-db-documentation-directory "/appdev.111/b28399/toc.htm") ;;  Oracle® Spatial Topology and Network Data Models Developer's Guide
                     (concat sql-oracle-db-documentation-directory "/text.111/b28304/toc.htm")   ;;  Text Reference                                    
                     ))
              ((string-equal sql-oracle-db-documentation-version "11.2")
               (list (concat sql-oracle-db-documentation-directory "/server.112/e25513/toc.htm") ;;  Oracle® Database Reference (init.ora parameter and DD views)
                     (concat sql-oracle-db-documentation-directory "/server.112/e26088/toc.htm") ;;  Oracle® Database SQL Language Reference
                     (concat sql-oracle-db-documentation-directory "/server.112/e10707/toc.htm") ;;  Oracle® Database Advanced Replication Management API Reference
                     (concat sql-oracle-db-documentation-directory "/appdev.112/e25788/toc.htm") ;;  Oracle® Database PL/SQL Packages and Types Reference
                     (concat sql-oracle-db-documentation-directory "/server.112/e16604/toc.htm") ;;  SQL*Plus® User's Guide and Reference             
                     (concat sql-oracle-db-documentation-directory "/appdev.112/e12510/toc.htm") ;;  Oracle® Application Express API Reference
                     (concat sql-oracle-db-documentation-directory "/appdev.112/e11830/toc.htm") ;;  Oracle® Spatial Developer's Guide
                     (concat sql-oracle-db-documentation-directory "/appdev.112/e11827/toc.htm") ;;  Oracle® Spatial GeoRaster Developer's Guide
                     (concat sql-oracle-db-documentation-directory "/appdev.112/e11831/toc.htm") ;;  Oracle® Spatial Topology and Network Data Models Developer's Guide
                     (concat sql-oracle-db-documentation-directory "/text.112/e24436/toc.htm")   ;;  Oracle® Text Reference
                     ))
              ((string-equal sql-oracle-db-documentation-version "12.1")
               (list (concat sql-oracle-db-documentation-directory "/REFRN/toc.htm") ;;  Oracle® Database Reference (init.ora parameter and DD views)
                     (concat sql-oracle-db-documentation-directory "/SQLRF/toc.htm") ;;  Oracle® Database SQL Language Reference
                     (concat sql-oracle-db-documentation-directory "/REPMA/toc.htm") ;;  Oracle® Database Advanced Replication Management API Reference
                     (concat sql-oracle-db-documentation-directory "/ARPLS/toc.htm") ;;  Oracle® Database PL/SQL Packages and Types Reference
                     (concat sql-oracle-db-documentation-directory "/SQPUG/toc.htm") ;;  SQL*Plus® User's Guide and Reference             
                     (concat sql-oracle-db-documentation-directory "/AEAPI/toc.htm") ;;  Oracle® Application Express API Reference
                     (concat sql-oracle-db-documentation-directory "/SPATL/toc.htm") ;;  Oracle® Spatial Developer's Guide
                     (concat sql-oracle-db-documentation-directory "/GEORS/toc.htm") ;;  Oracle® Spatial GeoRaster Developer's Guide
                     (concat sql-oracle-db-documentation-directory "/TOPOL/toc.htm") ;;  Oracle® Spatial Topology and Network Data Models Developer's Guide
                     (concat sql-oracle-db-documentation-directory "/CCREF/toc.htm") ;;  Oracle® Text Reference
                     ))
              ((string-equal sql-oracle-db-documentation-version "12.2")
               (list (concat sql-oracle-db-documentation-directory "/REFRN/toc.htm") ;;  Oracle® Database Reference (init.ora parameter and DD views)
                     (concat sql-oracle-db-documentation-directory "/SQLRF/toc.htm") ;;  Oracle® Database SQL Language Reference
;                     (concat sql-oracle-db-documentation-directory "/REPMA/toc.htm") ;;  Oracle® Database Advanced Replication Management API Reference    !!! doesn't exist anymore !!!
                     (concat sql-oracle-db-documentation-directory "/ARPLS/toc.htm") ;;  Oracle® Database PL/SQL Packages and Types Reference
                     (concat sql-oracle-db-documentation-directory "/SQPUG/toc.htm") ;;  SQL*Plus® User's Guide and Reference             
                     (concat sql-oracle-db-documentation-directory "/AEAPI/toc.htm") ;;  Oracle® Application Express API Reference
                     (concat sql-oracle-db-documentation-directory "/SPATL/toc.htm") ;;  Oracle® Spatial Developer's Guide
                     (concat sql-oracle-db-documentation-directory "/GEORS/toc.htm") ;;  Oracle® Spatial GeoRaster Developer's Guide
                     (concat sql-oracle-db-documentation-directory "/TOPOL/toc.htm") ;;  Oracle® Spatial Topology and Network Data Models Developer's Guide
                     (concat sql-oracle-db-documentation-directory "/CCREF/toc.htm") ;;  Oracle® Text Reference
                     ))
              ))
       ;; FIXME: Don't repeat dirnames from above, instead parse them
       (dir
        (cond ((string-equal sql-oracle-db-documentation-version "10.2")
               '("server.102/b14237" ;;  Reference (init.ora parameter and DD views)      
                 "server.102/b14200" ;;  SQL Reference                                    
                 "server.102/b14227" ;;  Advanced Replication Management API Reference    
                 "appdev.102/b14258" ;;  PL/SQL Packages and Types Reference              
                 "server.102/b14357" ;;  SQL*Plus® User's Guide and Reference             
                 "appdev.102/b14253" ;;  Application Developer's Guide - Workspace Manager
                 "appdev.102/b14255" ;;  Spatial User's Guide and Reference               
                 "text.102/b14218"   ;;  Text Reference
                 ))
              ((string-equal sql-oracle-db-documentation-version "11.1")
               '("server.111/b28320" ;;  Reference (init.ora parameter and DD views)      
                 "server.111/b28286" ;;  SQL Reference                                    
                 "server.111/b28327" ;;  Advanced Replication Management API Reference    
                 "appdev.111/b28419" ;;  PL/SQL Packages and Types Reference              
                 "server.111/b31189" ;;  SQL*Plus® User's Guide and Reference             
                 "appdev.111/b28396" ;;  Application Developer's Guide - Workspace Manager
                 "appdev.111/b28400" ;;  Spatial User's Guide and Reference               
                 "appdev.111/b28398" ;;  Oracle® Spatial GeoRaster Developer's Guide
                 "appdev.111/b28399" ;;  Oracle® Spatial Topology and Network Data Models Developer's Guide
                 "text.111/b28304"   ;;  Text Reference
                 ))
              ((string-equal sql-oracle-db-documentation-version "11.2")
               '("server.112/e25513" ;;  Oracle® Database Reference (init.ora parameter and DD views)
                 "server.112/e26088" ;;  Oracle® Database SQL Language Reference
                 "server.112/e10707" ;;  Oracle® Database Advanced Replication Management API Reference
                 "appdev.112/e25788" ;;  Oracle® Database PL/SQL Packages and Types Reference
                 "server.112/e16604" ;;  SQL*Plus® User's Guide and Reference             
                 "appdev.112/e12510" ;;  Oracle® Application Express API Reference
                 "appdev.112/e11830" ;;  Oracle® Spatial Developer's Guide
                 "appdev.112/e11827" ;;  Oracle® Spatial GeoRaster Developer's Guide
                 "appdev.112/e11831" ;;  Oracle® Spatial Topology and Network Data Models Developer's Guide
                 "text.112/e24436"   ;;  Oracle® Text Reference
                 ))
              ((string-equal sql-oracle-db-documentation-version "12.1")
               '("REFRN" ;;  Oracle® Database Reference (init.ora parameter and DD views)
                 "SQLRF" ;;  Oracle® Database SQL Language Reference
                 "REPMA" ;;  Oracle® Database Advanced Replication Management API Reference
                 "ARPLS" ;;  Oracle® Database PL/SQL Packages and Types Reference
                 "SQPUG" ;;  SQL*Plus® User's Guide and Reference             
                 "AEAPI" ;;  Oracle® Application Express API Reference
                 "SPATL" ;;  Oracle® Spatial Developer's Guide
                 "GEORS" ;;  Oracle® Spatial GeoRaster Developer's Guide
                 "TOPOL" ;;  Oracle® Spatial Topology and Network Data Models Developer's Guide
                 "CCREF" ;;  Oracle® Text Reference
                 ))
              ((string-equal sql-oracle-db-documentation-version "12.2")
               '("REFRN" ;;  Oracle® Database Reference (init.ora parameter and DD views)
                 "SQLRF" ;;  Oracle® Database SQL Language Reference
;                 "REPMA" ;;  Oracle® Database Advanced Replication Management API Reference
                 "ARPLS" ;;  Oracle® Database PL/SQL Packages and Types Reference
                 "SQPUG" ;;  SQL*Plus® User's Guide and Reference             
                 "AEAPI" ;;  Oracle® Application Express API Reference
                 "SPATL" ;;  Oracle® Spatial Developer's Guide
                 "GEORS" ;;  Oracle® Spatial GeoRaster Developer's Guide
                 "TOPOL" ;;  Oracle® Spatial Topology and Network Data Models Developer's Guide
                 "CCREF" ;;  Oracle® Text Reference
                 ))
              ))
       (sql-statement (let ((cw (current-word)))
                        (if (or
                             (equal (downcase cw) "create")
                             (equal (downcase cw) "alter")
                             (equal (downcase cw) "drop")
                             (equal (downcase cw) "set")
                             (equal (downcase cw) "explain")
                             (equal (downcase cw) "flashback")
                             (equal (downcase cw) "associate")
                             (equal (downcase cw) "disassociate")
                             )
                            (progn
                              (save-excursion
                                (forward-word 2)
                                (setq cw (concat cw " " (downcase (current-word))))))
                          cw
                          )))
       end
       beg
       url
       found
       directory)
    ;; There is NO direct documentation for CDB_* views, so lookup the corresponding DB_* view
    (if (string-match "^cdb" sql-statement)
	(setq sql-statement (replace-match "dba" nil nil sql-statement))
	)
    (while (and toc
                (not found))
      (if (file-readable-p (car toc))
          (find-file-noselect (car toc))
	(sql-oracle-show-info sql-oracle-db-documentation-help)
	(error "Cannot find local Oracle Database documentation")
	)
      (set-buffer (get-file-buffer (car toc)))
      (setq directory (car dir))
      (goto-char (point-min))
      (save-excursion
;        (if (re-search-forward (concat "\\(\">\\([0-9]+ \\)?" sql-statement "<\\)") nil t)
;        (if (re-search-forward (concat "\\(\">\\([0-9]+ \\)?" sql-statement " ?\\(Procedure\\|Function\\)?<\\)") nil t)
;; Templates: <a href="long_views.htm#sthref1317"><span class="secnum">5.17</span> DBA_WM_SYS_PRIVS</a>
;;            <a href="w_htp.htm#sthref12606">APPLETOPEN Procedure</a>
;;            <a href="w_htp.htm#sthref12606">SUBMIT Function</a>
;;            <a href="d_chngnt.htm#sthref1446">SYS.CHNF$_DESC Object Type</a>        
        (if (re-search-forward (concat "\\(\">\\([0-9]+ \\)?"
                                       "\\(<span.*/span> \\)?"
                                       "\\(sys.\\)?"
                                       sql-statement
                                       " ?\\(\\(\\(Member \\|Static \\)?Procedures?\\|Functions?\\)\\|\\(Table\\|Object\\)? ?Type\\|Functions? and Procedures?\\|View\\)?<\\)") nil t)
            (progn
              (setq end (match-beginning 1))
              (re-search-backward "\\(<a href=\"\\)")
              (setq beg (match-end 1))
              (setq found t))
          ))
      (setq toc (cdr toc))
      (setq dir (cdr dir))
      )
    (if found
        (progn
          (setq url (concat sql-oracle-db-documentation-url directory "/"
                            (buffer-substring beg end)))
          (if prefix-arg
              (w3m url)
	    (if (eq system-type 'cygwin)
		(browse-url (replace-regexp-in-string "file:///" (concat "file:///" sql-oracle-db-documentation-drive "//") url))
	      (browse-url url))
            )
          )
      (message "Sorry no documentation available for %s" sql-statement)
      )
    )
  )

(defun sql-oracle-trim (s)
  "Remove whitespace at beginning and end of string."
  (if (string-match "\\`[ \t\n\r]+" s) (setq s (replace-match "" t t s)))
  (if (string-match "[ \t\n\r]+\\'" s) (setq s (replace-match "" t t s)))
  s)

(defun sql-oracle-send-region-to-shell (beg end)
  "Send the current region to a shell process for execution. If process
does not exist create one."
  (interactive "r")
  (let ((process)
        (this-buffer)
        (cmd (buffer-substring beg end))
        )
    (setq this-buffer (current-buffer))
    (or (setq process (get-buffer-process "*shell*")) ; look for process 
        (setq process (get-buffer-process (shell))) ; or create process 
        (error "Unable to create SHELL session."))
    (set-buffer this-buffer)
    (switch-to-buffer-other-window (process-buffer process))
    (goto-char (point-max))
    (recenter 0)
    (insert (format (concat "\n                                    " sqlplus-output-message " '%s':\n")
                    (buffer-name this-buffer)))
    (set-marker (process-mark process) (point))
    (comint-send-string process (sql-oracle-trim cmd))
    (comint-send-string process "\n")
    (switch-to-buffer-other-window this-buffer)
    )
  )

(defun sql-oracle-beautify-sql (start end)
  "Replace the sql statement in the region with beautifyd version."
  (interactive "r")
  (write-region start end (concat (getenv "TMP") "/sqlToConvert$$.sql"))
  (delete-region start end)
  (call-process "sqlformatter.bat" nil t)
  (sql-oracle-kill-entire-line -2)
  (kill-line)
  )

(defun sql-oracle-goto-info-page ()
  "Read documentation for SQL Oracle Mode in the info system."
  (interactive)
  (require 'info)
  (Info-goto-node "(sql-oracle-mode)"))

(defun sqlplus-scroll-left ()
 "Scroll sqlplus buffer left"
 (interactive)
 (scroll-left 2 t)
)

(defun sqlplus-scroll-right ()
 "Scroll sqlplus buffer right"
 (interactive)
 (scroll-right 2 t)
)

(defun sqlplus-scroll-up ()
 "Scroll sqlplus buffer up"
 (interactive)
 (scroll-up 2)
)

(defun sqlplus-scroll-down ()
 "Scroll sqlplus buffer down"
 (interactive)
 (scroll-down 2)
)

(defun sqlplus-scroll-up-other-window ()
  "Scroll sqlplus buffer up"
  (interactive)
  (scroll-other-window 2))

(defun sqlplus-scroll-down-other-window ()
  "Scroll sqlplus buffer down"
  (interactive)
  (scroll-other-window-down 2))

(defun sqlplus-scroll-left-other-window ()
  "Scroll sqlplus buffer left"
  (interactive)
  (let ((this-buffer))
    (setq this-buffer (current-buffer))
    (switch-to-buffer-other-window "*sqlplus*")
    (scroll-left 2 t)
;    (switch-to-buffer this-buffer)
    ))

(defun sqlplus-scroll-right-other-window ()
  "Scroll sqlplus buffer right"
  (interactive)
  (let ((this-buffer))
    (setq this-buffer (current-buffer))
    (switch-to-buffer-other-window "*sqlplus*")
    (scroll-right 2 t)
;    (switch-to-buffer this-buffer)
    ))

(if (featurep 'xemacs) ;; for XEmacs

    (progn

      (defun sql-oracle-backward-codeblock ()
	"Move to beginning of next codeblock after an empty line"
	(interactive "_")
	;; TODO: skip also comments starting with --
	(while (and (looking-at "^$")
		    (not (bobp)))
	  (backward-char 1))
	(if (re-search-backward "^\n" (point-min) t)
	    nil
	  (goto-char (point-min))
	  ))

      (defun sql-oracle-forward-codeblock ()
	"Move to end of next codeblock"
	(interactive "_")
	(sql-oracle-skip-whitespace)
	(re-search-forward "^$" (point-max) t)
	)

      (defun sql-oracle-mark-codeblock ()
	"Put mark at the beginning of this codeblock, point at end.
The codeblock marked is the one that contains point or is before point."
	(interactive)
	(push-mark (point))
	(sql-oracle-backward-codeblock)
	(if (not (bobp))
	    (forward-char 1))
	(push-mark (point) nil t)
	(sql-oracle-forward-codeblock)
	)

      )

  (progn

    (defun sql-oracle-backward-codeblock ()
      "Move to beginning of next codeblock after an empty line"
      (interactive)
      ;; TODO: skip also comments starting with --
      (while (and (looking-at "^$")
		  (not (bobp)))
	(backward-char 1))
      (set-mark (point))
      (if (re-search-backward "^\n" (point-min) t)
	  nil
	(goto-char (point-min))
	)
      (forward-line 1)
      )

    (defun sql-oracle-forward-codeblock ()
      "Move to end of next codeblock"
      (interactive)
      (sql-oracle-skip-whitespace)
      (if (not (region-active-p))
	  (set-mark (point)))
      (re-search-forward "^$" (point-max) t)
      )

    (defun sql-oracle-mark-codeblock ()
      "Put mark at the beginning of this codeblock, point at end.
The codeblock marked is the one that contains point or is before point."
      (interactive)
      (sql-oracle-forward-codeblock)
      (sql-oracle-backward-codeblock)
      (exchange-point-and-mark)
      )

    )

  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions from sql-mode

(defun sql-oracle-skip-whitespace ()
  "Search forward for the first character that isn't a SPACE, TAB or NEWLINE."
  (interactive)
  (while (looking-at "[ \t\n]")
    (forward-char 1)))

(defun sql-oracle-kill-line-1 (arg entire-line)
  (kill-region (if entire-line
		   (save-excursion
		     (beginning-of-line)
		     (point))
		 (point))
	       ;; Don't shift point before doing the delete; that way,
	       ;; undo will record the right position of point.
;; FSF
;	       ;; It is better to move point to the other end of the kill
;	       ;; before killing.  That way, in a read-only buffer, point
;	       ;; moves across the text that is copied to the kill ring.
;	       ;; The choice has no effect on undo now that undo records
;	       ;; the value of point from before the command was run.
;              (progn
	       (save-excursion
		 (if arg
		     (forward-line (prefix-numeric-value arg))
		   (if (eobp)
		       (signal 'end-of-buffer nil))
		   (if (or (looking-at "[ \t]*$")
			   (or entire-line
			       (and kill-whole-line (bolp))))
		       (forward-line 1)
		     (end-of-line)))
		 (point))))

(defun sql-oracle-kill-entire-line (&optional arg)
  "Kill the entire line.
With prefix argument, kill that many lines from point.  Negative
arguments kill lines backward.

When calling from a program, nil means \"no arg\",
a number counts as a prefix arg."
  (interactive "*P")
  (sql-oracle-kill-line-1 arg t))

(defun sql-oracle-sql-read-password (prompt &optional default)
  "Read a password from the user. Echos a * for each character typed.
End with RET, LFD, or ESC. DEL or C-h rubs out.  ^U kills line.
Optional DEFAULT is password to start with."
  (let ((pass (if default default ""))
	(c 0)
	(echo-keystrokes 0)
	(cursor-in-echo-area t))
    (while (and (/= c ?\r) (/= c ?\n) (/= c ?\e))
      (message "%s%s"
	       prompt
	       (make-string (length pass) ?*))
      (setq c (read-char))
      (if (= c ?\C-u)
	  (setq pass "")
	(if (and (/= c ?\b) (/= c ?\177))
	    (setq pass (concat pass (char-to-string c)))
	  (if (> (length pass) 0)
	      (setq pass (substring pass 0 -1))))))
    (message nil)
    (sit-for 0)
    (substring pass 0 -1)))

(defun sql-oracle-sql-previous-word (&optional count)
  "Return the word before the word at point as a string.
The search is bounded to the current line.
Optional argument COUNT specifies how many words to go backwards."
  (interactive "p")
  (setq count (or count 1))
  (save-excursion
    (let  ((pw nil))
      (while (< 0 count)
	(setq pw (sql-oracle-sql-previous-word-1))
	(setq count (1- count))
	(or pw (setq count 0)))
      (cond
       ((stringp pw)
	pw)
       ((null pw)
	"")
       (t
	(current-word))))))
  
(defun sql-oracle-sql-previous-word-1 ()
  (cond
   ((char-equal (preceding-char) ?,)
    (forward-char -1)
    ",")
   ((char-equal (preceding-char) ?=)
    (forward-char -1)
    "=")
   ((char-equal (preceding-char) ?.) ; v.z.
    (forward-char -1)
    ".")
   ((char-equal (preceding-char) ?\() ; v.z.
    (forward-char -1)
    "(")
   (t
    (re-search-backward " \\|\n" nil t)
    (cond
     ((char-equal (preceding-char) ?,)
      (forward-char -1)
      ",")
     ((char-equal (preceding-char) ?=)
      (forward-char -1)
      "=")
     (t
      (if (forward-word -1)
	  t
	nil))))))

(defun sql-oracle-sql-get-keywords ()
  "Set the variable `sql-oracle-sql-keyword-list' to current sql keywords."
  (let ((keys sql-oracle-sql-keywords))
    (while keys
      (setq sql-oracle-sql-keyword-list (cons (cons (car keys) "1") sql-oracle-sql-keyword-list))
      (setq keys (cdr keys)))))

(defun sql-oracle-sql-get-operators ()
  "Set the variable `sql-oracle-sql-operator-list' to current sql operators."
  (let ((opers sql-oracle-sql-operators))
    (while opers
      (setq sql-oracle-sql-operator-list (cons (cons (car opers) "1") sql-oracle-sql-operator-list))
      (setq opers (cdr opers)))))

(defun sql-oracle-sql-dynamic-list-completions (completions)
  "List in help buffer sorted COMPLETIONS.
Typing anything but `s' flushes the help buffer."
  (let ((conf (current-window-configuration)))
    (with-output-to-temp-buffer " *Completions*"
      (display-completion-list (sort completions 'string-lessp)))
    (and sql-oracle-sql-xemacs
	 (not sql-oracle-sql-xemacs-19-12)
	 (progn
	   (set-buffer " *Completions*")
	   (goto-char (point-min))
	   (set-syntax-table sql-oracle-mode-syntax-table)
	   (forward-line 1)
	   (skip-chars-forward "[ \t\n]")
	   (let ((start (point)))
	     (while (forward-word 1)
	       (let ((e (make-extent start (point))))
		 (set-extent-property e 'highlight t)
		 (skip-chars-forward "[ \t\n]")
		 (setq start (point)))))))
    (sql-oracle-sql-restore-window-config conf)))

(defun sql-oracle-sql-restore-window-config (conf &optional message)
  (message "%s" (or message
		    "Press `s' to save completions, anything else to flush."))
  (sit-for 0)
  (setq sql-temp-string nil)
  (if (if (fboundp 'next-command-event)
          ;; lemacs
          (let ((ch (next-command-event)))
            (if (eq (event-to-character ch) ?s)
                t
	      (progn (if (and (button-event-p ch) (eq (event-button ch) 2))
			 (setq sql-temp-string (save-excursion
						 (mouse-set-point ch)
						 (current-word)))
		       (setq unread-command-event ch))
		     nil)))
	;; v19 FSFmacs
	(let ((ch (read-event)))
	  (if (eq ch ?s)
	      t
	    (progn (setq unread-command-events (list ch))
		   nil))))
      nil
;      (message nil)
    (set-window-configuration conf)
    (and sql-temp-string
	 (progn
	   (or (eq (preceding-char) ? )
	       (eq (preceding-char) ?,)
	       (eq (preceding-char) ?.)
	       (eq (preceding-char) ?=)
	       (backward-kill-word 1))
	   (insert sql-temp-string " "))))
  (message nil))

(defun sql-oracle-sql-get-tables ()
  "Set the variable `sql-oracle-sql-table-list' to the tables in the current database."
  (interactive)
  (setq sql-oracle-sql-modified-cache t)
  (message "Creating table completion list... (querying database...)")
  (let* ((the-old-window (selected-window))
	 (the-old-buffer (current-buffer))
	 (the-old-point (point))
	 (temp-buffer (get-buffer-create " SQL-TEMP"))
	 (command (concat (if sql-oracle-sql-database (concat "use " sql-oracle-sql-database "\ngo\n") "")
			  sql-oracle-sql-get-tables-command))
	 (sql-oracle-sql-getting-completions t)
	 (sql-oracle-sql-completion-buffer temp-buffer)
	 (sql-oracle-sql-evaluation-method 'foreground))
    (setq sql-old-point (point))
    (set-buffer temp-buffer)
    (erase-buffer)
    (set-buffer the-old-buffer)
    (sql-oracle-sql-evaluate command sql-oracle-sql-server sql-oracle-sql-user sql-oracle-sql-password sql-oracle-sql-batch-command-switches
		  sql-oracle-sql-matching-buffer t)
    (message "Creating table completion list... (parsing results...)")
    (set-buffer temp-buffer)
    (goto-char (point-min))
    (and (looking-at sql-oracle-sql-error-regexp)
	 (progn
	   (kill-buffer temp-buffer)
	   (error "Error parsing tables.")))
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
    (let ((new-table-list nil))
      (while (re-search-forward sql-oracle-sql-word-regexp nil t)
	(setq new-table-list
	      (cons (cons (match-string 0) "1") new-table-list)))
      (kill-buffer temp-buffer)
      (set-buffer the-old-buffer)
      (setq sql-oracle-sql-table-list new-table-list))
    (select-window the-old-window)
    (goto-char the-old-point)
    (message "Creating table completion list... done")))
  
(defun sql-oracle-sql-get-stored-procedures ()
  "Set the variable `sql-oracle-sql-stored-procedure-list' to the stored procedures
in the current database."
  (interactive)
  (setq sql-oracle-sql-modified-cache t)
  (message "Creating stored procedure completion list... (querying database...)")
  (let* ((the-old-window (selected-window))
	 (the-old-buffer (current-buffer))
	 (the-old-point (point))
	 (temp-buffer (get-buffer-create " SQL-TEMP"))
	 (command (concat (if sql-oracle-sql-database (concat "use " sql-oracle-sql-database "\ngo\n") "")
			  sql-oracle-sql-get-stored-procedures-command))
	 (sql-oracle-sql-getting-completions t)
	 (sql-oracle-sql-completion-buffer temp-buffer)
	 (sql-oracle-sql-evaluation-method 'foreground))
    (setq sql-old-point (point))
    (set-buffer temp-buffer)
    (erase-buffer)
    (set-buffer the-old-buffer)
    (sql-oracle-sql-evaluate command sql-oracle-sql-server sql-oracle-sql-user sql-oracle-sql-password sql-oracle-sql-batch-command-switches
		  sql-oracle-sql-matching-buffer t)
    (message "Creating stored procedure completion list... (parsing results...)")
    (set-buffer temp-buffer)
    (goto-char (point-min))
    (and (looking-at sql-oracle-sql-error-regexp)
	 (progn
	   (kill-buffer temp-buffer)
	   (error "Error parsing stored procedures.")))
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
      (goto-char (point-max))
      (goto-char (point-min))
      (kill-line 6)))
    (let ((new-stored-procedure-list nil))
      (while (re-search-forward sql-oracle-sql-word-regexp nil t)
	(setq new-stored-procedure-list
	      (cons (cons (match-string 0) "1") new-stored-procedure-list)))
      (kill-buffer temp-buffer)
      (set-buffer the-old-buffer)
      (setq sql-oracle-sql-stored-procedure-list new-stored-procedure-list))
    (select-window the-old-window)
    (goto-char the-old-point)
    (message "Creating stored procedure completion list... done")))

(defun sql-oracle-sql-evaluate (string server user password switches output-buffer
				       silent)
  "Evaluate STRING on SQL Server SERVER using login USER and PASSWORD.
If SWITCHES is non-nil, it is passed as a command-line argument to sql-oracle-sql-command.
Output from the evaluation is put in OUTPUT-BUFFER.

Method of evaluation is determined by the variable `sql-oracle-sql-evaluation-method'."
  (cond
   ((eq sql-oracle-sql-evaluation-method 'foreground)
    (sql-oracle-sql-evaluate-foreground string server user password switches
			     output-buffer)
    (or silent (sql-finish-evaluating-buffer output-buffer)))
   ((eq sql-oracle-sql-evaluation-method 'background)
    (sql-oracle-sql-evaluate-background string server user password switches
			     output-buffer))
   (t
    (error "Unknown evaluation method."))))

(defun sql-oracle-sql-evaluate-foreground (string server user password switches
				       output-buffer)
  "Evaluate STRING on SQL Server SERVER using login USER and PASSWORD.
If SWITCHES is non-nil, it is passed as a command-line argument to sql-oracle-sql-command.
Output from the evaluation is put in OUTPUT-BUFFER."
  (if sql-oracle-sql-stay-logged-in
      (sql-oracle-sql-process-send-string (get-buffer-process output-buffer) string)
    (let ((temp-eval-buffer (get-buffer-create " SQL-TEMP-EVAL")))
      (if sql-oracle-sql-getting-completions
	  (setq output-buffer sql-oracle-sql-completion-buffer))
      (set-buffer temp-eval-buffer)
      (erase-buffer)
      (insert string)
      (cond
       ((eq sql-oracle-sql-dataserver-type 'sybase)
	(if switches
	    (call-process-region (point-min) (point-max) sql-oracle-sql-command nil
				 output-buffer t 
				 (concat "-w" (int-to-string sql-max-frame-width))
				 (concat "-U" user) (concat "-P" password) 
				 (concat "-S" server)
				 switches)
	  (call-process-region (point-min) (point-max) sql-oracle-sql-command nil
			       output-buffer t 
			       (concat "-w" (int-to-string sql-max-frame-width))
			       (concat "-U" user) (concat "-P" password) 
			       (concat "-S" server))))
       ((eq sql-oracle-sql-dataserver-type 'oracle)
	(if switches
	    (call-process-region (point-min) (point-max) sql-oracle-sql-command nil
				 output-buffer t
                                 switches
				 (concat user "/" password "@" server))
	  (call-process-region (point-min) (point-max) sql-oracle-sql-command nil
			       output-buffer t
			       (concat user "/" password "@" server)))))
      (kill-buffer temp-eval-buffer))))

(defun sql-oracle-sql-evaluate-background (string server user password switches
				       output-buffer)
  "Send the contents of the buffer asynchronously to an SQL process.
With prefix ARG, send the contents of the region in the current buffer
to an SQL process.

On entry to this function, the hook variable `sql-oracle-sql-evaluate-buffer-hook'
is run with no args, if that variable is bound and has a non-nil value."
  (interactive "P")
  (if sql-oracle-sql-stay-logged-in
      (sql-oracle-sql-process-send-string (get-buffer-process output-buffer) string)
    (let ((command
           (cond
            ((eq sql-oracle-sql-dataserver-type 'oracle)
             (concat sql-oracle-sql-command " "
                     "-S "
                     user "/" password "@" server
                     " @" "/tmp/SQL_MODE_TEMP_FILE"))
            (t
             (concat sql-oracle-sql-command " "
                     " -w" (int-to-string sql-max-frame-width)
                     " -U" user
                     " -P" password
                     " -S" server
                     " < " "/tmp/SQL_MODE_TEMP_FILE")))))
      (write-region string nil
                    (cond
                     ((eq sql-oracle-sql-dataserver-type 'oracle)
                      "/tmp/SQL_MODE_TEMP_FILE.sql")
                     (t
                      "/tmp/SQL_MODE_TEMP_FILE")
                     nil 1))
      ;; TODO
      ;; Append exit in case of sqlplus to avoid running sqlplus command
;      (set-buffer batch-buffer)
      (setq sql-process (start-process-shell-command (downcase mode-name)
						     output-buffer
						     command))
      (set-process-sentinel sql-process 'sql-sentinel)
      (setq modeline-process
	    (concat ": " (symbol-name (process-status sql-process))))
;      (set-process-filter sql-process 'sql-filter)
      (set-marker (process-mark sql-process) (point) sql-oracle-sql-matching-buffer)
      (setq sql-oracle-sql-query-in-progress (cons sql-process sql-oracle-sql-query-in-progress)))))

(defun sql-oracle-sql-process-send-string (process string)
  (if (or (not process)
	  (not (processp process))
	  (not (eq (process-status process) 'run)))
      (setq process (sql-log-in)))
  (save-excursion
    (goto-char (point-min))
    (setq sql-go-count 1)
    (while (re-search-forward "\\<go\n" nil t)
      (setq sql-go-count (1+ sql-go-count))))
  (process-send-string process string)
  (setq sql-oracle-process-busy t)
  (if (eq sql-oracle-sql-evaluation-method 'foreground)
;      (while sql-oracle-process-busy
;	(accept-process-output))
      (accept-process-output nil 20)
    ))

(defun sql-oracle-sql-get-column-alias ()
  "Get the alias of the current column, if any."
  (interactive)
  (let* ((space-point (save-excursion (search-backward " " nil t) (point)))
	 (dot (save-excursion (search-backward "." nil t))))
    (if (and dot (> dot space-point))
	(buffer-substring (1+ space-point) dot)
      nil)))

(defun sql-oracle-sql-get-table-name (&optional alias)
  "Get the name of the table to do the completion.

If optional ALIAS is non-nil look for the table with that alias."
  (if alias
      (save-excursion
	(re-search-backward sql-oracle-sql-table-prefix-regexp nil t)
	(re-search-forward (concat " " alias "\\>"))
	(forward-word -2)
	(current-word))
    (save-excursion
      (end-of-line)
      (re-search-backward sql-oracle-sql-table-prefix-regexp nil t)
      (forward-word 2)
      (cond
       ((eq sql-oracle-sql-dataserver-type 'oracle)
        (if (looking-at "[ \t]*\\.[ \t]*")
            (forward-word 1))))
       (let ((table (current-word)))
         (if (string-equal table "from")
             (or sql-oracle-sql-last-table "")
           (if (and sql-oracle-sql-last-table
                    (or (string-equal table "")
                        (and sql-oracle-sql-table-list
                             (not (assoc table sql-oracle-sql-table-list)))))
               sql-oracle-sql-last-table
             table))))))

(defun sql-oracle-sql-get-column-name ()
  "Get the name of the column to do the completion."
  (save-excursion
    (re-search-backward sql-oracle-sql-operator-regexp nil t)
    (sql-oracle-sql-previous-word)))
;  (if (eq (preceding-char) ? )
;      (current-word)
;    (sql-oracle-sql-previous-word 2)))

(defun sql-oracle-sql-get-columns (&optional table)
  "Set the variable `sql-oracle-sql-column-list' to the columns in the current database.

If optional TABLE is non-nill, use that string as the table to get the columns
from.  Otherwise, scan backwards looking for something that looks like a table
name."
  (interactive)
  (setq sql-oracle-sql-modified-cache t)
  (message "Creating column completion list... (querying database...)")
  (let* ((the-old-window (selected-window))
	 (the-old-buffer (current-buffer))
	 (the-old-point (point))
	 (temp-buffer (get-buffer-create " SQL-TEMP"))
	 (table-name (or table (sql-oracle-sql-get-table-name)))
	 (command (concat (if sql-oracle-sql-database (concat "use " sql-oracle-sql-database "\ngo\n") "")
			  sql-oracle-sql-get-columns-command-prefix
                          (cond
                           ((eq sql-oracle-sql-dataserver-type 'oracle)
                            (upcase table-name))
                           (t
                            table-name))
			  sql-oracle-sql-get-columns-command-suffix))
	 (sql-oracle-sql-getting-completions t)
	 (sql-oracle-sql-completion-buffer temp-buffer)
	 (sql-oracle-sql-evaluation-method 'foreground))
    (setq sql-old-point (point))
    (set-buffer temp-buffer)
    (erase-buffer)
    (set-buffer the-old-buffer)
    (sql-oracle-sql-evaluate command sql-oracle-sql-server sql-oracle-sql-user sql-oracle-sql-password sql-oracle-sql-batch-command-switches
		  sql-oracle-sql-matching-buffer t)
    (message "Creating column completion list... (parsing results...)")
    (set-buffer temp-buffer)
    (goto-char (point-min))
    (and (looking-at sql-oracle-sql-error-regexp)
	 (progn
	   (kill-buffer temp-buffer)
	   (error "Error parsing columns.")))
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
    (let ((new-column-list nil)
          (column nil)
          (type nil))
      (while (re-search-forward sql-oracle-sql-word-regexp nil t)
	(setq column (match-string 0))
	(re-search-forward sql-oracle-sql-word-regexp nil t)
	(setq type (match-string 0))
	(setq new-column-list
	      (cons (cons column (cond
				  ((member type sql-oracle-sql-string-column-types)
				   1)
				  ((member type sql-oracle-sql-ignore-column-types)
				   2)
				  (t
				   0)))
		    new-column-list)))
	(kill-buffer temp-buffer)
	(set-buffer the-old-buffer)
	(setq sql-oracle-sql-column-list (cons (cons table-name new-column-list)
				    sql-oracle-sql-column-list)))
    (select-window the-old-window)
    (goto-char the-old-point)
    (message "Creating column completion list... done")))

(defun sql-oracle-sql-get-values (&optional table column)
  "Set the variable `sql-oracle-sql-value-list' to the values in the current database.

If optional TABLE is non-nill, use that string as the table to get the values
from.  Otherwise, scan backwards looking for something that looks like a table
name."
  (interactive)
  (setq sql-oracle-sql-modified-cache t)
  (message "Creating value completion list... (querying database...)")
  (let* ((the-old-window (selected-window))
	 (the-old-buffer (current-buffer))
	 (the-old-point (point))
	 (temp-buffer (get-buffer-create " SQL-TEMP"))
	 (table-name (or table (sql-oracle-sql-get-table-name)))
	 (column-name (or column (sql-oracle-sql-get-column-name)))
	 (command (concat (if sql-oracle-sql-database (concat "use " sql-oracle-sql-database "\ngo\n") "")
			  sql-oracle-sql-get-values-command-prefix
			  column-name
			  sql-oracle-sql-get-values-command-middle
			  table-name
			  sql-oracle-sql-get-values-command-suffix))
	 (sql-oracle-sql-getting-completions t)
	 (sql-oracle-sql-completion-buffer temp-buffer)
	 (sql-oracle-sql-evaluation-method 'foreground)
	 (column-list (cdr (assoc table-name sql-oracle-sql-column-list))))
    (setq sql-old-point (point))
    (set-buffer temp-buffer)
    (erase-buffer)
    (set-buffer the-old-buffer)
    (sql-oracle-sql-evaluate command sql-oracle-sql-server sql-oracle-sql-user sql-oracle-sql-password
		  sql-oracle-sql-batch-command-switches sql-oracle-sql-matching-buffer t)
    (message "Creating value completion list... (parsing results...)")
    (set-buffer temp-buffer)
    (goto-char (point-min))
    (and (looking-at sql-oracle-sql-error-regexp)
	 (progn
	   (kill-buffer temp-buffer)
	   (error "Error parsing values.")))
    (goto-char (point-max))
    (cond
     ((eq sql-oracle-sql-dataserver-type 'oracle)
      (goto-char (point-min))
      (kill-line 3))
     (t
      (forward-line -2)
      (kill-line 3)
      (goto-char (point-min))
      (kill-line 2)))
    (let* ((new-value-list nil)
	   (column-type (eq 0 (cdr (assoc column-name column-list)))))
      (while (re-search-forward sql-oracle-sql-line-regexp nil t)
	(let* ((value (match-string 1))
	       (no-quotes (or column-type
			      (string-equal "NULL" value)
			      (string-match "\"\\|'" value)
			      (string-equal "null" value)
			      (string-match "\(.*\)" value))))
	  (setq value (if no-quotes
			  value
                        (cond
                         ((eq sql-oracle-sql-dataserver-type 'oracle)
                          (concat "'" value "'"))
                         (t
                          (concat "\"" value "\"")))
                        ))
          (setq new-value-list
                (cons (cons value "1") new-value-list))))
          (kill-buffer temp-buffer)
          (set-buffer the-old-buffer)
          (setq sql-oracle-sql-value-list (cons (cons (cons table-name
                                                 column-name)
                                           new-value-list)
                                     sql-oracle-sql-value-list)))
        (select-window the-old-window)
        (goto-char the-old-point)
        (message "Creating value completion list... done")))

(defun sql-oracle-sql-get-users ()
  "Set the variable `sql-oracle-sql-user-list' to the users
in the current database."
  (interactive)
  (setq sql-oracle-sql-modified-cache t)
  (message "Creating user completion list... (querying database...)")
  (let* ((the-old-window (selected-window))
	 (the-old-buffer (current-buffer))
	 (the-old-point (point))
	 (temp-buffer (get-buffer-create " SQL-TEMP"))
	 (command (concat (if sql-oracle-sql-database (concat "use " sql-oracle-sql-database "\ngo\n") "")
			  sql-oracle-sql-get-users-command))
	 (sql-oracle-sql-getting-completions t)
	 (sql-oracle-sql-completion-buffer temp-buffer)
	 (sql-oracle-sql-evaluation-method 'foreground))
    (setq sql-old-point (point))
    (set-buffer temp-buffer)
    (erase-buffer)
    (set-buffer the-old-buffer)
    (sql-oracle-sql-evaluate command sql-oracle-sql-server sql-oracle-sql-user sql-oracle-sql-password sql-oracle-sql-batch-command-switches
		  sql-oracle-sql-matching-buffer t)
    (message "Creating user completion list... (parsing results...)")
    (set-buffer temp-buffer)
    (goto-char (point-min))
    (and (looking-at sql-oracle-sql-error-regexp)
	 (progn
	   (kill-buffer temp-buffer)
	   (error "Error parsing users.")))
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
    (let ((new-user-list nil))
      (while (re-search-forward sql-oracle-sql-word-regexp nil t)
	(setq new-user-list
	      (cons (cons (match-string 0) "1") new-user-list)))
      (kill-buffer temp-buffer)
      (set-buffer the-old-buffer)
      (setq sql-oracle-sql-user-list new-user-list))
    (select-window the-old-window)
    (goto-char the-old-point)
    (message "Creating user completion list... done")))

(defun sql-oracle-sql-popup-continuation-menu (menu title)
  "Pop up a menu with more... entries if overflow."
  (interactive)
  (let* ((count 0)
	 (split-menu
	  (mapcar
	   (function
	    (lambda (sublist)
	      (setq count (1+ count))
	      (cons (format "%s" (sql-oracle-sql-index-sublist sublist count))
		    (mapcar
		     (function
		      (lambda (menu)
			(vector (format "%s" (car menu))
				(list 'sql-oracle-sql-replace-word
				      (car menu))
				t)))
		     sublist))))
	   (sql-oracle-sql-split menu 20))))
    (popup-menu (cons title split-menu))))

(defun sql-oracle-sql-popup-table-list ()
  "Pop up a menu displaying the tables to choose from."
  (interactive)
  (or sql-oracle-sql-table-list
      (sql-oracle-sql-get-tables))
  (sql-oracle-sql-popup-continuation-menu sql-oracle-sql-table-list
			       (concat "Tables" (or sql-oracle-sql-database ""))))

(defun sql-oracle-sql-popup-column-list ()
  "Pop up a menu displaying the columns to choose from."
  (interactive)
  (let* ((alias (sql-oracle-sql-get-column-alias))
	 (table-name (sql-oracle-sql-get-table-name alias)))
    (if (not (assoc table-name sql-oracle-sql-column-list))
	(sql-oracle-sql-get-columns table-name))
    (sql-oracle-sql-popup-continuation-menu (cdr (assoc table-name sql-oracle-sql-column-list))
				 (concat "Columns for " table-name))))
	  
(defun sql-oracle-sql-popup-value-list ()
  "Pop up a menu displaying the values to choose from."
  (interactive)
  (let ((table-name (sql-oracle-sql-get-table-name))
	(column-name (sql-oracle-sql-get-column-name)))
    (if (not (assoc (cons table-name column-name) sql-oracle-sql-value-list))
	(sql-oracle-sql-get-values table-name column-name))
    (sql-oracle-sql-popup-continuation-menu (cdr (assoc (cons table-name column-name)
					     sql-oracle-sql-value-list))
				 (concat "Values for "
					 table-name
					 "."
					 column-name))))
	  
(defun sql-oracle-sql-popup-stored-procedure-list ()
  "Pop up a menu displaying the stored-procedures to choose from."
  (interactive)
  (or sql-oracle-sql-stored-procedure-list
      (sql-oracle-sql-get-stored-procedures))
  (sql-oracle-sql-popup-continuation-menu sql-oracle-sql-stored-procedure-list "Stored Procedures"))

(defun sql-oracle-sql-popup-user-list ()
  "Pop up a menu displaying the stored-procedures to choose from."
  (interactive)
  (or sql-oracle-sql-user-list
      (sql-oracle-sql-get-users))
  (sql-oracle-sql-popup-continuation-menu sql-oracle-sql-user-list "Users"))

(defun sql-oracle-sql-popup-keyword-list ()
  "Pop up a menu displaying the stored-procedures to choose from."
  (interactive)
  (or sql-oracle-sql-keyword-list
      (sql-oracle-sql-get-keywords))
  (sql-oracle-sql-popup-continuation-menu sql-oracle-sql-keyword-list "Keywords"))

(defun sql-oracle-sql-popup-operator-list ()
  "Pop up a menu displaying the stored-procedures to choose from."
  (interactive)
  (or sql-oracle-sql-operator-list
      (sql-oracle-sql-get-operators))
  (sql-oracle-sql-popup-continuation-menu sql-oracle-sql-operator-list "Operators"))

(defun sql-oracle-sql-split (list n)
  (let ((remain list)
        (result '())
        (sublist '())
        (i 0))
    (while remain
      (or (string-equal (car (car remain)) "
")
	  (setq sublist (cons (car remain) sublist)))
      (setq remain (cdr remain))
      (setq i (1+ i))
      (and (= i n)
           ;; We have finished a sublist
           (progn (setq result (cons sublist result))
                  (setq i 0)
                  (setq sublist '()))))
    ;; There might be a sublist (if the length of LIST mod n is != 0)
    ;; that has to be added to the result list.
    (and sublist
         (setq result (cons sublist result)))
    result))

(defun sql-oracle-sql-index-sublist-1 (sublist ix limit)
  (let ((s1 (substring (car (car sublist)) 0 (min limit ix)))
        (s2 (substring
             (car (nth (1- (length sublist)) sublist))
             0 (min (length (car (nth (1- (length sublist)) sublist))) ix))))
    (cons s1 s2)))

(defun sql-oracle-sql-index-sublist (sublist &rest count)
  (let* ((cmplength 100)
         (limit (length (car (car sublist))))
         (result (sql-oracle-sql-index-sublist-1 sublist cmplength limit))
         (str1 (car result))
         (str2 (cdr result)))
    (while (and (string-equal str1 str2) (< cmplength limit))
      (setq cmplength (1+ cmplength)
            result (sql-oracle-sql-index-sublist-1 sublist cmplength limit)
            str1 (car result)
            str2 (cdr result)))
    (cond ((not (string-equal str1 str2))
           (format "%s ... %s" str1 str2))
          ((< cmplength limit)
           (format "%s" str1))
          (t
           (format "%s ..." str1)))))

(defun sql-oracle-sql-popup-association-menu ()
  "Pop up a menu of all defined associations.
Associations are stored in the variable `sql-oracle-sql-association-alist'."
  (interactive)
  (if (null sql-oracle-sql-association-alist)
      nil
    (cond 
     ((eq major-mode 'sql-batch-mode)
      (popup-menu (sql-make-popup-menu 'sql-batch-mode)))
     ((eq major-mode 'sql-interactive-mode)
      (popup-menu (sql-make-popup-menu 'sql-interactive-mode)))
     ;; v.z.: added support for Oracle
     ((eq major-mode 'sql-oracle-mode)
      (popup-menu 'sql-oracle-mode-menu))
     (t
      (error "You must be in a sql-batch-mode or a sql-interactive-mode buffer.")))))

(defun sql-oracle-sql-clear-cached-data ()
  "Clear the cached table, column, and stored procedure data.
This function sets the value of the variables `sql-oracle-sql-table-list',
`sql-oracle-sql-column-list', `sql-oracle-sql-value-list', `sql-oracle-sql-stored-procedure-list'
and `sql-oracle-sql-user-list' to nil."
  (interactive)
  (setq sql-oracle-sql-table-list nil)
  (setq sql-oracle-sql-column-list nil)
  (setq sql-oracle-sql-value-list nil)
  (setq sql-oracle-sql-stored-procedure-list nil))

(defun sql-oracle-sql-clear-cached-table-data ()
  "Clear the cached table data."
  (interactive)
  (setq sql-oracle-sql-table-list nil))

(defun sql-oracle-sql-clear-cached-column-data ()
  "Clear the caced column data."
  (interactive)
  (setq sql-oracle-sql-column-list nil))

(defun sql-oracle-sql-clear-cached-value-data ()
  "Clear the caced value data."
  (interactive)
  (setq sql-oracle-sql-value-list nil))

(defun sql-oracle-sql-clear-cached-stored-procedure-data ()
  "Clear the cached stored procedure data."
  (interactive)
  (setq sql-oracle-sql-stored-procedure-list nil))

(defun sql-oracle-sql-clear-cached-database-data ()
  "Clear the cached database data."
  (interactive)
  (setq sql-oracle-sql-database-list nil))

(defun sql-oracle-sql-clear-cached-user-data ()
  "Clear the cached user data."
  (interactive)
  (setq sql-oracle-sql-user-list nil))

(defun sql-oracle-sql-load-cache-data ()
  "Load cached information from disk.
Use the file specified by appending the current server and database
to the value of the variable `sql-oracle-sql-cache-data-file-name'."
  (interactive)
  (let* ((cache-file (concat sql-oracle-sql-cache-data-file-name "-" sql-oracle-sql-server
                                 (if (not (eq sql-oracle-sql-dataserver-type 'oracle))
                                     (concat "-" sql-oracle-sql-database)))))
    (if (not (file-exists-p cache-file))
	(if (interactive-p)
	    (error "There is no saved cache file for this server and database")
	  ())
      (or (file-readable-p cache-file)
	  (error "Problems reading file %s" cache-file))
      (load-file cache-file)
      (setq sql-oracle-sql-modified-cache nil))))
  
(defun sql-oracle-sql-save-cache-data (&optional buffer)
  "Save the current cached information to disk for the current buffer.
Use the file specified by appending the current server and database
to the value of the variable `sql-cache-data-file-name'."
  (interactive)
  (and buffer (set-buffer buffer))
  (if (not (or (eq major-mode 'sql-batch-mode)
               (eq major-mode 'sql-oracle-mode))) ; v.z.: Add sql-oracle-mode support
      (if (interactive-p)
	  (error "You must be in a sql-batch-mode buffer.")
	())
    (if (not sql-oracle-sql-modified-cache)
	(if (interactive-p)
	    (message "(No changes need to be saved)")
	  ())
      (message "Saving cache data for %s..." sql-oracle-sql-server)
      (let* ((old-buffer (current-buffer))
	     (cache-file (concat sql-oracle-sql-cache-data-file-name "-" sql-oracle-sql-server
                                 (if (not (eq sql-oracle-sql-dataserver-type 'oracle))
                                     (concat "-" sql-oracle-sql-database))))
	     (cache-buffer (find-file-noselect cache-file))
	     (tables sql-oracle-sql-table-list)
	     (columns sql-oracle-sql-column-list)
	     (values sql-oracle-sql-value-list)
	     (stored-procedures sql-oracle-sql-stored-procedure-list)
	     (users sql-oracle-sql-user-list)
	     (databases sql-oracle-sql-database-list)
	     (old-standard-output standard-output))
	(set-buffer cache-buffer)
	(setq standard-output cache-buffer)
	(erase-buffer)
	(insert "(setq sql-oracle-sql-table-list\n      '")
	(prin1 tables)
	(insert ")\n\n")
	(insert "(setq sql-oracle-sql-column-list\n      '")
	(prin1 columns)
	(insert ")\n\n")
	(insert "(setq sql-oracle-sql-value-list\n      '")
	(prin1 values)
	(insert ")\n\n")
	(insert "(setq sql-oracle-sql-stored-procedure-list\n      '")
	(prin1 stored-procedures)
	(insert ")\n\n")
	(insert "(setq sql-oracle-sql-database-list\n      '")
	(prin1 databases)
	(insert ")\n\n")
	(insert "(setq sql-oracle-sql-user-list\n      '")
	(prin1 users)
	(insert ")\n\n")
	(or (file-directory-p (concat (getenv "HOME") "/.sql-oracle-cache-dir"))
	    (make-directory (concat (getenv "HOME") "/.sql-oracle-cache-dir")))
	(save-buffer)
	(set-buffer old-buffer)
	(setq standard-output old-standard-output)
	(setq sql-oracle-sql-modified-cache nil)
	(message "Saving cache data for %s... done" sql-oracle-sql-server)))))

(defun sql-oracle-sql-replace-word (string)
  "Replace the current word with STRING."
  (interactive)
  (or (char-equal (preceding-char) ? )
      (backward-kill-word 1))
  (or (char-equal (following-char) ? )
      (kill-word 1))
  (insert string)
  (if (looking-at " ")
      (forward-char 1)
    (insert " ")))

(defun sql-oracle-sql-make-popup-menu (mode)
  "Return a menu of all defined associations."
  (cond
   ((eq mode 'sql-batch-mode)
    (cons "SQL Batch Associations"
	  (sql-oracle-sql-make-association-menu sql-association-alist mode)))
   ((eq mode 'sql-interactive-mode)
    (cons "SQL Interactive Associations"
	  (sql-make-association-menu sql-association-alist mode)))
   (t
    nil)))

(provide 'sql-oracle-mode)

;; End of sql-oracle-mode.el
