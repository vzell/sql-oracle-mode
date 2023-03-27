;;; sql-oracle-doc.el --- SQL documentation for sql-oracle and SQL*Plus buffers

;; Copyright (C) 2006, 2007 Dr. Volker Zell <dr.volker.zell@oracle.com>

;; Author: Dr. Volker Zell <dr.volker.zell@oracle.com>
;; Maintainer: Dr. Volker Zell <dr.volker.zell@oracle.com>
;; Created: Wed 11 Jul 10:20:46 2006
;; Version: 1.0
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

(setq sql-command-table
      '(
("alter cluster" . (nil "SQL command" "ALTER CLUSTER command 

PURPOSE: 

    To redefine future storage allocations or to allocate an extent for 
    a cluster. 

SYNTAX: 

ALTER CLUSTER [schema.]cluster 
    [PCTUSED integer] [PCTFREE integer] 
    [SIZE integer [K|M] ] 
    [INITRANS integer] [MAXTRANS integer] 
    [STORAGE storage_clause] 
    [  PARALLEL ( [ DEGREE { integer | DEFAULT } ] 
                  [ INSTANCES { integer | DEFAULT } ] 

                ) 
     | NOPARALLEL ] 
    [  CACHE | NOCACHE  ] 
    [ALLOCATE EXTENT [( [SIZE integer [K|M] ] 
                        [DATAFILE 'filename'] 
                        [INSTANCE integer] )] ] 

where: 

schema 
    is the schema containing the cluster.  If you omit schema, Oracle 
    assumes the cluster is in your own schema. 

cluster 
    is the name of the cluster to be altered. 


SIZE 
    determines how many cluster keys will be stored in data blocks 
    allocated to the cluster.  You can only change the SIZE parameter 
    for an indexed cluster, not for a hash cluster.  For a description 
    of the SIZE parameter, see the CREATE CLUSTER command. 

PCTUSED 
PCTFREE 
INITRANS 
MAXTRANS 
    changes the values of these parameters for the cluster.  See the 

    PCTUSED, PCTFREE, INITRANS, and MAXTRANS parameters of the CREATE 
    TABLE command. 

STORAGE 
    changes the storage characteristics for the cluster.  See the 
    STORAGE clause clause. 

ALLOCATE EXTENT 
    explicitly allocates a new extent for the cluster. 
            SIZE 
                   specifies the size of the extent in bytes.  You can 
                   use K or M to specify the extent size in kilobytes or 

                   megabytes.  If you omit this parameter, Oracle 
                   determines the size based on the values of the 
                   cluster's STORAGE parameters. 
            DATAFILE 
                   specifies one of the data files in the cluster's 
                   tablespace to contain the new extent.  If you omit 
                   this parameter, Oracle chooses the data file. 
            INSTANCE 

                   makes the new extent available to the specified 
                   instance.  An instance is identified by the value of 
                   its initialization parameter INSTANCE_NUMBER.  If you 
                   omit this parameter, the extent is available to all 
                   instances.  Only use this parameter if you are using 
                   Oracle with the Parallel Server option in parallel 

                   mode. 

    Explicitly allocating an extent with this clause does not cause 
    Oracle to evaluate the cluster's storage parameters and determine a 
    new size for the next extent to be allocated.  You can only allocate 
    a new extent for an indexed cluster, not a hash cluster. 

PARALLEL 
    DEGREE specifies the number of query server processes that can scan 
    the cluster in parallel.  Either specify a positive integer or DEFAULT 

    which signifies to use the initialization parameter 
    PARALLEL_DEFAULT_SCANSIZE to estimate the number of query servers to use. 

    INSTANCES specifies the minimum number of instances that need to be 
    available before the cluster can be spread across all available instances 
    of a Parallel Server.  A positive integer specifies the number of 
    instances.  DEFAULT signifies that the parameter 
PARALLEL_MAX_PARTITIONSIZE 

    is used to calculate whether a table is split across all instances' buffer 
    caches. 

NOPARALLEL 
    specifies that queries on this cluster are not performed in parallel 
    by default.  A hint in the query still causes the query to be 
    performed in parallel. 

CACHE 
    specifies that blocks of this cluster are placed on the most recently 
    used end of the LRU list of the buffer cache when the a full table scan 

    is performed. 
    This option is useful for small lookup tables. 

NOCACHE 
    specifies that blocks of the cluster in the buffer cache follow the 
    standard LRU algorithm when a full table scan is performed. 

PREREQUISITES: 

    The cluster must be in your own schema or you must have ALTER ANY 
    CLUSTER system privilege. 

    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 

    must match the cluster's creation label or you must satisfy one of 
    these criteria: 

    * If the cluster's creation label is higher than your DBMS label, 
      you must have READUP and WRITEUP system privileges. 
    * If the cluster's creation label is lower than your DBMS label, you 
      must have WRITEDOWN system privilege. 
    * If the cluster's creation label and your DBMS label are 
      noncomparable, you must have READUP, WRITEUP, and WRITEDOWN system 

      privileges. 

SEE: 
    CREATE CLUSTER, CREATE TABLE, STORAGE clause " nil 'sql-insert-alter-cluster))
("alter database" . (nil "SQL command" "ALTER DATABASE command 

PURPOSE: 

    To alter an existing database in one of these ways: 

    * mount the database 
    * convert an Oracle Version 6 data dictionary when migrating to 
      Oracle7 
    * open the database 
    * choose archivelog or noarchivelog mode for redo log file groups 
    * perform media recovery 
    * add or drop a redo log file group or a member of a redo log file 

      group 
    * rename a redo log file member or a data file 
    * backup the current control file 
    * create a new data file in place of an old one for recovery 
      purposes 
    * take a data file online or offline 
    * enable or disable a thread of redo log file groups 
    * change the database's global name 
    * change the MAC mode 
    * equate the predefined label DBHIGH or DBLOW with an operating 

      system label 

SYNTAX: 

ALTER DATABASE [database] 
    { MOUNT [EXCLUSIVE | PARALLEL] 
    | CONVERT 
    | OPEN [RESETLOGS | NORESETLOGS] 
    | ARCHIVELOG 
    | NOARCHIVELOG 
    | RECOVER recover_clause 
    | ADD LOGFILE [THREAD integer] [GROUP integer] filespec 
                                [, [GROUP integer] filespec] ... 
    | ADD LOGFILE MEMBER 'filename' [REUSE] [, 'filename' [REUSE]] ... 

        TO { GROUP integer 
           | ('filename' [,'filename'] ...) 
           |  'filename' } 
                     [, 'filename' [REUSE] [, 'filename' [REUSE]] ... 
        TO { GROUP integer 
           | ('filename' [, 'filename'] ...) 
           |  'filename' } ] ... 
    | DROP LOGFILE {  GROUP integer 
                   | ('filename' [, 'filename'] ...) 
                   |  'filename' } 

               [,  {  GROUP integer 
                   | ('filename' [,'filename'] ...) 
                   |  'filename' } ] ... 
    | DROP LOGFILE MEMBER 'filename' [, 'filename'] ... 
    | RENAME FILE 'filename' [, 'filename'] ... 
               TO 'filename' [, 'filename'] ... 
    | BACKUP CONTROLFILE TO 'filename' [REUSE] 
    | CREATE DATAFILE 'filename' [, filename] ... 
                  [AS  filespec  [, filespec] ... 

    | DATAFILE 'filename' { ONLINE | OFFLINE [DROP] } 
    | ENABLE [PUBLIC] THREAD integer 
    | DISABLE         THREAD integer 
    | RENAME GLOBAL_NAME TO database[.domain]... 
    | SET { DBMAC {ON | OFF} 
          | DBHIGH = 'text' 
          | DBLOW  = 'text' } 
    | RESET COMPATIBILITY } 

where: 

database 
    identifies the database to be altered.  If you omit database, Oracle 

    alters the database identified by the value of the initialization 
    parameter DB_NAME.  You can only alter the database whose control 
    files are specified by the initialization parameter CONTROL_FILES. 
    Note that the database identifier is not related to the SQL*Net 
    database specification. 

You can only use the following options when the database is not mounted 
by your instance: 

MOUNT 

    mounts the database. 
            EXCLUSIVE 
                   mounts the database in exclusive mode.  This mode 
                   allows the database to be mounted by only one 
                   instance at a time.  You cannot use this option if 
                   another instance has already mounted the database. 
            PARALLEL 
                   mounts the database in parallel mode.  This mode 

                   allows the database to be mounted by multiple 
                   instances concurrently.  You can only use this option 
                   if you are using Oracle with the Parallel Server 
                   option.  You cannot use this option if another option 
                   has mounted the database in exclusive mode. 

    The default is EXCLUSIVE. 

CONVERT 
    completes the conversion of the Oracle Server Version 6 data dictionary. 

    After you use this option, the Version 6 data dictionary no longer 
    exists in the Oracle7 database.  Only use this option when you are 
    migrating to Oracle7.  For more information on using this option, 
    see the Oracle7 Server Migration Guide. 

You can only use the following options when the database is not mounted 
by your instance: 

MOUNT 
    mounts the database. 
            EXCLUSIVE 

                   mounts the database in exclusive mode.  This mode 
                   allows the database to be mounted by only one 
                   instance at a time.  You cannot use this option if 
                   another instance has already mounted the database. 
            PARALLEL 
                   mounts the database in parallel mode.  This mode 
                   allows the database to be mounted by multiple 

                   instances concurrently.  You can only use this option 
                   if you are using Oracle with the Parallel Server 
                   option.  You cannot use this option if another option 
                   has mounted the database in exclusive mode. 

    The default is EXCLUSIVE. 

CONVERT 
    completes the conversion of the Oracle Version 6 data dictionary. 
    After you use this option, the Version 6 data dictionary no longer 

    exists in the Oracle7 database.  Only use this option when you are 
    migrating to Oracle7.  For more information on using this option, 
    see the Oracle7 Server Migration Guide. 

You can only use the following option when your instance has the 
database mounted, but not open: 

OPEN 
    opens the database, making it available for normal use.  You must 
    mount the database before you can open it. 

            RESETLOGS 
                   resets the current log sequence number to 1 and 
                   invalidates all redo entries in the online and 
                   archived redo log files.  You must use this option to 
                   open the database after performing media recovery 
                   with a backup controlfile.  After opening the 
                   database with this option, you should perform a 

                   complete database backup. 
            NORESETLOGS 
                   leaves the log sequence number and redo log files in 
                   their current state. 

    You can only specify these options after performing incomplete 
    media recovery.  In any other case, Oracle uses the NORESETLOGS 
    automatically. 

You can only use the following options when your instance has the 

database mounted in exclusive mode, but not open: 

ARCHIVELOG 
    establishes archivelog mode for redo log file groups.  In this mode, 
    the contents of a redo log file group must be archived before the 
    group can be reused.  This option prepares for the possibility of 
    media recovery.  You can only use this option after shutting down 
    your instance normally or immediately with no errors and then 

    restarting it, mounting the database in exclusive mode. 

NOARCHIVELOG 
    establishes noarchivelog mode for redo log files.  In this mode, the 
    contents of a redo log file group need not be archived so that the 
    group can be reused.  This mode does not prepare for recovery after 
    media failure. 

You can only use the following option when your instance has the 
database mounted in exclusive mode: 


RECOVER 
    performs media recovery.  You only recover the entire database when 
    the database is closed.  You can recover tablespaces or data files 
    when the database is open or closed, provided the tablespaces or 
    data files to be recovered are not being used.  You cannot perform 
    media recovery if you are connected to Oracle through the multi- 
    threaded server architecture.  You can also perform media recovery 

    with the RECOVER SQL*DBA command. 

You can use any of the following options when your instance has the 
database mounted, open or closed, and the files involved are not in use: 

ADD LOGFILE 
    adds one or more redo log file groups to the specified thread, 
    making them available to the instance assigned the thread.  If you 
    omit the THREAD parameter, the redo log file group is added to the 
    thread assigned to your instance.  You need only use the THREAD 

    parameter if you are using Oracle with the Parallel Server option in 
    parallel mode. 

    Each filespec specifies a redo log file group containing one or more 
    members, or copies. 

    You can choose the value of the GROUP parameter for each redo log 
    file group.  Each value uniquely identifies the redo log file group 
    among all groups in all threads and can range from 1 to the 
    MAXLOGFILES value.  You cannot add multiple redo log file groups 

    having the same GROUP value.  If you omit this parameter, Oracle 
    generates its value automatically.  You can examine the GROUP value 
    for a redo log file group through the dynamic performance table. 

ADD LOGFILE MEMBER 
    adds new members to existing redo log file groups.  Each new member 
    is specified by 'filename'.  If the file already exists, it must be 
    the same size as the other group members and you must specify the 

    REUSE option.  If the file does not exist, Oracle creates a file of 
    the correct size.  You cannot add a member to a group if all of the 
    group's members have been lost through media failure. 

    You can specify an existing redo log file group in one of these 
    ways: 
            GROUP parameter 
                   You can specify the value of the GROUP parameter that 
                   identifies the redo log file group. 

            list of filenames 
                   You can list all members of the redo log file group. 
                   You must fully specify each filename according to the 
                   conventions for your operating system. 

DROP LOGFILE 
    drops all members of a redo log file group.  You can specify a redo 
    log file group in the same manners as the ADD LOGFILE MEMBER clause. 
    You cannot drop a redo log file group if all of its members have 

    been lost through media failure. 

DROP LOGFILE MEMBER 
    drops one or more redo log file members.  Each 'filename' must fully 
    specify a member using the conventions for filenames on your 
    operating system. 

    You cannot use this clause to drop all members of a redo log file 
    group that contain valid data.  To perform this operation, use the 
    DROP LOGFILE clause. 

RENAME FILE 

    renames data files or redo log file members.  This clause only 
    renames files in the control file, it does not actually rename them 
    on your operating system.  You must specify each filename using the 
    conventions for filenames on your operating system. 

BACKUP CONTROLFILE 
    backs up the current control file to the specified 'filename'.  If 
    the backup file already exists, you must specify the REUSE option. 


CREATE DATAFILE 
    creates a new data file in place of an old one.  You can use this 
    option to recreate a data file that was lost with no backup.  The 
    'filename' must identify a file that is or was once part of the 
    database.  The filespec specifies the name and size of the new data 
    file.  If you omit the AS clause, Oracle creates the new file with 
    the same name and size as the file specified by 'filename'. 


    Oracle creates the new file in the same state as the old file when 
    it was created.  You must perform media recovery on the new file to 
    return it to the state of the old file at the time it was lost. 

    You cannot create a new file based on the first data file of the 
    SYSTEM tablespace unless the database was created in archivelog 
    mode. 

DATAFILE 
    takes a data file online or offline.  If any other instance has the 

    database open, your instance must also have the database open: 
            ONLINE 
                   brings the data file online. 
            OFFLINE 
                   takes the data file offline. 
            DROP 
                   takes a data file offline when the database is in 
                   noarchivelog mode. 

You can only use the following options when your instance has the 

database open: 

ENABLE 
    enables the specified thread of redo log file groups.  The thread 
    must have at least two redo log file groups before you can enable 
    it. 
            PUBLIC 
                   makes the enabled thread available to any instance 
                   that does not explicitly request a specific thread 
                   with the initialization parameter THREAD. 


    If you omit the PUBLIC option, the thread is only available to the 
    instance that explicitly requests it with the initialization 
    parameter THREAD. 

DISABLE 
    disables the specified thread, making it unavailable to all 
    instances.  You cannot disable a thread if an instance using it has 
    the database mounted. 

RENAME GLOBAL_NAME 
    changes the global name of the database.  The database is the new 

    database name and can be as long as eight bytes.  The optional 
    domains specifies where the database is effectively located in the 
    network hierarchy.  Renaming your database automatically clears all 
    data from the shared pool in the SGA.  However, renaming your 
    database does not change global references to your database from 
    existing database links, synonyms, and stored procedures and 
    functions on remote databases.  Changing such references is the 

    responsibility of the administrator of the remote databases. 

SET 
    changes one of the following for your database: 
            DBMAC 
                   changes the mode in which Trusted Oracle is 
                   configured: 
                       ON 
                           configures Trusted Oracle in DBMS MAC 
                           mode. 
                       OFF 

                           configures Trusted Oracle in OS MAC 
                           mode. 
            DBHIGH 
                   equates the predefined label DBHIGH to the operating 
                   system label specified by 'text'. 
            DBLOW 
                   equates the predefined label DBLOW to the operating 
                   system label specified by 'text'. 

    You must specify labels in the default label format for your 

    session.  Changes made by this option take effect when you next 
    start your instance.  You can only use this clause if you are using 
    Trusted Oracle. 

RESET COMPATIBILITY 
    Issue the ALTER DATABASE RESET COMPATIBILITY command when restarting 
    the database with the COMPATIBILE initialization parameter set to an 
    earlier release. 

PREREQUISITES: 

    You must have ALTER DATABASE system privilege. 


SEE: 
    CREATE DATABASE, RECOVER clause "))
("alter function" . (nil "SQL command" "ALTER FUNCTION command 

PURPOSE: 

    To recompile a stand-alone stored function. 

SYNTAX: 

ALTER FUNCTION [schema.]function 
     COMPILE 

where: 

schema 
    is the schema containing the function.  If you omit schema, Oracle 
    assumes the function is in your own schema. 

function 
    is the name of the function to be recompiled. 

COMPILE 
    causes Oracle to recompile the function.  The COMPILE keyword is 

    required. 

PREREQUISITES: 

    The function must be in your own schema or you must have ALTER ANY 
    PROCEDURE system privilege. 

    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 
    must match the function's creation label or you must satisfy one of 
    these criteria: 

    * If the function's creation label is higher than your DBMS label, 
      you must have READUP and WRITEUP system privileges. 

    * If the function's creation label is lower than your DBMS label, 
      you must have WRITEDOWN system privilege. 
    * If the function's creation label and your DBMS label are 
      noncomparable, you must have READUP, WRITEUP, and WRITEDOWN system 
      privileges. 

SEE: 
    ALTER PROCEDURE, CREATE FUNCTION "))
("alter index" . (nil "SQL command" "ALTER INDEX command 

PURPOSE: 

    To change future storage allocation for data blocks in an index. 

SYNTAX: 

ALTER INDEX [schema.]index 
    [INITRANS integer] [MAXTRANS integer] 
    [STORAGE storage_clause] 

where: 

schema 
    is the schema containing the index.  If you omit schema, Oracle 
    assumes the index is in your own schema. 

index 
    is the name of the index to be altered. 


INITRANS 
MAXTRANS 
    changes the values of these parameters for the index.  See the 
    INITRANS and MAXTRANS parameters of the CREATE TABLE command. 

STORAGE 
    changes the storage parameters for the index.  See the STORAGE 
    clause. 

PREREQUISITES: 

    The index must be in your own schema or you must have ALTER ANY 
    INDEX system privilege. 

    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 

    must match the index's creation label or you must satisfy one of 
    these criteria: 

    * If the index's creation label is higher than your DBMS label, you 
      must have READUP and WRITEUP system privileges. 
    * If the index's creation label is lower than your DBMS label, you 
      must have WRITEDOWN system privilege. 
    * If the index's creation label and your DBMS label are 
      noncomparable, you must have READUP, WRITEUP, and WRITEDOWN system 

      privileges. 

SEE: 
    CREATE INDEX, CREATE TABLE, STORAGE clause "))
("alter package" . (nil "SQL command" "ALTER PACKAGE command 

PURPOSE: 

    To recompile a stored package. 

SYNTAX: 

ALTER PACKAGE [schema.]package 
    COMPILE [PACKAGE | BODY] 

where: 

schema 
    is the schema containing the package.  If you omit schema, Oracle 
    assumes the package is in your own schema. 

package 
    is the name of the package to be recompiled. 

COMPILE 
    recompiles the package specification or body.  The COMPILE keyword 

    is required. 

PACKAGE 
    recompiles the package body and specification. 

BODY 
    recompiles only the package body. 

    The default option is PACKAGE. 

PREREQUISITES: 

    The package must be in your own schema or you must have ALTER ANY 
    PROCEDURE system privilege. 

    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 
    must match the package's creation label or you must satisfy one of 

    these criteria: 

    * If the package's creation label is higher than your DBMS label, 
      you must have READUP and WRITEUP system privileges. 
    * If the package's creation label is lower than your DBMS label, you 
      must have WRITEDOWN system privilege. 
    * If the package's creation label and your DBMS label are 
      noncomparable, you must have READUP, WRITEUP, and WRITEDOWN system 
      privileges. 


SEE: 
    CREATE PACKAGE, CREATE PACKAGE BODY "))
("alter procedure" . (nil "SQL command" "ALTER PROCEDURE command 

PURPOSE: 

    To recompile a stand-alone stored procedure. 

SYNTAX: 

ALTER PROCEDURE [schema.]procedure 
    COMPILE 

where: 

schema 
    is the schema containing the procedure.  If you omit schema, Oracle 
    assumes the procedure is in your own schema. 

procedure 
    is the name of the procedure to be recompiled. 

COMPILE 

    causes Oracle to recompile the procedure.  The COMPILE keyword is 
    required. 

PREREQUISITES: 

    The procedure must be in your own schema or you must have ALTER ANY 
    PROCEDURE system privilege. 

    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 
    must match the procedure's creation label or you must satisfy one of 
    these criteria: 

    * If the procedure's creation label is higher than your DBMS label, 

      you must have READUP and WRITEUP system privileges. 
    * If the procedure's creation label is lower than your DBMS label, 
      you must have WRITEDOWN system privilege. 
    * If the procedure's creation label and your DBMS label are 
      noncomparable, you must have READUP, WRITEUP, and WRITEDOWN system 
      privileges. 

SEE: 
    ALTER FUNCTION, ALTER PACKAGE, CREATE PROCEDURE "))
("alter profile" . (nil "SQL command" "ALTER PROFILE command 

PURPOSE: 

    To add, modify, or remove a resource limit in a profile. 

SYNTAX: 

ALTER PROFILE profile 
    LIMIT   [SESSIONS_PER_USER          {integer | UNLIMITED | DEFAULT}] 
            [CPU_PER_SESSION            {integer | UNLIMITED | DEFAULT}] 
            [CPU_PER_CALL               {integer | UNLIMITED | DEFAULT}] 
            [CONNECT_TIME               {integer | UNLIMITED | DEFAULT}] 

            [IDLE_TIME                  {integer | UNLIMITED | DEFAULT}] 
            [LOGICAL_READS_PER_SESSION  {integer | UNLIMITED | DEFAULT}] 
            [LOGICAL_READS_PER_CALL     {integer | UNLIMITED | DEFAULT}] 
            [COMPOSITE_LIMIT            {integer | UNLIMITED | DEFAULT}] 
            [PRIVATE_SGA          {integer [K|M] | UNLIMITED | DEFAULT}] 

where: 

profile 
    is the name of the profile to be altered. 


integer 
    defines a new limit for a resource in this profile.  For information 
    on resource limits, see the CREATE PROFILE command. 

UNLIMITED 
    specifies that this profile allows unlimited use of the resource. 

DEFAULT 
    removes a resource limit from the profile.  Any user assigned the 
    profile is subject to the limit on the resource defined in the 
    DEFAULT profile in their subsequent sessions. 


PREREQUISITES: 

    You must have ALTER PROFILE system privilege. 

    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 
    must match the profile's creation label or you must satisfy one of 
    these criteria: 

    * If the profile's creation label is higher than your DBMS label, 
      you must have READUP and WRITEUP system privileges. 
    * If the profile's creation label is lower than your DBMS label, you 

      must have WRITEDOWN system privilege. 
    * If the profile's creation label and your DBMS label are 
      noncomparable, you must have READUP, WRITEUP, and WRITEDOWN system 
      privileges. 

SEE: 
    CREATE PROFILE "))
("alter resource cost" . (nil "SQL command" "ALTER RESOURCE COST command 

PURPOSE: 

    To specify a formula to calculate the total resource cost used in a 
    session.  For any session, this cost is limited by the value of the 
    COMPOSITE_LIMIT parameter in the user's profile. 

SYNTAX: 

ALTER RESOURCE 
    COST   [CPU_PER_SESSION              integer] 
           [CONNECT_TIME                 integer] 
           [LOGICAL_READS_PER_SESSION    integer] 

           [PRIVATE_SGA                  integer] 

where: 

integer 
    is the weight of each resource. 

CPU_PER_SESSION 
    is the amount of CPU time used by a session measured in hundredths 
    of seconds. 

CONNECT_TIME 
    is the elapsed time of a session measured in minutes. 

LOGICAL_READS_PER_SESSION 
    is the number of data blocks read during a session, including blocks 

    read from both memory and disk. 

PRIVATE_SGA 
    is the number of bytes of private space in the System Global Area 
    (SGA) used by a session.  This limit only applies if you are using 
    the multi-threaded server architecture and allocating private space 
    in the SGA for your session. 

PREREQUISITES: 

    You must have ALTER RESOURCE COST system privilege. 

    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 

    must match DBLOW or you must have WRITEDOWN system privileges. 

SEE: 
    CREATE PROFILE "))
("alter role" . (nil "SQL command" "ALTER ROLE command 

PURPOSE: 

    To change the authorization needed to enable a role. 

SYNTAX: 

ALTER ROLE role 
    { NOT IDENTIFIED 
    | IDENTIFIED {BY password | EXTERNALLY } 

where: 

role 
    is the name of the role to be created.  Oracle Corporation 
    recommends that the role contain at least one single-byte 
    character regardless of whether the database character set also 

    contains multi-byte characters. 

NOT IDENTIFIED 
    indicates that a user granted the role need not be verified when 
    enabling it. 

IDENTIFIED 
    indicates that a user granted the role must be verified when 
    enabling it with the SET ROLE command: 
            BY password 
                   The user must specify the password to Oracle when 
                   enabling the role.  The password can only contain 

                   single-byte characters from your database character 
                   set regardless of whether this character set also 
                   contains multi-byte characters. 
            EXTERNALLY 
                   The operating system verifies the user enabling to 
                   the role.  Depending on the operating system, the 
                   user may have to specify a password to the operating 

                   system when enabling the role. 

    If you omit both the NOT IDENTIFIED option and the IDENTIFIED 
    clause, the role defaults to NOT IDENTIFIED. 

PREREQUISITES: 

    You must either have been granted the role with the ADMIN OPTION or 
    have ALTER ANY ROLE system privilege. 

    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 
    must match the role's creation label or you must satisfy one of 

    these criteria: 

    * If the role's creation label is higher than your DBMS label, you 
      must have READUP and WRITEUP system privileges. 
    * If the role's creation label is lower than your DBMS label, you 
      must have WRITEDOWN system privilege. 
    * If the role's creation label and your DBMS label are 
      noncomparable, you must have READUP, WRITEUP, and WRITEDOWN system 
      privileges. 


SEE: 
    CREATE ROLE, SET ROLE "))
("alter rollback segment" . (nil "SQL command" "ALTER ROLLBACK SEGMENT command 

PURPOSE: 

    To alter a rollback segment in one of these ways: 

    * by bringing it online 
    * by taking it offline 
    * by changing its storage characteristics 

SYNTAX: 

ALTER ROLLBACK SEGMENT rollback_segment 
    { ONLINE 
    | OFFLINE 
    | STORAGE storage_clause } 

where: 

rollback_segment 
    specifies the name of an existing rollback segment. 


ONLINE 
    brings the rollback segment online. 

OFFLINE 
    takes the rollback segment offline. 

STORAGE 
    changes the rollback segment's storage characteristics. 

PREREQUISITES: 

    You must have ALTER ROLLBACK SEGMENT system privilege. 

    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 
    must match the rollback segment's creation label or you must satisfy 

    one of these criteria: 

    * If the rollback segment's creation label is higher than your DBMS 
      label, you must have READUP and WRITEUP system privileges. 
    * If the rollback segment's creation label is lower than your DBMS 
      label, you must have WRITEDOWN system privilege. 
    * If the rollback segment's creation label and your DBMS label are 
      noncomparable, you must have READUP, WRITEUP, and WRITEDOWN system 

      privileges. 

SEE: 
    CREATE ROLLBACK SEGMENT, CREATE TABLESPACE, STORAGE clause "))
("alter sequence" . (nil "SQL command" "ALTER SEQUENCE command 

PURPOSE: 

    To change the sequence in one of these ways: 

    * changing the increment between future sequence values 
    * setting or eliminating the minimum or maximum value 
    * changing the number of cached sequence numbers 
    * specifying whether or not sequence numbers must be ordered 

SYNTAX: 

ALTER SEQUENCE [schema.]sequence 
    [INCREMENT BY integer] 

    [MAXVALUE integer | NOMAXVALUE] 
    [MINVALUE integer | NOMINVALUE] 
    [CYCLE | NOCYCLE] 
    [CACHE integer | NOCACHE] 
    [ORDER | NOORDER] 

where: 

schema 
    is the schema to contain the sequence.  If you omit schema, Oracle 
    creates the sequence in your own schema. 

sequence 
    is the name of the sequence to be created. 

INCREMENT BY 
    specifies the interval between sequence numbers.  This value can be 

    any positive or negative Oracle integer, but it cannot be 0.  If 
    this value is negative, then the sequence descends.  If the 
    increment is positive, then the sequence ascends.  If you omit this 
    clause, the interval defaults to 1. 

MINVALUE 
    specifies the sequence's minimum value. 

NOMINVALUE 
    specifies a minimum value of 1 for an ascending sequence or -10 
    for a descending sequence. 


    The default is NOMINVALUE. 

MAXVALUE 
    specifies the maximum value the sequence can generate. 

NOMAXVALUE 
    specifies a maximum value of 10 
    for a descending sequence. 

    The default is NOMAXVALUE. 

START WITH 
    specifies the first sequence number to be generated.  You can use 
    this option to start an ascending sequence at a value greater than 
    its minimum or to start a descending sequence at a value less than 

    its maximum.  For ascending sequences, the default value is the 
    sequence's minimum value.  For descending sequences, the default 
    value is the sequence's maximum value. 

CYCLE 
    specifies that the sequence continues to generate values after 
    reaching either its maximum or minimum value.  After an ascending 
    sequence reaches its maximum value, it generates its minimum value. 
    After a descending sequence reaches its minimum, it generates its 

    maximum. 

NOCYCLE 
    specifies that the sequence cannot generate more values after 
    reaching its maximum or minimum value. 

    The default is NOCYCLE. 

CACHE 
    specifies how many values of the sequence Oracle preallocates and 
    keeps in memory for faster access.  The minimum value for this 
    parameter is 2.  For sequences that cycle, this value must be less 
    than the number of values in the cycle. 


NOCACHE 
    specifies that values of the sequence are not preallocated. 

    If you omit both the CACHE parameter and the NOCACHE option, Oracle 
    caches 20 sequence numbers by default.  However, if you are using 
    Oracle with the Parallel Server option in parallel mode and you 
    specify the ORDER option, sequence values are never cached, 
    regardless of whether you specify the CACHE parameter or the NOCACHE 

    option. 

ORDER 
    guarantees that sequence numbers are generated in order of request. 
    You may want to use this option if you are using the sequence 
    numbers as timestamps.  Guaranteeing order is usually not important 
    for sequences used to generate primary keys. 

NOORDER 
    does not guarantee sequence numbers are generated in order of 
    request. 

    If you omit both the ORDER and NOORDER options, Oracle chooses 

    NOORDER by default.  Note that the ORDER option is only necessary to 
    guarantee ordered generation if you are using Oracle with the 
    Parallel Server option in parallel mode.  If you are using exclusive 
    mode, sequence numbers are always generated in order. 

PREREQUISITES: 

    The sequence must be in your own schema or you must have ALTER 
    privilege on the sequence or you must have ALTER ANY SEQUENCE system 

    privilege. 

    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 
    must match the sequence's creation label or you must satisfy one of 
    these criteria: 

    * If the sequence's creation label is higher than your DBMS label, 
      you must have READUP and WRITEUP system privileges. 
    * If the sequence's creation label is lower than your DBMS label, 
      you must have WRITEDOWN system privilege. 

    * If the sequence's creation label and your DBMS label are 
      noncomparable, you must have READUP, WRITEUP, and WRITEDOWN system 
      privileges. 

SEE: 
    CREATE SEQUENCE, DROP SEQUENCE "))
("alter snapshot" . (nil "SQL command" "ALTER SNAPSHOT command 

PURPOSE: 

    To alter a snapshot in one of these ways: 

    * changing its storage characteristics 
    * changing its automatic refresh mode and times 

SYNTAX: 

ALTER SNAPSHOT [schema.]snapshot 
    [ PCTFREE  integer | PCTUSED  integer 
    | INITRANS integer | MAXTRANS integer 
    | STORAGE storage_clause ] ... 
    [ USING INDEX [   INITTRANS integer | MAXTRANS integer 

                    | STORAGE storage_clause] ... 
    [REFRESH [FAST | COMPLETE | FORCE] [START WITH date] [NEXT date]] 

where: 

schema 
    is the schema containing the snapshot.  If you omit schema, Oracle 
    assumes the snapshot is in your own schema. 

snapshot 
    is the name of the snapshot to be altered. 

PCTFREE 
PCTUSED 
INITRANS 
MAXTRANS 
    change the values of these parameters for the internal table that 

    Oracle uses to maintain the snapshot's data.  See the PCTFREE, 
    PCTUSED, INITRANS, and MAXTRANS parameters of the CREATE TABLE 
    command. 

STORAGE 
    changes the storage characteristics of the internal table that 
    Oracle uses to maintain the snapshot's data. 

REFRESH 
    changes the mode and times for automatic refreshes: 
            FAST 
                   specifies a fast refresh, or a refresh using the 

                   snapshot log associated with the master table. 
            COMPLETE 
                   specifies a complete refresh, or a refresh that 
                   reexecutes the snapshot's query. 
            FORCE 
                   specifies a fast refresh if one is possible or 
                   complete refresh if a fast refresh is not possible. 
                   Oracle decides whether a fast refresh is possible at 

                   refresh time. 
                   If you omit the FAST, COMPLETE, and FORCE options, 
                   Oracle uses FORCE by default. 
            START WITH 
                   specifies a date expression for the next 
                   automatic refresh time. 
            NEXT 
                   specifies a new date expression for calculating the 
                   interval between automatic refreshes. 


    START WITH and NEXT values must evaluate to times in the future. 

USING INDEX 
    alters the storage characteristics for the index on a simple 
    snapshot. 

PREREQUISITES: 

    The snapshot must be in your own schema or you must have ALTER ANY 
    SNAPSHOT system privilege. 

    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 
    must match the snapshot's creation label or you must satisfy one of 

    these criteria: 

    * If the snapshot's creation label is higher than your DBMS label, 
      you must have READUP and WRITEUP system privileges. 
    * If the snapshot's creation label is lower than your DBMS label, 
      you must have WRITEDOWN system privilege. 
    * If the snapshot's creation label and your DBMS label are 
      noncomparable, you must have READUP, WRITEUP, and WRITEDOWN system 
      privileges. 


    To change the storage characteristics of the internal table that 
    Oracle uses to maintain the snapshot's data, you must also have the 
    privileges to alter that table.  For information on these 
    privileges, see the ALTER TABLE command. 

SEE: 
    CREATE SNAPSHOT, DROP SNAPSHOT "))
("alter snapshot log" . (nil "SQL command" "ALTER SNAPSHOT LOG command 

PURPOSE: 

    Changes the storage characteristics of a snapshot log. 

SYNTAX: 

ALTER SNAPSHOT LOG ON [schema.]table 
    [PCTFREE  integer]    [PCTUSED  integer] 
    [INITRANS integer]    [MAXTRANS integer] 
    [STORAGE storage_clause] 

where: 

schema 
    is the schema containing the snapshot log and its master table.  If 
    you omit schema, Oracle assumes the snapshot log is in your own 

    schema. 

table 
    is the name of the master table associated with the snapshot log to 
    be altered. 

PCTFREE 
PCTUSED 
INITRANS 
MAXTRANS 
    change the values of these parameters for the snapshot log.  See the 
    PCTFREE, PCTUSED, INITRANS, and MAXTRANS parameters of the CREATE 
    TABLE command. 

STORAGE 
    changes the storage characteristics of the snapshot log. 


PREREQUISITES: 

    Since a snapshot log is simply a table, the privileges that 
    authorize operations on it are the same as those for a table.  To 
    change its storage characteristics, you must have the privileges 
    listed for the ALTER TABLE command. 

SEE: 
    CREATE SNAPSHOT, CREATE SNAPSHOT LOG, DROP SNAPSHOT LOG "))
("alter table" . (nil "SQL command" "ALTER TABLE command 

PURPOSE: 

    To alter the definition of a table in one of these ways: 

    * to add a column 
    * to add an integrity constraint 
    * to redefine a column (datatype, size, default value) 
    * to modify storage characteristics or other parameters 
    * to enable, disable, or drop an integrity constraint or trigger 
    * to explicitly allocate an extent 

SYNTAX: 


ALTER TABLE [schema.]table 
    [ADD {    { column datatype [DEFAULT expr] [column_constraint] ... 
              | table_constraint} 
         |  ( { column datatype [DEFAULT expr] [column_constraint] ... 
              | table_constraint} 
           [, { column datatype [DEFAULT expr] [column_constraint] ... 
              | table_constraint} ] ... ) } ] 
    [MODIFY {   column [datatype] [DEFAULT expr] [column_constraint] ... 

            |  (column [datatype] [DEFAULT expr] [column_constraint] ... 
 [, column datatype [DEFAULT expr] [column_constraint] ...] ...) } ] 
    [PCTFREE  integer] [PCTUSED  integer] 
    [INITRANS integer] [MAXTRANS integer] 
    [STORAGE storage_clause] 
    [DROP drop_clause] ... 
    [ALLOCATE EXTENT [( [SIZE integer [K|M] ] 
                        [DATAFILE 'filename'] 
                        [INSTANCE integer] )] 

    [  PARALLEL ( [ DEGREE { integer | DEFAULT } ] 
                  [ INSTANCES { integer | DEFAULT } ] 
                ) 
     | NOPARALLEL ] 
    [  CACHE | NOCACHE  ] 
    [ ENABLE   enable_clause 
    | DISABLE disable_clause ] ... 

where: 

schema 
    is the schema containing the table.  If you omit schema, Oracle 
    assumes the table is in your own schema. 

table 

    is the name of the table to be altered. 

ADD 
    adds a column or integrity constraint. 

MODIFY 
    modifies a the definition of an existing column.  If you omit any of 
    the optional parts of the column definition (datatype, default 
    value, or column constraint), these parts remain unchanged. 

column 
    is the name of the column to be added or modified. 

datatype 

    specifies a datatype for a new column or a new datatype for an 
    existing column. 

DEFAULT 
    specifies a default value for a new column or a new default for an 
    existing column.  Oracle assigns this value to the column if a 
    subsequent INSERT statement omits a value for the column.  The 
    datatype of the default value must match the datatype specified for 
    the column.  A DEFAULT expression cannot contain references to other 

    columns, the pseudocolumns CURRVAL, NEXTVAL, LEVEL, and ROWNUM, or 
    date constants that are not fully specified. 

column_constraint 
    adds or removes a NOT NULL constraint to or from and existing 
    column. 

table_constraint 
    adds an integrity constraint to the table. 

PCTFREE 
PCTUSED 
INITRANS 
MAXTRANS 
    changes the value of one of these parameters for the table.  See the 

    PCTFREE, PCTUSED, INITRANS, and MAXTRANS parameters of the CREATE 
    TABLE command. 

STORAGE 
    changes the storage characteristics of the table. 

DROP 
    drops an integrity constraint. 

ALLOCATE EXTENT 
    explicitly allocates a new extent for the table. 
            SIZE 
                   specifies the size of the extent in bytes.  You can 
                   use K or M to specify the extent size in kilobytes or 

                   megabytes.  If you omit this parameter, Oracle 
                   determines the size based on the values of the 
                   table's STORAGE parameters. 
            DATAFILE 
                   specifies one of the data files in the table's 
                   tablespace to contain the new extent.  If you omit 
                   this parameter, Oracle chooses the data file. 
            INSTANCE 

                   makes the new extent available to the specified 
                   instance.  An instance is identified by the value of 
                   its initialization parameter INSTANCE_NUMBER.  If you 
                   omit this parameter, the extent is available to all 
                   instances.  Only use this parameter if you are using 
                   Oracle with the Parallel Server option in parallel 

                   mode. 

            Explicitly allocating an extent with this clause does 
            affect the size for the next extent to be allocated 
            as specified by the NEXT and PCTINCREASE storage 
            parameters. 

PARALLEL 
    DEGREE specifies the number of query server processes that can scan 
    the table in parallel.  Either specify a positive integer or DEFAULT 
    which signifies to use the initialization parameter 

    PARALLEL_DEFAULT_SCANSIZE to estimate the number of query servers to use. 

    INSTANCES specifies the minimum number of instances that need to be 
    available before the table can be spread across all available instances 
    of a Parallel Server.  A positive integer specifies the number of 
    instances.  DEFAULT signifies that the parameter PARALLEL_MAX_PARTITIONSIZE 
    is used to calculate whether a table is split across all instances' buffer 

    caches. 

NOPARALLEL 
    specifies that queries on this table are not performed in parallel 
    by default.  A hint in the query still causes the query to be 
    performed in parallel. 

CACHE 
    specifies that blocks of this table are placed on the most recently 
    used end of the LRU list of the buffer cache when the a full table scan 
    is performed. 
    This option is useful for small lookup tables. 


NOCACHE 
    specifies that blocks of the table in the buffer cache follow the 
    standard LRU algorithm when a full table scan is performed. 

ENABLE 
    enables a single integrity constraint or all triggers associated 
    with the table. 

DISABLE 
    disables a single integrity constraint or all triggers associated 
    with the table. 

    Integrity constraints specified in these clauses must be defined in 

    the ALTER TABLE statement or in a previously issued statement.  You 
    can also enable and disable integrity constraints with the ENABLE 
    and DISABLE keywords of the CONSTRAINT clause.  If you define an 
    integrity constraint but do not explicitly enable or disable it, 
    Oracle enables it by default. 

PREREQUISITES: 

    The table must be in your own schema or you must have ALTER 
    privilege on the table or you must have ALTER ANY TABLE system 

    privilege. 

    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 
    must match the table's creation label or you must satisfy one of 
    these criteria: 

    * If the table's creation label is higher than your DBMS label, you 
      must have READUP and WRITEUP system privileges. 
    * If the table's creation label is lower than your DBMS label, you 
      must have WRITEDOWN system privilege. 

    * If the table's creation label and your DBMS label are 
      noncomparable, you must have READUP, WRITEUP, and WRITEDOWN system 
      privileges. 

SEE: 
    CONSTRAINT clause, CREATE TABLE, DISABLE clause, DROP clause,
    ENABLE clause, STORAGE clause "))
("alter tablespace" . (nil "SQL command" "ALTER TABLESPACE command 

PURPOSE: 

    To alter an existing tablespace in one of these ways: 

    * to add or rename data file(s) 
    * to change default storage parameters 
    * to take the tablespace online or offline 
    * to begin or end a backup 

SYNTAX: 

ALTER TABLESPACE tablespace 
    { ADD DATAFILE filespec [, filespec] ... 
    | RENAME DATAFILE 'filename' [,'filename'] ... 

                   TO 'filename' [,'filename'] ... 
    | DEFAULT STORAGE storage_clause 
    | ONLINE 
    | OFFLINE [NORMAL | TEMPORARY | IMMEDIATE] 
    | READ ONLY 
    | READ WRITE 
    | {BEGIN | END} BACKUP} 

where: 

tablespace 
    is the name of the tablespace to be altered. 

ADD DATAFILE 
    adds the data file specified by filespec to the tablespace.  You can 

    add a data file while the tablespace is online or offline.  Be sure 
    that the data file is not already in use by another database. 

RENAME DATAFILE 
    renames one or more of the tablespace's data files.  Take the 
    tablespace offline before renaming the data file.  Each 'filename' 
    must fully specify a data file using the conventions for filenames 
    on your operating system. 

    This clause only associates the tablespace with the new file rather 

    than the old one.  This clause does not actually change the name of 
    the operating system file.  You must change the name of the file 
    through your operating system. 

DEFAULT STORAGE 
    specifies the new default storage parameters for objects 
    subsequently created in the tablespace. 

ONLINE 
    brings the tablespace online. 

OFFLINE 
    takes the tablespace offline and prevents further access to its 

    segments. 
            NORMAL 
                   performs a checkpoint for all data files in the 
                   tablespace.  All of these data files must be online. 
                   You need not perform media recovery on this 
                   tablespace before bringing it back online.  You must 
                   use this option if the database is in noarchivelog 
                   mode. 

            TEMPORARY 
                   performs a checkpoint for all online data files in 
                   the tablespace but does not ensure that all files can 
                   be written.  Any offline files may require media 
                   recovery before you bring the tablespace back online. 
            IMMEDIATE 
                   does not ensure that tablespace files are available 
                   and does not perform a checkpoint.  You must perform 

                   media recovery on the tablespace before bringing it 
                   back online. 

    The default is NORMAL. 

    Before taking a tablespace offline for a long period of time, you 
    may want to alter any users who have been assigned the tablespace as 
    either a default or temporary tablespace.  When the tablespace is 
    offline, these users cannot allocate space for objects or sort areas 

    in the tablespace.  You can reassign users new default and 
    temporary tablespaces with the ALTER USER command. 

READ ONLY 
    specifies that write operations are not permitted on the table- 
    space. 
    Before using this option, the tablespace must meet these pre- 
    requisites: 

          * the tablespace must be online 
          * there must not be any active transactions in the entire 

            database 
          * the tablespace must not contain any active rollback 
            segments 
          * the tablespace must not be involved in an online 
            backup 
          * the COMPATIBLE initialization parameter must be set to 
            7.1.0 or greater 

READ WRITE 
    specifies that a read-only tablespace be made writeable.  To 
    issue this command, all of the datafiles in the tablespace must 

    be online. 

BEGIN BACKUP 
    signifies that an online backup is to be performed on the data files 
    that comprise this tablespace.  This option does not prevent users 
    from accessing the tablespace.  This option is used for control file 
    and redo log record keeping.  You must use this option before 
    beginning an online backup. 

    While the backup is in progress, you cannot perform any of these 

    operations: 

          * take the tablespace offline normally 
          * shutdown the instance 
          * begin another backup of the tablespace 

END BACKUP 
    signifies that an online backup of the tablespace  is complete.  Use 
    this option as soon as possible after completing an online backup. 

PREREQUISITES: 

    If you have ALTER TABLESPACE system privilege, you can perform any 

    of this command's operations.  If you have MANAGE TABLESPACE system 
    privilege, you can only perform these operations: 

    * to take the tablespace online or offline 
    * to begin or end a backup 

    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 
    must match the tablespace's creation label or you must satisfy one 
    of these criteria: 

    * If the tablespace's creation label is higher than your DBMS 

      label, you must have READUP and WRITEUP system privileges. 
    * If the tablespace's creation label is lower than your DBMS label, 
       you must have WRITEDOWN system privilege. 
    * If the tablespace's creation label and your DBMS label are 
      noncomparable, you must have READUP, WRITEUP, and WRITEDOWN system 
      privileges. 

    If you are using Trusted Oracle in DBMS MAC mode, to add a data 

    file, your operating system process label must be the equivalent of 
    DBHIGH. 

SEE: 
    CREATE DATABASE, CREATE TABLESPACE, DROP TABLESPACE, STORAGE clause "))
("alter trigger" . (nil "SQL command" "ALTER TRIGGER command 

PURPOSE: 

    To perform one of these operations on a database trigger: 

    * enable 
    * disable 

SYNTAX: 

ALTER TRIGGER [schema.]trigger 
    { ENABLE 
    | DISABLE } 

where: 

schema 
    is the schema containing the trigger.  If you omit schema, Oracle 
    assumes the trigger is in your own schema. 

trigger 

    is the name of the trigger to be altered. 

ENABLE 
    enables the trigger. 

DISABLE 
    disables the trigger. 

PREREQUISITES: 

    The trigger must be in your own schema or you must have ALTER ANY 
    TRIGGER system privilege. 

    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 
    must match the trigger's creation label or you must satisfy one of 
    these criteria: 


    * If the trigger's creation label is higher than your DBMS label, 
      you must have READUP and WRITEUP system privileges. 
    * If the trigger's creation label is lower than your DBMS label, you 
      must have WRITEDOWN system privilege. 
    * If the trigger's creation label and your DBMS label are 
      noncomparable, you must have READUP, WRITEUP, and WRITEDOWN system 
      privileges. 

SEE: 

    CREATE TRIGGER, DISABLE clause, DROP TRIGGER, ENABLE clause "))
("alter user" . (nil "SQL command" "ALTER USER command 

PURPOSE: 

    To change any of these characteristics of a database user: 

    * password 
    * default tablespace for object creation 
    * tablespace for temporary segments created for the user 
    * tablespace access and tablespace quotas 
    * limits on database resources 
    * default roles 

SYNTAX: 

ALTER USER user 
    [IDENTIFIED {BY password | EXTERNALLY}] 

    [DEFAULT TABLESPACE tablespace] 
    [TEMPORARY TABLESPACE tablespace] 
    [QUOTA {integer [K|M] | UNLIMITED} ON tablespace] ... 
    [PROFILE profile] 
    [DEFAULT ROLE { role [, role] ... 
                  | ALL [EXCEPT role [, role] ...] 
                  | NONE}] 

where: 

user 
    is the user to be altered. 

IDENTIFIED 
    indicates how Oracle permits user access. 

            BY 
                   specifies a new password for the user.  The password 
                   does not appear in quotes and is not case-sensitive. 
                   The password can only contain single-byte characters 
                   from your database character set regardless of 
                   whether this character set also contains multi-byte 
                   characters. 
            EXTERNALLY 

                   indicates that Oracle verifies user access with the 
                   operating system, rather than with a password.  See 
                   the CREATE USER command. 

    Although you do not need privileges to change your own password, you 
    must have ALTER USER system privilege to change from BY password to 
    EXTERNALLY or vice-versa. 

DEFAULT TABLESPACE 
    specifies the default tablespace for object creation. 


TEMPORARY TABLESPACE 
    specifies the tablespace for the creation of temporary segments for 
    operations such as sorting that require more space than is available 
    in memory. 

QUOTA 
    establishes a space quota of integer bytes on the tablespace for the 
    user.  This quota is the maximum space in tablespace that can be 
    allocated for objects in the user's schema. You can use K or M to 

    specify the quota in kilobytes or megabytes.  You need not have 
    quota on the tablespace to establish a quota on the tablespace for 
    another user.  See the CREATE USER command. 

    If you reduce an existing quota to a value below the space allocated 
    for existing objects in the user's schema in the tablespace, no more 
    space in the tablespace can be allocated to objects in the schema. 

    Note that an ALTER USER statement can contain multiple QUOTA clauses 

    for multiple tablespaces. 

            UNLIMITED 
                   places no limit on the space in the tablespace 
                   allocated to objects in the user's schema. 

PROFILE 
    changes the user's profile to profile.  In subsequent sessions, the 
    user is subject to the limits defined in the new profile. 

    To assign the default limits to the user, assign the user the 
    DEFAULT profile. 


DEFAULT ROLE 
    establishes default roles for the user.  Oracle enables the user's 
    default roles at logon.  By default, all roles granted to the user 
    are default roles. 
            ALL 
                   makes all the roles granted to the user default 
                   roles, except those listed in the EXCEPT clause. 
            NONE 
                   makes none of the roles granted to the user default 

                   roles. 

PREREQUISITES: 

    You must have ALTER USER privilege.  However, you can change your 
    own password without this privilege. 

SEE: 
    CREATE PROFILE, CREATE ROLE, CREATE TABLESPACE, CREATE USER "))
("alter view" . (nil "SQL command" "ALTER VIEW command 

PURPOSE: 

    To recompile a view. 

SYNTAX: 

ALTER VIEW [schema.]view 
    COMPILE 

where: 

schema 
    is the schema containing the view.  If you omit schema, Oracle 
    assumes the view is in your own schema. 

view 
    is the name of the view to be recompiled. 

COMPILE 
    causes Oracle to recompile the view.  The COMPILE keyword is 

    required. 

PREREQUISITES: 

    The view must be in your own schema or you must have ALTER ANY TABLE 
    system privilege. 

    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 
    must match the view's creation label or you must satisfy one of 
    these criteria: 

    * If the view's creation label is higher than your DBMS label, you 
      must have READUP and WRITEUP system privileges. 

    * If the view's creation label is lower than your DBMS label, you 
      must have WRITEDOWN system privilege. 
    * If the view's creation label and your DBMS label are 
      noncomparable, you must have READUP, WRITEUP, and WRITEDOWN system 
      privileges. 

SEE: 
    CREATE VIEW "))
("analyze" . (nil "SQL command" "ANALYZE command 

PURPOSE: 

    To perform one of these functions on an index, table, or cluster: 

    * to collect statistics about the object used by the optimizer and 
      store them in the data dictionary 
    * to delete statistics about the object from the data dictionary 
    * to validate the structure of the object 
    * to identify migrated and chained rows of the table or cluster 

SYNTAX: 


ANALYZE 
    { INDEX [schema.]index 
            { { COMPUTE STATISTICS 
              | ESTIMATE STATISTICS [SAMPLE integer {ROWS | PERCENT}] 
              | DELETE STATISTICS } 
            | VALIDATE STRUCTURE } 
    | {TABLE [schema.]table | CLUSTER [schema.]cluster} 
            { { COMPUTE 
              | ESTIMATE [SAMPLE integer {ROWS | PERCENT}] 
              | DELETE } STATISTICS 

            | VALIDATE STRUCTURE [CASCADE] 
            | LIST CHAINED ROWS [INTO [schema.]table] } } 

where: 

INDEX 
    identifies an index to be analyzed.  If you omit schema, Oracle 
    assumes the index is in your own schema. 

TABLE 
    identifies a table to be analyzed.  If you omit schema, Oracle 
    assumes the table is in your own schema.  When you collect 
    statistics for a table, Oracle also automatically collects the 

    statistics for each of the table's indexes. 

CLUSTER 
    identifies a cluster to be analyzed.  If you omit schema, Oracle 
    assumes the cluster is in your own schema.  When you collect 
    statistics for a cluster, Oracle also automatically collects the 
    statistics for all the cluster's tables and all their indexes, 
    including the cluster index. 

COMPUTE STATISTICS 
    computes exact statistics about the analyzed object and stores them 

    in the data dictionary. 

ESTIMATE STATISTICS 
    estimates statistics about the analyzed object and stores them in 
    the data dictionary. 
            SAMPLE 
                   specifies the amount of data from the analyzed object 
                   Oracle samples to estimate statistics.  If you omit 
                   this parameter, Oracle samples 1064 rows.  If you 
                   specify more than half of the data, Oracle reads all 

                   the data and computes the statistics. 
            ROWS 
                   causes Oracle to sample integer rows of the table or 
                   cluster or integer entries from the index.  The 
                   integer must be at least 1. 
            PERCENT 
                   causes Oracle to sample integer percent of the rows 
                   from the table or cluster or integer percent of the 

                   index entries.  The integer can range from 1 to 99. 

DELETE STATISTICS 
    deletes any statistics about the analyzed object that are currently 
    stored in the data dictionary. 

VALIDATE STRUCTURE 
    validates the structure of the analyzed object.  If you use this 
    option when analyzing a cluster, Oracle automatically validates the 
    structure of the cluster's tables. 


CASCADE 
    validates the structure of the indexes associated with the table or 
    cluster.  If you use this option when validating a table, Oracle 
    also validates the table's indexes.  If you use this option when 
    validating a cluster, Oracle also validates all the clustered 
    tables' indexes, including the cluster index. 

LIST CHAINED ROWS 
    identifies migrated and chained rows of the analyzed table or 

    cluster.  You cannot use this option when analyzing an index. 
            INTO 
                   specifies a table into which Oracle lists the 
                   migrated and chained rows.  If you omit schema, 
                   Oracle assumes the list table is in your own schema. 
                   If you omit this clause altogether, Oracle assumes 
                   that the table is named CHAINED_ROWS.  The list table 

                   must be on your local database. 

PREREQUISITES: 

    The object to be analyzed must be in your own schema or you must 
    have the ANALYZE ANY system privilege. 

    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 
    must match the creation label of the object to be analyzed or you 
    must satisfy one of these criteria: 

    * If the object's creation label is higher than your DBMS label, you 

      must have READUP and WRITEUP system privileges. 
    * If the object's creation label is lower than your DBMS label, you 
      must have WRITEDOWN system privilege. 
    * If the object's creation label and your DBMS label are 
      noncomparable, you must have READUP, WRITEUP, and WRITEDOWN system 
      privileges. 

    If you want to list chained rows of a table or cluster into a list 
    table, the list table must be in your own schema or you must have 

    INSERT privilege on the list table or you must have INSERT ANY TABLE 
    system privilege.  If you are using Trusted Oracle in DBMS MAC mode, 
    the list table must also meet the criteria for the analyzed object 
    described above. 

SEE: 
    Chapter 13, The Optimizer, in the Oracle7 Server Concepts Manual, 
    and Chapter 22, Tuning I/O, in the Oracle7 Server Administrator's 
    Guide. "))
("audit" . (nil "SQL command" "AUDIT command (SQL Statements) 

PURPOSE: 

    To choose specific SQL statements for auditing in subsequent user 
    sessions.  To choose particular schema objects for auditing, use the 
    AUDIT command (Schema Objects). 

SYNTAX: 

AUDIT {statement_opt | system_priv} 
   [, {statement_opt | system_priv} ] ... 
    [BY user [, user] ...] 
    [BY {SESSION | ACCESS}] 
    [WHENEVER [NOT] SUCCESSFUL] 


where: 

statement_opt 
    chooses specific SQL statements for auditing. 

system_priv 
    chooses SQL statements that are authorized by the specified system 
    privilege for auditing. 

BY user 
    chooses only SQL statements issued by specified users for auditing. 
    If you omit this clause, Oracle audits all users' statements. 

BY SESSION 
    causes Oracle to write a single record for all SQL statements of the 

    same type issued in the same session. 

BY ACCESS 
    causes Oracle to write one record for each audited statement. 

    If you specify statement options or system privileges that audit 
    Data Definition Language statements, Oracle automatically audits by 
    access regardless of whether you specify the BY SESSION or BY ACCESS 
    option. 

    For statement options and system privileges that audit other types 

    of SQL statements, you can specify either the BY SESSION or BY 
    ACCESS option.  BY SESSION is the default. 

WHENEVER SUCCESSFUL 
    chooses auditing only for SQL statements that complete successfully. 
            NOT 
                   chooses auditing only for statements that fail, or 
                   result in errors. 

    If you omit the WHENEVER clause, Oracle audits SQL statements 
    regardless of success or failure. 


PREREQUISITES: 

    You must have AUDIT SYSTEM system privilege. 

    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 
    must dominate the creation label of the users whose SQL statements 
    you are auditing. 

SEE: 
    AUDIT (Schema Objects), NOAUDIT (SQL Statements) "))
("comment" . (nil "SQL command" "COMMENT command 

PURPOSE: 

    To add a comment about a table, view, snapshot, or column into the 
    data dictionary. 

SYNTAX: 

COMMENT ON {  TABLE [schema.]{table | view | snapshot} 
           | COLUMN [schema.]{table | view | snapshot}.column } 
    IS 'text' 

where: 

TABLE 
    specifies the schema and name of the table, view, or snapshot to be 
    commented. 


COLUMN 
    specifies the name of the column of a table, view, or snapshot to be 
    commented. 

    If you omit schema, Oracle assumes the table, view, or snapshot is 
    in your own schema. 

IS 'text' 
    is the text of the comment. 

PREREQUISITES: 

    The table, view, or snapshot must be in your own schema or you must 
    have COMMENT ANY TABLE system privilege. 

    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 

    must match the creation label of the table, view, snapshot, or 
    column. 

SEE: 
    Comments "))
("create cluster" . (nil "SQL command" "CREATE CLUSTER command 

PURPOSE: 

    To create a cluster.  A cluster is a schema object that contains one 
    or more tables that all have one or more columns in common. 

SYNTAX: 

CREATE CLUSTER [schema.]cluster 
    (column datatype [,column datatype] ... ) 
    [PCTUSED integer] [PCTFREE integer] 
    [SIZE integer [K|M] ] 
    [INITRANS integer] [MAXTRANS integer] 
    [TABLESPACE tablespace] 

    [STORAGE storage_clause] 
    [  PARALLEL ( [ DEGREE { integer | DEFAULT } ] 
                  [ INSTANCES { integer | DEFAULT } ] 
                ) 
     | NOPARALLEL ] 
    [  CACHE | NOCACHE  ] 
    [INDEX 
    | [HASH IS column] HASHKEYS integer] 

where: 

schema 
    is the schema to contain the cluster.  If you omit schema, Oracle 
    creates the cluster in your current schema. 


cluster 
    is the name of the cluster to be created. 

column 
    is the name of a column of the cluster key. 

datatype 
    is the datatype of a cluster key column.  A cluster key column can 
    have any internal datatype except LONG or LONG RAW. 

PCTUSED 
    specifies the limit that Oracle uses to determine when additional 
    rows can be added to a cluster's data block.  The value of this 

    parameter is expressed as a whole number and interpreted as a 
    percentage. 

PCTFREE 
    specifies the space reserved in each of the cluster's data blocks 
    for future expansion.  The value of the parameter is expressed as a 
    whole number and interpreted as a percentage. 

INITRANS 
    specifies the initial number of concurrent update transactions 
    allocated for data blocks of the cluster.  The value of this 

    parameter for a cluster cannot be less than 2 or more than the value 
    of the MAXTRANS parameter.  The default value is the greater of the 
    INITRANS value for the cluster's tablespace and 2. 

MAXTRANS 
    specifies the maximum number of concurrent update transactions for 
    any given data block belonging to the cluster.  The value of this 
    parameter cannot be less than the value of the INITRANS parameter. 

    The maximum value of this parameter is 255.  The default value is 
    the MAXTRANS value for the tablespace to contain the cluster. 

    For a complete description of the PCTUSED, PCTFREE, INITRANS, and 
    MAXTRANS parameters, see the CREATE TABLE. 

SIZE 
    specifies the amount of space in bytes to store all rows with the 
    same cluster key value or the same hash value.  You can use K or M 

    to specify this space in kilobytes or megabytes.  The value of this 
    parameter cannot exceed the size of a data block.  If you omit this 
    parameter, Oracle reserves one data block for each cluster key value 
    or hash value. 

TABLESPACE 
    specifies the tablespace in which the cluster is created. 

STORAGE 
    specifies how data blocks are allocated to the cluster. 

PARALLEL 

    DEGREE specifies the number of query server processes that can scan 
    the cluster in parallel.  Either specify a positive integer or DEFAULT 
    which signifies to use the initialization parameter 
    PARALLEL_DEFAULT_SCANSIZE to estimate the number of query servers to use. 

    INSTANCES specifies the minimum number of instances that need to be 
    available before the cluster can be spread across all available instances 

    of a Parallel Server.  A positive integer specifies the number of 
    instances.  DEFAULT signifies that the parameter PARALLEL_MAX_PARTITIONSIZE 
    is used to calculate whether a table is split across all instances' buffer 
    caches. 

NOPARALLEL 
    specifies that queries on this cluster are not performed in parallel 
    by default.  A hint in the query still causes the query to be 
    performed in parallel. 


CACHE 
    specifies that blocks of this cluster are placed on the most recently 
    used end of the LRU list of the buffer cache when the a full table scan 
    is performed. 
    This option is useful for small lookup tables. 

NOCACHE 
    specifies that blocks of the cluster in the buffer cache follow the 
    standard LRU algorithm when a full table scan is performed. 

PARALLEL 
     specifies the number of processes that can scan the tables in the 

     in parallel. 
     You can only specify positive integer values greater than 1.  If you 
     do not specify an integer, the degree of parallelism is based on 
     an estimate of the size of the table and the value of the 
     PARALLEL_DEFAULT_SCANSIZE initialization parameter. 

NOPARALLEL 
    specifies that queries on this cluster are not performed in parallel 
    by default.  A hint in the query still causes the query to be 

    performed in parallel. 

CACHE 
     specifies that the entire table is to be placed in the buffer cache. 
     This option is useful for small lookup tables. 

NOCACHE 
    specifies that blocks of the table in the buffer cache follow the 
    standard LRU algorithm. 

CACHE PARTITIONS 
    specifies the cluster is to be partitioned and cached on all instances 
    of a parallel server available for parallel query processing, 

    if at least the specified number of instances is available.  If 
PARALLEL 
    DEGREE specifies the number of query server processes that can scan 
    the cluster in parallel.  Either specify a positive integer or DEFAULT 
    which signifies to use the initialization parameter 
    PARALLEL_DEFAULT_SCANSIZE to estimate the number of query servers to use. 

    INSTANCES specifies the minimum number of instances that need to be 

    available before the cluster can be spread across all available instances 
    of a Parallel Server.  A positive integer specifies the number of 
    instances.  DEFAULT signifies that the parameter PARALLEL_MAX_PARTITIONSIZE 
    is used to calculate whether a table is split across all instances' buffer 
    caches. 

NOPARALLEL 
    specifies that queries on this cluster are not performed in parallel 
    by default.  A hint in the query still causes the query to be 

    performed in parallel. 

CACHE 
    specifies that blocks of this cluster are placed on the most recently 
    used end of the LRU list of the buffer cache when the a full table scan 
    is performed. 
    This option is useful for small lookup tables. 

NOCACHE 
    specifies that blocks of the cluster in the buffer cache follow the 
    standard LRU algorithm when a full table scan is performed. 


INDEX 
    creates an indexed cluster.  In an indexed cluster, rows are stored 
    together based on their cluster key values 
. 
HASH IS 
    specifies a column to be used as the hash function for a hash 
    cluster.  In a hash cluster, rows are stored together based on their 
    hash values.  The hash function specifies the hash value for each 
    row in the cluster.  The value that you specify must be the only 

    column of the cluster key and have a datatype of NUMBER with a scale 
    of 0.  Each value in the column must be a non-negative integer. 

    If you omit this parameter, Oracle uses an internal hash function 
    for the hash cluster.  The cluster key of a hash column can have one 
    or more columns of any datatype.  Hash clusters with composite 
    cluster keys or cluster keys made up of non-integer columns must use 

    the internal hash function. 

HASHKEYS 
    creates a hash cluster and specifies the number of hash values for a 
    hash cluster.  Oracle rounds the HASHKEYS value up to the nearest 
    prime number to obtain the actual number of hash values.  The 
    minimum value for this parameter is 2. 

    If you omit both the INDEX option and the HASHKEYS parameter, Oracle 
    creates an indexed cluster by default. 


PREREQUISITES: 

    To create a cluster in your own schema, you must have CREATE CLUSTER 
    system privilege.  To create a cluster in another user's schema, you 
    must have CREATE ANY CLUSTER system privilege.  Also, the owner of 
    the schema to contain the cluster must have either space quota on 
    the tablespace containing the cluster or UNLIMITED TABLESPACE system 
    privilege. 

    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 

    must dominate the label of the tablespace to contain the cluster. 
    To create a cluster in another user's schema, your DBMS label must 
    dominate the creation label of the owner of the schema. 

SEE: 
    CREATE INDEX, CREATE TABLE, STORAGE clause "))
("create controlfile" . (nil "SQL command" "CREATE CONTROLFILE command 

PURPOSE: 

    To recreate a control file in one of these cases: 

    * All copies of your existing control files have been lost through 
      media failure. 
    * You want to change the name of the database. 
    * You want to change the maximum number of redo log file groups, 
      redo log file members, archived redo log files, data files, or 
      instances that can concurrently have the database mounted and 

      open. 

    Warning:  Oracle Corporation recommends that you perform a full 
    backup of all files in the database before using this command. 

SYNTAX: 

CREATE CONTROLFILE [REUSE] 
    [SET] DATABASE database 
    LOGFILE [GROUP integer] filespec [, [GROUP integer] filespec] ... 
    {RESETLOGS | NORESETLOGS} 
    DATAFILE filespec [, filespec] ... 
    [MAXLOGFILES integer] 

    [MAXLOGMEMBERS integer] 
    [MAXLOGHISTORY integer] 
    [MAXDATAFILES integer] 
    [MAXINSTANCES integer] 
    [ARCHIVELOG | NOARCHIVELOG] 

where: 

REUSE 
    specifies that existing control files identified by the 
    initialization parameter CONTROL_FILES can be reused, thus ignoring 
    and overwriting any and all information they may currently contain. 
    If you omit this option and any of these control files already 

    exist, Oracle returns an error. 

SET DATABASE 
    changes the name of the database.  The name of a database can be as 
    long as eight bytes. 

DATABASE 
    specifies the name of the database.  The value of this parameter 
    must be the existing database name established by the previous 
    CREATE DATABASE statement or CREATE CONTROLFILE statement. 

LOGFILE 
    specifies the redo log file groups for your database.  You must list 

    all members of all redo log file groups.  These files must all 
    exist. 

RESETLOGS 
    ignores the contents of the files listed in the LOGFILE clause. 
    Each filespec in the LOGFILE clause must specify the SIZE parameter. 
    Oracle assigns all redo log file groups to thread 1 and enables this 
    thread for public use by any instance.  After using this option, you 
    must open the database using the RESETLOGS option of the ALTER 

    DATABASE command. 

NORESETLOGS 
    specifies that all files in the LOGFILE clause should be used as 
    they were when the database was last open.  These files must be the 
    current redo log files rather than restored backups.  Oracle 
    reassigns the redo log file groups to the threads to which they were 
    previously assigned and reenables the threads as they were 
    previously enabled.  If you specify GROUP values, Oracle verifies 

    these values with the GROUP values when the database was last open. 

DATAFILE 
    specifies the data files of the database.  You must list all data 
    files.  These files must all exist, although they may be restored 
    backups that require media recovery. 

MAXLOGFILES 
    specifies the maximum number of redo log file groups that can ever 
    be created for the database.  Oracle uses this value to determine 

    how much space in the control file to allocate for the names of redo 
    log files.  The default and maximum values depend on your operating 
    system.  The value that you specify should not be less than the 
    greatest GROUP value for any redo log file group. 

    Note that the number of redo log file groups accessible to your 
    instance is also limited by the initialization parameter LOG_FILES. 

MAXLOGMEMBERS 

    specifies the maximum number of members, or copies, for a redo log 
    file group.  Oracle uses this value to determine how much space in 
    the control file to allocate for the names of redo log files. The 
    minimum value is 1.  The maximum and default values depend on your 
    operating system. 

MAXLOGHISTORY 
    specifies the maximum number of archived redo log file groups for 
    automatic media recovery of the Oracle Parallel Server.  Oracle uses 

    this value to determine how much space in the control file to 
    allocate for the names of archived redo log files.  The minimum 
    value is 0.  The default value is a multiple of the MAXINSTANCES 
    value and varies depending on your operating system.  The maximum 
    value is limited only by the maximum size of the control file.  Note 
    that this parameter is only useful if you are using Oracle with the 
    Parallel Server option in both parallel mode and archivelog mode. 


MAXDATAFILES 
    specifies the maximum number of data files that can ever be created 
    for the database.  The minimum value is 1.  The maximum and default 
    values depend on your operating system.  The value you specify 
    should not be less than the total number of data files ever in the 
    database, including those for tablespaces that have been dropped. 

    Note that the number of data files accessible to your instance is 

    also limited by the initialization parameter DB_FILES. 

MAXINSTANCES 
    specifies the maximum number of instances that can simultaneously 
    have the database mounted and open.  This value takes precedence 
    over the value of the initialization parameter INSTANCES.  The 
    minimum value is 1.  The maximum and default values depend on your 
    operating system. 

ARCHIVELOG 
    establishes the mode of archiving the contents of redo log files 

    before reusing them.  This option prepares for the possibility of 
    media recovery as well as instance recovery. 

NOARCHIVELOG 
    establishes the initial mode of reusing redo log files without 
    archiving their contents.  This option prepares for the possibility 
    of instance recovery but not media recovery. 

    If you omit both the ARCHIVELOG and NOARCHIVELOG options, Oracle 
    chooses noarchivelog mode by default.  After creating the control 

    file, you can change between archivelog mode and noarchivelog mode 
    with the ALTER DATABASE command. 

PREREQUISITES: 

    You must have the OSDBA role enabled.  The database must not be 
    mounted by any instance. 

    If you are using Trusted Oracle in DBMS MAC mode, your operating 
    system label must be the equivalent of DBHIGH. 

SEE: 
    CREATE DATABASE "))
("create database" . (nil "SQL command" "CREATE DATABASE command 

PURPOSE: 

    To create a database, making it available for general use, with 
    these options: 

    * to establish a maximum number of instances, data files, redo log 
      files groups, or redo log file members 
    * to specify names and sizes of data files and redo log files 
    * to choose a mode of use for the redo log 

    Warning:  This command prepares a database for initial use and 

    erases any data currently in the specified files.  Only use this 
    command when you understand its ramifications. 

SYNTAX: 

CREATE DATABASE [database] 
    [CONTROLFILE REUSE] 
    [LOGFILE [GROUP integer] filespec [, [GROUP integer] filespec] ...] 
    [MAXLOGFILES integer ] 
    [MAXLOGMEMBERS integer] 
    [MAXLOGHISTORY integer] 
    [DATAFILE filespec [, filespec] ...] 
    [MAXDATAFILES integer] 

    [MAXINSTANCES integer] 
    [ARCHIVELOG | NOARCHIVELOG] 
    [EXCLUSIVE] 
    [CHARACTER SET charset] 

where: 

database 
    is the name of the database to be created and can be up to eight 
    bytes long.  Oracle writes this name into the control file.  If you 
    subsequently issue an ALTER DATABASE statement and that explicitly 
    specifies a database name, Oracle verifies that name with the name 

    in the control file. 

    The database name may only contain the alphabetic characters: 
          * alphabetic characters (A...Z) 
          * numbers (0...9) 
          * an underscore (_) 
          * a dollar sign ($) 
          * a pound sign (#) 

    The database cannot be a SQL*DBA reserved word.  If you omit 
    database from a CREATE DATABASE statement, Oracle uses the name 
    specified by the initialization parameter DB_NAME. 


CONTROLFILE REUSE 
    reuses existing control files identified by the initialization 
    parameter CONTROL_FILES, thus ignoring and overwriting any 
    information they currently contain.  This option is usually used 
    only when you are recreating a database, rather than creating one 
    for the first time.  You cannot use this option if you also specify 
    a parameter value that requires that the control file be larger than 

    the existing files.  These parameters are  MAXLOGFILES, 
    MAXLOGMEMBERS, MAXLOGHISTORY, MAXDATAFILES, and MAXINSTANCES. 

    If you omit this option and any of the files specified by 
    CONTROL_FILES already exist, Oracle returns an error. 

LOGFILE 
    specifies one or more files to be used as redo log files.  Each 
    filespec specifies a redo log file group containing one or more redo 
    log file members, or copies.  All redo log files specified in a 

    CREATE DATABASE statement are added to redo log thread number 1. 

    You can also choose the value of the GROUP parameter for the redo 
    log file group.  Each value uniquely identifies a redo log file 
    group and can range from 1 to the value of the MAXLOGFILES 
    parameter.  You cannot specify multiple redo log file groups having 
    the same GROUP value.  If you omit this parameter, Oracle generates 
    its value automatically.  You can examine the GROUP value for a redo 

    log file group through the dynamic performance table V$LOG. 

    If you omit the LOGFILE clause, Oracle creates two redo log file 
    groups by default.  The names and sizes of the default files vary 
    depending on your operating system. 

MAXLOGFILES 
    specifies the maximum number of redo log file groups that can ever 
    be created for the database.  Oracle uses this value to determine 
    how much space in the control file to allocate for the names of redo 

    log files.  The default, minimum, and maximum values vary depending 
    on your operating system. 

    The number of redo log file groups accessible to your instance is 
    also limited by the initialization parameter LOG_FILES. 

MAXLOGMEMBERS 
    specifies the maximum number of members, or copies, for a redo log 
    file group.  Oracle uses this value to determine how much space in 
    the control file to allocate for the names of redo log files. The 

    minimum value is 1.  The maximum and default values vary depending 
    on your operating system. 

MAXLOGHISTORY 
    specifies the maximum number of archived redo log files for 
    automatic media recovery of Oracle with the Parallel Server option. 
    Oracle uses this value to determine how much space in the control 
    file to allocate for the names of archived redo log files.  The 
    minimum value is 0.  The default value is a multiple of the 

    MAXINSTANCES value and varies depending on your operating system. 
    The maximum value is limited only by the maximum size of the control 
    file.  Note that this parameter is only useful if you are using the 
    Oracle with the Parallel Server option in parallel mode and 
    archivelog mode. 

DATAFILE 
    specifies one or more files to be used as data files.  These files 
    all become part of the SYSTEM tablespace.  If you omit this clause, 

    Oracle creates one data file by default.  The name and size of this 
    default file depends on your operating system. 

MAXDATAFILES 
    specifies the maximum number of data files that can ever be created 
    for the database.  The minimum value is 1.  The maximum and default 
    values depend on your operating system. 

    The number of data files accessible to your instance is also limited 
    by the initialization parameter DB_FILES 


MAXINSTANCES 
    specifies the maximum number of instances that can simultaneously 
    have this database mounted and open.  This value takes precedence 
    over the value of the initialization parameter INSTANCES.  The 
    minimum value is 1.  The maximum and default values depend on your 
    operating system. 

ARCHIVELOG 
    establishes archivelog mode for redo log file groups.  In this mode, 

    the contents of a redo log file group must be archived before the 
    group can be reused.  This option prepares for the possibility of 
    media recovery. 

NOARCHIVELOG 
    establishes noarchivelog mode for redo log files groups.  In this 
    mode, the contents of a redo log file group need not be archived 
    before the group can be reused.  This option does not prepares for 
    the possibility of media recovery. 


    The default is noarchivelog mode.  After creating the database, you 
    can change between archivelog mode and noarchivelog mode with the 
    ALTER DATABASE command. 

EXCLUSIVE 
    mounts the database in exclusive mode after it is created.  This 
    mode allows only your instance to access the database.  Oracle 
    automatically mounts the database in exclusive mode after creating 
    it, so this keyword is entirely optional. 


    For multiple instances to access the database, you must first create 
    the database, close and dismount the database, and then mount it in 
    parallel mode.  For information on closing, dismounting, and 
    mounting the database, see the ALTER DATABASE command. 

CHARACTER SET 
    specifies the character set the database uses to store data.  You 
    cannot change the database character set after creating the 

    database.  The supported character sets and default value of this 
    parameter depends on your operating system. 

PREREQUISITES: 

    You must have the OSDBA role enabled. 

    If you are using Trusted Oracle and you plan to use the database in 
    DBMS MAC mode, your operating system label should be the equivalent 
    of DBLOW. 

SEE: 
    ALTER DATABASE, CREATE ROLLBACK SEGMENT, CREATE TABLESPACE "))
("create database link" . (nil "SQL command" "CREATE DATABASE LINK command 

PURPOSE: 

    To create a database link.  A database link is an object in the 
    local database that allows you to access objects on a remote 
    database or to mount a secondary database in read-only mode.  The 
    remote database can be either an Oracle or a non-Oracle database. 

SYNTAX: 

CREATE [PUBLIC] DATABASE LINK dblink 
    [CONNECT TO user IDENTIFIED BY password] 

    [USING 'connect_string'] 

where: 

PUBLIC 
    creates a public database link available to all users.  If you omit 
    this option, the database link is private and is available only to 
    you. 

dblink 
    is the complete or partial name of the database link.  For 
    guidelines for naming database links, see the section Referring to 
    Objects in Remote Databases. 

CONNECT TO user 

IDENTIFIED BY password 
    is the username and password used to connect to the remote database. 
    If you omit this clause, the database link uses the username and 
    password of each user who uses the database link. 

USING 
    specifies either: 

            * the database specification of a remote database 
            * the specification of a secondary database for a read-only 
              mount. 


    For information on specifying remote databases, see the SQL*Net 
    User's Guide for your specific SQL*Net protocol. 

    Read-only mounts are only available in Trusted Oracle and can only 
    be specified for public database links. 

PREREQUISITES: 

    To create a private database link, you must have CREATE DATABASE 
    LINK system privilege.  To create a public database link, you must 
    have CREATE PUBLIC DATABASE LINK system privilege.  Also, you must 

    have CREATE SESSION privilege on a remote database.  SQL*Net must be 
    installed on both the local and remote databases. 

SEE: 
    CREATE SYNONYM, SELECT "))
("create function" . (nil "SQL command" "CREATE FUNCTION command 

PURPOSE: 

    To create a stand-alone stored function.  A stored function is a set 
    of PL/SQL statements you can call by name.  Stored functions are 
    very similar to procedures, except that a function returns a value 
    to the environment in which it is called. 

SYNTAX: 

CREATE [OR REPLACE] FUNCTION [schema.]function 
    [ (argument [IN] datatype 
    [, argument [IN] datatype] ...)] 

    RETURN datatype 
    {IS | AS} pl/sql_subprogram_body 

where: 

OR REPLACE 
    recreates the function if it already exists.  You can use this 
    option to change the definition of an existing function without 
    dropping, recreating, and regranting object privileges previously 
    granted on the function.  If you redefine a function, Oracle 
    recompiles it.  For information on recompiling functions, see the 

    ALTER FUNCTION command. 

    Users who had previously been granted privileges on a redefined 
    function can still access the function without being regranted the 
    privileges. 

schema 
    is the schema to contain the function.  If you omit schema, Oracle 
    creates the function in your current schema. 

function 
    is the name of the function to be created. 

argument 

    is the name of an argument to the function.  If the function does 
    not accept arguments, you can omit the parentheses following the 
    function name. 

IN 
    specifies that you must supply a value for the argument when calling 
    the function.  This is always true for function arguments, so this 
    keyword is entirely optional. 

    A procedure, rather than a stored function, can accept arguments for 

    which the procedure passes a value back to the calling environment 
    after execution. 

datatype 
    is the datatype of an argument.  An argument can have any datatype 
    supported by PL/SQL. 

    The datatype cannot specify a length, precision, or scale.  Oracle 
    derives the length, precision, or scale of an argument from the 
    environment from which the function is called. 

RETURN datatype 

    specifies the datatype of the function's return value.  Because 
    every function must return a value, this clause is required.  The 
    return value can have any datatype supported by PL/SQL. 

    The datatype cannot specify a length, precision, or scale.  Oracle 
    derives the length, precision, or scale of the return value from the 
    environment from which the function is called. 

pl/sql_subprogram_body 

    is the definition of the function.  Function definitions are written 
    in PL/SQL. 

    To embed a CREATE FUNCTION statement inside an Oracle Precompiler 
    program, you must terminate the statement with the keyword END-EXEC 
    followed by the embedded SQL statement terminator for the specific 
    language. 

PREREQUISITES: 

    Before a stored function can be created, the user SYS must run the 

    SQL script DBMSSTDX.SQL.  The exact name and location of this script 
    may vary depending on your operating system. 

    To create a function in your own schema, you must have CREATE 
    PROCEDURE system privilege.  To create a function in another user's 
    schema, you must have CREATE ANY PROCEDURE system privilege. 

    If you are using Trusted Oracle in DBMS MAC mode, you can create a 
    function in another user's schema if your DBMS label dominates the 

    creation label of the other user. 


SEE: 
    ALTER FUNCTION, CREATE PACKAGE, CREATE PACKAGE BODY, CREATE 
    PROCEDURE, DROP FUNCTION "))
("create index" . (nil "SQL command" "CREATE INDEX command 

PURPOSE: 

    To create an index on one or more columns of a table or a cluster. 
    An index is a database object that contains an entry for each value 
    that appears in the indexed column(s) of the table or cluster and 
    provides direct, fast access to rows. 

SYNTAX: 

CREATE INDEX [schema.]index 
    ON { [schema.]table (column [ASC|DESC][, column [ASC|DESC]] ...) 

       | CLUSTER [schema.]cluster } 
    [INITRANS integer] [MAXTRANS integer] 
    [TABLESPACE tablespace] 
    [STORAGE storage_clause] 
    [PARALLEL ( [DEGREE { integer | DEFAULT }] 
                [INSTANCES {integer | DEFAULT }] 
              ) 
    | NOPARALLEL ] 
    [PCTFREE integer] 
    [NOSORT] 

where: 

schema 
    is the schema to contain the index.  If you omit schema, Oracle 

    creates the index in your own schema. 

index 
    is the name of the index to be created. 

table 
    is the name of the table for which the index is to be created.  If 
    you do not qualify table with schema, Oracle assumes the table is 
    contained in your own schema. 

column 
    is the name of a column in the table.  An index can have as many as 
    16 columns.  A column of an index cannot be of datatype LONG or LONG 

    RAW. 

ASC 
DESC 
    are allowed for DB2 syntax compatibility, although indexes are 
    always created in ascending order.  Indexes on character data are 
    created in ascending order of the character values in the database 
    character set. 

CLUSTER 
    specifies the cluster for which a cluster index is to be created. 
    If you do not qualify cluster with schema, Oracle assumes the 

    cluster is contained in your current schema.  You cannot create a 
    cluster index for a hash cluster. 

INITRANS 
MAXTRANS 
    establishes values for these parameters for the index.  See the 
    INITRANS and MAXTRANS parameters of the CREATE TABLE command. 

TABLESPACE 
    is the name of the tablespace to hold the index.  If you omit this 
    option, Oracle creates the index in the default tablespace of the 

    owner of the schema containing the index. 

STORAGE 
    establishes the storage characteristics for the index. 

PARALLEL 
    DEGREE specifies the number of processes that create an index 
    in parallel.  DEFAULT specifies the degree of parallelism is 
    based on the parallelism specified in the table's definition. 

NOPARALLEL 
    specifies that the index should not be created in parallel. 


PCTFREE 
    is the percentage of space to leave free for updates and insertions 
    within each of the index's data blocks. 

NOSORT 
    indicates to Oracle that the rows are stored in the database in 
    ascending order and therefore Oracle does not have to sort the rows 
    when creating the index. 

PREREQUISITES: 

    To create an index in your own schema, one of these conditions must 

    be true: 

    * The table or cluster to be indexed must be in your own schema. 
    * You must have INDEX privilege on the table to be indexed. 
    * You must have CREATE ANY INDEX system privilege. 

    To create an index in another schema, you must have CREATE ANY INDEX 
    system privilege. 

    Also, the owner of the schema to contain the index must have either 
    space quota on the tablespace to contain the index or UNLIMITED 

    TABLESPACE system privilege. 

    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 
    must dominate the tablespace's label and match the table's label. 
    If the table was created at DBHIGH or DBLOW, you must explicitly set 
    your label to DBHIGH or DBLOW.  You can create an index in another 
    user's schema if your DBMS label dominates the creation label of the 
    other user. 

SEE: 

    ALTER INDEX, CONSTRAINT clause, DROP INDEX, STORAGE clause "))
("create package" . (nil "SQL command" "CREATE PACKAGE command 

PURPOSE: 

    To create the specification for a stored package.  A package is an 
    encapsulated collection of related procedures, functions, and other 
    program objects stored together in the database.  The specification 
    declares these objects. 

SYNTAX: 

CREATE [OR REPLACE] PACKAGE [schema.]package 
    {IS | AS} pl/sql_package_spec 

where: 


OR REPLACE 
    recreates the package specification if it already exists.  You can 
    use this option to change the specification of an existing package 
    without dropping, recreating, and regranting object privileges 
    previously granted on the package.  If you change a package 
    specification, Oracle recompiles it.  For information on recompiling 
    package specifications, see the ALTER PROCEDURE command. 


    Users who had previously been granted privileges on a redefined 
    package can still access the package without being regranted the 
    privileges. 

schema 
    is the schema to contain the package.  If you omit schema, Oracle 
    creates the package in your own schema. 

package 
    is the name of the package to be created. 

pl/sql_package_spec 
    is the package specification.  The package specification can declare 

    program objects.  Package specifications are written in PL/SQL. 

    To embed a CREATE PACKAGE statement inside an Oracle Precompiler 
    program, you must terminate the statement with the keyword END-EXEC 
    followed by the embedded SQL statement terminator for the specific 
    language. 

PREREQUISITES: 

    Before a package can be created, the user SYS must run the SQL 
    script DBMSSTDX.SQL.  The exact name and location of this script may 

    vary depending on your operating system. 

    To create a package in your own schema, you must have CREATE 
    PROCEDURE system privilege.  To create a package in another user's 
    schema, you must have CREATE ANY PROCEDURE system privilege. 

    If you are using Trusted Oracle in DBMS MAC mode, you can only 
    create a package in another user's schema if your DBMS label 
    dominates the creation label of the other user. 


    To create a package, you must be using Oracle with the procedural 
    option. 

SEE: 
    ALTER PACKAGE, CREATE FUNCTION, CREATE PACKAGE BODY, CREATE 
    PROCEDURE, DROP PACKAGE "))
("create package body" . (nil "SQL command" "CREATE PACKAGE BODY command 

PURPOSE: 

    To create the body of a stored package.  A package is an 
    encapsulated collection of related procedures, stored functions, and 
    other program objects stored together in the database.  The body 
    defines these objects. 

SYNTAX: 

CREATE [OR REPLACE] PACKAGE BODY [schema.]package 
    {IS | AS} pl/sql_package_body 

where: 

OR REPLACE 

    recreates the package body if it already exists.  You can use this 
    option to change the body of an existing package without dropping, 
    recreating, and regranting object privileges previously granted on 
    it.  If you change a package body, Oracle recompiles it.  For 
    information on recompiling package bodies, see the ALTER PACKAGE 
    command. 

    Users who had previously been granted privileges on a redefined 

    package can still access the package without being regranted the 
    privileges. 

schema 
    is the schema to contain the package.  If you omit schema, Oracle 
    creates the package in your current schema. 

package 
    is the name of the package to be created. 

pl/sql_package_body 
    is the package body.  The package body can declare and define 
    program objects.  Package bodies are written in PL/SQL. 


    To embed a CREATE PACKAGE BODY statement inside an Oracle 
    Precompiler program, you must terminate the statement with the 
    keyword END-EXEC followed by the embedded SQL statement terminator 
    for the specific language. 

PREREQUISITES: 

    Before a package can be created, the user SYS must run the SQL 
    script DBMSSTDX.SQL.  The exact name and location of this script may 
    vary depending on your operating system. 


    To create a package in your own schema, you must have CREATE 
    PROCEDURE system privilege.  To create a package in another user's 
    schema, you must have CREATE ANY PROCEDURE system privilege. 

    If you are using Trusted Oracle in DBMS MAC mode, you can only 
    create a package in another user's schema if your DBMS label 
    dominates the creation label of the other user. 

    To create a package, you must be using Oracle with the procedural 

    option. 

SEE: 
    ALTER PACKAGE, CREATE FUNCTION, CREATE PACKAGE, CREATE PROCEDURE, 
    DROP PACKAGE "))
("create procedure" . (nil "SQL command" "CREATE PROCEDURE command 

PURPOSE: 

    To create a stand-alone stored procedure.  A procedure  is a group 
    of PL/SQL statements that you can call by name. 

SYNTAX: 

CREATE [OR REPLACE] PROCEDURE [schema.]procedure 
    [ (argument [IN | OUT | IN OUT] datatype 
    [, argument [IN | OUT | IN OUT] datatype] ...)] 
    {IS | AS} pl/sql_subprogram_body 

where: 

OR REPLACE 

    recreates the procedure if it already exists.  You can use this 
    option to change the definition of an existing procedure without 
    dropping, recreating, and regranting object privileges previously 
    granted on it.  If you redefine a procedure, Oracle recompiles it. 
    For information on recompiling procedures, see the ALTER PROCEDURE 
    command. 

    Users who had previously been granted privileges on a redefined 

    procedure can still access the procedure without being regranted the 
    privileges. 

schema 
    is the schema to contain the procedure.  If you omit schema, Oracle 
    creates the procedure in your current schema. 

procedure 
    is the name of the procedure to be created. 

argument 
    is the name of an argument to the procedure.  If the procedure does 
    not accept arguments, you can omit the parentheses following the 

    procedure name. 

IN 
    specifies that you must specify a value for the argument when 
    calling the procedure. 

OUT 
    specifies that the procedure passes a value for this argument back 
    to its calling environment after execution. 

IN OUT 
    specifies that you must specify a value for the argument when 
    calling the procedure and that the procedure passes a value back to 

    its calling environment after execution. 

    If you omit IN, OUT, and IN OUT, the argument defaults to IN. 

datatype 
    is the datatype of an argument.  An argument can have any datatype 
    supported by PL/SQL. 

    The datatype cannot specify a length, precision, or scale.  Oracle 
    derives the length, precision, or scale of an argument from the 
    environment from which the procedure is called. 


pl/sql_subprogram_body 
    is the definition of the procedure.  Procedure definitions are 
    written in PL/SQL. 

    To embed a CREATE PROCEDURE statement inside an Oracle Precompiler 
    program, you must terminate the statement with the keyword END-EXEC 
    followed by the embedded SQL statement terminator for the specific 
     language. 

PREREQUISITES: 

    Before a procedure can be created, the user SYS must run the SQL 

    script DBMSSTDX.SQL.  The exact name and location of this script may 
    vary depending on your operating system. 

    To create a procedure in your own schema, you must have CREATE 
    PROCEDURE system privilege.  To create a procedure in another 
    schema, you must have CREATE ANY PROCEDURE system privilege. 

    If you are using Trusted Oracle in DBMS MAC mode, you can only 
    create a procedure in another user's schema if your DBMS label 

    dominates the creation label of the other user. 

    To create a procedure, you must be using Oracle with the procedural 
    option. 

SEE: 
    ALTER PROCEDURE, CREATE FUNCTION, CREATE PACKAGE, CREATE PACKAGE 
    BODY, DROP PROCEDURE "))
("create profile" . (nil "SQL command" "CREATE PROFILE command 

PURPOSE: 

    To create a profile.  A profile is a set of limits on database 
    resources.  If you assign the profile to a user, that user cannot 
    exceed these limits. 

SYNTAX: 

CREATE PROFILE profile 
    LIMIT   [SESSIONS_PER_USER          {integer | UNLIMITED | DEFAULT}] 
            [CPU_PER_SESSION            {integer | UNLIMITED | DEFAULT}] 
            [CPU_PER_CALL               {integer | UNLIMITED | DEFAULT}] 

            [CONNECT_TIME               {integer | UNLIMITED | DEFAULT}] 
            [IDLE_TIME                  {integer | UNLIMITED | DEFAULT}] 
            [LOGICAL_READS_PER_SESSION  {integer | UNLIMITED | DEFAULT}] 
            [LOGICAL_READS_PER_CALL     {integer | UNLIMITED | DEFAULT}] 
            [COMPOSITE_LIMIT            {integer | UNLIMITED | DEFAULT}] 
            [PRIVATE_SGA          {integer [K|M] | UNLIMITED | DEFAULT}] 


where: 

profile 
    is the name of the profile to be created. 

SESSIONS_PER_USER 
    limits a user to integer concurrent sessions. 

CPU_PER_SESSION 
    limits the CPU time for a session.  This value is expressed in 
    hundredths of seconds. 

CPU_PER_CALL 
    limits the CPU time for a call (a parse, execute, or fetch).  This 
    value is expressed in hundredths of seconds. 


CONNECT_TIME 
    limits the total elapsed time of a session.  This value is expressed 
    in minutes. 

IDLE_TIME 
    limits periods of continuous inactive time during a session.  This 
    value is expressed in minutes.  Long-running queries and other 
    operations are not subject to this limit. 

LOGICAL_READS_PER_SESSION 
    limits the number of data blocks read in a session, including blocks 

    read from memory and disk, to integer blocks. 

LOGICAL_READS_PER_CALL 
    limits the number of data blocks read for a call to process a SQL 
    statement (a parse, execute, or fetch) to integer blocks. 

PRIVATE_SGA 
    limits the amount of private space a session can allocate in the 
    shared pool of the System Global Area (SGA) to integer bytes.  You 
    can also use the K or M to specify this limit in kilobytes or 

    megabytes.  This limit only applies if you are using the multi- 
    threaded server architecture.  The private space for a session in 
    the SGA includes private SQL and PL/SQL areas, but not shared SQL 
    and PL/SQL areas. 

COMPOSITE_LIMIT 
    limits the total resource cost for a session.  You must express the 
    value of this parameter in service units. 

    Oracle calculates the total resource cost as a weighted sum of these 

    resources: 

            * CPU_PER_SESSION 
            * CONNECT_TIME 
            * LOGICAL_READS_PER_SESSION 
            * PRIVATE_SGA 

    For information on how to specify the weight for each session 
    resource see the ALTER RESOURCE COST command. 

UNLIMITED 
    indicates that a user assigned this profile can use an unlimited 
    amount of this resource. 

DEFAULT 

    omits a limit for this resource in this profile.  A user assigned 
    this profile is subject to the limit for this resource specified in 
    the DEFAULT profile. 

PREREQUISITES: 

    You must have CREATE PROFILE system privilege. 

SEE: 
    ALTER PROFILE, ALTER RESOURCE COST, ALTER SYSTEM, ALTER USER, DROP 
    PROFILE "))
("create role" . (nil "SQL command" "CREATE ROLE command 

PURPOSE: 

    To create a role.  A role is a set of privileges that can be granted 
    to users or to other roles. 

SYNTAX: 

CREATE ROLE role 
    [ NOT IDENTIFIED 
    | IDENTIFIED {BY password | EXTERNALLY} ] 

where: 

role 
    is the name of the role to be created.  Oracle Corporation 
    recommends that the role contain at least one single-byte character 

    regardless of whether the database character set also contains 
    multi-byte characters. 

NOT IDENTIFIED 
    indicates that a user granted the role need not be verified when 
    enabling it. 

IDENTIFIED 
    indicates that a user granted the role must be verified when 
    enabling it with the SET ROLE command: 
            BY password 
                   The user must specify the password to Oracle when 

                   enabling the role.  The password can only contain 
                   single-byte characters from your database character 
                   set regardless of whether this character set also 
                   contains multi-byte characters. 
            EXTERNALLY 
                   The operating system verifies the user enabling to 
                   the role.  Depending on the operating system, the 

                   user may have to specify a password to the operating 
                   system when enabling the role. 

    If you omit both the NOT IDENTIFIED option and the IDENTIFIED 
    clause, the role defaults to NOT IDENTIFIED. 

PREREQUISITES: 

    You must have CREATE ROLE system privilege. 

SEE: 
    ALTER ROLE, DROP ROLE, GRANT (System Privileges and Roles), REVOKE 

    (System Privileges and Roles), SET ROLE "))
("create rollback segment" . (nil "SQL command" "CREATE ROLLBACK SEGMENT command 

PURPOSE: 

    To create a rollback segment.  A rollback segment is an object that 
    is used by Oracle to store data necessary to reverse, or undo, 
    changes made by transactions. 

SYNTAX: 

CREATE [PUBLIC] ROLLBACK SEGMENT rollback_segment 
    [TABLESPACE tablespace] 
    [STORAGE storage_clause] 

where: 

PUBLIC 
    specifies that the rollback segment is public and is available to 

    any instance.  If you omit this option, the rollback segment is 
    private and is only available to the instance naming it in its 
    initialization parameter ROLLBACK_SEGMENTS. 

rollback_segment 
    is the name of the rollback segment to be created. 

TABLESPACE 
    identifies the tablespace in which the rollback segment is created. 
    If you omit this option, Oracle creates the rollback segment in the 

    SYSTEM tablespace. 

STORAGE 
    specifies the characteristics for the rollback segment. 

PREREQUISITES: 

    You must have CREATE ROLLBACK SEGMENT system privilege.  Also, you 
    must have either space quota on the tablespace to contain the 
    rollback segment or UNLIMITED TABLESPACE system privilege. 

    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 
    must dominate the tablespace's label. 


SEE: 
    ALTER ROLLBACK SEGMENT, CREATE DATABASE, CREATE TABLESPACE, DROP 
    ROLLBACK SEGMENT, STORAGE clause "))
("create schema" . (nil "SQL command" "CREATE SCHEMA command 

PURPOSE: 

    To create multiple tables and views and perform multiple grants in a 
    single transaction. 

SYNTAX: 

CREATE SCHEMA AUTHORIZATION schema 
    { CREATE TABLE command 
    | CREATE VIEW command 
    | GRANT command } ... 

where: 

schema 
    is the name of the schema.  The schema name must be the same as your 
    Oracle username. 


CREATE TABLE command 
    is a CREATE TABLE statement to be issued as part of this CREATE 
    SCHEMA statement. 

CREATE VIEW command 
    is a CREATE VIEW statement to be issued as part of this CREATE 
    SCHEMA statement. 

GRANT command 
    is a GRANT statement (Objects Privileges) to be issued as part of 
    this CREATE SCHEMA statement. 

    The CREATE SCHEMA statement only supports the syntax of these 

    commands as defined by standard SQL, rather than the complete syntax 
    supported by Oracle. 

PREREQUISITES: 

    The CREATE SCHEMA statement can include CREATE TABLE, CREATE VIEW, 
    and GRANT statements.  To issue a CREATE SCHEMA statement, you must 
    have the privileges necessary to issue the included statements. 

SEE: 
    CREATE TABLE, CREATE VIEW, GRANT "))
("create sequence" . (nil "SQL command" "CREATE SEQUENCE command 

PURPOSE: 

    To create a sequence.  A sequence is a database object from which 
    multiple users may generate unique integers.  You can use sequences 
    to automatically generate primary key values. 

SYNTAX: 

CREATE SEQUENCE [schema.]sequence 
    [INCREMENT BY integer] 
    [START WITH integer] 
    [MAXVALUE integer | NOMAXVALUE] 
    [MINVALUE integer | NOMINVALUE] 

    [CYCLE | NOCYCLE] 
    [CACHE integer | NOCACHE] 
    [ORDER | NOORDER] 

where: 

schema 
    is the schema to contain the sequence.  If you omit schema, Oracle 
    creates the sequence in your own schema. 

sequence 
    is the name of the sequence to be created. 

INCREMENT BY 
    specifies the interval between sequence numbers.  This value can be 
    any positive or negative Oracle integer, but it cannot be 0.  If 

    this value is negative, then the sequence descends.  If the 
    increment is positive, then the sequence ascends.  If you omit this 
    clause, the interval defaults to 1. 

MINVALUE 
    specifies the sequence's minimum value. 

NOMINVALUE 
    specifies a minimum value of 1 for an ascending sequence or -10 
    for a descending sequence. 

    The default is NOMINVALUE. 

MAXVALUE 

    specifies the maximum value the sequence can generate. 

NOMAXVALUE 
    specifies a maximum value of 10 
    for a descending sequence. 

    The default is NOMAXVALUE. 

START WITH 
    specifies the first sequence number to be generated.  You can use 
    this option to start an ascending sequence at a value greater than 
    its minimum or to start a descending sequence at a value less than 

    its maximum.  For ascending sequences, the default value is the 
    sequence's minimum value.  For descending sequences, the default 
    value is the sequence's maximum value. 

CYCLE 
    specifies that the sequence continues to generate values after 
    reaching either its maximum or minimum value.  After an ascending 
    sequence reaches its maximum value, it generates its minimum value. 
    After a descending sequence reaches its minimum, it generates its 

    maximum. 

NOCYCLE 
    specifies that the sequence cannot generate more values after 
    reaching its maximum or minimum value. 

    The default is NOCYCLE. 

CACHE 
    specifies how many values of the sequence Oracle preallocates and 
    keeps in memory for faster access.  The minimum value for this 
    parameter is 2.  For sequences that cycle, this value must be less 
    than the number of values in the cycle. 


NOCACHE 
    specifies that values of the sequence are not preallocated. 

    If you omit both the CACHE parameter and the NOCACHE option, Oracle 
    caches 20 sequence numbers by default.  However, if you are using 
    Oracle with the Parallel Server option in parallel mode and you 
    specify the ORDER option, sequence values are never cached, 
    regardless of whether you specify the CACHE parameter or the NOCACHE 

    option. 

ORDER 
    guarantees that sequence numbers are generated in order of request. 
    You may want to use this option if you are using the sequence 
    numbers as timestamps.  Guaranteeing order is usually not important 
    for sequences used to generate primary keys. 

NOORDER 
    does not guarantee sequence numbers are generated in order of 
    request. 

    If you omit both the ORDER and NOORDER options, Oracle chooses 

    NOORDER by default.  Note that the ORDER option is only necessary to 
    guarantee ordered generation if you are using Oracle with the 
    Parallel Server option in parallel mode.  If you are using exclusive 
    mode, sequence numbers are always generated in order. 

PREREQUISITES: 

    To create a sequence in your own schema, you must have CREATE 
    SEQUENCE privilege. 

    To create a sequence in another user's schema, you must have CREATE 

    ANY SEQUENCE privilege.  If you are using Trusted Oracle in DBMS MAC 
    mode, your DBMS label must dominate the creation label of the owner 
    of the schema to contain the sequence. 

SEE: 
    ALTER SEQUENCE, DROP SEQUENCE "))
("create snapshot" . (nil "SQL command" "CREATE SNAPSHOT command 

PURPOSE: 

    To create a snapshot.  A snapshot is a table that contains the 
    results of a query of one or more tables or views, often located on 
    a remote database. 

SYNTAX: 

CREATE SNAPSHOT [schema.]snapshot 
    [ [PCTFREE  integer] [PCTUSED  integer] 
      [INITRANS integer] [MAXTRANS integer] 
      [TABLESPACE tablespace] 
      [STORAGE storage_clause] 

    [ USING INDEX [  PCTFREE integer | TABLESPACE tablespace 
                    | INITTRANS integer | MAXTRANS integer 
                    | STORAGE storage_clause ] ... 
    | [CLUSTER cluster (column [, column]...)] ] 
    [ REFRESH [FAST | COMPLETE | FORCE] [START WITH date] [NEXT date]] 
    AS subquery 

where: 

schema 
    is the schema to contain the snapshot.  If you omit schema, Oracle 

    creates the snapshot in your schema. 

snapshot 
    is the name of the snapshot to be created. 

    Oracle chooses names for the table, views, and index used to 
    maintain the snapshot by prefixing the snapshot name.  To limit 
    these names to 30 bytes and allow them to contain the entire 
    snapshot name, Oracle Corporation recommends that you limit your 
    snapshot names to 23 bytes. 


PCTFREE 
PCTUSED 
INITRANS 
MAXTRANS 
    establishes values for these parameters for the internal table 
    Oracle uses to maintain the snapshot's data. 

TABLESPACE 
    specifies the tablespace in which the snapshot is to be created.  If 
    you omit this option, Oracle creates the snapshot in the default 
    tablespace of the owner of the snapshot's schema. 

STORAGE 
    establishes storage characteristics for the table Oracle uses to 

    maintain the snapshot's data. 

USING INDEX 
    specifies the storage characteristics for the index on a simple 
    snapshot.  If the USING INDEX clause not specified, the index is 
    create with the same tablespace and storage parameters as the 
    snapshot. 

CLUSTER 
    creates the snapshot as part of the specified cluster.  Since a 
    clustered snapshot uses the cluster's space allocation, do not use 

    the PCTFREE, PCTUSED, INITRANS, or MAXTRANS parameters, the 
    TABLESPACE option, or the STORAGE clause in conjunction with the 
    CLUSTER option. 

REFRESH 
    specifies how and when Oracle automatically refreshes the snapshot: 
            FAST 
                   specifies a fast refresh, or a refresh using only the 
                   updated data stored in the snapshot log associated 
                   with the master table. 

            COMPLETE 
                   specifies a complete refresh, or a refresh that re- 
                   executes the snapshot's query. 
            FORCE 
                   specifies a fast refresh if one is possible or 
                   complete refresh if a fast refresh is not possible. 
                   Oracle decides whether a fast refresh is possible at 
                   refresh time. 

                   If you omit the FAST, COMPLETE, and FORCE options, 
                   Oracle uses FORCE by default. 
            START WITH 
                   specifies a date expression for the first automatic 
                   refresh time. 
            NEXT 
                   specifies a date expression for calculating the 
                   interval between automatic refreshes. 

    Both the START WITH and NEXT values must evaluate to a time in the 

    future.  If you omit the START WITH value, Oracle determines the 
    first automatic refresh time by evaluating the NEXT expression when 
    you create the snapshot.  If you specify a START WITH value but omit 
    the NEXT value, Oracle refreshes the snapshot only once.  If you 
    omit both the START WITH and NEXT values or if you omit the REFRESH 
    clause entirely, Oracle does not automatically refresh the snapshot. 


AS subquery 
    specifies the snapshot query.  When you create the snapshot, Oracle 
    executes this query and places the results in the snapshot.  The 
    select list can contain up to 253 expressions.  A snapshot query is 
    subject to the same restrictions as a view query. 

PREREQUISITES: 

    To create a snapshot in your own schema, you must have CREATE 
    SNAPSHOT system privilege.  To create a snapshot in another user's 

    schema, you must have CREATE ANY SNAPSHOT system privilege. 

    Before a snapshot can be created, the user SYS must run the SQL 
    script DBMSSNAP.SQL on both the database to contain the snapshot and 
    the database(s) containing the tables and views of the snapshot's 
    query.  This script creates the package SNAPSHOT which contains both 
    public and private stored procedures used for refreshing the 
    snapshot and purging the snapshot log.  The exact name and location 

    of this script may vary depending on your operating system. 

    When you create a snapshot, Oracle creates a table, two views, and 
    an index in the schema of the snapshot.  Oracle uses these objects 
    to maintain the snapshot's data.  You must have the privileges 
    necessary to create these objects.  For information on these 
    privileges, see the CREATE TABLE, CREATE VIEW, and CREATE INDEX 

    commands. 

    The owner of the schema containing the snapshot must have either 
    space quota on the tablespace to contain the snapshot or UNLIMITED 
    TABLESPACE system privilege.  Also, both you (the creator) and the 
    owner must also have the privileges necessary to issue the 
    snapshot's query. 

    To create a snapshot, you must be using Oracle with the procedural 
    option.  To create a snapshot on a remote table or view, you must 

    also be using the distributed option. 

SEE: 
    ALTER SNAPHSOT, CREATE SNAPSHOT LOG, DROP SNAPSHOT "))
("create snapshot log" . (nil "SQL command" "CREATE SNAPSHOT LOG command 

PURPOSE: 

    To create a snapshot log.  A snapshot log is a table associated with 
    the master table of a snapshot.  Oracle stores changes to the master 
    table's data in the snapshot log and then uses the snapshot log to 
    refresh the master table's snapshots. 

SYNTAX: 

CREATE SNAPSHOT LOG ON [schema.]table 
    [PCTFREE  integer]    [PCTUSED  integer] 

    [INITRANS integer]    [MAXTRANS integer] 
    [TABLESPACE tablespace] 
    [STORAGE storage_clause] 

where: 

schema 
    is the schema containing the snapshot log's master table.  If you 
    omit schema, Oracle assumes the master table is contained in your 
    own schema. Oracle creates the snapshot log in the schema of its 
    master table.  You cannot create a snapshot log for a table in the 

    schema of the user SYS. 

table 
    is the name of the master table for which the snapshot log is to be 
    created.  You cannot create a snapshot log for a view. 

    Oracle chooses names for the table and trigger used to maintain the 
    snapshot log by prefixing the master table name.  To limit these 
    names to 30 bytes and allow them to contain the entire master table 
    name, Oracle Corporation recommends that you limit master table 

    names to 24 bytes. 

PCTFREE 
PCTUSED 
INITRANS 
MAXTRANS 
    establishes values for these parameters for the snapshot log. 

TABLESPACE 
    specifies the tablespace in which the snapshot log is to be created. 
    If you omit this option, Oracle creates the snapshot log in the 
    default tablespace the owner of the snapshot log's schema. 

STORAGE 
    establishes storage characteristics for the snapshot log. 


PREREQUISITES: 

    You must have the privileges necessary to create a table in the 
    schema of the master table.  For information on these privileges, 
    see the CREATE TABLE command. 

    Before a snapshot log can be created, the user SYS must run the SQL 
    script DBMSSNAP.SQL on the database containing the master table. 
    This script creates the package SNAPSHOT which contains both public 

    and private stored procedures used for refreshing the snapshot and 
    urging the snapshot log.  The exact name and location of this script 
    may vary depending on your operating system. 

    You must also have the privileges to create a trigger on the master 
    table.  For information on these privileges, see the CREATE TRIGGER 
    command. 


    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 

    must dominate the label of the tablespace in which the snapshot log 
    is to be stored. 

SEE: 
    ALTER SNAPSHOT LOG, CREATE SNAPSHOT, DROP SNAPSHOT LOG "))
("create synonym" . (nil "SQL command" "CREATE SYNONYM command 

PURPOSE: 

    To create a synonym.  A synonym is an alternative name for a table, 
    view, sequence, procedure, stored function, package, snapshot, or 
    another synonym. 

SYNTAX: 

CREATE [PUBLIC] SYNONYM [schema.]synonym 
    FOR [schema.]object[@dblink] 

where: 

PUBLIC 
    creates a public synonym.  Public synonyms are accessible to all 

    users.  If you omit this option, the synonym is private and is 
    accessible only within its schema. 

schema 
    is the schema to contain the synonym.  If you omit schema, Oracle 
    creates the synonym in your own schema. 

synonym 
    is the name of the synonym to be created. 

FOR 
    identifies the object for which the synonym is created.  If you do 
    not qualify object with schema, Oracle assumes that the object is in 

    your own schema.  The object can be of these types: 

          * table 
          * view 
          * sequence 
          * stored procedure, function, or package 
          * snapshot 
          * synonym 

    The object cannot be contained in a package.  Note that the object 
    need not currently exist and you need not have privileges to access 
    the object. 

    You can use a complete or partial dblink to create a synonym for an 

    object on a remote database where the object is located.  If you 
    specify dblink and omit schema, the synonym refers to an object in 
    the schema specified by the database link.  Oracle Corporation 
    recommends that you specify the schema containing the object in the 
    remote database. 

    If you omit dblink, Oracle assumes the object is located on the 
    local database. 

PREREQUISITES: 


    To create a private synonym in your own schema, you must have CREATE 
    SYNONYM system privilege. 

    To create a private synonym in another user's schema, you must have 
    CREATE ANY SYNONYM system privilege.  If you are using Trusted 
    Oracle in DBMS MAC mode, your DBMS label must dominate the creation 
    label of the owner of schema to contain the synonym. 

    To create a PUBLIC synonym, you must have CREATE PUBLIC SYNONYM 

    system privilege. 

SEE: 
    CREATE DATABASE LINK, CREATE TABLE, CREATE VIEW "))
("create table" . (nil "SQL command" "CREATE TABLE command 

PURPOSE: 

    To create a table, the basic structure to hold user data, specifying 
    this information: 

    * column definitions 
    * integrity constraints 
    * the table's tablespace 
    * storage characteristics 
    * an optional cluster 
    * data from an arbitrary query 

SYNTAX: 

CREATE TABLE [schema.]table 
     ( { column datatype [DEFAULT expr] [column_constraint] ... 

       | table_constraint} 
    [, { column datatype [DEFAULT expr] [column_constraint] ... 
       | table_constraint} ]...) 
    [ [PCTFREE  integer] [PCTUSED  integer] 
      [INITRANS integer] [MAXTRANS integer] 
      [TABLESPACE tablespace] 
      [STORAGE storage_clause] 
    [  PARALLEL ( [ DEGREE { integer | DEFAULT } ] 
                  [ INSTANCES { integer | DEFAULT } ] 
                ) 

     | NOPARALLEL ] 
    [  CACHE | NOCACHE  ] 
    | [CLUSTER cluster (column [, column]...)] ] 
    [ ENABLE   enable_clause 
    | DISABLE disable_clause ] ... 
    [AS subquery] 

where: 

schema 
    is the schema to contain the table.  If you omit schema, Oracle 
    creates the table in your own schema. 

table 
    is the name of the table to be created. 

column 

    specifies the name of a column of the table.  The number of columns 
    in a table can range from 1 to 254. 

datatype 
    is the datatype of a column. 

DEFAULT 
    specifies a value to be assigned to the column if a subsequent 
    INSERT statement omits a value for the column.  The datatype of the 
    expression must match the datatype of the column.  A DEFAULT 
    expression cannot contain references to other columns, the 

    pseudocolumns CURRVAL, NEXTVAL, LEVEL, and ROWNUM, or date constants 
    that are not fully specified. 

column_constraint 
    defines an integrity constraint as part of the column definition. 

table_constraint 
    defines an integrity constraint as part of the table definition. 

PCTFREE 
    specifies the percentage of space in each of the table's data blocks 
    reserved for future updates to the table's rows.  The value of 

    PCTFREE must be a positive integer from 1 to 99.  A value of 0 
    allows the entire block to be filled by inserts of new rows.  The 
    default value is 10.  This value reserves 10% of each block for 
    updates to existing rows and allows inserts of new rows to fill a 
    maximum of 90% of each block. 

    PCTFREE has the same function in the commands that create and alter 
    clusters, indexes, snapshots, and snapshot logs.  The combination of 

    PCTFREE and PCTUSED determines whether inserted rows will go into 
    existing data blocks or into new blocks. 

PCTUSED 
    specifies the minimum percentage of used space that Oracle maintains 
    for each data block of the table.  A block becomes a candidate for 
    row insertion when its used space falls below PCTUSED.  PCTUSED is 
    specified as a positive integer from 1 to 99 and defaults to 40. 


    PCTUSED has the same function in the commands that create and alter 
    clusters, snapshots, and snapshot logs. 

    The sum of PCTFREE and PCTUSED must be less than 100.  You can use 
    PCTFREE and PCTUSED together use space within a table more 
    efficiently. 

INITRANS 
    specifies the initial number of transaction entries allocated within 
    each data block allocated to the table.  This value can range from 1 

    to 255 and defaults to 1.  In general, you should not change the 
    INITRANS value from its default. 

    Each transaction that updates a block requires a transaction entry 
    in the block.  The size of a transaction entry depends on your 
    operating system. 

    This parameter ensures that a minimum number of concurrent 
    transactions can update the block and helps avoid the overhead of 
    dynamically allocating a transaction entry. 


    The INITRANS parameter serves the same purpose in clusters, indexes, 
    snapshots, and snapshot logs as in tables.  The minimum and default 
    INITRANS value for a cluster or index is 2, rather than 1. 

MAXTRANS 
    specifies the maximum number of concurrent transactions that can 
    update a data block allocated to the table.  This limit does not 
    apply to queries.  This value can range from 1 to 255 and the 

    default is a function of the data block size.  You should not change 
    the MAXTRANS value from its default. 

    If the number concurrent transactions updating a block exceeds the 
    INITRANS value, Oracle dynamically allocates transaction entries in 
    the block until either the MAXTRANS value is exceeded or the block 
    has no more free space. 

    The MAXTRANS parameter serves the same purpose in clusters, 

    snapshots, and snapshot logs as in tables. 

TABLESPACE 
    specifies the tablespace in which Oracle creates the table.  If you 
    omit this option, then Oracle creates the table in the default 
    tablespace of the owner of the schema containing the table. 

STORAGE 
    specifies the storage characteristics for the table.  This clause 
    has performance ramifications for large tables.  Storage should be 

    allocated to minimize dynamic allocation of additional space. 

PARALLEL 
    DEGREE specifies the number of query server processes that can scan 
    the table in parallel.  Either specify a positive integer or DEFAULT 
    which signifies to use the initialization parameter 
    PARALLEL_DEFAULT_SCANSIZE to estimate the number of query servers to use. 

    INSTANCES specifies the minimum number of instances that need to be 

    available before the table can be spread across all available instances 
    of a Parallel Server.  A positive integer specifies the number of 
    instances.  DEFAULT signifies that the parameter PARALLEL_MAX_PARTITIONSIZE 
    is used to calculate whether a table is split across all instances' buffer 
    caches. 

NOPARALLEL 
    specifies that queries on this table are not performed in parallel 
    by default.  A hint in the query still causes the query to be 

    performed in parallel. 

CACHE 
    specifies that blocks of this table are placed on the most recently 
    used end of the LRU list of the buffer cache when the a full table scan 
    is performed. 
    This option is useful for small lookup tables. 

NOCACHE 
    specifies that blocks of the table in the buffer cache follow the 
    standard LRU algorithm when a full table scan is performed. 


CLUSTER 
    specifies that the table is to be part of the cluster.  The columns 
    listed in this clause are the table columns that correspond to the 
    cluster's columns.  Generally, the cluster columns of a table are 
    the column or columns that comprise its primary key or a portion of 
    its primary key. 

    Specify one column from the table for each column in the cluster 
    key.  The columns are matched by position, not by name.  Since a 

    clustered table uses the cluster's space allocation, do not use the 
    PCTFREE, PCTUSED, INITRANS, or MAXTRANS parameters, the TABLESPACE 
    option, or the STORAGE clause in conjunction with the CLUSTER 
    option. 

ENABLE 
    enables an integrity constraint. 

DISABLE 
    disables an integrity constraint. 

    Constraints specified in the ENABLE and DISABLE clauses of a CREATE 
    TABLE statement must be defined in the statement.  You can also 

    enable and disable constraints with the ENABLE and DISABLE keywords 
    of the CONSTRAINT clause.  If you define a constraint but do not 
    explicitly enable or disable it, Oracle enables it by default. 

    You cannot use the ENABLE and DISABLE clauses in a CREATE TABLE 
    statement to enable and disable triggers. 

AS subquery 
    inserts the rows returned by the subquery into the table upon its 
    creation. 


    If you include this clause, the column definitions can only specify 
    column names, default values, and integrity constraints, not 
    datatypes.  Oracle derives column datatypes and lengths from the 
    subquery.  Oracle also automatically defines NOT NULL constraints on 
    columns in the new table if they existed on the corresponding 
    columns of the selected table and the subquery does not modify the 

    column value with a SQL function or operator.  A CREATE TABLE 
    statement cannot contain both the AS clause and a referential 
    integrity constraint definition. 

    The number of columns must equal the number of expressions in the 
    subquery.  If all expressions in the subquery are columns, you can 
    omit the columns from the table definition entirely.  In this case, 
    the names of the columns of table are the same as the columns in the 

    subquery. 

PREREQUISITES: 

    To create a table in your own schema, you must have CREATE TABLE 
    system privilege.  To create a table in another user's schema, you 
    must have CREATE ANY TABLE system privilege.  Also, the owner of the 
    schema to contain the table must have either space quota on the 
    tablespace to contain the table or UNLIMITED TABLESPACE system 
    privilege. 

SEE: 

    ALTER TABLE, CONSTRAINT clause, CREATE CLUSTER, CREATE INDEX, CREATE 
    TABLESPACE, DISABLE clause, DROP TABLE, ENABLE clause, STORAGE clause "))
("create tablespace" . (nil "SQL command" "CREATE TABLESPACE command 

PURPOSE: 

    To create a tablespace.  A tablespace is an allocation of space in 
    the database that can contain objects. 

SYNTAX: 

CREATE TABLESPACE tablespace 
    DATAFILE filespec [, filespec] ... 
    [DEFAULT STORAGE storage_clause] 
    [ONLINE | OFFLINE] 

where: 

tablespace 
    is the name of the tablespace to be created. 


DATAFILE 
    specifies the data file or files to comprise the tablespace. 

DEFAULT STORAGE 
    specifies the default storage parameters for all objects created in 
    the tablespace. 

ONLINE 
    makes the tablespace available immediately after creation to users 
    who have been granted access to the tablespace. 

OFFLINE 
    makes the tablespace unavailable after immediately after creation. 


    If you omit both the ONLINE and OFFLINE options, Oracle creates the 
    tablespace online by default.  The data dictionary view 
    DBA_TABLESPACES indicates whether each tablespace is online or 
    offline. 

PREREQUISITES: 

    You must have CREATE TABLESPACE system privilege.  Also, the SYSTEM 
    tablespace must contain at least two rollback segments including the 
    SYSTEM rollback segment. 


SEE: 
    ALTER TABLESPACE, DROP TABLESPACE "))
("create trigger" . (nil "SQL command" "CREATE TRIGGER command 

PURPOSE: 

    To create and enable a database trigger.  A database trigger is a 
    stored PL/SQL block that is associated with a table.  Oracle 
    automatically executes a trigger when a specified SQL statement is 
    issued against the table. 

SYNTAX: 

CREATE [OR REPLACE] TRIGGER [schema.]trigger 
    {BEFORE | AFTER} 
    {DELETE | INSERT | UPDATE [OF column [, column] ...]} 

[OR {DELETE | INSERT | UPDATE [OF column [, column] ...]}] ... 
    ON [schema.]table 
    [ [REFERENCING { OLD [AS] old [NEW [AS] new] 
                   | NEW [AS] new [OLD [AS] old] } ] 
     FOR EACH ROW 
     [WHEN (condition)] ] 
    pl/sql_block 

where: 

OR REPLACE 
    recreates the trigger if it already exists.  You can use this option 
    to change the definition of an existing trigger without first 

    dropping it. 

schema 
    is the schema to contain the trigger.  If you omit schema, Oracle 
    creates the trigger in your own schema. 

trigger 
    is the name of the trigger to be created. 

BEFORE 
    indicates that Oracle fires the trigger before executing the 
    triggering statement. 

AFTER 
    indicates that Oracle fires the trigger after executing the 
    triggering statement. 


DELETE 
    indicates that Oracle fires the trigger whenever a DELETE statement 
    removes a row from the table. 

INSERT 
    indicates that Oracle fires the trigger whenever an INSERT 
    statement adds a row to table. 

UPDATE...OF 
    indicates that Oracle fires the trigger whenever an UPDATE statement 
    changes a value in one of the columns specified in the OF clause. 
    If you omit the OF clause, Oracle fires the trigger whenever an 

    UPDATE statement changes a value in any column of the table. 

ON 
    specifies the schema and name of the table on which the trigger is 
    to be created.  If you omit schema, Oracle assumes the table is in 
    our own schema.  You cannot create a trigger on a table in the 
    schema SYS. 

REFERENCING 
    specifies correlation names.  You can use correlation names in the 
    PL/SQL block and WHEN clause of a row trigger to refer specifically 

    to old and new values of the current row.  The default correlation 
    names are OLD and NEW.  If your row trigger is associated with a 
    table named OLD or NEW, you can use this clause to specify different 
    correlation names to avoid confusion between the table name and the 
    correlation name. 

FOR EACH ROW 
    designates the trigger to be a row trigger.  Oracle fires a row 
    trigger once for each row that is affected by the triggering 

    statement and meets the optional trigger constraint defined in the 
    WHEN clause. 

    If you omit this clause, the trigger is a statement trigger.  Oracle 
    fires a statement trigger only once when the triggering statement is 
    issued if the optional trigger constraint is met. 

WHEN 
    specifies the trigger restriction.  The trigger restriction contains 
    a SQL condition that must be satisfied for Oracle to fire the 

    trigger.  This condition must contain correlation names and cannot 
    contain a query. 

    You can only specify a trigger restriction for a row trigger. 
    Oracle evaluates this condition for each row affected by the 
    triggering statement. 

pl/sql_block 
    is the PL/SQL block that Oracle executes to fire the trigger. 

    Note that the PL/SQL block of a trigger cannot contain transaction 

    control SQL statements (COMMIT, ROLLBACK, and SAVEPOINT). 

To embed a CREATE TRIGGER statement inside an Oracle Precompiler 
program, you must terminate the statement with the keyword END-EXEC 
followed by the embedded SQL statement terminator for the specific 
language. 

PREREQUISITES: 

    Before a trigger can be created, the user SYS must run the SQL 
    script DBMSSTDX.SQL.  The exact name and location of this script may 

    vary depending on your operating system. 

    To issue this statement, you must have one of these system 
    privileges: 
            CREATE  TRIGGER 
                   This system privilege allows you to create a trigger 
                   in your own schema on a table in your own schema. 
            CREATE ANY TRIGGER 
                   This system privilege allows you to create a trigger 
                   in any user's schema on a table in any user's schema. 


    If the trigger issues SQL statements or calls procedures or 
    functions, then the owner of the schema to contain the trigger must 
    have the privileges necessary to perform these operations.  These 
    privileges must be granted directly to the owner, rather than 
    acquired through roles. 

    To create a trigger, you must be using Oracle with the procedural 
    option. 

SEE: 
    ALTER TRIGGER, DISABLE clause, DROP TRIGGER, ENABLE clause "))
("create user" . (nil "SQL command" "CREATE USER command 

PURPOSE: 

    To create a database user, or an account through which you can log 
    in to the database, and establish the means by which Oracle permits 
    access by the user.  You can optionally assign these properties to 
    the user: 

    * default tablespace 
    * temporary tablespace 
    * quotas for allocating space in tablespaces 
    * profile containing resource limits 


SYNTAX: 

CREATE USER user 
    IDENTIFIED {BY password | EXTERNALLY} 
    [DEFAULT TABLESPACE tablespace] 
    [TEMPORARY TABLESPACE tablespace] 
    [QUOTA {integer [K|M] | UNLIMITED} ON tablespace] ... 
    [PROFILE profile] 

where: 

user 
    is the name of the 
    user to be created.  This name can only contain characters from your 
    database character set.  Oracle Corporation recommends that the user 

    contain at least one single-byte character regardless of whether the 
    database character set also contains multi-byte characters. 

IDENTIFIED 
    indicates how Oracle permits user access: 
            BY password 
                   The user must specify this password to logon.  The 
                   password can only contain single-byte characters from 
                   your database character set regardless of whether 

                   this character set also contains multi-byte 
                   characters. 
            EXTERNALLY 
                   Oracle verifies user access through the operating 
                   system. 

DEFAULT TABLESPACE 
    identifies the default tablespace for objects that the user creates. 
    If you omit this clause, objects default to the SYSTEM tablespace. 

TEMPORARY TABLESPACE 

    identifies the tablespace for the user's temporary segments.  If you 
    omit this clause, temporary segments default to the SYSTEM 
    tablespace. 

QUOTA 
    allows the user to allocate space in the tablespace and optionally 
    establishes a quota of integer bytes.  This quota is the maximum 
    space in the tablespace the user can allocate.  You can also use the 
    K or M to specify the quota in kilobytes or megabytes. 

            UNLIMITED 
                   allows the user to allocate space in the tablespace 
                   without bound. 

PROFILE 
    assigns the profile named profile to the user.  The profile limits 
    the amount of database resources the user can use.  If you omit this 
    clause, Oracle assigns the DEFAULT profile to the user. 

PREREQUISITES: 

    You must have CREATE USER system privilege. 


    If you are using Trusted Oracle in DBMS MAC mode, you must meet 
    additional prerequisites to perform the optional assignments of this 
    statement: 

    * To assign a default or temporary tablespace, your DBMS label must 
      dominate the tablespace's creation label. 
    * To assign a profile, your DBMS label must dominate the profile's 
      creation label. 

SEE: 
    ALTER USER, CREATE PROFILE, CREATE TABLESPACE "))
("create view" . (nil "SQL command" "CREATE VIEW command 

PURPOSE: 

    To define a view, a logical table based on one or more tables or 
    views. 

SYNTAX: 

CREATE [OR REPLACE] [FORCE | NOFORCE] VIEW [schema.]view 
    [(alias [,alias]...)] 
    AS subquery 
    [WITH CHECK OPTION [CONSTRAINT constraint]] 

where: 

OR REPLACE 
    recreates the view if it already exists.  You can use this option to 

    change the definition of an existing view without dropping, 
    recreating, and regranting object privileges previously granted on 
    it. 

FORCE 
    creates the view regardless of whether the view's base tables exist 
    or the owner of the schema containing the view has privileges on 
    them.  Note that both of these conditions must be true before any 
    SELECT, INSERT, UPDATE, or DELETE statements can be issued against 

    the view. 

NOFORCE 
    creates the view only if the base tables exist and the owner of the 
    schema containing the view has privileges on them. 

    The default is NOFORCE. 

schema 
    is the schema to contain the view.  If you omit schema, Oracle 
    creates the view in your own schema. 

view 
    is the name of the view. 

alias 
    specifies names for the expressions selected by the view's 

    query.  The number of aliases must match the number of expressions 
    selected by the view.  Aliases must follow the rules for naming 
    schema objects.  Aliases must be unique within the view. 

    If you omit the aliases, Oracle derives them from the columns or 
    column aliases in the view's query.  For this reason, you must use 
    aliases if the view's query contains expressions rather than only 
    column names. 


AS subquery 
    identifies columns and rows of the table(s) that the view is based 
    on.  A view's query can be any SELECT statement without the ORDER BY 
    or FOR UPDATE clauses.  Its select list can contain up to 254 
    expressions. 

WITH CHECK OPTION 
    specifies that inserts and updates performed through the view must 
    result in rows that the view query can select.  The CHECK OPTION 

    cannot make this guarantee if there is a subquery in the query of 
    this view or any view on which this view is based. 

CONSTRAINT 
    is the name assigned to the CHECK OPTION constraint. If you omit 
    this identifier, Oracle automatically assigns the constraint a name 
    of this form: 
            SYS_Cn 
    where 
            n 
                   is an integer that makes the constraint name 

                   unique within the database. 

PREREQUISITES: 

    To create a view in your own schema, you must have CREATE VIEW 
    system privilege.  To create a view in another user's schema, you 
    must have CREATE ANY VIEW system privilege. 

    The owner of the schema containing the view must have the privileges 
    necessary to either select, insert, update, or delete rows from all 
    the tables or views on which the view is based.  For information on 

    these privileges, see the SELECT, INSERT, UPDATE, and DELETE 
    commands.  The owner must be granted these privileges directly, 
    rather than through a role. 

SEE: 
    CREATE TABLE, CREATE SYNONYM, DROP VIEW, RENAME "))
("drop cluster" . (nil "SQL command" "DROP CLUSTER command 

PURPOSE: 

    To remove a cluster from the database. 

SYNTAX: 

DROP CLUSTER [schema.]cluster 
    [INCLUDING TABLES [CASCADE CONSTRAINTS] ] 

where: 

schema 
    is the schema containing the cluster.  If you omit schema, Oracle 
    assumes the cluster is in your own schema. 

cluster 
    is the name of the cluster to be dropped. 

INCLUDING TABLES 

    drops all tables that belong to the cluster.  If you omit this 
    clause, and the cluster still contains tables, Oracle returns an 
    error and does not drop the cluster. 

CASCADE CONSTRAINTS 
    drops all referential integrity constraints from tables outside the 
    cluster that refer to primary and unique keys in the tables of the 
    cluster.  If you omit this option and such referential integrity 

    constraints exist, Oracle returns an error and does not drop the 
    cluster. 

PREREQUISITES: 

    The cluster must be in your own schema or you must have DROP ANY 
    CLUSTER system privilege. 

    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 
    must match the cluster's creation label or you must satisfy one of 
    these criteria: 

    * If the cluster's creation label is higher than your DBMS label, 

      you must have READUP and WRITEUP system privileges. 
    * If the cluster's creation label is lower than your DBMS label, you 
      must have WRITEDOWN system privilege. 
    * If the cluster's creation label and your DBMS label are 
      noncomparable, you must have READUP, WRITEUP, and WRITEDOWN system 
      privileges. 

SEE: 
    DROP TABLE "))
("drop database link" . (nil "SQL command" "DROP DATABASE LINK command 

PURPOSE: 

    To remove a database link from the database. 

SYNTAX: 

DROP [PUBLIC] DATABASE LINK dblink 

where: 

PUBLIC 
    must be specified to drop a PUBLIC database link. 

dblink 
    specifies the database link to be dropped. 

PREREQUISITES: 

    To drop a private database link, the database link must be in your 
    own schema.  To drop a PUBLIC database link, you must have DROP 

    PUBLIC DATABASE LINK system privilege. 

    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 
    must match the database link's creation label or you must satisfy 
    one of these criteria: 

    * If the database link's creation label is higher than your DBMS 
      label, you must have READUP and WRITEUP system privileges. 
    * If the database link's creation label is lower than your DBMS 

      label, you must have WRITEDOWN system privilege. 
    * If the database link's creation label and your DBMS label are 
      noncomparable, you must have READUP, WRITEUP, and WRITEDOWN 
      system privileges. 

SEE: 
    CREATE DATABASE LINK "))
("drop function" . (nil "SQL command" "DROP FUNCTION command 

PURPOSE: 

    To remove a stand-alone stored function from the database. 

SYNTAX: 

DROP FUNCTION [schema.]function 

where: 

schema 
    is the schema containing the function.  If you omit schema, Oracle 
    assumes the function is in your own schema. 

function 
    is the name of the function to be dropped. 

PREREQUISITES: 

    The function must be in your own schema or you must have DROP ANY 

    PROCEDURE system privilege. 

    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 
    must match the function's creation label or you must satisfy one of 
    these criteria: 

    * If the function's creation label is higher than your DBMS label, 
      you must have READUP and WRITEUP system privileges. 
    * If the function's creation label is lower than your DBMS label, 
      you must have WRITEDOWN system privilege. 

    * If the function's creation label and your DBMS label are 
      noncomparable, you must have READUP, WRITEUP, and WRITEDOWN system 
      privileges. 

SEE: 
    CREATE FUNCTION "))
("drop index" . (nil "SQL command" "DROP INDEX command 

PURPOSE: 

    To remove an index from the database. 

SYNTAX: 

DROP INDEX [schema.]index 

where: 

schema 
    is the schema containing the index.  If you omit schema, Oracle 
    assumes the index is in your own schema. 

index 
    is the name of the index to be dropped. 

PREREQUISITES: 

    The index must be in your own schema or you must have DROP ANY INDEX 

    system privilege. 

    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 
    must match the index's creation label or you must satisfy one of 
    these criteria: 

    * If the index's creation label is higher than your DBMS label, you 
      must have READUP and WRITEUP system privileges. 
    * If the index's creation label is lower than your DBMS label, you 
      must have WRITEDOWN system privilege. 

    * If the index's creation label and your DBMS label are 
      noncomparable, you must have READUP, WRITEUP, and WRITEDOWN system 
      privileges. 

SEE: 
    ALTER INDEX, CREATE INDEX, CREATE TABLE "))
("drop package" . (nil "SQL command" "DROP PACKAGE command 

PURPOSE: 

    To remove a stored package from the database. 

SYNTAX: 

DROP PACKAGE [BODY] [schema.]package 

where: 

BODY 
    drops only the body of the package.  If you omit this option, Oracle 
    drops both the body and specification of the package. 

schema 
    is the schema containing the package.  If you omit schema, Oracle 
    assumes the package is in your own schema. 


package 
    is the name of the package to be dropped. 

PREREQUISITES: 

    The package must be in your own schema or you must have DROP ANY 
    PROCEDURE system privilege. 

    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 
    must match the cluster's creation label or you must satisfy one of 
    these criteria: 

    * If the package's creation label is higher than your DBMS label, 

      you must have READUP and WRITEUP system privileges. 
    * If the package's creation label is lower than your DBMS label, you 
      must have WRITEDOWN system privilege. 
    * If the package's creation label and your DBMS label are 
      noncomparable, you must have READUP, WRITEUP, and WRITEDOWN system 
      privileges. 

SEE: 
    CREATE PACKAGE "))
("drop procedure" . (nil "SQL command" "DROP PROCEDURE command 

PURPOSE: 

    To remove a stand-alone stored procedure from the database. 

SYNTAX: 

DROP PROCEDURE [schema.]procedure 

where: 

schema 
    is the schema containing the procedure.  If you omit schema, Oracle 
    assumes the procedure is in your own schema. 

procedure 
    is the name of the procedure to be dropped. 

PREREQUISITES: 


    The procedure must be in your own schema or you must have DROP ANY 
    PROCEDURE system privilege. 

    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 
    must match the cluster's creation label or you must satisfy one of 
    these criteria: 

    * If the procedure's creation label is higher than your DBMS label, 
      you must have READUP and WRITEUP system privileges. 
    * If the procedure's creation label is lower than your DBMS label, 

      you must have WRITEDOWN system privilege. 
    * If the procedure's creation label and your DBMS label are 
      noncomparable, you must have READUP, WRITEUP, and WRITEDOWN system 
      privileges. 

SEE: 
    CREATE PROCEDURE "))
("drop profile" . (nil "SQL command" "DROP PROFILE command 

PURPOSE: 

    To remove a profile from the database. 

SYNTAX: 

DROP PROFILE profile 
    [CASCADE] 

where: 

profile 
    is the name of the profile to be dropped. 

CASCADE 
    deassigns the profile from any users to whom it is assigned.  Oracle 
    automatically assigns the DEFAULT profile to such users.  You must 
    specify this option to drop a profile that is currently assigned to 

    users. 

PREREQUISITES: 

    You must have DROP PROFILE system privilege. 

    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 
    must match the profile's creation label or you must satisfy one of 
    these criteria: 

    * If the profile's creation label is higher than your DBMS label, 
      you must have READUP and WRITEUP system privileges. 
    * If the profile's creation label is lower than your DBMS label, you 

      must have WRITEDOWN system privilege. 
    * If the profile's creation label and your DBMS label are 
      noncomparable, you must have READUP, WRITEUP, and WRITEDOWN system 
      privileges. 

SEE: 
    CREATE PROFILE "))
("drop role" . (nil "SQL command" "DROP ROLE command 

PURPOSE: 

    To remove a role from the database. 

SYNTAX: 

DROP ROLE role 

where: 

role 
    is the role to be dropped. 

PREREQUISITES: 

    You must have been granted the role with the ADMIN OPTION or have 
    DROP ANY ROLE system privilege. 

    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 
    must match the role's creation label or you must satisfy one of 

    these criteria: 

    * If the role's creation label is higher than your DBMS label, you 
      must have READUP and WRITEUP system privileges. 
    * If the role's creation label is lower than your DBMS label, you 
      must have WRITEDOWN system privilege. 

    If the role's creation label and your DBMS label are noncomparable, 
    you must have READUP, WRITEUP, and WRITEDOWN system privileges. 

SEE: 

    CREATE ROLE, SET ROLE "))
("drop rollback segment" . (nil "SQL command" "DROP ROLLBACK SEGMENT command 

PURPOSE: 

    To remove a rollback segment from the database. 

SYNTAX: 

DROP ROLLBACK SEGMENT rollback_segment 

where: 

rollback_segment 
    is the name the rollback segment to be dropped. 

PREREQUISITES: 

    You must have DROP ROLLBACK SEGMENT system privilege. 

    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 

    must match the rollback segment's creation label or you must satisfy 
    one of these criteria: 

    * If the rollback segment's creation label is higher than your DBMS 
      label, you must have READUP and WRITEUP system privileges. 
    * If the rollback segment's creation label is lower than your DBMS 
      label, you must have WRITEDOWN system privilege. 
    * If the rollback segment's creation label and your DBMS label are 

      noncomparable, you must have READUP, WRITEUP, and WRITEDOWN system 
      privileges. 

SEE: 
    ALTER ROLLBACK SEGMENT, CREATE ROLLBACK SEGMENT, CREATE TABLESPACE "))
("drop sequence" . (nil "SQL command" "DROP SEQUENCE command 

PURPOSE: 

    To remove a sequence from the database. 

SYNTAX: 

DROP SEQUENCE [schema.]sequence 

where: 

schema 
    is the schema containing the sequence.  If you omit schema, Oracle 
    assumes the sequence is in your own schema. 

sequence 
    is the name of the sequence to be dropped. 

PREREQUISITES: 

    The sequence must be in your own schema or you must have DROP ANY 

    SEQUENCE system privilege. 

    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 
    must match the sequence's creation label or you must satisfy one of 
    these criteria: 

    * If the sequence's creation label is higher than your DBMS label, 
      you must have READUP and WRITEUP system privileges. 
    * If the sequence's creation label is lower than your DBMS label, 
      you must have WRITEDOWN system privilege. 

    * If the sequence's creation label and your DBMS label are 
      noncomparable, you must have READUP, WRITEUP, and WRITEDOWN system 
      privileges. 

SEE: 
    ALTER SEQUENCE, CREATE SEQUENCE "))
("drop snapshot" . (nil "SQL command" "DROP SNAPSHOT command 

PURPOSE: 

    To remove a snapshot from the database. 

SYNTAX: 

DROP SNAPSHOT [schema.]snapshot 

where: 

schema 
    is the schema containing the snapshot.  If you omit schema, Oracle 
    assumes the snapshot is in your own schema. 

snapshot 
    is the name of the snapshot to be dropped. 

PREREQUISITES: 

    The snapshot must be in your own schema or you must have DROP ANY 

    SNAPSHOT system privilege.  You must also have the privileges to 
    drop the internal table, views, and index that Oracle uses to 
    maintain the snapshot's data.  For information on these privileges, 
    see the DROP TABLE, DROP VIEW, and DROP INDEX commands. 

SEE: 
    CREATE SNAPSHOT "))
("drop snapshot log" . (nil "SQL command" "DROP SNAPSHOT LOG command 

PURPOSE: 

    To remove a snapshot log from the database. 

SYNTAX: 

DROP SNAPSHOT LOG ON [schema.]table 

where: 

schema 
    is the schema containing the snapshot log and its master table.  If 
    you omit schema, Oracle assumes the snapshot log and master table 
    are in your own schema. 

table 
    is the name of the master table associated with the snapshot log to 

    be dropped. 

PREREQUISITES: 

    Since a snapshot log consists of a table and a trigger, the 
    privileges that authorize operations on it are the same as for a 
    table.  To drop a snapshot log, you must have the privileges listed 
    for the DROP TABLE command.  You must also have the privileges to 
    drop a trigger from the snapshot log's master table.  For 
    information on these privileges, see the DROP TRIGGER command. 


SEE: 
    CREATE SNAPSHOT LOG "))
("drop synonym" . (nil "SQL command" "DROP SYNONYM command 

PURPOSE: 

    To remove a synonym from the database. 

SYNTAX: 

DROP [PUBLIC] SYNONYM [schema.]synonym 

where: 

PUBLIC 
    must be specified to drop a public synonym. 

schema 
    is the schema containing the synonym.  If you omit schema, Oracle 
    assumes the synonym is in your own schema. 

synonym 
    is the name of the synonym to be dropped. 


PREREQUISITES: 

    If you want to drop a private synonym, either the synonym must be in 
    your own schema or you must have DROP ANY SYNONYM system privilege. 
    If you want to drop a PUBLIC synonym, either the synonym must be in 
    your own schema or you must have DROP ANY PUBLIC SYNONYM system 
    privilege. 

    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 
    must match the synonym's creation label or you must satisfy one of 

    these criteria: 

    * If the synonym's creation label is higher than your DBMS label, 
      you must have READUP and WRITEUP system privileges. 
    * If the synonym's creation label is lower than your DBMS label, you 
      must have WRITEDOWN system privilege. 
    * If the synonym's creation label and your DBMS label are 
      noncomparable, you must have READUP, WRITEUP, and WRITEDOWN system 
      privileges. 


SEE: 
    CREATE SYNONYM "))
("drop table" . (nil "SQL command" "DROP TABLE command 

PURPOSE: 

    To remove a table and all its data from the database. 

SYNTAX: 

DROP TABLE [schema.]table 
    [CASCADE CONSTRAINTS] 

where: 

schema 
    is the schema containing the table.  If you omit schema, Oracle 
    assumes the table is in your own schema. 

table 
    is the name of the table to be dropped. 

CASCADE CONSTRAINTS 

    drops all referential integrity constraints that refer to primary 
    and unique keys in the dropped table.  If you omit this option, and 
    such referential integrity constraints exist, Oracle returns an 
    error and does not drop the table. 

PREREQUISITES: 

    The table must be in your own schema or you must have DROP ANY TABLE 
    system privilege. 

    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 

    must match the table's creation label or you must satisfy one of 
    these criteria: 

    * If the table's creation label is higher than your DBMS label, you 
      must have READUP and WRITEUP system privileges. 
    * If the table's creation label is lower than your DBMS label, you 
      must have WRITEDOWN system privilege. 
    * If the table's creation label and your DBMS label are 
      noncomparable, you must have READUP, WRITEUP, and WRITEDOWN system 

      privileges. 

SEE: 
    ALTER TABLE, CREATE INDEX, CREATE TABLE, DROP CLUSTER "))
("drop tablespace" . (nil "SQL command" "DROP TABLESPACE command 

PURPOSE: 

    To remove a tablespace from the database. 

SYNTAX: 

DROP TABLESPACE tablespace 
    [INCLUDING CONTENTS [CASCADE CONSTRAINTS]] 

where: 

tablespace 
    is the name of the tablespace to be dropped. 

INCLUDING CONTENTS 
    drops all the content of the tablespace.  You must specify this 
    clause to drop a tablespace that contains any database objects.  If 

    you omit this clause, and the tablespace is not empty, Oracle 
    returns an error and does not drop the tablespace. 

CASCADE CONSTRAINTS 
    drops all referential integrity constraints from tables outside the 
    tablespace that refer to primary and unique keys in the tables of 
    the tablespace.  If you omit this option and such referential 
    integrity constraints exist, Oracle returns an error and does not 

    drop the tablespace. 

PREREQUISITES: 

    You must have DROP TABLESPACE system privilege.  No rollback 
    segments in the tablespace can be assigned active transactions. 

    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 
    must match the tablespace's creation label or you must satisfy one 
    of these criteria: 

    * If the tablespace's creation label is higher than your DBMS label, 

      you must have READUP and WRITEUP system privileges. 
    * If the tablespace's creation label is lower than your DBMS label, 
      you must have WRITEDOWN system privilege. 
    * If the tablespace's creation label and your DBMS label are 
      noncomparable, you must have READUP, WRITEUP, and WRITEDOWN system 
      privileges. 

SEE: 
    ALTER TABLESPACE, CREATE DATABASE, CREATE TABLESPACE "))
("drop trigger" . (nil "SQL command" "DROP TRIGGER command 

PURPOSE: 

    To remove a database trigger from the database. 

SYNTAX: 

DROP TRIGGER [schema.]trigger 

where: 

schema 
    is the schema containing the trigger.  If you omit schema, Oracle 
    assumes the trigger is in your own schema. 

trigger 
    is the name of the trigger to be dropped. 

PREREQUISITES: 

    The trigger must be in your own schema or you must have DROP ANY 

    TRIGGER system privilege. 

    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 
    must match the trigger's creation label or you must satisfy one of 
    these criteria: 

    * If the trigger's creation label is higher than your DBMS label, 
      you must have READUP and WRITEUP system privileges. 
    * If the trigger's creation label is lower than your DBMS label, you 
      must have WRITEDOWN system privilege. 

    * If the trigger's creation label and your DBMS label are 
      noncomparable, you must have READUP, WRITEUP, and WRITEDOWN system 
      privileges. 

SEE: 
    CREATE TRIGGER "))
("drop user" . (nil "SQL command" "DROP USER command 

PURPOSE: 

    To remove a database user and optionally remove the user's objects. 

SYNTAX: 

DROP USER user [CASCADE] 

where: 

user 
    is the user to be dropped. 

CASCADE 
    drops all objects in the user's schema before dropping the user. 
    You must specify this option to drop a user whose schema contains 
    any objects. 

PREREQUISITES: 


    You must have DROP USER system privilege. 

    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 
    must match the user's creation label or you must satisfy one of 
    these criteria: 

    * If the user's creation label is higher than your DBMS label, you 
      must have READUP and WRITEUP system privileges. 
    * If the user's creation label is lower than your DBMS label, you 
      must have WRITEDOWN system privilege. 

    * If the user's creation label and your DBMS label are 
      noncomparable, you must have READUP, WRITEUP, and WRITEDOWN system 
      privileges. 

SEE: 
    CREATE USER, other DROP commands "))
("drop view" . (nil "SQL command" "DROP VIEW command 

PURPOSE: 

    To remove a view from the database. 

SYNTAX: 

DROP VIEW [schema.]view 

where: 

schema 
    is the schema containing the view.  If you omit schema, Oracle 
    assumes the view is in your own schema. 

view 
    is the name of the view to be dropped. 

PREREQUISITES: 

    The view must be in your own schema or you must have DROP ANY VIEW 

    system privilege. 

    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 
    must match the view's creation label or you must satisfy one of 
    these criteria: 

    * If the view's creation label is higher than your DBMS label, you 
      must have READUP and WRITEUP system privileges. 
    * If the view's creation label is lower than your DBMS label, you 
      must have WRITEDOWN system privilege. 

    * If the view's creation label and your DBMS label are 
      noncomparable, you must have READUP, WRITEUP, and WRITEDOWN system 
      privileges. 

SEE: 
    CREATE SYNONYM, CREATE TABLE, CREATE VIEW "))
("grant" . (nil "SQL command" "GRANT command (System Privileges and Roles) 

PURPOSE: 

    To grant system privileges and roles to users and roles.  To grant 
    object privileges, use the GRANT command (Object Privileges). 

SYNTAX: 

GRANT {system_priv | role} [, {system_priv | role}] ... 
    TO {user | role | PUBLIC} [, {user | role | PUBLIC}] ... 
    [WITH ADMIN OPTION] 

where: 

system_priv 
    is a system privilege to be granted. 


role 
    is a role to be granted 

TO 
    identifies users or roles to which system privileges and roles are 
    granted. 
            PUBLIC 
                   grants system privileges or roles to all users. 

WITH ADMIN OPTION 
    allows the grantee to grant the system privilege or role to other 
    users or roles.  If you grant a role with ADMIN OPTION, the grantee 
    can also alter or drop the role. 


PREREQUISITES: 

    To grant a system privilege, you must either have been granted the 
    system privilege with the ADMIN OPTION or have been granted GRANT 
    ANY PRIVILEGE system privilege. 

    To grant a role, you must either have been granted the role with the 
    ADMIN OPTION or have been granted GRANT ANY ROLE system privilege or 
    have created the role. 

    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 

    must dominate both the label at which the system privilege or role 
    was granted to you and the creation label of the grantee user or 
    role. 

SEE: 
    ALTER USER, CREATE USER, GRANT (Object Privileges), REVOKE 
    Privileges and Roles) "))
("noaudit" . (nil "SQL command" "NOAUDIT command (SQL Statements) 

PURPOSE: 

    To stop auditing chosen by the AUDIT command (SQL Statements).  To 
    stop auditing chosen by the AUDIT command (Schema Objects), use the 
    NOAUDIT command (Schema Objects). 

SYNTAX: 

NOAUDIT {statement_opt | system_priv} 
     [, {statement_opt | system_priv} ] ... 
    [BY user [, user] ...] 
    [WHENEVER [NOT] SUCCESSFUL] 


where: 

statement_opt 
    is a statement option for which auditing is stopped. 

system_priv 
    is a system privilege for which auditing is stopped. 

BY 
    stops auditing only for SQL statements issued by specified users in 
    their subsequent sessions.  If you omit this clause, Oracle stops 
    auditing for all users' statements. 

WHENEVER SUCCESSFUL 
    stops auditing only for SQL statements that complete successfully. 


NOT 
    stops auditing only for statements that result in Oracle errors. 

    If you omit the WHENEVER clause entirely, Oracle stops auditing for 
    all statements, regardless of success or failure. 

PREREQUISITES: 

    You must have AUDIT SYSTEM system privilege. 

    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 
    must match the label at which the auditing option was set or you 

    must satisfy one of these criteria: 

    * If the auditing option was set at a label higher than your DBMS 
      label, you must have READUP and WRITEUP system privileges. 
    * If the auditing option was set at a label lower than your DBMS 
      label, you must have WRITEDOWN system privilege. 
    * If the auditing option was set at a label noncomparable to your 
      DBMS label, you must have READUP, WRITEUP, and WRITEDOWN system 

      privileges. 

SEE: 
    AUDIT (SQL Statements), NOAUDIT (Schema Objects) "))
("rename" . (nil "SQL command" "RENAME command 

PURPOSE: 

    To rename a table, view, sequence, or private synonym. 

SYNTAX: 

RENAME old TO new 

where: 

old 
    is the current name of an existing table, view, sequence, or private 
    synonym. 

new 
    is the new name to be given to the existing object. 

PREREQUISITES: 

    The object must be in your own schema. 

    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 

    must match the object's creation label or you must satisfy one of 
    these criteria: 

    * If the object's creation label is higher than your DBMS label, you 
      must have READUP and WRITEUP system privileges. 
    * If the object's creation label is lower than your DBMS label, you 
      must have WRITEDOWN system privilege. 
    * If the object's creation label and your DBMS label are 
      noncomparable, you must have READUP, WRITEUP, and WRITEDOWN system 

      privileges. 

SEE: 
    CREATE SEQUENCE, CREATE SYNONYM, CREATE TABLE, CREATE VIEW "))
("revoke" . (nil "SQL command" "REVOKE command (System Privileges and Roles) 

PURPOSE: 

    To revoke system privileges and roles from users and roles.  To 
    revoke object privileges from users and roles, use the REVOKE 
    command (Object Privileges). 

SYNTAX: 

REVOKE {system_priv | role} [, {system_priv | role}] ... 
    FROM {user | role | PUBLIC} [, {user | role | PUBLIC}] ... 

where: 

system_priv 
    is a system privilege to be revoked. 


role 
    is a role to be revoked. 

FROM 
    identifies users and roles from which the system privileges or roles 
    are revoked. 
            PUBLIC 
                   revokes the system privilege or role from all users. 

PREREQUISITES: 

    You must have been granted the system privilege or role with the 
    ADMIN OPTION.  Also, you can revoke any role if you have the GRANT 

    ANY ROLE system privilege. 

    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 
    must match the label at which the system privilege or role was 
    granted or you must satisfy one of these criteria: 

    * If the label at which the system privilege or role was granted is 
      higher than your DBMS label, you must have READUP and WRITEUP 
      system privileges. 
    * If the label at which the system privilege or role was granted is 

      lower than your DBMS label, you must have WRITEDOWN system 
      privilege. 
    * If the label at which the system privilege or role is 
      noncomparable to your DBMS label, you must have READUP, WRITEUP, 
      and WRITEDOWN system privileges. 

SEE: 
    GRANT (System Privileges and Roles), REVOKE (Object Privileges) "))
("truncate" . (nil "SQL command" "TRUNCATE command 

PURPOSE: 

    To remove all rows from a table or cluster. 

SYNTAX: 

TRUNCATE {TABLE [schema.]table | CLUSTER [schema.]cluster} 
    [ {DROP | REUSE} STORAGE] 

where: 

TABLE 
    specifies the schema and name of the table to be truncated.  If you 
    omit schema, Oracle assumes the table is in your own schema.  This 
    table cannot be part of a cluster. 


    When you truncate a table, Oracle also automatically deletes all 
    data in the table's indexes. 

CLUSTER 
    specifies the schema and name of the cluster to be truncated.  If 
    you omit schema, Oracle assumes the cluster is in your own schema. 
    You can only truncate an indexed cluster, not a hash cluster. 

    When you truncate a cluster, Oracle also automatically deletes all 
    data in the cluster's tables' indexes. 


DROP STORAGE 
    deallocates the space from the deleted rows from the table or 
    cluster.  This space can subsequently be used by other objects in 
    the tablespace. 

REUSE STORAGE 
    leaves the space from the deleted rows allocated to the table or 
    cluster.  This space can be subsequently used only by new data in 
    the table or cluster resulting from inserts or updates. 

    The DROP STORAGE or REUSE STORAGE option that you choose also 

    applies to the space freed by the data deleted from associated 
    indexes. 

    If you omit both the REUSE STORAGE and DROP STORAGE options, Oracle 
    uses the DROP STORAGE option by default. 

PREREQUISITES: 

    The table or cluster must be in your schema or you must have DELETE 
    ANY TABLE system privilege. 

    If you are using Trusted Oracle, your DBMS label must match the 
    creation label of the table or cluster or you must satisfy one of 

    these criteria: 

    * If the creation label of the table or cluster is higher than your 
      DBMS label, you must have READUP and WRITEUP system privileges. 
    * If the creation label of the table or cluster is lower than your 
      DBMS label, you must have WRITEDOWN system privilege. 
    * If the creation label of the table or cluster is noncomparable to 
      your DBMS label, you must have READUP, WRITEUP, and WRITEDOWN 

      system privileges. 

SEE: 
    DELETE, DROP CLUSTER, DROP TABLE "))
("update" . (nil "SQL command" "UPDATE command 

PURPOSE: 

    To change existing values in a table or in a view's base table. 

SYNTAX: 

UPDATE [schema.]{table | view}[@dblink] [alias] 
    SET { (column [, column] ...) = (subquery) 
        |  column = { expr | (subquery) } } 
     [,    { (column [, column] ...) = (subquery) 
        |  column = { expr | (subquery) } } ] ... 
    [WHERE condition] 

where: 


schema 
    is the schema containing the table or view.  If you omit schema, 
    Oracle assumes the table or view is in your own schema. 

table 
view 
    is the name of the table to be updated.  If you specify view, Oracle 
    updates the view's base table. 

dblink 
    is a complete or partial name of a database link to a remote 
    database where the table or view is located.  You can only use a 

    database link to update a remote table or view if you are using 
    Oracle with the distributed option. 

    If you omit dblink, Oracle assumes the table or view is on the local 
    database. 

alias 
    is used to relabel the name of the reference in the other clauses of 
    the command. 

column 
    is the name of a column of the table or view that is to be updated. 
    If you omit a column of the table from the SET clause, that column's 

    value remains unchanged. 

expr 
    is the new value assigned to the corresponding column.  This 
    expression can contain host variables and optional indicator 
    variables. 

subquery 
    is a SELECT statement that returns new values that are assigned to 
    the corresponding columns. 

WHERE 
    restricts the rows updated to those for which the specified 
    condition is TRUE.  If you omit this clause, Oracle updates all rows 

    in the table or view. 

PREREQUISITES: 

    For you to update values in a table, the table must be in your own 
    schema or you must have UPDATE privilege on the table. 

    For you to update values in the base table of a view, the owner of 
    the schema containing the view must have UPDATE privilege on the 
    base table.  Also, if the view is in a schema other than your own, 
    you must have UPDATE privilege on the view. 


    The UPDATE ANY TABLE system privilege also allows you to update 
    values in any table or any view's base table. 

    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 
    must match the creation label of the table or view: 

    * If the creation label of the table or view is higher than your 
      DBMS label, you must have READUP and WRITEUP system privileges 
    * If the creation label of the table or view is lower than your DBMS 

      label, you must have WRITEDOWN system privilege. 
    * If the creation label of your table or view is noncomparable to 
      your DBMS label, you must have READUP, WRITEUP, and WRITEDOWN 
      system privileges. 

SEE: 
    DELETE, INSERT "))
("delete" . (nil "SQL command" "DELETE command 

PURPOSE: 

    To remove rows from a table or from a view's base table. 

SYNTAX: 

DELETE [FROM] [schema.]{table | view}[@dblink] [alias] 
    [WHERE condition] 

where: 

schema 
    is the schema containing the table or view.  If you omit schema, 
    Oracle assumes the table or view is in your own schema. 

table 
view 
    is the name of a table from which the rows are to be deleted.  If 

    you specify view, Oracle deletes rows from the view's base table. 

dblink 
    is the complete or partial name of a database link to a remote 
    database where the table or view is located.  You can only delete 
    rows from a remote table or view if you are using Oracle with the 
    distributed option. 

    If you omit dblink, Oracle assumes that the table or view is located 
    on the local database. 


alias 
    is an alias assigned to the table.  Aliases are generally used in 
    DELETE statements with correlated queries. 

WHERE 
    deletes only rows that satisfy the condition.  The condition can 
    reference the table and can contain a subquery.  If you omit this 
    clause, Oracle deletes all rows from the table. 

PREREQUISITES: 

    For you to delete rows from a table, the table must be in your own 

    schema or you must have DELETE privilege on the table. 

    For you to delete rows from the base table of a view, the owner of 
    the schema containing the view must have DELETE privilege on the 
    base table.  Also, if the view is in a schema other than your own, 
    you must be granted DELETE privilege on the view. 

    The DELETE ANY TABLE system privilege also allows you to delete rows 
    from any table or any view's base table. 


    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 
    must dominate the creation label of the table or view or you must 
    meet one of these criteria: 

    * If the creation label of the table or view is higher than your 
      DBMS label, you must have READUP and WRITEUP system privileges. 
    * If the creation label of your table or view is noncomparable to 
      your DBMS label, you must have READUP, WRITEUP, and WRITEDOWN 

      system privileges. 

    In addition, for each row to be deleted, your DBMS label must match 
    the row's label or you must meet one of these criteria: 

    * If the row's label is higher than your DBMS label, you must have 
      READUP and WRITEUP system privileges. 
    * If the row's label is lower than your DBMS label, you must have 
      WRITEDOWN system privilege. 
    * If the row's label is noncomparable to your DBMS label, you must 

      have READUP, WRITEUP, and WRITEDOWN system privileges. 

SEE: 
    DROP TABLE, TRUNCATE "))
("explain plan" . (nil "SQL command" "EXPLAIN PLAN command 

PURPOSE: 

    To determine the execution plan Oracle follows to execute a 
    specified SQL statement.  This command inserts a row describing each 
    step of the execution plan into a specified table.  If you are using 
    cost-based optimization, this command also determines the cost of 
    executing the statement. 

SYNTAX: 

EXPLAIN PLAN 
    [SET STATEMENT ID = 'text'] 

    [INTO [schema.]table[@dblink]] 
    FOR statement 

where: 

SET 
    specifies the value of the STATEMENT_ID column for the rows of the 
    execution plan in the output table.  If you omit this clause, the 
    STATEMENT_ID value defaults to null. 

INTO 
    specifies the schema, name, and database containing the output 
    table.  This table must exist before you use the EXPLAIN PLAN 

    command.  If you omit schema, Oracle assumes the table is in your 
    own schema. 

    The dblink can be a complete or partial name of a database link to a 
    remote Oracle7 database where the output table is located.  You can 
    only specify a remote output table if you are using Oracle with the 
    distributed option.  If you omit dblink, Oracle assumes the table is 
    on your local database. 

    If you omit the INTO clause altogether, Oracle assumes an output 

    table named PLAN_TABLE in your own schema on your local database. 

FOR 
    specifies a SELECT, INSERT, UPDATE, or DELETE statement for which 
    the execution plan is generated. 

PREREQUISITES: 

    To issue an EXPLAIN PLAN statement, you must have the privileges 
    necessary to insert rows into an existing output table that you 
    specify to hold the execution plan.  For information on these 

    privileges, see the INSERT command. 

    You must also have the privileges necessary to execute the SQL 
    statement for which you are find the execution plan.  If the SQL 
    statement accesses a view, you must have privileges to access any 
    tables and views on which the view is based.  If the view is based 
    on another view that is based on a table, you must have privileges 
    to access both the other view and its underlying table. 


    To examine the execution plan produced by an EXPLAIN PLAN statement, 
    you must have the privileges necessary to query the output table. 
    For more information on these privileges, see the SELECT command. 

    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 
    must dominate the output table's creation label or you must satisfy 
    one of these criteria: 

    * If the output table's creation label is higher than your DBMS 

      label, you must have READUP and WRITEUP system privileges. 
    * If the output table's creation label and your DBMS label are 
      noncomparable, you must have READUP, WRITEUP, and WRITEDOWN system 
      privileges. 

SEE: 
    Appendix B, Performance Diagnostic Tools, in the Oracle7 Server 
    Application Developer's Guide. "))
("insert" . (nil "SQL command" "INSERT command 

PURPOSE: 

    To add rows to a table or to a view's base table. 

SYNTAX: 

INSERT INTO [schema.]{table | view}[@dblink] 
    [ (column [, column] ...) ] 
    {VALUES (expr [, expr] ...) | subquery} 

where: 

schema 
    is the schema containing the table or view.  If you omit schema, 
    Oracle assumes the table or view is in your own schema. 

table 

view 
    is name of the table into which rows are to be inserted.  If you 
    specify view, Oracle inserts rows into the view's base table. 

dblink 
    is a complete or partial name of a database link to a remote 
    database where the table or view is located.  You can only insert 
    rows into a remote table or view if you are using Oracle with the 
    distributed option. 

    If you omit dblink, Oracle assumes that the table or view is on the 

    local database. 

column 
    is a column of the table or view.  In the inserted row, each column 
    in this list is assigned a value from the VALUES clause or the 
    subquery. 

    If you omit one of the table's columns from this list, the column's 
    value for the inserted row is the column's default value as 
    specified when the table was created.  If you omit the column list 
    altogether, the VALUES clause or query must specify values for all 

    columns in the table. 

VALUES 
    specifies a row of values to be inserted into the table or view. 
    You must specify a value in the VALUES clause for each column in the 
    column list. 

subquery 
    is a SELECT statement that returns rows that are inserted into the 
    table.  The select list of this subquery must have the same number 
    of columns as the column list of the INSERT statement. 


PREREQUISITES: 

    For you to insert rows into a table, the table must be in your own 
    schema or you must have INSERT privilege on the table. 

    For you to insert rows into the base table of a view, the owner of 
    the schema containing the view must have INSERT privilege on the 
    base table.  Also, if the view is in a schema other than your own, 
    you must have INSERT privilege on the view. 


    The INSERT ANY TABLE system privilege also allows you to insert rows 
    into any table or any view's base table. 

    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 
    must dominate the creation label of the table or view: 

    * If the creation label of the table or view is higher than your 
      DBMS label, you must have READUP and WRITEUP system privileges. 
    * If the creation label of your table or view is noncomparable to 

      your DBMS label, you must have READUP, WRITEUP, and WRITEDOWN 
      system privileges. 

SEE: 
    DELETE, UPDATE "))
("lock table" . (nil "SQL command" "LOCK TABLE command 

PURPOSE: 

    To lock one or more tables in a specified mode.  This lock manually 
    overrides automatic locking and permits or denies access to a table 
    or view by other users for the duration of your operation. 

SYNTAX: 

LOCK TABLE [schema.]{table | view}[@dblink] 
         [, [schema.]{table | view}[@dblink] ]... 
    IN lockmode MODE 
    [NOWAIT] 


where: 

schema 
    is the schema containing the table or view.  If you omit schema, 
    Oracle assumes the table or view is in your own schema. 

table view 
    is the name of the table to be locked.  If you specify view, Oracle 
    locks the view's base tables. 

dblink 
    is a database link to a remote Oracle7 database where the table or 
    view is located.  You can only lock tables and views on a remote 

    database if you are using Oracle with the distributed option.  All 
    tables locked by a LOCK TABLE statement must be on the same 
    database. 

    If you omit dblink, Oracle assumes the table or view is on the local 
    database. 

lockmode 
    is one of these: 

          * ROW SHARE 
          * ROW EXCLUSIVE 
          * SHARE UPDATE 
          * SHARE 
          * SHARE ROW EXCLUSIVE 

          * EXCLUSIVE 

NOWAIT 
    specifies that Oracle returns control to you immediately if the 
    specified table is already locked by another user.  In this case, 
    Oracle returns a message indicating that the table is already locked 
    by another user. 

    If you omit this clause, Oracle waits until the table is available, 
    locks it, and returns control to you. 

PREREQUISITES: 


    The table or view must be in your own schema or you must have LOCK 
    ANY TABLE system privilege or you must have any object privilege on 
    the table or view. 

    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 
    must dominate the creation label of the table or view or you must 
    have READUP system privilege. 

SEE: 
    COMMIT, DELETE, INSERT, ROLLBACK, SAVEPOINT, UPDATE "))
("select" . (nil "SQL command" "SELECT command 

PURPOSE: 

    To retrieve data from one or more tables, views, or snapshots. 

SYNTAX: 

SELECT [DISTINCT | ALL] { * 
                        | { [schema.]{table | view | snapshot}.* 
                          | expr }  [ [AS] c_alias ] 
                       [, { [schema.]{table | view | snapshot}.* 
                          | expr } [ [AS] c_alias ]  ] ... } 
    FROM [schema.]{table | view | snapshot}[@dblink] [t_alias] 

      [, [schema.]{table | view | snapshot}[@dblink] [t_alias] ] ... 
    [WHERE condition ] 
    [ [START WITH condition] CONNECT BY condition] 
    [GROUP BY expr [, expr] ... [HAVING condition] ] 
    [{UNION | UNION ALL | INTERSECT | MINUS} SELECT command ] 
    [ORDER BY {expr|position} [ASC | DESC] 
           [, {expr|position} [ASC | DESC]] ...] 
    [FOR UPDATE [OF [[schema.]{table | view}.]column 

                 [, [[schema.]{table | view}.]column] ...] [NOWAIT] ] 

where: 

DISTINCT 
    returns only one copy of each set of duplicate rows selected. 
    Duplicate rows are those with matching values for each expression in 
    the select list. 

ALL 
    returns all rows selected, including all copies of duplicates. 

    The default is ALL. 

* 
    selects all columns from all tables, views, or snapshots listed in 

    the FROM clause. 

table.* 
view.* 
snapshot.* 
    selects all columns from the specified table, view, or snapshot. 
    You can use the schema qualifier to select from a table, view, or 
    snapshot in a schema other than your own. 

    If you are using Trusted Oracle, the * does not select the ROWLABEL 
    column.  To select this column, you must explicitly specify it in 
    the select list. 


expr 
    selects an expression, usually based on columns values, from one of 
    the tables, views, or snapshots in the FROM clause.  A column name 
    in this list can only contain be qualified with schema if the table, 
    view, or snapshot containing the column is qualified with schema in 
    the FROM clause. 

c_alias 
    provides a different name for the column expression and causes the 

    alias to be used in the column heading.  A column alias does not 
    affect the actual name of the column.  Column aliases can be 
    referenced in the ORDER BY clause but in no other clauses in a 
    statement. 

schema 
    is the schema containing the selected table, view, or snapshot.  If 
    you omit schema, Oracle assumes the table, view, or snapshot is in 
    your own schema. 

table 

view 
snapshot 
    is the name of a table, view, or snapshot from which data is 
    selected. 

dblink 
    is complete or partial name for a database link to a remote database 
    where the table, view, or snapshot is located.  Note that this 
    database need not be an Oracle7 database. 

    If you omit dblink, Oracle assumes that the table, view, or snapshot 
    is on the local database. 


t_alias 
    provides a different name for the table, view, or snapshot for the 
    purpose of evaluating the query and is most often used in a 
    correlated query.  Other references to the table, view, or snapshot 
    throughout the query must refer to the alias. 

WHERE 
    restricts the rows selected to those for which the condition is 
    TRUE.  If you omit this clause, Oracle returns all rows from the 

    tables, views, or snapshots in the FROM clause. 

START WITH 
CONNECT BY 
    returns rows in a hierarchical order. 

GROUP BY 
    groups the selected rows based on the value of expr for each row and 
    returns a single row of summary information for each group. 

HAVING 
    restricts the groups of rows returned to those groups for which the 
    specified condition is TRUE.  If you omit this clause, Oracle 

    returns summary rows for all groups. 

UNION 
UNION ALL 
INTERSECT 
MINUS 
    combines the rows returned by two SELECT statement using a set 
    operation. 

AS 
    can optionally precede a column alias.  To comply with the ANSI SQL92 
    standard, column aliases must be preceded by the AS keyword. 

ORDER BY 
    orders rows returned by the statement. 
            expr 

                   orders rows based on their value for expr.  The 
                   expression is based on columns in the select list or 
                   columns in the tables, views, or snapshots in the 
                   FROM clause. 
            position 
                   orders rows based on their value for the expression 
                   in this position of the select list. 
            ASC 

            DESC 
                   specifies either ascending or descending order.  ASC 
                   is the default. 
    The ORDER BY clause can reference column aliases defined in the 
    SELECT list. 

FOR UPDATE 
    locks the selected rows. 

NOWAIT 
    returns control to you if the SELECT statement attempts to lock a 
    row that is locked by another user.  If you omit this clause, Oracle 

    waits until the row is available and then returns the results of the 
    SELECT statement. 

PREREQUISITES: 

    For you to select data from a table or snapshot, the table or 
    snapshot must be in your own schema or you must have SELECT 
    privilege on the table or snapshot. 

    For you to select rows from the base tables of a view, the owner of 
    the schema containing the view must have SELECT privilege on the 

    base tables.  Also, if the view is in a schema other than your own, 
    you must have SELECT privilege on the view. 

    The SELECT ANY TABLE system privilege also allows you to select data 
    from any table or any snapshot or any view's base table. 

    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 
    must dominate the creation label of each queried table, view, or 
    snapshot or you must have READUP system privileges. 


SEE: 
    DELETE, UPDATE "))
("commit" . (nil "SQL command" "COMMIT command 

PURPOSE: 

    To end your current transaction and make permanent all changes 
    performed in the transaction.  This command also erases all 
    savepoints in the transaction and releases the transaction's locks. 
    You can also use this command to manually commit an in-doubt 
    distributed transaction. 

SYNTAX: 

COMMIT [WORK] 
    [ COMMENT 'text' 
    | FORCE 'text' [, integer] ] 


where: 

WORK 
    is supported only for compliance with standard SQL.  The statements 
    COMMIT and COMMIT WORK are equivalent. 

COMMENT 
    specifies a comment to be associated with the current transaction. 
    The 'text' is a quoted literal of up to 50 characters that Oracle 
    stores in the data dictionary view DBA_2PC_PENDING along with the 
    transaction ID if the transaction becomes in-doubt. 


FORCE 
    manually commits an in-doubt distributed transaction.  The 
    transaction is identified by the 'text' containing its local or 
    global transaction ID.  To find the IDs of such transactions, query 
    the data dictionary view DBA_2PC_PENDING.  You can also use the 
    integer to specifically assign the transaction a system change 
    number (SCN).  If you omit the integer, the transaction is committed 

    using the current SCN. 

    COMMIT statements using the FORCE clause are not supported in 
    PL/SQL. 

PREREQUISITES: 

    You need no privileges to commit your current transaction. 

    To manually commit a distributed in-doubt transaction that you 
    originally committed, you must have FORCE TRANSACTION system 
    privilege.  To manually commit a distributed in-doubt transaction 
    that was originally committed by another user, you must have FORCE 

    ANY TRANSACTION system privilege. 

SEE: 
    ROLLBACK, SAVEPOINT, SET TRANSACTION 
"))
("rollback" . (nil "SQL command" "ROLLBACK command 

PURPOSE: 

    To undo work done in the current transaction. 

    You can also use this command to manually undo the work done by an 
    in-doubt distributed transaction. 

SYNTAX: 

ROLLBACK [WORK] 
    [ TO [SAVEPOINT] savepoint 
    | FORCE 'text' ] 

where: 

WORK 
    is optional and is provided for ANSI compatibility. 

TO 
    rolls back the current transaction to the specified savepoint.  If 

    you omit this clause, the ROLLBACK statement rolls back the entire 
    transaction. 

FORCE 
    manually rolls back an in-doubt distributed transaction.  The 
    transaction is identified by the 'text' containing its local or 
    global transaction ID.  To find the IDs of such transactions, query 
    the data dictionary view DBA_2PC_PENDING. 

    ROLLBACK statements with the FORCE clause are not supported in 

    PL/SQL. 

PREREQUISITES: 

    To roll back your current transaction, no privileges are necessary. 

    To manually roll back an in-doubt distributed transaction that you 
    originally committed, you must have FORCE TRANSACTION system 
    privilege.  To manually roll back an in-doubt distributed 
    transaction originally committed by another user, you must have 
    FORCE ANY TRANSACTION system privilege. 


SEE: 
    COMMIT, SAVEPOINT, SET TRANSACTION "))
("savepoint" . (nil "SQL command" "SAVEPOINT command 

PURPOSE: 

    To identify a point in a transaction to which you can later roll 
    back. 

SYNTAX: 

SAVEPOINT savepoint 

where: 

savepoint 
    is the name of the savepoint to be created. 

PREREQUISITES: 

    None. 

SEE: 
    COMMIT, ROLLBACK, SET TRANSACTION "))
("set transaction" . (nil "SQL command" "SET TRANSACTION command 

PURPOSE: 

    To perform one of these operations on your current transaction: 

    * establish your current transaction as either a read-only or a 
      read-write transaction 
    * assign your current transaction to a specified rollback segment 

SYNTAX: 

SET TRANSACTION 
    { READ ONLY 
    | READ WRITE 
    | USE ROLLBACK SEGMENT rollback_segment } 


where: 

READ ONLY 
    establishes the current transaction as a read-only transaction. 

READ WRITE 
    establishes the current transaction as a read-write transaction. 

USE ROLLBACK SEGMENT 
    assigns the current transaction to the specified rollback segment. 
    This option also establishes the transaction as a read-write 
    transaction. 

    You cannot use the READ ONLY option and the USE ROLLBACK SEGMENT 

    clause in a single SET TRANSACTION statement or in different 
    statements in the same transaction.  Read-only transactions do not 
    generate rollback information and therefore are not assigned 
    rollback segments. 

PREREQUISITES: 

    A SET TRANSACTION statement must be the first statement in your 
    transaction.  However, every transaction need not begin with a SET 
    TRANSACTION statement. 


SEE: 
    COMMIT, ROLLBACK, SAVEPOINT "))
("alter session" . (nil "SQL command" "ALTER SESSION command 

PURPOSE: 

    To alter your current session in one of these ways: 

    * to enable or disable the SQL trace facility 
    * to change the values of NLS parameters 
    * to change your DBMS session label in Trusted Oracle 
    * to change the default label format for your session 
    * to close a database link 
    * to send advice to remote databases for forcing an in-doubt 

      distributed transaction 
    * to permit or prohibit procedures and stored functions from issuing 
      COMMIT and ROLLBACK statements 
    * to change the goal of the cost-based optimization approach 

SYNTAX: 

ALTER SESSION 
   { SET { SQL_TRACE              = { TRUE | FALSE } 
         | GLOBAL_NAMES           = { TRUE | FALSE } 
         | NLS_LANGUAGE           =   language 
         | NLS_TERRITORY          =   territory 

         | NLS_DATE_FORMAT        =  'fmt' 
         | NLS_DATE_LANGUAGE      =   language 
         | NLS_NUMERIC_CHARACTERS =  'text' 
         | NLS_ISO_CURRENCY       =   territory 
         | NLS_CURRENCY           =  'text' 
         | NLS_SORT               = { sort  | BINARY } 
         | LABEL                  = {'text' | DBHIGH | DBLOW | OSLABEL } 
         | MLS_LABEL_FORMAT       =  'fmt' 
         | OPTIMIZER_GOAL         = { RULE | ALL_ROWS | FIRST_ROWS | CHOOSE } 

         | FLAGGER                = { ENTRY | INTERMEDIATE | FULL | OFF } 
         | CLOSE_CACHED_OPEN_CURSORS = { TRUE | FALSE }    
   } ... 
   | CLOSE DATABASE LINK dblink 
   | ADVISE {COMMIT | ROLLBACK | NOTHING} 
   | {ENABLE | DISABLE} COMMIT IN PROCEDURE } 

where: 

SQL_TRACE 
    controls the SQL trace facility for your session: 
            TRUE 
                   enables the SQL trace facility. 

            FALSE 
                   disables the SQL trace facility. 

GLOBAL_NAMES 
    controls the enforcement of global name resolution for your session: 
            TRUE 
                   enables the enforcement of global name resolution. 
            FALSE 
                   disables the enforcement of global name resolution. 

    For information on enabling and disabling global name resolution 

    with this parameter, see the ALTER SYSTEM command. 

NLS_LANGUAGE 
    changes the language in which Oracle returns errors and other 
    messages.  This parameter also implicitly specifies new values for 
    these items: 

          * language for day and month names and abbreviations and 
            spelled values of other date format elements 
          * sort sequence 

NLS_TERRITORY 

    implicitly specifies new values for these items: 

          * default date format 
          * decimal character and group separator 
          * local currency symbol 
          * ISO currency symbol 
          * first day of the week for D date format element 

NLS_DATE_FORMAT 
    explicitly specifies a new default date format. 

NLS_DATE_LANGUAGE 
    explicitly changes the language for day and month names and 

    abbreviations and spelled values of other date format elements. 

NLS_NUMERIC_CHARACTERS 
    explicitly specifies a new decimal character and group separator. 
    The 'text' value must have this form: 
            'dg' 
    where: 
            d 
                   is the new decimal character. 
            g 
                   is the new group separator. 

    The decimal character and the group separator must be different and 

    can only be single-byte characters. 

NLS_ISO_CURRENCY 
    explicitly specifies the territory whose ISO currency symbol should 
    be used. 

NLS_CURRENCY 
    explicitly specifies a new local currency symbol. 

NLS_SORT 
    changes the sequence into which Oracle sorts character values. 
            sort 
                   specifies the name of a linguistic sort sequence. 
            BINARY 

                   specifies a binary sort. 

LABEL 
    changes your DBMS session label to either: 

          * the label specified by 'text' in your session's default 
            label format 
          * the label equivalent to DBHIGH 
          * the label equivalent to DBLOW 
          * your operating system label using OSLABEL 

MLS_LABEL_FORMAT 
    changes the default label format for your session. 


OPTIMIZER_GOAL 
    specifies the approach and goal of the optimizer for your session: 
            RULE 
                   specifies the rule-based approach. 
            ALL_ROWS 
                   specifies the cost-based approach and optimizes for 
                   best throughput. 
            FIRST_ROWS 
                   specifies the cost-based approach and optimizes for 
                   best response time. 

            CHOOSE 
                   causes the optimizer to choose an optimization 
                   approach based on the presence of statistics in the 
                   data dictionary. 

FLAGGER 
    specifies that non-SQL92 compliant syntax should be flagged for the 
    session.  Oracle flags any non-standard constructs as errors and 
    displays the violating syntax. 
    Currently there is no difference between entry, intermediate, and 

    full level flagging.  These options will become significant as 
    Oracle conforms to SQL92 intermediate and full level standards. 
    OFF disables FIPS flagging. 

CLOSE_CACHED_OPEN_CURSORS 
    controls whether cursors opened and cached in memory by PL/SQL are 
    automatically closed at each COMMIT.  A value of FALSE signifies 
    that cursors opened by PL/SQL are held open so that subsequent 
    executions need not open a new cursor.  A value of TRUE causes 

    open cursors to be closed at each COMMIT or ROLLBACK. 
CLOSE DATABASE LINK 
    closes the database link dblink, eliminating your session's 
    connection to the remote database.  The database link cannot be 
    currently in use by an active transaction or an open cursor. 

ADVISE 
    sends advice for forcing a distributed transaction to a remote 
    database.  This advice appears on the remote database in the ADVICE 

    column of the DBA_2PC_PENDING data dictionary view in the event the 
    distributed transaction becomes in-doubt.  The following are advice 
    options: 
            COMMIT 
                   places the value 'C' in DBA_2PC_PENDING.ADVICE. 
            ROLLBACK 
                   places the value 'R' in DBA_2PC_PENDING.ADVICE. 
            NOTHING 
                   places the value ' ' in DBA_2PC_PENDING.ADVICE. 


COMMIT IN PROCEDURE 
    specifies whether procedures and stored functions can issue COMMIT 
    and ROLLBACK statements: 
            ENABLE 
                   permits procedures and stored functions to issue 
                   these statements. 
            DISABLE 
                   prohibits procedures and stored functions from 
                   issuing these statements. 

PREREQUISITES: 


    To enable and disable the SQL trace facility or to change the 
    default label format, you must have ALTER SESSION system privilege. 

    To raise your session label, you must have WRITEUP and READUP system 
    privileges.  To lower your session label, you must have WRITEDOWN 
    system privilege.  To change your session label laterally, you must 
    have READUP, WRITEUP, and WRITEDOWN system privileges. 


    To perform the other operations of this command, you do not need any 
    privileges. 

SEE: 
    Tuning SQL Statements and Appendix B of the Oracle Server 
    Application Developer's Guide. "))
("set role" . (nil "SQL command" "SET ROLE command 

PURPOSE: 

    To enable and disable roles for your current session. 

SYNTAX: 

SET ROLE     { role [IDENTIFIED BY password] 
        [, role [IDENTIFIED BY password] ] ... 
         | ALL [EXCEPT role [, role] ...] 
         | NONE } 

where: 

role 
    is a role to be enabled for the current session.  Any roles not 
    listed are disabled for the current session. 

            password 
                   is the password for a role.  If the role has a 
                   password, you must specify the password to enable the 
                   role. 

ALL EXCEPT 
    enables all roles granted to you for the current session, except 
    those listed in the EXCEPT clause.  Roles listed in the EXCEPT 
    clause must be roles granted directly to you; they cannot be roles 

    granted to you through other roles.  You cannot use this option to 
    enable roles with passwords that have been granted directly to you. 

    If you list a role in the EXCEPT clause that has been granted to you 
    both directly and through another role, the role is still enabled by 
    virtue of your enabling the role to which it has been granted. 

NONE 
    disables all roles for the current session. 


PREREQUISITES: 

    You also must already have been granted the roles that you name in 
    this statement. 

    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 
    must dominate the label at which these roles were granted to you. 

SEE: 
    ALTER USER, CREATE ROLE "))
("alter system" . (nil "SQL command" "ALTER SYSTEM command 

PURPOSE: 

    To dynamically alter your Oracle instance in one of these ways: 

    * to enable or disable resource limits 
    * to manage shared server processes or dispatcher processes for the 
      multi-threaded server architecture 
    * to explicitly switch redo log file groups 
    * to explicitly perform a checkpoint 
    * to verify access to data files 
    * to restrict logons to Oracle to only those users with RESTRICTED 

      SESSION system privilege 
    * to enable distributed recovery in a single-process environment 
    * to disable distributed recovery 
    * to manually archive redo log file groups or to enable or disable 
       automatic archiving 
    * to clear all data from the shared pool in the System Global Area 
      (SGA) 
    * to terminate a session 

SYNTAX: 

ALTER SYSTEM 
    { {ENABLE | DISABLE} RESTRICTED SESSION 

    | FLUSH SHARED_POOL 
    | {CHECKPOINT | CHECK DATAFILES} [GLOBAL | LOCAL] 
    | SET { RESOURCE_LIMIT           = { TRUE | FALSE } 
          | GLOBAL_NAMES             = { TRUE | FALSE } 
          | MTS_DISPATCHERS          = 'protocol, integer' 
          | MTS_SERVERS              = integer 
          | LICENSE_MAX_SESSIONS     = integer 
          | LICENSE_SESSIONS_WARNING = integer 
          | LICENSE_MAX_USERS        = integer 

          | SESSION_CACHED_CURSORS   = integer } ... 
    | SWITCH LOGFILE 
    | {ENABLE | DISABLE} DISTRIBUTED RECOVERY 
    | ARCHIVE LOG archive_log_clause 
    | KILL SESSION 'integer1, integer2' } 

where: 

You can use these options regardless of whether your instance has the 
database dismounted or mounted, open or closed: 

ENABLE RESTRICTED SESSION 
    allows only users with RESTRICTED SESSION system privilege to logon 

    to Oracle. 

DISABLE RESTRICTED SESSION 
    reverses the effect of the ENABLE RESTRICTED SESSION option, 
    allowing all users with CREATE SESSION system privilege to logon to 
    Oracle. 

FLUSH SHARED_POOL 
    clears all data from the shared pool in the System Global Area 
    (SGA). 

You can use these options when your instance has the database mounted, 
open or closed: 


CHECKPOINT 
    performs a checkpoint. 
            GLOBAL 
                   performs a checkpoint for all instances that have 
                   opened the database. 
            LOCAL 
                   performs a checkpoint only for the thread of redo log 
                   file groups for your instance.  You can only use this 
                   option when your instance has the database open. 


    If you omit both the GLOBAL and LOCAL options, Oracle performs a 
    global checkpoint. 

CHECK DATAFILES 
    verifies access to online data files. 
            GLOBAL 
                   verifies that all instances that have opened the 
                   database can access all online data files. 
            LOCAL 
                   verifies that your instance can access all online 

                   data files. 

    If you omit both the GLOBAL and LOCAL options, Oracle uses GLOBAL by 
    default. 

You can only use these parameters and options when your instance has the 
database open: 

RESOURCE_LIMIT 
    controls resource limits. 
            TRUE 
                   enables resource limits. 
            FALSE 
                   disables resource limits. 


GLOBAL_NAMES 
    controls the enforcement of global naming: 
            TRUE 
                   enables the enforcement of global names. 
            FALSE 
                   disables the enforcement of global names. 

MTS_SERVERS 
    specifies a new minimum number of shared server processes. 

MTS_DISPATCHERS 
    specifies a new number of dispatcher processes: 
            protocol 

                   is the network protocol of the dispatcher processes. 
            integer 
                   is the new number of dispatcher processes of the 
                   specified protocol. 

    You can specify multiple MTS_DISPATCHERS parameters in a single 
    command for multiple network protocols. 

LICENSE_MAX_SESSIONS 
    limits the number of sessions on your instance.  A value of 0 

    disables the limit. 

LICENSE_SESSIONS_WARNING 
    establishes a threshold of sessions over which Oracle writes warning 
    messages to the ALERT file for subsequent sessions.  A value of 0 
    disables the warning threshold. 

LICENSE_MAX_USERS 
    limits the number of users on your database.  A value of 0 disables 
    the limit. 

SESSION_CACHED_CURSORS 
    specify a positive integer for the maximum number of session cursors 

    kept in the cursor cache. 

SWITCH LOGFILE 
    switches redo log file groups. 

ENABLE DISTRIBUTED RECOVERY 
    enables distributed recovery.  In a single-process environment, you 
    must use this option to initiate distributed recovery. 

DISABLE DISTRIBUTED RECOVERY 
    disables distributed recovery. 

ARCHIVE LOG 
    manually archives redo log files or enables or disables automatic 

    archiving. 

KILL SESSION 
    terminates a session.  You must identify the session with both of 
    these values from the V$SESSION: 
            integer1 
                   is the value of the SID column. 
            integer2 
                   is the value of the SERIAL# column. 

PREREQUISITES: 

    You must have ALTER SYSTEM system privilege. 

    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 

    must be the equivalent of DBHIGH. 

SEE: 
    ALTER SESSION, CREATE PROFILE, CREATE USER "))
("archive log" . (nil "SQL clause" "ARCHIVE LOG clause 

PURPOSE: 

    To manually archive redo log file groups or to enable or disable 
    automatic archiving. 

SYNTAX: 

ARCHIVE LOG [THREAD integer] 
    { { SEQ integer 
      | CHANGE integer 
      | CURRENT 
      | GROUP integer 
      | LOGFILE 'filename' 
      | NEXT 
      | ALL 
      | START } 
      [TO 'location'] 

    | STOP } 

where: 

THREAD 
    specifies thread containing the redo log file group to be archived. 
    You only need to specify this parameter if you are using Oracle with 
    the Parallel Server option in parallel mode. 

SEQ 
    manually archives the online redo log file group identified by the 
    log sequence number integer in the specified thread.  If you omit 
    the THREAD parameter, Oracle archives the specified group from the 

    thread assigned to your instance. 

CHANGE 
    manually archives the online redo log file group containing the redo 
    log entry with the system change number (SCN) specified by integer 
    in the specified thread.  If the SCN is in the current redo log file 
    group, Oracle performs a log switch.  If you omit the THREAD 
    parameter, Oracle archives the groups containing this SCN from all 
    enabled threads.  You can only use this option when your instance 

    has the database open. 

CURRENT 
    manually archives the current redo log file group of the specified 
    thread, forcing a log switch.  If you omit the THREAD parameter, 
    Oracle archives the current redo log file groups from all enabled 
    threads.  You can only use this option when your instance has the 
    database open. 

GROUP 
    manually archives the online redo log file group with the specified 

    GROUP value.  You can determine the GROUP value for a redo log file 
    group by examining the data dictionary view DBA_LOG_FILES.  If you 
    specify both the THREAD and GROUP parameters, the specified redo log 
    file group must be in the specified thread. 

LOGFILE 
    manually archives the online redo log file group containing the redo 
    log file member identified by 'filename'.  If you specify both the 

    THREAD and LOGFILE parameters, the specified redo log file group 
    must be in the specified thread. 

NEXT 
    manually archives the next online redo log file group from the 
    specified thread that is full but has not yet been archived.  If you 
    omit the THREAD parameter, Oracle archives the earliest unarchived 
    redo log file group from any enabled thread. 

ALL 
    manually archives all online redo log file groups from the specified 

    thread that are full but have not been archived.  If you omit the 
    THREAD parameter, Oracle archives all full unarchived redo log file 
    groups from all enabled threads. 

START 
    enables automatic archiving of redo log file groups.  You can only 
    enable automatic archiving for the thread assigned to your instance. 

TO 
    specifies the location to which the redo log file group is archived. 

    The value of this parameter must be a fully-specified file location 
    following the conventions of your operating system.  If you omit 
    this parameter, Oracle archives the redo log file group to the 
    location specified by the initialization parameter LOG_ARCHIVE_DEST. 

STOP 
    disables automatic archiving of redo log file groups.  You can only 
    disable automatic archiving for the thread assigned to your 

    instance. 

PREREQUISITES: 

    The ARCHIVE LOG clause must appear in an ALTER SYSTEM command.  You 
    must have the privileges necessary to issue this statement.  For 
    information on these privileges, see the ALTER SYSTEM command. 

    You must also have the OSDBA or OSOPER role enabled. 

    You can use most of the options of this clause when your instance 
    has the database mounted, open or closed.  Options that require your 

    instance to have the database open are noted. 

    If you are using Trusted Oracle in DBMS MAC mode, your DBMS label 
    must be the equivalent of DBHIGH. 

SEE: 
    ALTER SYSTEM "))
("constraint" . (nil "SQL clause" "CONSTRAINT clause 

PURPOSE: 

    To define an integrity constraint.  An integrity constraint is a 
    rule that restricts the values for one or more columns in a table. 

SYNTAX: 

Column constraint: 

[CONSTRAINT constraint] 
{ [NOT] NULL 
| {UNIQUE | PRIMARY KEY} 
|  REFERENCES [schema.]table [(column)] 
        [ON DELETE CASCADE] 
|  CHECK (condition) } 
{ [ USING INDEX [PCTFREE integer] 

                [INITRANS integer] [MAXTRANS integer] 
                [TABLESPACE tablespace] 
                [STORAGE storage_clause] 
                [PARALLEL [integer] | NOPARALLEL] ] 
  [ EXCEPTIONS INTO [schema.]table 
| DISABLE } 

Table constraint: 

[CONSTRAINT constraint] 
{ {UNIQUE | PRIMARY KEY} (column [,column] ...) 
|  FOREIGN KEY (column [,column] ...) 
         REFERENCES [schema.]table [(column [,column] ...)] 

        [ON DELETE CASCADE] 
| CHECK (condition) } 
{ [ USING INDEX [PCTFREE integer] 
                [INITRANS integer] [MAXTRANS integer] 
                [TABLESPACE tablespace] 
                [STORAGE storage_clause] 
                [PARALLEL [integer] | NOPARALLEL] ] 
  [ EXCEPTIONS INTO [schema.]table[@dblink] 
| DISABLE } 

where: 

CONSTRAINT 
    identifies the integrity constraint by the name constraint.  Oracle 

    stores this name in the data dictionary along with the definition of 
    the integrity constraint.  If you omit this identifier, Oracle 
    generates a name with this form: 
            SYS_Cn 
    where 
            n 
                   is an integer that makes the name unique 
                   within the database. 

    For the names and definitions of integrity constraints, query the 

    data dictionary. 

NULL 
    specifies that a column can contain null values. 

NOT NULL 
    specifies that a column cannot contain null values. 

    If you do not specify NULL or NOT NULL in a column definition, NULL 
    is the default. 

UNIQUE 
    designates a column or combination of columns as a unique key. 

PRIMARY KEY 
    designates a column or combination of columns as the table's primary 

    key. 

FOREIGN KEY 
    designates a column or combination of columns as the foreign key in 
    a referential integrity constraint. 

REFERENCES 
    identifies the primary or unique key that is referenced by a foreign 
    key in a referential integrity constraint. 

ON DELETE CASCADE 
    specifies that Oracle maintains referential integrity by 
    automatically removing dependent foreign key values if you remove a 

    referenced primary or unique key value. 

CHECK 
    specifies a condition that each row in the table must satisfy. 

USING INDEX 
    specifies parameters for the index Oracle uses to enforce a UNIQUE 
    or PRIMARY KEY constraint.  The name of the index is the same as the 
    name of the constraint.  You can choose the values of the INITRANS, 
    MAXTRANS, TABLESPACE, STORAGE, and PCTFREE parameters for the index. 

    For information on these parameters, see the CREATE TABLE command. 

    Only use this clause when enabling UNIQUE and PRIMARY KEY 
    constraints. 

  PARALLEL 
     specifies the number of processes that create the index in parallel. 
     You can only specify positive integer values greater than 1.  If you 
     do not specify an integer, the degree of parallelism is based on 
     the parallelism specified in the table's definition. 


  NOPARALLEL 
     specifies that the index should not be created in parallel. 

EXCEPTIONS INTO 
    identifies a table into which Oracle places information about rows 
    that violate an enabled integrity constraint.  This table must exist 
    before you use this option.  If you omit schema, Oracle assumes the 
    exception table is in your own schema.  The exception table must be 
    on your local database. 


DISABLE 
    disables the integrity constraint.  If an integrity constraint is 
    disabled, Oracle does not enforce it. 

    If you do not specify this option, Oracle automatically enables the 
    integrity constraint. 

    You can also enable and disable integrity constraints with the 
    ENABLE and DISABLE clauses of the CREATE TABLE and ALTER TABLE 
    commands. 

PREREQUISITES: 


    CONSTRAINT clauses can appear in either CREATE TABLE or ALTER TABLE 
    commands.  To define an integrity constraint, you must have the 
    privileges necessary to issue one of these commands.  See the CREATE 
    TABLE and ALTER TABLE commands. 

    Defining a constraint may also require additional privileges or 
    preconditions that depend on the type of constraint. 

SEE: 
    ALTER TABLE, CREATE TABLE, DISABLE clause, ENABLE clause "))
("disable" . (nil "SQL clause" "DISABLE clause 

PURPOSE: 

    To disable an integrity constraint or all triggers associated with a 
    table: 

    * If you disable an integrity constraint, Oracle does not enforce 
      it.  However, disabled integrity constraints appear in the data 
      dictionary along with enabled integrity constraints. 
    * If you disable a trigger, Oracle does not fire it if its 
      triggering condition is satisfied. 


SYNTAX: 

DISABLE {    { UNIQUE (column [, column] ...) 
             | PRIMARY KEY 
             | CONSTRAINT constraint } 
                 [CASCADE] 
        |    ALL TRIGGERS } 

where: 

UNIQUE 
    disables the UNIQUE constraint defined on the specified column or 
    combination of columns. 

PRIMARY KEY 
    disables the table's PRIMARY KEY constraint. 


CONSTRAINT 
    disables the integrity constraint with the name constraint. 

CASCADE 
    disables any integrity constraints that depend on the specified 
    integrity constraint.  To disable a primary or unique key that is 
    part of a referential integrity constraint, you must specify this 
    option. 

ALL TRIGGERS 
    disables all triggers associated with the table.  This option can 

    only appear in a DISABLE clause in an ALTER TABLE statement, not a 
    CREATE TABLE statement. 

PREREQUISITES: 

    A DISABLE clause that disables an integrity constraint can appear in 
    either a CREATE TABLE or ALTER TABLE command.  To disable an 
    integrity constraint, you must have the privileges necessary to 
    issue one of these commands.  For information on these privileges, 
    see the CREATE TABLE and ALTER TABLE commands. 


    For an integrity constraint to appear in a DISABLE clause, one of 
    these conditions must be true: 

    * the integrity constraint must be defined in the containing 
      statement. 
    * the integrity constraint must already have been defined and 
      enabled in previously issued statements. 

    A DISABLE clause that disables triggers can only appear in an ALTER 
    TABLE statement.  To disable triggers with a DISABLE clause, you 

    must have the privileges necessary to issue this statement.  For 
    information on these privileges, see the ALTER TABLE command.  Also, 
    the triggers must be in your own schema or you must have ALTER ANY 
    TRIGGER system privilege. 

SEE: 
    ALTER TABLE, ALTER TRIGGER, CONSTRAINT clause, CREATE TABLE, CREATE 
    TRIGGER, ENABLE clause "))
("drop" . (nil "SQL clause" "DROP clause 

PURPOSE: 

    To remove an integrity constraint from the database. 

SYNTAX: 

DROP { PRIMARY KEY 
     | UNIQUE (column [, column] ...) 
     | CONSTRAINT constraint } 
     [CASCADE] 

where: 

PRIMARY KEY 
    drops the table's PRIMARY KEY constraint. 

UNIQUE 
    drops the UNIQUE constraint on the specified columns. 

CONSTRAINT 

    drops the integrity constraint named constraint. 

CASCADE 
    drops all other integrity constraints that depend on the dropped 
    integrity constraint. 

PREREQUISITES: 

    The DROP clause can appear in an ALTER TABLE statement.  To drop an 
    integrity constraint, you must have the privileges necessary to 
    issue an ALTER TABLE statement.  For information on these 
    privileges, see the ALTER TABLE command earlier. 


SEE: 
    ALTER TABLE, CONSTRAINT clause "))
("enable" . (nil "SQL clause" "ENABLE clause 

PURPOSE: 

    To enable an integrity constraint or all triggers associated with a 
    table: 

    * If you enable a constraint, Oracle enforces it by applying it to 
      all data in the table.  All table data must satisfy an enabled 
      constraint. 
    * If you enable a trigger, Oracle fires the trigger whenever its 
      triggering condition is satisfied. 

SYNTAX: 


ENABLE { {UNIQUE (column [, column] ...) 
         |PRIMARY KEY 
         |CONSTRAINT constraint} 
            [USING INDEX [INITRANS integer] 
                         [MAXTRANS integer] 
                         [TABLESPACE tablespace] 
                         [STORAGE storage_clause] 
                         [PCTFREE integer] 
              [PARALLEL [integer] | NOPARALLEL] 
            [EXCEPTIONS INTO [schema.]table ] 

       | ALL TRIGGERS } 

where: 

UNIQUE 
    enables the UNIQUE constraint defined on the specified column or 
    combination of columns. 

PRIMARY KEY 
    enables the table's PRIMARY KEY constraint. 

CONSTRAINT 
    enables the integrity constraint named constraint. 

USING INDEX 
    specifies parameters for the index Oracle creates to enforce a 
    UNIQUE or PRIMARY KEY constraint.  Oracle gives the index the same 

    name as the constraint.  You can choose the values of the INITRANS, 
    MAXTRANS, TABLESPACE, STORAGE, and PCTFREE parameters for the index. 
    For information on these parameters, see the CREATE TABLE command. 

    Only use these parameters when enabling UNIQUE and PRIMARY KEY 
    constraints. 

  PARALLEL 
     specifies the number of processes that create the index in parallel. 
     You can only specify positive integer values greater than 1.  If you 

     do not specify an integer, the degree of parallelism is based on 
     the parallelism specified in the table's definition. 

  NOPARALLEL 
     specifies that the index should not be created in parallel. 

EXCEPTIONS INTO 
    identifies an table into which Oracle places information about rows 
    that violate the integrity constraint.  The table must exist before 
    you use this option.  If you omit schema, Oracle assumes the 

    exception table is in your own schema.  The exception table must be 
    on your local database. 

ALL TRIGGERS 
    enables all triggers associated with the table.  You can only use 
    this option in an ENABLE clause in an ALTER TABLE statement, not a 
    CREATE TABLE statement. 

PREREQUISITES: 

    An ENABLE clause that enables an integrity constraint can appear in 
    either a CREATE TABLE or ALTER TABLE statement.  To enable a 

    constraint in this manner, you must have the privileges necessary to 
    issue one of these statements.  For information on these privileges, 
    see the CREATE TABLE or ALTER TABLE command. 

    If you enable a UNIQUE or PRIMARY KEY constraint, Oracle creates an 
    index on the columns of the unique or primary key in the schema 
    containing the table.  To enable such a constraint, you must have 

    the privileges necessary to create the index.  For information on 
    these privileges, see the CREATE INDEX command. 

    If you enable a referential integrity constraint, the referenced 
    UNIQUE or PRIMARY KEY constraint must already be enabled. 

    For an integrity constraint to appear in an ENABLE clause, one of 
     these conditions must be true: 

    * the integrity constraint must be defined in the containing 

      statement 
    * the integrity constraint must already have been defined and 
      disabled in a previously issued statement 

    An ENABLE clause that enables triggers can appear in an ALTER TABLE 
    statement.  To enable triggers with the ENABLE clause, you must have 
    the privileges necessary to issue this statement.  For information 
    on these privileges, see the ALTER TABLE command.  Also, the 

    triggers must be in your own schema or you must have ALTER ANY 
    TRIGGER system privilege. 

SEE: 
    ALTER TABLE, ALTER TRIGGER, CONSTRAINT clause, CREATE TABLE, CREATE 
    TRIGGER, DISABLE clause, STORAGE clause "))
("filespec" . (nil "SQL clause" "FILESPEC clause

PURPOSE: 

    To either specify a file as a data file or specify a group of one or 
    more files as a redo log file group. 

SYNTAX: 

Data files: 

'filename' [SIZE integer [K|M] ] [REUSE] 

Redo log file groups: 

{  'filename' 
| ('filename' [, 'filename'] ...)} 
    [SIZE integer [K|M] ] [REUSE] 

where: 

'filename' 
    is the name of either a data file or a redo log file member.  A redo 

    log file group can have one or more members, or copies.  Each 
    'filename' must be fully specified according to the conventions for 
    your operating system. 

SIZE 
    specifies the size of the file.  If you omit this parameter, the 
    file must already exist. 
            K 
                   specifies the size in kilobytes. 
            M 
                   specifies the size in megabytes. 


    If you omit K and M, the size is specified in bytes. 

REUSE 
    allows Oracle to reuse an existing file.  If the file already 
    exists, Oracle verifies that its size matches the value of the SIZE 
    parameter.  If the file does not exist, Oracle creates it.  If you 
    omit this option, the file must not already exist and Oracle creates 
    the file. 

    The REUSE option is only significant when used with the SIZE option. 

    If you omit the SIZE option, Oracle expects the file to exist 
    already.  Note that whenever Oracle uses an existing file, the 
    file's previous contents are lost. 

PREREQUISITES: 

    A filespec can appear in either CREATE DATABASE, ALTER DATABASE, 
    CREATE TABLESPACE, or ALTER TABLESPACE commands.  You must have the 
    privileges necessary to issue one of these commands.  For 
    information on these privileges, see the CREATE DATABASE, ALTER 

    DATABASE, CREATE TABLESPACE, and ALTER TABLESPACE commands. 

SEE: 
    ALTER DATABASE, ALTER TABLESPACE, CREATE DATABASE, CREATE 
    TABLESPACE "))
("recover" . (nil "SQL clause" "RECOVER clause 

PURPOSE: 

    To perform media recovery. 

SYNTAX: 

RECOVER     [AUTOMATIC] [FROM 'location'] 
        { [DATABASE]    [ UNTIL CANCEL 
                    | UNTIL TIME date 
                    | UNTIL CHANGE integer 
                    | USING BACKUP CONTROLFILE ] 
         [PARALLEL integer | NOPARALLEL] 
        | TABLESPACE tablespace [, tablespace] ... 

          [PARALLEL integer | NOPARALLEL] 
        | DATAFILE 'filename' [, 'filename'] ... 
          [PARALLEL integer | NOPARALLEL] 
        | LOGFILE 'filename' 
        | CONTINUE [DEFAULT] 
        | CANCEL } 

where: 

AUTOMATIC 
    automatically generates the names of the redo log files to apply 
    during media recovery.  If you omit this option, Oracle prompts you 
    for names of redo log files and you must specify them by issuing 

    ALTER DATABASE statements with the LOGFILE parameter. 

FROM 
    specifies the location from which the archived redo log file group 
    is read.  The value of this parameter must be a fully-specified file 
    location following the conventions of your operating system.  If you 
    omit this parameter, Oracle assumes the archived redo log file group 
    is in the location specified by the initialization parameter 

    LOG_ARCHIVE_DEST 

DATABASE 
    recovers the entire database.  This is the default option.  You can 
    only use this option when the database is closed. 

PARALLEL 
    specifies the number of recovery processes used to apply redo 
    entries to datafiles.  The specified value overrides the 
    RECOVERY_PARALLELISM initialization parameter.  Specified values 
    must be positive integers greater than 1. 


NOPARALLEL 
    specifies that recovery should proceed using only one recovery 
    process. 

UNTIL CANCEL 
    performs cancel-based recovery.  This option recovers the database 
    until you cancel recovery by issuing an ALTER DATABASE statement 
    with a RECOVER clause containing the CANCEL option. 

UNTIL TIME 
    performs time-based recovery.  This parameter recovers the database 

    to the time specified by the date.  The date must be a character 
    literal in the format 'YYYY-MM-DD:HH24:MI:SS'. 

UNTIL CHANGE 
    performs change-based recovery.  This parameter recovers the 
    database to a transaction consistent state immediately prior to the 
    system change number (SCN) specified by integer. 

USING BACKUP CONTROLFILE 
    specifies that a backup control file is being used instead of the 

    current control file. 

TABLESPACE 
    recovers only the specified tablespaces.  You can use this option if 
    the database is open or closed, provided the tablespaces to be 
    recovered are not being used. 

DATAFILE 
    recovers the specified data files.  You can use this option when the 
    database is open or closed, provided the data files to be recovered 
    are not being used. 


LOGFILE 
    continues media recovery by applying the specified the redo log 
    file. 

CONTINUE 
    continues multi-instance recovery after it has been interrupted to 
    disable a thread. 

CONTINUE DEFAULT 
    continues recovery by applying the redo log file that Oracle has 
    automatically generated. 

CANCEL 
    terminates cancel-based recovery. 

PREREQUISITES: 


    The RECOVER clause must appear in a ALTER DATABASE statement.  You 
    must have the privileges necessary to issue this statement.  For 
    information on these privileges, see the ALTER DATABASE command. 

    You must also have the OSDBA role enabled.  You cannot be connected 
    to Oracle through the multi-threaded server architecture. Your 
    instance must have the database mounted in exclusive mode. 


SEE: 
    ALTER DATABASE "))
("storage" . (nil "SQL clause" "STORAGE clause 

PURPOSE: 

    To specify storage characteristics for tables, indexes, clusters, 
    and rollback segments, and the default storage characteristics for 
    tablespaces. 

SYNTAX: 

STORAGE (    [INITIAL             integer [K|M] ] 
            [NEXT              integer [K|M] ] 
            [PCTINCREASE         integer] 
            [MINEXTENTS         integer] 
            [MAXEXTENTS          integer] 

            [OPTIMAL         {integer  [K|M] | NULL}] 
            [FREELIST GROUPS     integer] 
            [FREELISTS         integer] ) 

where: 

INITIAL 
    specifies the size in bytes of the object's first extent.  Oracle 
    allocates space for this extent when you create the object.  You can 
    also use K or M to specify this size in kilobytes or megabytes.  The 
    default value is the size of 5 data blocks.  The minimum value is 

    the size of 2 data blocks.  The maximum value varies depending on 
    your operating system.  Oracle rounds values up to the next multiple 
    of the data block size. 

NEXT 
    specifies the size in bytes of the next extent to be allocated to 
    the object.  You can also use K or M to specify the size in 
    kilobytes or megabytes.  The default value is the size of 5 data 
    blocks.  The minimum value is the size of 1 data block.  The maximum 

    value varies depending on your operating system.  Oracle rounds 
    values up to the next multiple of the data block size. 

PCTINCREASE 
    specifies the percent by which each extent after the second grows 
    over the previous extent.  The default value is 50,  meaning that 
    each subsequent extent is 50% larger than the preceding extent. The 
    minimum value is 0, meaning all extents after the first are the same 

    size.  The maximum value varies depending on your operating system. 

    You cannot specify PCTINCREASE for rollback segments.  Rollback 
    segments always have a PCTINCREASE value of 0. 

    Oracle rounds the calculated size of each new extent up to the next 
    multiple of the data block size. 

MINEXTENTS 
    specifies the total number of extents allocated when the segment is 
    created.  This parameter allows you to allocate a large amount of 

    space when you create an object, even if the space available is not 
    contiguous.  The default and minimum value is 1, meaning that Oracle 
    only allocates the initial extent, except for rollback segments for 
    which the default and minimum value is 2.  The maximum value varies 
    depending on your operating system. 

    If the MINEXTENTS value is greater than 1, then Oracle calculates 
    the size of subsequent extents based on the values of the INITIAL, 

    NEXT, and PCTINCREASE parameters. 

MAXEXTENTS 
    specifies the total number of extents, including the first, that 
    Oracle can allocate for the object.  The minimum value is 1.  The 
    default and maximum values vary depending your data block size. 

OPTIMAL 
    specifies an optimal size in bytes for a rollback segment.  You can 
    also use K or M to specify this size in kilobytes or megabytes. 

    Oracle tries to maintain this size for the rollback segment by 
    dynamically deallocating extents when their data is no longer needed 
    for active transactions.  Oracle deallocates as many extents as 
    possible without reducing the total size of the rollback segment 
    below the OPTIMAL value.  This parameter is only for rollback 
    segments and not for other objects. 
            NULL 
                   specifies no optimal size for the rollback segment, 

                   meaning that Oracle never deallocates the rollback 
                   segment's extents.  This is the default behavior. 

    The value of this parameter cannot be less than the space initially 
    allocated for the rollback segment specified by the MINEXTENTS, 
    INITIAL, NEXT, and PCTINCREASE parameters. The maximum value varies 
    depending on your operating system.  Oracle rounds values to the 

    next multiple of the data block size. 

FREELIST GROUPS 
    specifies the number of groups of free lists for a table, cluster, 
    or index.  The default and minimum value for this parameter is 1. 
    You should only use this parameter if you are using Oracle with the 
    Parallel Server option in parallel mode. 

FREELISTS 
    specifies the number of free lists for each of the free list groups 

    for the table, cluster, or index.  The default and minimum value for 
    this parameter is 1, meaning that each free list group contains one 
    free list.  The maximum value of this parameter depends on the data 
    block size.  If you specify a FREELISTS value that is too large, 
    Oracle returns an error indicating the maximum value. 

    You can only specify the FREELISTS parameter in CREATE TABLE, CREATE 
    CLUSTER, and CREATE INDEX statements.  You can only specify the 

    FREELIST GROUPS parameter in CREATE TABLE and CREATE CLUSTER 
    statements. 

PREREQUISITES: 

    The STORAGE clause can appear in commands that create or alter any 
    of these objects: 

    * clusters 
    * indexes 
    * rollback segments 
    * snapshots 
    * snapshot logs 
    * tables 
    * tablespaces 

    To change the value of a STORAGE parameter, you must have the 

    privileges necessary to issue one of these commands. 

SEE: 
    CREATE CLUSTER, CREATE INDEX, CREATE ROLLBACK SEGMENT, CREATE TABLE, 
    CREATE TABLESPACE "))
("char" . (nil "ORACLE SQL Datatype" "CHAR Datatype 

    The CHAR datatype specifies a fixed length character string.  When 
    you create a table with a CHAR column, you can supply the column 
    length in bytes.  Oracle subsequently ensures that all values stored 
    in that column have this length.  If you insert a value that is 
    shorter than the column length, Oracle blank-pads the value to 
    column length.  If you try to insert a value that is too long for 

    the column, Oracle returns an error. 

    The default length for a CHAR column is 1 byte.  The maximum length 
    of CHAR data is 255 bytes.  Oracle compares CHAR values using blank- 
    padded comparison semantics. 

SEE: 
    Character Datatypes, VARCHAR Datatype, VARCHAR2 Datatype "))
("varchar" . (nil "ORACLE SQL Datatype" "VARCHAR Datatype 

    The VARCHAR datatype is currently synonymous with the VARCHAR2 
    datatype.  Oracle Corporation recommends that you use VARCHAR2 
    rather than VARCHAR.  In a future version of Oracle, VARCHAR might 
    be a separate datatype used for variable length character strings 
    compared with different comparison semantics. 

SEE: 
    Character Datatypes, CHAR Datatype, VARCHAR2 Datatype "))
("varchar2" . (nil "ORACLE SQL Datatype" "VARCHAR2 Datatype 

    The VARCHAR2 datatype specifies a variable length character string. 
    When you create a VARCHAR2 column, you can supply the maximum number 
    of bytes of data that it can hold.  Oracle subsequently stores each 
    value in the column exactly as you specify it, provided it does not 
    exceed the column's maximum length.  If you try to insert a value 
    that exceeds this length, Oracle returns an error. 


    You must specify a maximum length for a VARCHAR2 column.  The 
    maximum length of VARCHAR2 data is 2000 bytes.  Oracle compares 
    VARCHAR2 values using non-padded comparison semantics. 

SEE: 
    Character Datatypes, CHAR Datatype, VARCHAR Datatype "))
("long" . (nil "ORACLE SQL Datatype" "LONG Datatype 

    LONG columns store variable length character strings containing up 
    to 2 gigabytes, or 2 
    characteristics of VARCHAR2 columns.  You can use LONG columns to 
    store long text strings.  Oracle uses LONG columns in the data 
    dictionary to store the text of view definitions.  The length of 
    LONG values may also be limited by the memory available on your 
    computer. 


    You can reference LONG columns in SQL statements in these places: 

    * SELECT lists 
    * SET clauses of UPDATE statements 
    * VALUES clauses of INSERT statements 

    The use of LONG values are subject to some restrictions: 

    * A table cannot contain more than one LONG column. 
    * LONG columns cannot appear in integrity constraints (except for 
      NULL and NOT NULL constraints). 

    * LONG columns cannot be indexed. 
    * A procedure or stored function cannot accept a LONG argument. 
    * A stored function cannot return a LONG value. 
    * Within a single SQL statement, all LONG columns, sequences, 
      updated tables, and locked tables must be located on the same 
      database. 

    Also, LONG columns cannot appear in certain parts of SQL statements: 

    * WHERE, GROUP BY, ORDER BY, or CONNECT BY clauses or with the 

      DISTINCT operator in SELECT statements 
    * SQL Functions (such as SUBSTR or INSTR) 
    * expressions or conditions 
    * select lists of queries containing GROUP BY clauses 
    * select lists of subqueries or queries combined by set operators 
    * select lists of CREATE TABLE AS SELECT statements 

    You can use the Oracle Call Interfaces to retrieve a portion of a 
    LONG value from the database. 


SEE: 
      Character Datatypes, VARCHAR2 Datatype "))
("mlslabel" . (nil "ORACLE SQL Datatype" "MLSLABEL Datatype 

    The MLSLABEL datatype is used to store the binary format a label 
    used on a secure operating system.  Labels are used by Trusted 
    Oracle to mediate access to information.  You can also define 
    columns with this datatype if you are the standard Oracle Server. "))
("number" . (nil "ORACLE SQL Datatype" "NUMBER Datatype 

    The NUMBER datatype is used to store zero, positive and negative 
    fixed and floating point numbers with magnitudes between 1.0 x 
    10 
    digits of precision.  If you specify an arithmetic expression whose 
    value has a magnitude greater than or equal to 1.0 x 10 
    returns an error.  You can specify a fixed point number datatype 
    with this syntax: 

    NUMBER(p,s) 


where: 

p 
    is the precision, or the total number of digits. Oracle guarantees 
    the portability of numbers with precision ranging from 1 to 38. 
s 
    is the scale, or the number of digits to the right of the decimal 
    point. The scale can range from -84 to 127. 

You can also use one of these alternate forms: 

NUMBER(p) 
    is a fixed point number with precision p and scale 0. 


NUMBER 
    is a floating point number with precision 38.  Note that a scale 
    value is not applicable for floating point numbers. 

SCALE AND PRECISION 
    Specify the scale and precision of a number column for extra 
    integrity checking on input.  Specifying scale and precision does 
    not force all values to a fixed length.  If a value exceeds the 
    precision, Oracle returns an error.  If a value exceeds the scale, 

    Oracle rounds it. 

    These examples show how Oracle stores data using different 
    precisions and scales. 

Actual Data            Specified as            Stored as 
-----------            ------------            --------- 
7456123.89             NUMBER                  7456123.89 
7456123.89             NUMBER (9)              7456124 
7456123.89             NUMBER (9,2)            7456123.89 

7456123.89             NUMBER (9,1)            7456123.9 
7456123.8              NUMBER (6)              exceeds precision 
7456123.8              NUMBER (15,1)           7456123.8 
7456123.89             NUMBER (7,-2)           7456100 
7456123.89             NUMBER(7,2)             exceeds precision 

NEGATIVE SCALE 
    If the scale is negative, the actual data is rounded to the 
    specified  number of places to the left of the decimal point. For 

    example, a specification of (10,-2) means to round to hundreds. 

SCALE GREATER THAN PRECISION 
    You can specify a scale that is greater than precision, although it 
    is uncommon.  In this case, the precision specifies the maximum 
    number of digits to the right of the decimal point.  As with all 
    number datatypes, if the value exceeds the precision, Oracle returns 
    an error.  If the value exceeds the scale, Oracle rounds the value. 

    For example, a column defined as NUMBER(4,5) requires a zero for the 
    first digit after the decimal point and rounds all values past the 
    fifth digit after the decimal point.  The following examples show 
    the effects of a scale greater than precision: 

Actual Data            Specified as            Stored as 
-----------            ------------            --------- 
.01234                 NUMBER(4,5)             .01234 

.00012                 NUMBER(4,5)             .00012 
.000127                NUMBER(4,5)             .00013 
.0000012               NUMBER(2,7)             .0000012 
.00000123              NUMBER(2,7)             .0000012 

FLOATING POINT NUMBERS 
    Oracle also allows you to specify floating point numbers.  A 
    floating point value either can have a decimal point anywhere from 
    the first to the last digit or can omit the decimal point 

    altogether.  A scale value is not applicable to floating point 
    numbers because there is no restriction on the number of digits that 
    can appear after the decimal point. 

    You can specify floating point numbers with the appropriate forms of 
    the NUMBER datatype discussed in the section  on page -.  Oracle 
    also supports the ANSI datatype FLOAT.  You can specify this 
    datatype using one of these syntactic forms: 


FLOAT 
    specifies a floating point number with decimal precision 38, or a 
    binary precision of 126. 

    FLOAT(b) 
            specifies a floating point number with binary precision b. 
            The precision b can range from 1 to 126. 

    To convert from binary to decimal precision, multiply b by 0.30103. 
    To convert from decimal to binary precision, multiply the decimal 
    precision by 3.32193.  The maximum of 126 digits of binary precision 

    is roughly equivalent to 38 digits of decimal precision. 

SEE: 
    DATE Datatype, LONG Datatype "))
("raw" . (nil "ORACLE SQL Datatype" "RAW Datatype

    The RAW and LONG RAW datatypes are used for byte-oriented data (for 
    example, binary data or byte strings) to store character strings, 
    floating point data, and binary data such as graphics images and 
    digitized sound.  Oracle returns RAW values as hexadecimal character 
    values.  RAW data can only be stored and retrieved.  You cannot 
    perform string manipulation on RAW data. 


    RAW is equivalent to VARCHAR2 and LONG RAW to LONG except that there 
    is no conversion between database and session character set.  While 
    CHAR, VARCHAR2, and LONG data is automatically converted between the 
    database character set and the user-side character set for the 
    session, there is no such conversion for RAW and LONG RAW data. 
    LONG RAW data is subject to the same restrictions as LONG data. 


SEE: 
    CHAR Datatype, LONG Datatype, VARCHAR2 Datatype "))
("long raw" . (nil "ORACLE SQL Datatype" "LONG RAW Datatype

    The RAW and LONG RAW datatypes are used for byte-oriented data (for 
    example, binary data or byte strings) to store character strings, 
    floating point data, and binary data such as graphics images and 
    digitized sound.  Oracle returns RAW values as hexadecimal character 
    values.  RAW data can only be stored and retrieved.  You cannot 
    perform string manipulation on RAW data. 


    RAW is equivalent to VARCHAR2 and LONG RAW to LONG except that there 
    is no conversion between database and session character set.  While 
    CHAR, VARCHAR2, and LONG data is automatically converted between the 
    database character set and the user-side character set for the 
    session, there is no such conversion for RAW and LONG RAW data. 
    LONG RAW data is subject to the same restrictions as LONG data. 


SEE: 
    CHAR Datatype, LONG Datatype, VARCHAR2 Datatype "))
("rowid" . (nil "ORACLE SQL Datatype" "ROWID Datatype 

    Each row in the database has an address.  You can examine a row's 
    address by querying the pseudocolumn ROWID.  Values of this 
    pseudocolumn are hexadecimal strings representing the address of 
    each row.  These string have the datatype ROWID.  For more 
    information on the ROWID pseudocolumn, see Pseudocolumns.  You can 
    also create tables and clusters that contain actual columns having 

    the ROWID datatype.  Oracle does not guarantee that the values of 
    such columns are valid ROWIDs. 

    Character values representing ROWIDs: 

    block.row.file 

where: 

block 
    is a hexadecimal string identifying the data block of the data file 
    containing the row.  The length of this string may vary depending on 
    your operating system. 

row 
    is a four-digit hexadecimal string identifying the row in the data 

    block.  The first row in the block has the number 0. 

file 
    is a hexadecimal string identifying the database file containing the 
    row.  The first data file has the number 1.  The length of this 
    string may vary depending on your operating system. 

EXAMPLE: 

    Consider this ROWID value: 

    0000000F.0000.0002 

    The row corresponding to this ROWID is the first row (0000) in the 

    fifteenth data block (0000000F) of the second data file (0002). 

SEE: 
    Pseudocolumns, ROWID "))
("null" . (nil "ORACLE SQL Datatype" "NULL Datatype

    (It's not a real datatype)

    If a row lacks a value for a particular column, that column is said 
    to be null, or to contain a null.  Nulls can appear in columns of 
    any datatype that are not restricted by NOT NULL or PRIMARY KEY 
    integrity constraints.  Use a null when the actual value is unknown 
    or when a value would not be meaningful. 

    Oracle currently treats a character value with a length of zero as 
    null.  However, this may not continue to be true in future versions 

    of Oracle. 

    Do not use null to represent a value of zero, because they are not 
    equivalent.  Any arithmetic expression containing a null always 
    evaluates to null.  For example, null added to 10 is null.  In fact, 
    all operators (except concatenation) return null when given a null 
    operand. 

NULLS IN SQL functions: 

    All scalar functions (except NVL and TRANSLATE) return null when 

    given a null argument.  The NVL function can be used to return a 
    value when a null occurs.  For example, the expression NVL(COMM,0) 
    returns 0 if COMM is null or the value of COMM if it is not null. 

    Most group functions ignore nulls.  For example, consider a query 
    that averages the five values 1000, null, null, null, and 2000. 
    Such a query ignores the nulls and calculates the average to be 
    (1000+2000)/2 = 1500. 


NULLS WITH COMPARISON OPERATORS: 

    To test for nulls, only use the comparison operators IS NULL and IS 
    NOT NULL.  If you use any other operator with nulls and the result 
    depends on the value of the null, the result is unknown.  Because 
    null represents a lack of data, a null cannot be equal or unequal to 
    any value or to another null.  However, note that Oracle considers 
    two nulls to be equal when evaluating a DECODE expression. 


NULLS IN CONDITIONS: 

    Oracle treats conditions evaluating to unknown values as FALSE.  For 
    example, since the condition COMM = NULL is always unknown, a SELECT 
    statement with this condition in its WHERE clause returns no rows. 
    Note that Oracle returns no error message in this case. 

    The following table summarizes results of conditions using nulls. 

    If A is:       Condition           Evaluates to: 


    10             a IS NULL           FALSE 
    10             a IS NOT NULL       TRUE 
    NULL           a IS NULL           TRUE 
    NULL           a IS NOT NULL       FALSE 
    10             a = NULL            Unknown 
    10             a != NULL           Unknown 
    NULL           a = NULL            Unknown 
    NULL           a != NULL           Unknown 

SEE: 
    Arithmetic Operators, Comparison Operators, NVL "))
("and" . (nil "SQL operator" "AND Operator 

    The following table shows the results of combining two expressions 
    with the AND operator. 

    AND        TRUE           FALSE          NULL 
    TRUE       TRUE           FALSE          NULL 
    FALSE      FALSE          FALSE          FALSE 
    NULL       NULL           FALSE          NULL 

SEE: 
    Logical Operators, OR Operator, NOT Operator "))
("not" . (nil "SQL operator" "NOT Operator 

    The following table shows the result of applying the NOT operator to 
    an expression. 

    NOT     TRUE        FALSE        NULL 
            FALSE       TRUE         NULL 

SEE: 
    Logical Operators, AND Operator, OR Operator "))
("or" . (nil "SQL operator" "OR Operator 

    The following table shows the results of combining two logical 
    expressions with the OR operator. 

    OR        TRUE            FALSE          NULL 
    TRUE      TRUE            TRUE           TRUE 
    FALSE     TRUE            FALSE          NULL 
    NULL      TRUE            NULL           NULL 

SEE: 
    Logical Operators, AND Operator, NOT Operator "))
("like" . (nil "SQL operator" "LIKE Operator 

    The LIKE operator is used in character string comparisons with 
    pattern matching.  The syntax for a condition using the LIKE 
    operator is shown in this diagram: 

SYNTAX: 

char1 [NOT] LIKE char2 [ESCAPE 'c'] 

where: 

char1 
    is a value to be compared with a pattern.  This value can have 
    datatype CHAR or VARCHAR2. 

NOT 
    logically inverts the result of the condition, returning FALSE if 

    the condition evaluates to TRUE and TRUE if it evaluates to FALSE. 

char2 
    is the pattern to which char1 is compared.  The pattern is a value 
    of datatype CHAR or VARCHAR2 and can contain the special pattern 
    matching characters % and _. 

ESCAPE 
    identifies a single character as the escape character.  The escape 
    character can be used to cause Oracle to interpret % or _ literally, 

    rather than as a special character, in the pattern. 
    If you wish to search for strings containing an escape character, you 
    must specify this character twice.  For example, if the escape 
    character is '/', to search for the string 'client/server', you must 
    specify, 'client//server'. 

    While the equal (=) operator exactly matches one character value to 
    another, the LIKE operator matches a portion of one character value 

    to another by searching the first value for the pattern specified by 
    the second. 

    With the LIKE operator, you can compare a value to a pattern rather 
    than to a constant.  The pattern can only appear after the LIKE 
    keyword.  For example, you can issue the following query to find the 
    salaries of all employees with names beginning with 'SM': 

    SELECT sal 
        FROM emp 
        WHERE ename LIKE 'SM%' 


    The following query finds the salaries of all employees with the 
    name 'SM%', since the query uses the equality operator instead of 
    the LIKE operator: 

    SELECT sal 
        FROM emp 
        WHERE ename = 'SM%' 

    The following query finds the salaries of all employees with the 
    name 'SM%'.  Oracle interprets 'SM%' as a text literal, rather than 
    as a pattern, because it precedes the LIKE operator: 


    SELECT sal 
        FROM emp 
        WHERE 'SM%' LIKE ename 

    Patterns often use special characters that Oracle matches with 
    different characters in the value: 

    * An underscore (_) in the pattern matches exactly one character (as 
      opposed to one byte in a multi-byte character set) in the value. 
    * A percent sign (%) in the pattern can match zero or more 
      characters (as opposed to bytes in a multi-byte character set) in 

      the value.  Note that the pattern '%' cannot match a null. 

    Case is significant in all conditions comparing character 
    expressions including the LIKE and equality (=) operators.  You can 
    use the UPPER() function to perform a case insensitive match, as in 
    this condition: 

    UPPER(ename) LIKE 'SM%' 

    When LIKE is used to search an indexed column for a pattern, the 
    performance benefit associated with the index is lost if the first 

    character in the pattern is % or _.  If the leading character in the 
    pattern is not % or _, there is some performance benefit to the 
    index because Oracle can restrict the comparison to rows known to 
    begin with the specified first character. 

EXAMPLE I: 

    This condition is true for all ENAME values beginning with MA: 

    ename LIKE 'MA%' 

    All of these ENAME values make the condition TRUE: 


    MARTIN, MA, MARK, MARY 

    Since case is significant, ENAME values beginning with Ma, ma, and 
    mA make the condition FALSE. 

EXAMPLE II: 

    Consider this condition: 

    ename LIKE 'SMITH_' 

    This condition is true for these ENAME values: 

    SMITHE, SMITHY, SMITHS 

    This condition is false for 'SMITH', since the special character _ 
    must match exactly one character of the ENAME value. 


THE ESCAPE OPTION: 

    You can include the actual characters % or _ in the pattern by using 
    the ESCAPE option.  The ESCAPE option identifies the escape 
    character.  If the escape character appears in the pattern before 
    the character % or _, Oracle interprets this character literally in 
    the pattern, rather than as a special pattern matching character. 

EXAMPLE III: 

    To search for any employees with the character string 'A_B' in their 

    name: 

    SELECT ename 
        FROM emp 
        WHERE ename LIKE '%A\\_B%' ESCAPE '\\' 

    The ESCAPE option identifies the backslash (\\) as the escape 
    character.  In the pattern, the escape character precedes the 
    underscore (_).  This causes Oracle to interpret the underscore 
    literally, rather than as a special pattern matching character. "))
("intersect" . (nil "SQL operator" "INTERSECT Operator 

EXAMPLE: 

    This statement combines the results with the INTERSECT operator 
    which returns only those rows returned by both queries: 

    SELECT part FROM orders_list1 
    INTERSECT 
    SELECT part FROM orders_list2 

    PART 
    ---------- 
    TAILPIPE 

SEE: 
    MINUS Operator, Set Operators, UNION ALL Operator, UNION Operator "))
("minus" . (nil "SQL operator" "MINUS Operator 

EXAMPLE: 

    This statement combines the results with the MINUS operator which 
    returns only those rows returned by the first query but not in the 
    second: 

    SELECT part FROM orders_list1 
    MINUS 
    SELECT part FROM orders_list2 

    PART 
    ---------- 
    SPARKPLUG 
    FUEL PUMP 

SEE: 
    INTERSECT Operator, Set Operators, UNION ALL Operator, UNION 

    OPERATOR "))
("union all" . (nil "SQL operator" "UNION ALL Operator 

EXAMPLE: 

    This statement combines the results with the UNION ALL operator 
    which does not eliminate duplicate selected rows: 

    SELECT part FROM orders_list1 
    UNION ALL 
    SELECT part FROM orders_list2 

    PART 
    ---------- 
    SPARKPLUG 
    FUEL PUMP 
    FUEL PUMP 
    TAILPIPE 
    CRANKSHAFT 
    TAILPIPE 

    TAILPIPE 

    Note that the UNION operator returns only distinct rows that appear 
    in either result, while the UNION ALL operator returns all rows.  A 
    PART value that appears multiple times in either or both queries 
    (such as 'FUEL PUMP') is returned only once by the UNION operator, 
    but multiple times by the UNION ALL operator. 

SEE: 
    INTERSECT Operator, MINUS Operator, Set Operators, UNION Operator "))
("union" . (nil "SQL operator" "UNION Operator 

EXAMPLE: 

    This statement combines the results with the UNION operator, which 
    eliminates duplicate selected rows: 

    SELECT part FROM orders_list1 
    UNION 
    SELECT part FROM orders_list2 

    PART 
    ---------- 
    SPARKPLUG 
    FUEL PUMP 
    TAILPIPE 
    CRANKSHAFT 

SEE: 
    INTERSECT Operator, MINUS Operator, Set Operators, UNION ALL 

    OPERATOR "))
("prior" . (nil "SQL operator" "PRIOR Operator

PRIOR   Evaluates the following expression    SELECT empno, ename, mgr 
        for the parent row of the current     FROM emp 

        row in a hierarchical, or             CONNECT BY 
        tree-structured, query.  In such a         PRIOR empno = mgr 
        query, you must use this operator 
        in the CONNECT BY clause to define 
        the relationship between parent and 
        child rows.  You can also use 
        this operator in other parts of a 
        SELECT statement that performs a 
        hierarchical query.  The PRIOR 

        operator is a unary operator and 
        has the same precedence as the 
        unary + and - arithmetic operators. "))
("abs" . (nil "Function" "ABS 

SYNTAX: 

    ABS(n) 

PURPOSE: 

    Returns the absolute value of n. 

EXAMPLE: 

    SELECT ABS(-15) \"Absolute\" 
        FROM DUAL 

    Absolute 
    -------- 
          15 

SEE: 
    SIGN "))
("ceil" . (nil "Function" "CEIL 

SYNTAX: 

    CEIL(n) 

PURPOSE: 

    Returns smallest integer greater than or equal to n. 

EXAMPLE: 

    SELECT CEIL(15.7) \"Ceiling\" 
        FROM DUAL 

    Ceiling 
    ------- 
         16 

SEE: 
    FLOOR "))
("cos" . (nil "Function" "COS 

SYNTAX: 

    COS(n) 

PURPOSE: 

    Returns the cosine of n (an angle expressed in radians). 

EXAMPLE: 

    SELECT COS(180 * 3.14159265359/180) 
    \"Cosine of 180 degrees\" 
        FROM DUAL 

    Cosine of 180 degrees 
    --------------------- 
                   -1 

SEE: 
    COSH, SIN, SINH, TAN, TANH "))
("cosh" . (nil "Function" "COSH 

SYNTAX: 

    COSH(n) 

PURPOSE: 

    Returns the hyperbolic cosine of n. 

EXAMPLE: 

    SELECT COSH(0) \"Hyperbolic cosine of 0\" 
        FROM DUAL 

    Hyperbolic cosine of 0 
    ---------------------- 
                         1 

SEE: 
    COS, SIN, SINH, TAN, TANH "))
("exp" . (nil "Function" "EXP 

SYNTAX: 

    EXP(n) 

PURPOSE: 

    Returns e raised to the nth power;  e = 2.71828183 ... 

EXAMPLE: 

    SELECT EXP(4) \"e to the 4th power\" 
        FROM DUAL 

    e to the 4th power 
    ------------------ 
          54.59815 

SEE: 
    LN, LOG, POWER, SQRT "))
("floor" . (nil "Function" "FLOOR 

SYNTAX: 

    FLOOR(n) 

PURPOSE: 

    Returns largest integer equal to or less than n. 

EXAMPLE: 

    SELECT FLOOR(15.7) \"Floor\" 
        FROM DUAL 

    Floor 
    ----- 
       15 

SEE: 
    CEIL "))
("ln" . (nil "Function" "LN 

SYNTAX: 

    LN(n) 

PURPOSE: 

    Returns the natural logarithm of n, where n is greater than 0. 

EXAMPLE: 

    SELECT LN(95) \"Natural log of 95\" 
        FROM DUAL 

    Natural log of 95 
    ----------------- 
       4.55387689 

SEE: 
    EXP, LOG, POWER, SQRT "))
("log" . (nil "Function" "LOG 

SYNTAX: 

    LOG(m,n) 

PURPOSE: 

    Returns the logarithm, base m, of n.  The base m can be any positive 
    number other than 0 or 1 and n can be any positive number. 

EXAMPLE: 

    SELECT LOG(10,100) \"Log base 10 of 100\" 
        FROM DUAL 

    Log base 10 of 100 
    ------------------ 
                     2 

SEE: 
    EXP, LN, POWER, SQRT "))
("mod" . (nil "Function" "MOD 

SYNTAX: 

    MOD(m,n) 

PURPOSE: 

    Returns remainder of m divided by n.  Returns m if n is 0. 

EXAMPLE: 

    SELECT MOD(11,4) \"Modulus\" 
        FROM DUAL 

    Modulus 
    ------- 
      3 

Note: 
    This function behaves differently from the classical mathematical 
    modulus function when m is negative.  The classical modulus can be 

    expressed in terms of the MOD function with this formula: 

    m - n * FLOOR(m/n) 

EXAMPLE: 

    This statement illustrates the difference between the MOD function 
    and the classical modulus: 

    SELECT m, n, MOD(m,n), 
    m - n * FLOOR(m/n) \"Classical Modulus\" 
        FROM test_mod_table 

     M    N  MOD(M,N) Classical Modulus 
    --- ---- -------- ----------------- 

     11    4        3                 3 
    -11    4       -3                 1 
     11   -4        3                -1 
    -11   -4       -3                -3 

SEE: 
    ROUND, TRUNC "))
("power" . (nil "Function" "POWER 

SYNTAX: 

    POWER(m,n) 

PURPOSE: 

    Returns m raised to the nth power.  The base m and the exponent n 
    can be any numbers, but if m is negative, n must be an integer. 

EXAMPLE: 

    SELECT POWER(3,2) \"Raised\" 
        FROM DUAL 

    Raised 
    ------ 
         9 

SEE: 
    EXP, LN, LOG, SQRT "))
("round (number" . (nil "Function" "ROUND (NUMBER) 

SYNTAX: 

    ROUND(n[,m]) 

PURPOSE: 

    Returns n rounded to m places right of the decimal point; if m is 
    omitted, to 0 places. m can be negative to round off digits left of 
    the decimal point. m must be an integer. 

EXAMPLES: 

    SELECT ROUND(15.193,1) \"Round\" 
        FROM DUAL 

    Round 
    ----- 
    15.2 

    SELECT ROUND(15.193,-1) \"Round\" 

        FROM DUAL 

    Round 
    ----- 
    20 

SEE: 
    MOD, TRUNC "))
("sign" . (nil "Function" "SIGN 

SYNTAX: 

    SIGN(n) 

PURPOSE: 

    If n<0, the function returns -1; if n=0, the function returns 0; if 
    n>0, the function returns 1. 

EXAMPLE: 

    SELECT SIGN(-15) \"Sign\" 
        FROM DUAL 

    Sign 
    ---- 
      -1 

SEE: 
    ABS "))
("sin" . (nil "Function" "SIN 

SYNTAX: 

    SIN(n) 

PURPOSE: 

    Returns the sine of n (an angle expressed in radians). 

EXAMPLE: 

    SELECT SIN(30 * 3.14159265359/180) 
    \"Sine of 30 degrees\" 
        FROM DUAL 

    Sine of 30 degrees 
    ------------------ 
                    .5 

SEE: 
    COS, COSH, SINH, TAN, TANH "))
("sinh" . (nil "Function" "SINH 

SYNTAX: 

    SINH(n) 

PURPOSE: 

    Returns the hyperbolic sine of n. 

EXAMPLE: 

    SELECT SINH(1) \"Hyperbolic sine of 1\" 
        FROM DUAL 

    Hyperbolic sine of 1 
    -------------------- 
              1.17520119 

SEE: 
    COS, COSH, SIN, TAN, TANH "))
("sqrt" . (nil "Function" "SQRT 

SYNTAX: 

    SQRT(n) 

PURPOSE: 

    Returns square root of n.  The value n cannot be negative.  SQRT 
    returns a \"real\" result. 

EXAMPLE: 

    SELECT SQRT(26) \"Square root\" 
        FROM DUAL 

    Square root 
    ----------- 
     5.09901951 

SEE: 
    EXP, LOG, LN, POWER "))
("tan" . (nil "Function" "TAN 

SYNTAX: 

    TAN(n) 

PURPOSE: 

    Returns the tangent of n (an angle expressed in radians). 

EXAMPLE: 

    SELECT TAN(135 * 3.14159265359/180) 
    \"Tangent of 135 degrees\" 
        FROM DUAL 

    Tangent of 135 degrees 
    ---------------------- 
                        -1 

SEE: 
    COS, COSH, SIN, SINH, TANH "))
("tanh" . (nil "Function" "TANH 

SYNTAX: 

    TANH(n) 

PURPOSE: 

    Returns the hyperbolic tangent of n. 

EXAMPLE: 

    SELECT TANH(.5) \"Hyperbolic tangent of .5\" 
        FROM DUAL 

    Hyperbolic tangent of .5 
    ------------------------ 
                  .462117157 

SEE: 
    COS, COSH, SIN, SINH, TAN "))
("trunc (number" . (nil "Function" "TRUNC (NUMBER) 

SYNTAX: 

    TRUNC(n[,m]) 

PURPOSE: 

    Returns n truncated to m decimal places; if m is omitted, to 0 
    places.  m can be negative to truncate (make zero) m digits left of 
    the decimal point. 

EXAMPLES: 

    SELECT TRUNC(15.79,1) \"Truncate\" 
        FROM DUAL 

    Truncate 
    -------- 
        15.7 

    SELECT TRUNC(15.79,-1) \"Truncate\" 

        FROM DUAL 

    Truncate 
    -------- 
          10 

SEE: 
    MOD, ROUND "))
("chr" . (nil "Function" "CHR 

SYNTAX: 

    CHR(n) 

PURPOSE: 

    Returns the character having the binary equivalent to n in the 
    database character set. 

EXAMPLE: 

    SELECT CHR(75) \"Character\" 
        FROM DUAL 

    Character 
    --------- 
    K 

SEE: 
    ASCII "))
("concat" . (nil "Function" "CONCAT 

SYNTAX: 

    CONCAT(char1, char2) 

PURPOSE: 

    Returns char1 concatenated with char2.  This function is 
    equivalent to the concatenation operator (||). 

EXAMPLE: 

    This example uses nesting to concatenate three character strings: 

    SELECT CONCAT( CONCAT(ename, ' is a '), job) \"Job\" 
        FROM emp 
        WHERE empno = 7900 

       Job 

    ------------------------- 
    JAMES is a CLERK "))
("initcap" . (nil "Function" "INITCAP 

SYNTAX: 

    INITCAP(char) 

PURPOSE: 

    Returns char, with the first letter of each word in uppercase, all 
    other letters in lowercase.  Words are delimited by white space or 
    characters that are not alphanumeric. 

EXAMPLE: 

    SELECT INITCAP('the soap') \"Capitalized\" 
        FROM DUAL 

    Capitalized 
    -------------------- 
    The Soap 


SEE: 
    LOWER, NLS_INITCAP, NLS_LOWER, NLS_UPPER, UPPER "))
("lower" . (nil "Function" "LOWER 

SYNTAX: 

    LOWER(char) 

PURPOSE: 

    Returns char, with all letters lowercase.  The return value has the 
    same datatype as the argument char (CHAR or VARCHAR2). 

EXAMPLE: 

    SELECT LOWER('MR. SAMUEL HILLHOUSE') \"Lowercase\" 
        FROM DUAL 

    Lowercase 
    -------------------- 
    mr. samuel hillhouse 

SEE: 
    INITCAP, NLS_INITCAP, NLS_LOWER, NLS_UPPER, UPPER "))
("lpad" . (nil "Function" "LPAD 

SYNTAX: 

    LPAD(char1,n [,char2]) 

PURPOSE: 

    Returns char1, left-padded to length n with the sequence of 
    characters in char2; char2 defaults to ' ', a single blank.  If 
    char1 is longer than n, this function returns the portion of char1 
    that fits in n. 

    The argument n is the total length of the return value as it is 
    displayed on your terminal screen.  In most character sets, this is 

    also the number of characters in the return value.  However, in some 
    multi-byte character sets, the display length of a character string 
    can differ from the number of characters in the string. 

EXAMPLE: 

    SELECT LPAD('Page 1',15,'*.') \"LPAD example\" 
        FROM DUAL 

    LPAD example 
    --------------- 
    *.*.*.*.*Page 1 

SEE: 
    LTRIM, RPAD, RTRIM "))
("ltrim" . (nil "Function" "LTRIM 

SYNTAX: 

    LTRIM(char[,set]) 

PURPOSE: 

    Removes characters from the left of char, with initial characters 
    removed up to the first character not in set; set defaults to ' ', a 
    single blank. 

EXAMPLE: 

    SELECT LTRIM('xyxXxyLAST WORD','xy') 
    \"Left trim example\" 
        FROM DUAL 

    Left trim example 
    ----------------- 

    XxyLAST WORD 

SEE: 
    LPAD, RPAD, RTRIM "))
("nls_initcap" . (nil "Function" "NLS_INITCAP 

SYNTAX: 

    NLS_INITCAP(char [, 'nlsparams'] ) 

PURPOSE: 

    Returns char, with the first letter of each word in uppercase, all 
    other letters in lowercase.  Words are delimited by white space or 
    characters that are not alphanumeric.  The value of 'nlsparams' can 
    have this form: 

'NLS_SORT = sort' 

    where sort is either a linguistic sort sequence or BINARY.  The 

    linguistic sort sequence handles special linguistic requirements for 
    case conversions.  Note that these requirements can result in a 
    return value of a different length than the char.  If you omit 
    'nlsparams', this function uses the default sort sequence for your 
    session. 

EXAMPLE: 

    SELECT NLS_INITCAP('ijsland', 'NLS_SORT = XDutch') \"Capitalized\" 
        FROM DUAL 

    Capitalized 

    -------------------- 
    IJsland 

SEE: 
    INITCAP, LOWER, NLS_LOWER, NLS_UPPER, UPPER "))
("nls_lower" . (nil "Function" "NLS_LOWER 

SYNTAX: 

    NLS_LOWER(char [, 'nlsparams'] ) 

PURPOSE: 

    Returns char, with all letters lowercase.  The 'nlsparams' can have 
    the same form and serve the same purpose as in the NLS_INITCAP 
    function. 

EXAMPLE: 

    SELECT NLS_LOWER('CITTA''', 'NLS_SORT = XItalian') \"Lowercase\" 
        FROM DUAL 

    Lowercase 
    ----------------- 

    citta 

SEE: 
    INITCAP, LOWER, NLS_INITCAP, NLS_UPPER, UPPER "))
("nls_upper" . (nil "Function" "NLS_UPPER 

SYNTAX: 

    NLS_UPPER(char [, 'nlsparams'] ) 

PURPOSE: 

    Returns char, with all letters uppercase.  The 'nlsparams' can have 
    the same form and serve the same purpose as in the NLS_INITCAP 
    function. 

EXAMPLE: 

    SELECT NLS_UPPER('grob', 'NLS_SORT = XGerman') \"Uppercase\" 
        FROM DUAL 

    Uppercase 
    --------- 
    GROSS 


SEE: 
    INITCAP, LOWER, NLS_INITCAP, NLS_LOWER, UPPER "))
("replace" . (nil "Function" "REPLACE 

SYNTAX: 

    REPLACE(char, search_string [,replacement_string]) 

PURPOSE: 

    Returns char with every occurrence of search_string replaced with 
    replacement_string.  If replacement_string is omitted or null, all 
    occurrences of search_string are removed.  If search_string is null, 
    char is returned.  This function provides a superset of the 
    functionality provided by the TRANSLATE function.  TRANSLATE 

    provides single character, one to one, substitution.  REPLACE allows 
    you to substitute one string for another as well as to remove 
    character strings. 

EXAMPLE: 

    SELECT REPLACE('JACK and JUE','J','BL') \"Changes\" 
        FROM DUAL 

    Changes 
    ------------ 
    BLACK and BLUE 

SEE: 
    SOUNDEX, SUBSTR, SUBSTRB, TRANSLATE "))
("rpad" . (nil "Function" "RPAD 

SYNTAX: 

    RPAD(char1, n [,char2]) 

PURPOSE: 

    Returns char1, right-padded to length n with char2, replicated as 
    any times as necessary; char2 defaults to ' ', a single blank.  If 
    char1 is longer than n, this function returns the portion of char1 
    that fits in n. 

    The argument n is the total length of the return value as it is 
    displayed on your terminal screen.  In most character sets, this is 

    also the number of characters in the return value.  However, in some 
    multi-byte character sets, the display length of a character string 
    can differ from the number of characters in the string. 

EXAMPLE: 

    SELECT RPAD(ename,11,'ab') \"RPAD example\" 
        FROM emp 
        WHERE ename = 'TURNER' 

    RPAD example 
    ------------ 
    TURNERababa 

SEE: 

    LPAD, LTRIM, RTRIM "))
("rtrim" . (nil "Function" "RTRIM 

SYNTAX: 

    RTRIM(char [,set]) 

PURPOSE: 

    Returns char, with final characters removed after the last character 
    not in set; set defaults to ' ', a single blank. 

EXAMPLE: 

    SELECT RTRIM('TURNERyxXxy','xy') 
    \"Right trim example\" 
    FROM DUAL 

    Right trim example 
    ------------------ 
    TURNERyxX 

SEE: 
    LPAD, LTRIM, RPAD "))
("soundex" . (nil "Function" "SOUNDEX 

SYNTAX: 

    SOUNDEX(char) 

PURPOSE: 

    Returns a character string containing the phonetic representation of 
    char.  This function allows you to compare words that are spelled 
    differently, but sound alike in English. 

    The phonetic representation is defined in The Art of Computer 
    Programming, Volume 3: Sorting and Searching, by Donald E. Knuth. 

EXAMPLE: 


    SELECT ename 
        FROM emp 
        WHERE SOUNDEX(ename) = 
              SOUNDEX('SMYTHE') 

    ENAME 
    ---------- 
    SMITH 

SEE: 
    REPLACE, SUBSTR, SUBSTRB, TRANSLATE "))
("substr" . (nil "Function" "SUBSTR 

SYNTAX: 

    SUBSTR(char,m [,n]) 

PURPOSE: 

    Returns a portion of char, beginning at character m, n characters 
    long.  If m is positive, Oracle counts from the beginning of char to 
    find the first character.  If m is negative, Oracle counts backwards 
    from the end of char.  The value m cannot be 0.  If n is omitted, 
    Oracle returns all characters to the end of char.  The value n 

    cannot be less than 1. 

EXAMPLES: 

    SELECT SUBSTR('ABCDEFG',3,2) \"Substring\" 
        FROM DUAL 

    Substring 
    ---------- 
    CD 

    SELECT SUBSTR('ABCDEFG',-3,2) \"Reversed Substring\" 
        FROM DUAL 

    Reversed Substring 
    ------------------ 
    EF 

SEE: 
    REPLACE, SOUNDEX, SUBSTRB, TRANSLATE "))
("substrb" . (nil "Function" "SUBSTRB 

SYNTAX: 

    SUBSTRB(char,m [,n]) 

PURPOSE: 

    The same as SUBSTR, except that the arguments m and n are expressed 
    in bytes, rather than in characters.  For a single-byte database 
    character set, SUBSTRB is equivalent to SUBSTR. 

EXAMPLE: 

    Assume a double-byte database character set: 

    SELECT SUBSTRB('ABCDEFG',5,4) \"Substring with bytes\" 
        FROM DUAL 


    Substring with bytes 
    -------------------- 
    CD 

SEE: 
    REPLACE, SOUNDEX, SUBSTR, TRANSLATE "))
("translate" . (nil "Function" "TRANSLATE 

SYNTAX: 

    TRANSLATE(char,from,to) 

PURPOSE: 

    Returns char with all occurrences of each character in from replaced 
    by its corresponding character in to.  Characters in char that are 
    not in from are not replaced.  The argument from can contain more 
    characters than to.  In this case, the extra characters at the end 
    of from have no corresponding characters in to.  If these extra 

    characters appear in char, they are removed from the return value. 
    You cannot use empty string for to in order to remove all characters 
    in from the return value.  Oracle interprets the empty string 
    as null, and if this function has a null argument, it returns null. 

EXAMPLES: 

    This statement translates a license number.  All letters 'ABC...Z' 
    are translated to 'X' and all digits '012...9' are translated to 

    '9': 

    SELECT TRANSLATE('2KRW229', '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ', 
                     '9999999999XXXXXXXXXXXXXXXXXXXXXXXXXX') 
    \"Translate example\" 
        FROM DUAL 

    Translate example 
    ----------------- 
    9XXX999 

    This statement returns a license number with the characters removed 
    and the digits remaining: 

    SELECT TRANSLATE('2KRW229', '0123456789ABCDEFGHIJKLMNOPQRSTUVWXYZ', 

                     '0123456789') 
    \"Translate example\" 
        FROM DUAL 

    Translate example 
    ----------------- 
    2229 

SEE: 
    REPLACE, SOUNDEX, SUBSTR, SUBSTRB "))
("upper" . (nil "Function" "UPPER 

SYNTAX: 

    UPPER(char) 

PURPOSE: 

    Returns char, with all letters uppercase.  The return value 
    has the same datatype as the argument char. 

EXAMPLE: 

    SELECT UPPER('Large') \"Uppercase\" 
        FROM DUAL 

    Uppercase 
    --------- 
    LARGE 

SEE: 
    INITCAP, LOWER, NLS_INITCAP, NLS_LOWER, NLS_UPPER "))
("ascii" . (nil "Function" "ASCII 

SYNTAX: 

    ASCII(char) 

PURPOSE: 

    Returns the decimal representation in the database character set of 
    the first byte of char.  If your database character set is 7-bit 
    ASCII, this function returns an ASCII value.  If your database 
    character set is EBCDIC Code Page 500, this function returns an 
    EBCDIC value.  Note that there is not a similar EBCDIC character 

    function. 

EXAMPLE: 

    SELECT ASCII('Q') 
        FROM DUAL 

    ASCII('Q') 
    ---------- 
            81 

SEE: 
    CHR "))
("instr" . (nil "Function" "INSTR 

SYNTAX: 

    INSTR(char1,char2[,n[,m]]) 

PURPOSE: 

    Searches char1 beginning with its nth character for the mth 
    occurrence of char2 and returns the position of the character in 
    char1 that is the first character of this occurrence.  If n is 
    negative, Oracle counts and searches backward from the end of char1. 
    The value of m must be positive.  The default values of both n and m 

    are 1, meaning Oracle begins searching at the first character of 
    char1 for the first occurrence of char2.  The return value is 
    relative to the beginning of char1, regardless of the value of n, 
    and is expressed in characters.  If the search is unsuccessful (if 
    char2 does not appear m times after the nth character of char1) the 
    return value is 0. 

EXAMPLES: 

    SELECT INSTR('CORPORATE FLOOR','OR', 3, 2) \"Instring\" 

        FROM DUAL 

    Instring 
    --------- 
           14 

    SELECT INSTR('CORPORATE FLOOR','OR', -3, 2) 
    \"Reversed Instring\" 
        FROM DUAL 

    Reversed Instring 
    ----------------- 
                    2 

SEE: 
    INSTRB, LENGTH, LENGTHB, NLSSORT, SUBSTR, SUBSTRB "))
("instrb" . (nil "Function" "INSTRB 

SYNTAX: 

    INSTRB(char1,char2[,n[,m]]) 

PURPOSE: 

    The same as INSTR, except that n and the return value are expressed 
    in bytes, rather than in characters.  For a single-byte database 
    character set, INSTRB is equivalent to INSTR. 

EXAMPLE: 

    Assume a double-byte database character set: 

    SELECT INSTRB('CORPORATE FLOOR','OR',5,2) 
    \"Instring in bytes\" 

        FROM DUAL 

    Instring in bytes 
    ----------------- 
                   27 

SEE: 
    INSTR, LENGTH, LENGTHB, NLSSORT, SUBSTR, SUBSTRB "))
("length" . (nil "Function" "LENGTH 

SYNTAX: 

    LENGTH(char) 

PURPOSE: 

    Returns the length of char in characters.  If char has datatype 
    CHAR, the length includes all trailing blanks.  If char is null, 
    this function returns null. 

EXAMPLE: 

    SELECT LENGTH('CANDIDE') \"Length in characters\" 
        FROM DUAL 

    Length in characters 
    -------------------- 
                       7 


SEE: 
    INSTR, INSTRB, LENGTHB, NLSSORT, SUBSTR, SUBSTRB "))
("lengthb" . (nil "Function" "LENGTHB 

SYNTAX: 

    LENGTHB(char) 

PURPOSE: 

    Returns the length of char in bytes.  If char is null, this function 
    returns null.  For a single-byte database character set, LENGTHB is 
    equivalent to LENGTH. 

EXAMPLE: 

    Assume a double-byte database character set: 

    SELECT LENGTH('CANDIDE') \"Length in bytes\" 
        FROM DUAL 

       Length in bytes 

       --------------- 
                    14 

SEE: 
    INSTR, INSTRB, LENGTH, NLSSORT, SUBSTR, SUBSTRB "))
("nlssort" . (nil "Function" "NLSSORT 

SYNTAX: 

    NLSSORT(char [, 'nlsparams']) 

PURPOSE: 

    Returns the string of bytes used to sort char.  The value of 
    'nlsparams' can have the form 

'NLS_SORT = sort' 

    where sort is a linguistic sort sequence or BINARY.  If you omit 
    'nlsparams', this function uses the default sort sequence for your 
    session.  If you specify BINARY, this function returns char. 


EXAMPLE: 

    This function can be used to specify comparisons based on a 
    linguistic sort sequence rather on the binary value of a string: 

    SELECT * FROM emp 
    WHERE NLSSORT(ename, 'NLS_SORT = German') > 
          NLSSORT('B',   'NLS_SORT = German') 

SEE: 
    INSTR, INSTRB, LENGTH, LENGTHB, SUBSTR, SUBSTRB "))
("add_months" . (nil "Function" "ADD_MONTHS 

SYNTAX: 

    ADD_MONTHS(d,n) 

PURPOSE: 

    Returns the date d plus n months.  The argument n can be any 
    integer.  If d is the last day of the month or if the resulting 
    month has fewer days than the day component of d, then the result is 
    the last day of the resulting month.  Otherwise, the result has the 
    same day component as d. 

EXAMPLE: 

    SELECT TO_CHAR(ADD_MONTHS(hiredate,1), 

               'DD-MON-YYYY') \"Next month\" 
        FROM emp 
        WHERE ename = 'SMITH' 

    Next month 
    ----------- 
    17-JAN-1981 

SEE: 
    MONTHS_BETWEEN "))
("last_day" . (nil "Function" "LAST_DAY 

SYNTAX: 

    LAST_DAY(d) 

PURPOSE: 

    Returns the date of the last day of the month that contains d. You 
    might use this function to determine how many days are left in the 
    current month. 

EXAMPLES: 

    SELECT SYSDATE, LAST_DAY(SYSDATE) \"Last\", 
    LAST_DAY(SYSDATE) - SYSDATE \"Days Left\" 
        FROM DUAL 

    SYSDATE    Last       Days Left 

    ---------  ---------  --------- 
    18-NOV-92  30-NOV-92         12 

    SELECT TO_CHAR(ADD_MONTHS(LAST_DAY(hiredate),5), 
    'DD-MON-YYYY') \"Five months\" 
    FROM emp 
        WHERE ename = 'MARTIN' 

    Five months 
    ----------- 
    28-FEB-1992 

SEE: 
    NEXT_DAY "))
("months_between" . (nil "Function" "MONTHS_BETWEEN 

SYNTAX: 

    MONTHS_BETWEEN(d1,d2) 

PURPOSE: 

    Returns number of months between dates d1 and d2.  If d1 is later 
    than d2, result is positive; if earlier, negative.  If d1 and d2 are 
    either the same days of the month or both last days of months, the 
    result is always an integer; otherwise Oracle calculates the 
    fractional portion of the result based on a 31-day month and also 

    considers the difference in time components of d1 and d2. 

EXAMPLE: 

    SELECT MONTHS_BETWEEN(TO_DATE('02-02-1992', 'MM-DD-YYYY'), 
                      TO_DATE('01-01-1992', 'MM-DD-YYYY')) 
    \"Months\" 
         FROM DUAL 

        Months 
    ---------- 
    1.03225806 

SEE: 
    ADD_MONTHS "))
("new_time" . (nil "Function" "NEW_TIME 

SYNTAX: 

    NEW_TIME(d,z1,z2) 

PURPOSE: 

    Returns the date and time in time zone z2 when date and time in time 
    zone z1 are d.  The arguments z1 and z2 can be any of these text 
    strings: 

    'AST' or 'ADT'        Atlantic Standard or Daylight Time 
    'BST' or 'BDT'        Bering Standard or Daylight Time 
    'CST' or 'CDT'        Central Standard or Daylight Time 

    'EST' or 'EDT'        Eastern Standard or Daylight Time 
    'GMT'                 Greenwich Mean Time 
    'HST' or 'HDT'        Alaska-Hawaii Standard Time or Daylight Time. 
    'MST' or 'MDT'        Mountain Standard or Daylight Time 
    'NST'                 Newfoundland Standard Time 
    'PST' or 'PDT'        Pacific Standard or Daylight Time 
    'YST' or 'YDT'        Yukon Standard or Daylight Time "))
("next_day" . (nil "Function" "NEXT_DAY 

SYNTAX: 

    NEXT_DAY(d,char) 

PURPOSE: 

    Returns the date of the first weekday named by char that is later 
    than the date d.  The argument char must be a day of the week in 
    your session's date language.  The return value has the same hours, 
    minutes, and seconds component as the argument d. 

EXAMPLE: 

    This example returns the date of the next Tuesday after March 15, 

    1992. 

    SELECT NEXT_DAY('15-MAR-92','TUESDAY') \"NEXT DAY\" 
    FROM DUAL 

    NEXT DAY 
    --------- 
    17-MAR-92 

SEE: 
    LAST_DAY "))
("round (number" . (nil "Function" "ROUND (NUMBER) 

SYNTAX: 

    ROUND(n[,m]) 

PURPOSE: 

    Returns n rounded to m places right of the decimal point; if m is 
    omitted, to 0 places. m can be negative to round off digits left of 
    the decimal point. m must be an integer. 

EXAMPLES: 

    SELECT ROUND(15.193,1) \"Round\" 
        FROM DUAL 

    Round 
    ----- 
    15.2 

    SELECT ROUND(15.193,-1) \"Round\" 

        FROM DUAL 

    Round 
    ----- 
    20 

SEE: 
    MOD, TRUNC "))
("sysdate" . (nil "Function" "SYSDATE 

SYNTAX: 

    SYSDATE 

PURPOSE: 

    Returns the current date and time.  Requires no arguments.  In 
    distributed SQL statements, this function returns the date and time 
    on your local database.  You cannot use this function in the 
    condition of a CHECK constraint. 

EXAMPLE: 

    SELECT TO_CHAR(SYSDATE, 'MM-DD-YYYY HH24:MI:SS') NOW 
       FROM DUAL 


    NOW 
    ------------------- 
    10-29-1993 20:27:11 

SEE: 
    Pseudocolumns "))
("trunc (number" . (nil "Function" "TRUNC (NUMBER) 

SYNTAX: 

    TRUNC(n[,m]) 

PURPOSE: 

    Returns n truncated to m decimal places; if m is omitted, to 0 
    places.  m can be negative to truncate (make zero) m digits left of 
    the decimal point. 

EXAMPLES: 

    SELECT TRUNC(15.79,1) \"Truncate\" 
        FROM DUAL 

    Truncate 
    -------- 
        15.7 

    SELECT TRUNC(15.79,-1) \"Truncate\" 

        FROM DUAL 

    Truncate 
    -------- 
          10 

SEE: 
    MOD, ROUND "))
("round and trunc" . (nil "Function" "ROUND and TRUNC 

    The following table lists the format models to be used with the 
    ROUND and TRUNC date functions and the units to which they round and 
    truncate dates.  The default model, 'DD', returns the date rounded 
    or truncated to the day with a time of midnight. 

    The starting day of the week used by the format models DAY, DY, and 
    D is specified implicitly by the initialization parameter 

    NLS_TERRITORY. 

Format Model          Rounding or Truncating Unit 
------------          --------------------------- 

CC, SCC,              Century 

SYYYY, YYYY           Year (rounds up on July 1) 
YEAR, SYEAR 
YYY, YY, Y 

Format Model          Rounding or Truncating Unit 
------------          --------------------------- 
IYYY, IYY, IY, I,     ISO Year 

Q                     Quarter (rounds up on the sixteenth day of the 

                      second month of the quarter) 

MONTH, MON, MM, RM    Month (rounds up on the sixteenth day) 

WW                    Same day of the week as the first day of the year 

IW                    Same day of the week as the first day of the ISO 
                      year 

W                     Same day of the week as the first day of the month 

DDD, DD, J            Day 

DAY, DY, D            Starting day of the week 


Format Model          Rounding or Truncating Unit 
------------          --------------------------- 
HH, HH12, HH24        Hour 

MI                    Minute 

SEE: 
    ROUND, TRUNC "))
("dump" . (nil "Function" "DUMP 

SYNTAX: 

    DUMP(expr [,return_format 
        [, start_position [, length]] ] ) 

PURPOSE: 

    Returns a VARCHAR2 value containing the datatype code, length in 
    bytes, and internal representation of expr.  The argument 
    return_format specifies the format of the return value and can have 
    any of these values: 

            8      returns result in octal notation. 

            10     returns result in decimal notation. 
            16     returns result in hexadecimal notation. 
            17     returns result as single characters. 

    The arguments start_position and length combine to determine which 
    portion of the internal representation to return.  The default is to 
    return the entire internal representation in decimal notation. 

    If expr is null, this function returns 'NULL'. 


EXAMPLES: 

    SELECT DUMP(ename, 8, 3, 2) \"OCTAL\" 
        FROM emp 
        WHERE ename = 'SCOTT' 

    OCTAL 
    --------------------------------- 
    Type=1 Len=5: 117,124 

    SELECT DUMP(ename, 10, 3, 2) \"ASCII\" FROM emp 
         WHERE ename = 'SCOTT' 

    ASCII 
    ---------------------------- 
    Type=1 Len=5: 79,84 

    SELECT DUMP(ename, 16, 3, 2) \"HEX\" FROM emp 

        WHERE ename = 'SCOTT' 

    HEX 
    ---------------------------- 
    Type=1 Len=5: 4f,54 

    SELECT DUMP(ename, 17, 3, 2) \"CHAR\" FROM emp 
        WHERE ename = 'SCOTT' 

    CHAR 
    ----------------------- 
    Type=1 Len=5: O,T 

SEE: 
    VARCHAR2 Datatype "))
("greatest" . (nil "Function" "GREATEST 

SYNTAX: 

    GREATEST(expr [,expr] ...) 

PURPOSE: 

    Returns the greatest of the list of exprs.  All exprs after the 
    first are implicitly converted to the datatype of the first prior to 
    the comparison.  Oracle compares the exprs using non-padded 
    comparison semantics.  Character comparison is based on the value of 
    the character in the database character set.  One character is 

    greater than another if it has a higher value.  If the value 
    returned by this function is character data, its datatype is always 
    VARCHAR2. 

EXAMPLE: 

    SELECT GREATEST('HARRY','HARRIOT','HAROLD') \"GREATEST\" 
        FROM DUAL 

    GREATEST 
    -------- 
    HARRY 

SEE: 
    GREATEST_LB, LEAST, LEAST_UB "))
("greatest_lb" . (nil "Function" "GREATEST_LB 

SYNTAX: 

    GREATEST_LB(label [,label] ...) 

PURPOSE: 

    Returns the greatest lower bound of the list of labels.  Each label 
    must either have datatype MLSLABEL or RAW MLSLABEL or be a quoted 
    literal in the default label format.  The return value has datatype 
    RAW MLSLABEL. 

SEE: 
    GREATEST, LEAST, LEAST_UB "))
("least" . (nil "Function" "LEAST 

SYNTAX: 

    LEAST(expr [,expr] ...) 

PURPOSE: 

    Returns the least of the list of exprs.  All exprs after the first 
    are implicitly converted to the datatype of the first prior to the 
    comparison.  Oracle compares the exprs using non-padded comparison 
    semantics.  If the value returned by this function is character 
    data, its datatype is always VARCHAR2. 

EXAMPLE: 


    SELECT LEAST('HARRY','HARRIOT','HAROLD') 
    \"LEAST\" 
        FROM DUAL 

    LEAST 
    ------ 
    HAROLD 

SEE: 
    GREATEST, GREATEST_LB, LEAST_UB "))
("least_ub" . (nil "Function" "LEAST_UB 

SYNTAX: 

    LEAST_UB(label [,label] ...) 

PURPOSE: 

    Returns the least upper bound of the list of labels. Each label must 
    have datatype MLSLABEL or be a quoted literal in the default label 
    format.  The return value has datatype RAW MLSLABEL. 

SEE: 
    GREATEST, GREATEST_LB, LEAST "))
("nvl" . (nil "Function" "NVL 

SYNTAX: 

    NVL(expr1, expr2) 

PURPOSE: 

    If expr1 is null, returns expr2; if expr1 is not null, returns 
    expr1.  The arguments expr1 and expr2 can have any datatype.  If 
    their datatypes are different, Oracle converts expr2 to the datatype 
    of expr1 before comparing them.  The datatype of the return value is 
    always the same as the datatype of expr1, unless expr1 is character 

    data in which case the return value's datatype is VARCHAR2. 

EXAMPLE: 

    SELECT ename, 
    NVL(TO_CHAR(COMM),'NOT APPLICABLE') \"COMMISSION\" 
        FROM emp 
        WHERE deptno = 30 


    ENAME     COMMISSION 
    --------- ----------- 
    ALLEN     300 
    WARD      500 
    MARTIN    1400 
    BLAKE     NOT APPLICABLE 
    TURNER    0 
    JAMES     NOT APPLICABLE 


SEE: 
    Nulls "))
("uid" . (nil "Function" "UID 

SYNTAX: 

    UID 

PURPOSE: 

    Returns an integer that uniquely identifies the current user. 

SEE: 
    USER "))
("user" . (nil "Function" "USER 

SYNTAX: 

    USER 

PURPOSE: 

    Returns the current Oracle user with the datatype VARCHAR2. 

    In a distributed SQL statement, the UID and USER functions identify 
    the user on your local database.  You cannot use these functions in 
    the condition of a CHECK constraint. 

EXAMPLE: 

    SELECT USER, UID 
        FROM DUAL 

    USER          UID 

    --------- ------- 
    OPS$KING        9 

SEE: 
    UID "))
("userenv" . (nil "Function" "USERENV 

SYNTAX: 

    USERENV(option) 

PURPOSE: 

    Returns information of VARCHAR2 datatype about the current session. 
    This information can be useful for writing an application-specific 
    audit trail table or for determining the language-specific 
    characters currently used by your session.  You cannot use this 
    function in the condition of a CHECK constraint.  The argument 

    option can have any of these values: 

            'ENTRYID' 
                   returns available auditing entry identifier.  You 
                   cannot use this option in distributed SQL statements. 
            'LABEL' 
                   returns your current session label.  This option is 
                   only applicable for Trusted Oracle. 
            'LANGUAGE' 
                   returns the language and territory currently used by 

                   your session along with the database character set in 
                   this form: 

                   language_territory.characterset 

            'SESSIONID' 
                   returns your auditing session identifier.  You cannot 
                   use this option in distributed SQL statements. 
            'TERMINAL' 
                   returns the operating system identifier for your 

                   current session's terminal.  In distributed SQL 
                   statements, this option returns the identifier for 
                   your local session. 

EXAMPLE: 

    SELECT USERENV('LANGUAGE') \"Language\" 
        FROM DUAL; 

    Language 
    -------------------------- 
    AMERICAN_AMERICA.US7ASCII "))
("vsize" . (nil "Function" "VSIZE 

SYNTAX: 

    VSIZE(expr) 

PURPOSE: 

    Returns the number of bytes in the internal representation of expr. 
    If expr is null, this function returns null. 

EXAMPLE: 

    SELECT ename, VSIZE(ename) \"BYTES\" 
        FROM emp 
        WHERE deptno = 10 

    ENAME          BYTES 
    ---------- --------- 
    CLARK              5 
    KING               4 

    MILLER             6 "))
("chartorowid" . (nil "Function" "CHARTOROWID 

SYNTAX: 

    CHARTOROWID(char) 

PURPOSE: 

    Converts a value from CHAR or VARCHAR2 datatype to ROWID datatype. 

EXAMPLE: 

    SELECT ename 
        FROM emp 
        WHERE ROWID = CHARTOROWID('0000000F.0003.0002') 

    ENAME 
    ----- 
    SMITH 

SEE: 
    CHAR Datatype, VARCHAR2 Datatype, ROWID Datatype "))
("convert" . (nil "Function" "CONVERT 

SYNTAX: 

    CONVERT(char, dest_char_set [,source_char_set] ) 

PURPOSE: 

    Converts a character string from one character set to another. 

    The char argument is the value to be converted. 

    The dest_char_set argument is the name of the character set to which 
    char is converted. 

    The source_char_set argument is the name of the character set in 
    which char is stored in the database.  The default value is the 

    database character set. 

    Both the destination and source character set arguments can be 
    either literals or columns containing the name of the character set. 

    For complete correspondence in character conversion, it is essential 
    that the destination character set contains a representation of all 
    the characters defined in the source character set.  Where a 
    character does not exist in the destination character set, a 

    replacement character appears.  Replacement characters can be 
    defined as part of a character set definition. 

Common character sets include: 

US7ASCII 
    US 7-bit ASCII character set 
WE8DEC 
    DEC West European 8-bit character set 
WE8HP 
    HP West European Laserjet 8-bit character set 
F7DEC 
    DEC French 7-bit character set 
WE8EBCDIC500 
    IBM West European EBCDIC Code Page 500 

WE8PC850 
    IBM PC Code Page 850 
WE8ISO8859P1 
    ISO 8859-1 West European 8-bit character set 

EXAMPLE: 

    SELECT CONVERT('Grob','WE8HP','WE8DEC') \"Conversion\" 
    FROM DUAL 

    Conversion 
    ---------- 
    Grob "))
("hextoraw" . (nil "Function" "HEXTORAW 

SYNTAX: 

    HEXTORAW(char) 

PURPOSE: 

    Converts char containing hexadecimal digits to a raw value. 

EXAMPLE: 

    INSERT INTO graphics (raw_column) 
        SELECT HEXTORAW('7D') 
        FROM DUAL 

SEE: 
    RAW and LONG RAW Datatypes, RAWTOHEX "))
("rawtohex" . (nil "Function" "RAWTOHEX 

SYNTAX: 

    RAWTOHEX(raw) 

PURPOSE: 

    Converts raw to a character value containing its hexadecimal 
    equivalent. 

EXAMPLE: 

    SELECT RAWTOHEX(raw_column) \"Graphics\" 
        FROM graphics 

    Graphics 
    -------- 
    7D 

SEE: 
    HEXTORAW, RAW and LONG RAW Datatypes "))
("rowidtochar" . (nil "Function" "ROWIDTOCHAR 

SYNTAX: 

    ROWIDTOCHAR(rowid) 

PURPOSE: 

    Converts a ROWID value to VARCHAR2 datatype.  The result of this 
    conversion is always 18 characters long. 

EXAMPLE: 

    SELECT ROWID FROM graphics 
    WHERE ROWIDTOCHAR(ROWID) LIKE '%F38%' 

    ROWID 
    ------------------ 
    00000F38.0001.0001 

SEE: 
    ROWID Datatype, VARCHAR2 Datatype "))
("to_char (date conversion" . (nil "Function" "TO_CHAR (date conversion) 

SYNTAX: 

    TO_CHAR(d [, fmt [, 'nlsparams'] ]) 

PURPOSE: 

    Converts d of DATE datatype to a value of VARCHAR2 datatype in the 
    format specified by the date format fmt.  If you omit fmt, d is 
    converted to a VARCHAR2 value in the default date format. 

    The 'nlsparams' specifies the language in which month and day names 
    and abbreviations are returned.  This argument can have this form: 


    'NLS_DATE_LANGUAGE = language' 

    If you omit nlsparams, this function uses the default date language 
    for your session. 

EXAMPLE: 

    SELECT TO_CHAR(HIREDATE,'Month DD, YYYY') 
    \"New date format\" 
        FROM emp 
        WHERE ename = 'SMITH' 

    New date format 
    ------------------------------- 
    December 17, 1980 

SEE: 
    TO_CHAR (label conversion), TO_CHAR (number conversion), TO_DATE 

TO_CHAR (label conversion) 

SYNTAX: 

    TO_CHAR(label [, fmt]) 

PURPOSE: 

    Converts label of MLSLABEL datatype to a value of VARCHAR2 datatype, 
    using the optional label format fmt.  If you omit fmt, label is 
    converted to a VARCHAR2 value in the default label format. 

SEE: 
    MLSLABEL Datatype, TO_CHAR (date conversion), TO_CHAR (number 
    conversion), TO_LABEL 

TO_CHAR (number conversion) 

SYNTAX: 

    TO_CHAR(n [, fmt [, 'nlsparams'] ]) 

PURPOSE: 

    Converts n of NUMBER datatype to a value of VARCHAR2 datatype, using 
    the optional number format fmt.  If you omit fmt, n is converted to 
    a VARCHAR2 value exactly long enough to hold its significant digits. 

    The 'nlsparams' specifies these characters that are returned by 
       number format elements: 


    * decimal character 
    * group separator 
    * local currency symbol 
    * international currency symbol 

    This argument can have this form: 

    'NLS_NUMERIC_CHARACTERS = ''dg'' 
     NLS_CURRENCY = ''text'' 
     NLS_ISO_CURRENCY = ''text'' ' 

    The characters d and g represent the decimal character and group 
    separator, respectively.  They must be different single-byte 

    characters.  Note that within the quoted string, you must use two 
    single-quotes to represent one around the parameter values. 

    If you omit 'nlsparams' or any one of the parameters, this function 
    uses the default parameter values for your session. 

EXAMPLE: 

    SELECT TO_CHAR(17145,'L099G999', 
               'NLS_NUMERIC_CHARACTERS = ''.,'' 
                NLS_CURRENCY = ''AUD'' ') \"Char\" 

        FROM DUAL 

    Char 
    -------------- 
        AUD017,145 

SEE: 
    NUMBER Datatype, TO_CHAR (date conversion), TO_CHAR (label 
    conversion), VARCHAR2 Datatype "))
("to_date" . (nil "Function" "TO_DATE 

SYNTAX: 

    TO_DATE(char [, fmt [, 'nlsparams'] ]) 

PURPOSE: 

    Converts char of CHAR or VARCHAR2 datatype to a value of DATE 
    datatype.  The fmt is a date format specifying the format of char. 
    If you omit fmt, char must be in the default date format.  If fmt is 
    'J', for Julian, then char must be a number. 

    The 'nlsparams' has the same purpose in this function as in the 

    TO_CHAR function for date conversion. 

    Do not use the TO_DATE function with a DATE value for the char 
    argument.  The returned DATE value can have a different century 
    value than the original char, depending on fmt or the default date 
    format. 

EXAMPLE: 

    INSERT INTO bonus (bonus_date) 
        SELECT TO_DATE('January 15, 1989, 11:00 A.M.', 
                    'Month dd, YYYY, HH:MI A.M.', 

                    'NLS_DATE_LANGUAGE = American') 
        FROM DUAL 

SEE: 
    CHAR Datatype, DATE Datatype, VARCHAR2 Datatype "))
("to_label" . (nil "Function" "TO_LABEL 

SYNTAX: 

    TO_LABEL(char [,fmt]) 

PURPOSE: 

    Converts char, a value of datatype CHAR or VARCHAR2 containing a 
    label in the format specified by the optional parameter fmt, to a 
    value of MLSLABEL datatype.  If you omit fmt, char must be in the 
    default label format. 

SEE: 
    CHAR Datatype, MLSLABEL Datatype, VARCHAR2 Datatype "))
("to_multi_byte" . (nil "Function" "TO_MULTI_BYTE 

SYNTAX: 

    TO_MULTI_BYTE(char) 

PURPOSE: 

    Returns char with all of its single-byte characters converted to 
    their corresponding multi-byte characters.  Any single-byte 
    characters in char that have no multi-byte equivalents appear in the 
    output string as single-byte characters.  This function is only 
    useful if your database character set contains both single-byte and 

    multi-byte characters. "))
("to_number" . (nil "Function" "TO_NUMBER 

SYNTAX: 

    TO_NUMBER(char [,fmt [, 'nlsparams'] ]) 

PURPOSE: 

    Converts char, a value of CHAR or VARCHAR2 datatype containing a 
    number in the format specified by the optional format model fmt, to 
    a value of NUMBER datatype. 

    The 'nlsparams' has the same purpose in this function as in the 
    TO_CHAR function for number conversion. 

EXAMPLE: 

    UPDATE emp 

        SET sal = sal + 
            TO_NUMBER('AUD100.00', 'L999D99' 
                  'NLS_NUMERIC_CHARACTERS = ''.,'', 
                   NLS_CURRENCY = ''AUD'' ') 
            WHERE ename = 'BLAKE' 

SEE: 
    CHAR Datatype, NUMBER Datatype, VARCHAR2 Datatype "))
("to_single_byte" . (nil "Function" "TO_SINGLE_BYTE 

SYNTAX: 

    TO_SINGLE_BYTE(char) 

PURPOSE: 

    Returns char with all of its multi-byte characters converted to 
    their corresponding single-byte characters.  Any multi-byte 
    characters in char that have no single-byte equivalents appear in 
    the output as multi-byte characters.  This function is only useful 
    if your database character set contains both single-byte and multi- 

    byte characters. 

EXAMPLE: 

    SELECT TO_CHAR( NEW_TIME(TO_DATE('17:47','hh24:mi'), 
                  'PST','GMT'), 'hh24:mi') \"GREENWICH TIME\" 
        FROM DUAL 

    GREENWICH TIME 
    ------------- 
    01:47 "))
("bitand" . (nil "Function" "BITAND                          

SYNTAX: 

    BITAND(number1, number2)

PURPOSE: 

    Undocumented function.    

EXAMPLE: 
    
    SELECT BITAND(1,2) FROM DUAL


    B
    -
    0
"))
("avg" . (nil "Function" "AVG 

SYNTAX: 

    AVG([DISTINCT|ALL] n) 

PURPOSE: 

    Returns average value of n. 

EXAMPLE: 

    SELECT AVG(sal) \"Average\" 
        FROM emp 

       Average 
    ---------- 
    2073.21429 

SEE: 
    MAX, MIN, SUM "))
("count" . (nil "Function" "COUNT 

SYNTAX: 

    COUNT({* | [DISTINCT|ALL] expr}) 

PURPOSE: 

    Returns the number of rows in the query. 

    If you specify expr, this function returns rows where expr is not 
    null.  You can count either all rows, or only distinct values of 
    expr. 

    If you specify the asterisk (*), this function returns all rows, 
    including duplicates and nulls. 

EXAMPLES: 


    SELECT COUNT(*) \"Total\" 
        FROM emp 

    Total 
    ----- 
       14 

    SELECT COUNT(job) \"Count\" 
        FROM emp 

    Count 
    ----- 
       14 

    SELECT COUNT(DISTINCT job) \"Jobs\" 
        FROM emp 

    Jobs 
    ---- 
       5 "))
("glb" . (nil "Function" "GLB 

SYNTAX: 

    GLB([DISTINCT|ALL] label) 

PURPOSE: 

    Returns the greatest lower bound of label. 

SEE: 
    LUB "))
("lub" . (nil "Function" "LUB 

SYNTAX: 

    LUB([DISTINCT|ALL] label) 

PURPOSE: 

    Returns the least upper bound of label. 

    The return values have datatype MLSLABEL. 

SEE: 
    GLB "))
("max" . (nil "Function" "MAX 

SYNTAX: 

    MAX([DISTINCT|ALL] expr) 

PURPOSE: 

    Returns maximum value of expr. 

EXAMPLE: 

    SELECT MAX(sal) \"Maximum\" 
        FROM emp 

    Maximum 
    ------- 
    5000 

SEE: 
    AVG, MIN, SUM "))
("min" . (nil "Function" "MIN 

SYNTAX: 

    MIN([DISTINCT|ALL] expr) 

PURPOSE: 

    Returns minimum value of expr. 

EXAMPLE: 

    SELECT MIN(hiredate) \"Minimum Date\" 
        FROM emp 

    Minimum Date 
    ------------ 
    17-DEC-80 

Note: 
    The DISTINCT and ALL options have no effect on the MAX and MIN 
    functions. 

SEE: 
    AVG, MAX, SUM "))
("stddev" . (nil "Function" "STDDEV 

SYNTAX: 

    STDDEV([DISTINCT|ALL] x) 

PURPOSE: 

    Returns standard deviation of x, a number.  Oracle calculates the 
    standard deviation as the square root of the variance defined for 
    the VARIANCE group function. 

EXAMPLE: 

    SELECT STDDEV(sal) \"Deviation\" 
        FROM emp 

    Deviation 
    ---------- 
    1182.50322 

SEE: 

    VARIANCE "))
("sum" . (nil "Function" "SUM 

SYNTAX: 

    SUM([DISTINCT|ALL] n) 

PURPOSE: 

    Returns sum of values of n. 

EXAMPLE: 

    SELECT SUM(sal) \"Total\" 
        FROM emp 

    Total 
    ----- 
    29025 

SEE: 
    AVG, MAX, MIN "))
("variance" . (nil "Function" "VARIANCE 

SYNTAX: 

    VARIANCE([DISTINCT|ALL]x) 

PURPOSE: 

    Returns variance of x, a number.  Oracle calculates the variance of 
    x using this formula: 

       where: 
            xi 
                   is one of the elements of x. 
            n 
                   is the number of elements in the set x.  If n is 1, 
                   the variance is defined to be 0. 


EXAMPLE: 

    SELECT VARIANCE(sal) \"Variance\" 
        FROM emp 

      Variance 
    ---------- 
    1389313.87 

SEE: 
    STDDEV "))
("decode" . (nil "Function" "DECODE

    An expression using the special DECODE syntax: 

SYNTAX: 

    DECODE( expr, search, result [, search, result] ... [, default] ) 

    To evaluate this expression, Oracle compares expr to each search 
    value one by one.  If expr is equal to a search, Oracle returns the 
    corresponding result.  If no match is found, Oracle returns default, 
    or, if default is omitted, returns null.  If expr and search contain 

    character data, Oracle compares them using non-padded comparison 
    semantics. 

    The search, result, and default values can be expressions. 

    Oracle evaluates each search value only before comparing it to expr, 
    rather than evaluating all search values before comparing any of 
    them with expr.  Consequently, Oracle never evaluates a search if a 
    previous search is equal to expr. 

    Oracle automatically converts expr and each search value to the 

    datatype of the first search value before comparing.  Oracle 
    automatically converts the return value to the same datatype as the 
    first result.  If the first result has the datatype CHAR or if the 
    first result is null, then Oracle converts the return value to the 
    datatype VARCHAR2. 

    In a DECODE expression, Oracle considers two nulls to be equivalent. 
    If expr is null, Oracle returns the result of the first search that 

    is also null. 

    The maximum number of components in the DECODE expression, including 
    expr, searches, results, and default is 255. 

EXAMPLE: 

    This expression decodes the value DEPTNO.  If DEPTNO is 10, the 
    expression evaluates to 'ACCOUNTING'; if DEPTNO is 20, it evaluates 
    to 'RESEARCH'; etc.  If DEPTNO is not 10, 20, 30, or 40, the 
    expression returns 'NONE'. 

    DECODE (deptno, 10, 'ACCOUNTING', 

                    20, 'RESEARCH', 
                    30, 'SALES', 
                    40, 'OPERATION', 
                        'NONE') "))
("currval" . (nil "Function" "CURRVAL

    A sequence is a schema object that can generate unique sequential 
    values.  These values are often used for primary and unique keys. 
    You can refer to sequence values in SQL statements with these 
    pseudocolumns: 

CURRVAL 
    returns the current value of a sequence. 

NEXTVAL 
    increments the sequence and returns the next value. 

    You must qualify CURRVAL and NEXTVAL with the name of the sequence: 


    sequence.CURRVAL 
    sequence.NEXTVAL 

    To refer to the current or next value of a sequence in the schema of 
    another user, you must have been granted either SELECT object 
    privilege on the sequence or SELECT ANY SEQUENCE system privilege 
    and you must qualify the sequence with the schema containing it: 

    schema.sequence.CURRVAL 
    schema.sequence.NEXTVAL 

    To refer to the value of a sequence on a remote database, you must 

    qualify the sequence with a complete or partial name of a database 
    link: 

    schema.sequence.CURRVAL@dblink 
    schema.sequence.NEXTVAL@dblink 

    If you are using Trusted Oracle in DBMS MAC mode, you can only refer 
    to a sequence if your DBMS label dominates the sequence's creation 
    label or if one of these criteria is satisfied: 

    * If the sequence's creation label is higher than your DBMS label, 

      you must have READUP and WRITEUP system privileges. 
    * If the sequence's creation label is lower than your DBMS label, 
      you must have WRITEDOWN system privilege. 
    * If the sequence's creation label and your DBMS label are 
      noncomparable, you must have READUP, WRITEUP, and WRITEDOWN system 
      privileges. 

    If you are using Trusted Oracle in OS MAC mode, you cannot refer to 
    a sequence with a lower creation label than your DBMS label. 


USING SEQUENCE VALUES: 

    You can use CURRVAL and NEXTVAL in these places: 

    * the SELECT list of a SELECT statement 
    * the VALUES clause of an INSERT statement 
    * the SET clause of an UPDATE statement 

    You cannot use CURRVAL and NEXTVAL in these places: 

    * a subquery 
    * a view's query or snapshot's query 
    * a SELECT statement with the DISTINCT operator 

    * a SELECT statement with a GROUP BY or ORDER BY clause 
    * a SELECT statement that is combined with another SELECT statement 
      with the UNION, INTERSECT, or MINUS set operator 
    * the WHERE clause of a SELECT statement 
    * DEFAULT value of a column in a CREATE TABLE or ALTER TABLE 
      statement 
    * the condition of a CHECK constraint 

    Also, within a single SQL statement, all referenced sequences, LONG 

    columns, updated tables, and locked tables must be located on the 
    same database. 

    When you create a sequence, you can define its initial value and the 
    increment between its values.  The first reference to NEXTVAL 
    returns the sequence's initial value.  Subsequent references to 
    NEXTVAL increment the sequence value by the defined increment and 
    return the new value.  Any reference to CURRVAL always returns the 

    sequence's current value, which is the value returned by the last 
    reference to NEXTVAL.  Note that before you use CURRVAL for a 
    sequence in your session, you must first increment the sequence with 
    NEXTVAL. 

    You can only increment a sequence once in a single SQL statement. 
    If a statement contains more than one reference to NEXTVAL for a 
    sequence, Oracle increments the sequence once and returns the same 

    value for all occurrences of NEXTVAL.  If a statement contains 
    references to both CURRVAL and NEXTVAL, Oracle increments the 
    sequence and returns the same value for both CURRVAL and NEXTVAL 
    regardless of their order within the statement. 

    A sequence can be accessed by many users concurrently with no 
    waiting or locking. 

Example I: 

    This example selects the current value of the employee sequence: 


    SELECT empseq.currval 
        FROM DUAL 

Example II: 

    This example increments the employee sequence and uses its value for 
    a new employee inserted into the employee table: 

    INSERT INTO emp 
        VALUES (empseq.nextval, 'LEWIS', 'CLERK', 
                7902, SYSDATE, 1200, NULL, 20) 

Example III: 

    This example adds a new order with the next order number to the 

    master order table and then adds suborders with this number to the 
    detail order table: 

    INSERT INTO master_order(orderno, customer, orderdate) 
        VALUES (orderseq.nextval, 'Al''s Auto Shop', SYSDATE) 
    INSERT INTO detail_order (orderno, part, quantity) 
        VALUES (orderseq.currval, 'SPARKPLUG', 4) 
    INSERT INTO detail_order (orderno, part, quantity) 
        VALUES (orderseq.currval, 'FUEL PUMP', 1) 

    INSERT INTO detail_order (orderno, part, quantity) 
        VALUES (orderseq.currval, 'TAILPIPE', 2) 

SEE: 
    INSERT, Pseudocolumns, SELECT, UPDATE "))
("nextval" . (nil "Function" "NEXTVAL 

    A sequence is a schema object that can generate unique sequential 
    values.  These values are often used for primary and unique keys. 
    You can refer to sequence values in SQL statements with these 
    pseudocolumns: 

CURRVAL 
    returns the current value of a sequence. 

NEXTVAL 
    increments the sequence and returns the next value. 

    You must qualify CURRVAL and NEXTVAL with the name of the sequence: 


    sequence.CURRVAL 
    sequence.NEXTVAL 

    To refer to the current or next value of a sequence in the schema of 
    another user, you must have been granted either SELECT object 
    privilege on the sequence or SELECT ANY SEQUENCE system privilege 
    and you must qualify the sequence with the schema containing it: 

    schema.sequence.CURRVAL 
    schema.sequence.NEXTVAL 

    To refer to the value of a sequence on a remote database, you must 

    qualify the sequence with a complete or partial name of a database 
    link: 

    schema.sequence.CURRVAL@dblink 
    schema.sequence.NEXTVAL@dblink 

    If you are using Trusted Oracle in DBMS MAC mode, you can only refer 
    to a sequence if your DBMS label dominates the sequence's creation 
    label or if one of these criteria is satisfied: 

    * If the sequence's creation label is higher than your DBMS label, 

      you must have READUP and WRITEUP system privileges. 
    * If the sequence's creation label is lower than your DBMS label, 
      you must have WRITEDOWN system privilege. 
    * If the sequence's creation label and your DBMS label are 
      noncomparable, you must have READUP, WRITEUP, and WRITEDOWN system 
      privileges. 

    If you are using Trusted Oracle in OS MAC mode, you cannot refer to 
    a sequence with a lower creation label than your DBMS label. 


USING SEQUENCE VALUES: 

    You can use CURRVAL and NEXTVAL in these places: 

    * the SELECT list of a SELECT statement 
    * the VALUES clause of an INSERT statement 
    * the SET clause of an UPDATE statement 

    You cannot use CURRVAL and NEXTVAL in these places: 

    * a subquery 
    * a view's query or snapshot's query 
    * a SELECT statement with the DISTINCT operator 

    * a SELECT statement with a GROUP BY or ORDER BY clause 
    * a SELECT statement that is combined with another SELECT statement 
      with the UNION, INTERSECT, or MINUS set operator 
    * the WHERE clause of a SELECT statement 
    * DEFAULT value of a column in a CREATE TABLE or ALTER TABLE 
      statement 
    * the condition of a CHECK constraint 

    Also, within a single SQL statement, all referenced sequences, LONG 

    columns, updated tables, and locked tables must be located on the 
    same database. 

    When you create a sequence, you can define its initial value and the 
    increment between its values.  The first reference to NEXTVAL 
    returns the sequence's initial value.  Subsequent references to 
    NEXTVAL increment the sequence value by the defined increment and 
    return the new value.  Any reference to CURRVAL always returns the 

    sequence's current value, which is the value returned by the last 
    reference to NEXTVAL.  Note that before you use CURRVAL for a 
    sequence in your session, you must first increment the sequence with 
    NEXTVAL. 

    You can only increment a sequence once in a single SQL statement. 
    If a statement contains more than one reference to NEXTVAL for a 
    sequence, Oracle increments the sequence once and returns the same 

    value for all occurrences of NEXTVAL.  If a statement contains 
    references to both CURRVAL and NEXTVAL, Oracle increments the 
    sequence and returns the same value for both CURRVAL and NEXTVAL 
    regardless of their order within the statement. 

    A sequence can be accessed by many users concurrently with no 
    waiting or locking. 

Example I: 

    This example selects the current value of the employee sequence: 


    SELECT empseq.currval 
        FROM DUAL 

Example II: 

    This example increments the employee sequence and uses its value for 
    a new employee inserted into the employee table: 

    INSERT INTO emp 
        VALUES (empseq.nextval, 'LEWIS', 'CLERK', 
                7902, SYSDATE, 1200, NULL, 20) 

Example III: 

    This example adds a new order with the next order number to the 

    master order table and then adds suborders with this number to the 
    detail order table: 

    INSERT INTO master_order(orderno, customer, orderdate) 
        VALUES (orderseq.nextval, 'Al''s Auto Shop', SYSDATE) 
    INSERT INTO detail_order (orderno, part, quantity) 
        VALUES (orderseq.currval, 'SPARKPLUG', 4) 
    INSERT INTO detail_order (orderno, part, quantity) 
        VALUES (orderseq.currval, 'FUEL PUMP', 1) 

    INSERT INTO detail_order (orderno, part, quantity) 
        VALUES (orderseq.currval, 'TAILPIPE', 2) 

SEE: 
    INSERT, Pseudocolumns, SELECT, UPDATE "))
("level" . (nil "Function" "LEVEL 

    For each row returned by a hierarchical query, the LEVEL 
    pseudocolumn returns 1 for a root node, 2 for a child of a root, and 
    so on.  A root node is the highest node within an inverted tree. A 
    child node is any non-root node.  A parent node is any row that has 
    children.  A leaf node is any row without children. 

    To define a hierarchical relationship in a query, you must use the 

    START WITH and CONNECT BY clauses.  For more information on using 
    the LEVEL pseudocolumn, see the SELECT command. 

SEE: 
    Pseudocolumns, SELECT "))
("rownum" . (nil "Function" "ROWNUM 

    For each row returned by a query, the ROWNUM pseudocolumn returns a 
    number indicating the order in which Oracle selects the row from a 
    table or set of joined rows.  The first row selected has a ROWNUM of 
    1, the second has 2, and so on. 

    You can use ROWNUM to limit the number of rows returned by a query, 
    as in this example: 

    SELECT * 
        FROM emp 

        WHERE ROWNUM < 10 

    You can also use ROWNUM to assign unique values to each row of a 
    table, as in this example: 

    UPDATE tabx 
        SET col1 = ROWNUM 

    Oracle assigns a ROWNUM value to each row as it is retrieved, before 
    rows are sorted for an ORDER BY clause, so an ORDER BY clause 
    normally does not affect the ROWNUM of each row.  However, if an 
    ORDER BY clause causes Oracle to use an index to access the data, 

    Oracle may retrieve the rows in a different order than without the 
    index, so the ROWNUMs may be different than without the ORDER BY 
    clause. 

    Note that conditions testing for ROWNUM values greater than a 
    positive integer are always false.  For example, this query returns 
    no rows: 

    SELECT * FROM emp 
        WHERE ROWNUM > 1 

    The first row fetched is assigned a ROWNUM of 1 and makes the 

    condition false.  The second row to be fetched is now the first row 
    and is also assigned a ROWNUM of 1 and also makes the condition 
    false.  All rows subsequently fail to satisfy the condition, so no 
    rows are returned. 

SEE: 
    Pseudocolumns, ROWID "))
("rowid" . (nil "Function" "ROWID 

    For each row in the database, the ROWID pseudocolumn returns a row's 
    address.  ROWID values contain information necessary to locate a 
    row: 

    * which data block in the data file 
    * which row in the data block (first row is 0) 
    * which data file (first file is 1) 

    In most cases, a ROWID value uniquely identifies a row in the 
    database.  However, rows in different tables that are stored 

    together in the same cluster can have the same ROWID. 

    Values of the ROWID pseudocolumn have the datatype ROWID. 

    ROWID values have several important uses: 

    * They are the fastest means of accessing a single row. 
    * They can show you how a table's rows are stored. 
    * They are unique identifiers for rows in a table. 

    A ROWID does not change during the lifetime of its row.  However, 

    you should not use ROWID as a table's primary key.  If you delete 
    and reinsert a row with the Import and Export utilities, for 
    example, its ROWID may change.  If you delete a row, Oracle may 
    reassign its ROWID to a new row inserted later. 

    Although you can use the ROWID pseudocolumn in the SELECT and WHERE 
    clauses of a query, these pseudocolumn values are not actually 
    stored in the database.  You cannot insert, update, or delete a 

    value of the ROWID pseudocolumn. 

EXAMPLE: 

    This statement selects the address of all rows that contain data for 
    employees in department 20: 

    SELECT ROWID, ename 
        FROM emp 
        WHERE deptno = 20 

    ROWID                  ENAME 
    -----------------     ---------- 
    0000000F.0000.0002     SMITH 
    0000000F.0003.0002     JONES 
    0000000F.0007.0002     SCOTT 

    0000000F.000A.0002     ADAMS 
    0000000F.000C.0002     FORD 

SEE: 
    Pseudocolumns, ROWNUM "))
("@" . (nil "SQL*Plus command" "@ 



 Purpose	

Runs the specified command file. 

 Syntax	

@ file_name[.ext] [arg1 arg2 ... ]

 Terms and Clauses	

Refer to the following list for a description of each term or clause


file_name[.ext]	Represents the command file you wish to run. If you omit ext, SQL*Plus 
assumes the default command-file extension (normally SQL). For 
information on changing the default extension, see the SUFFIX variable of 
the SET command in this chapter. 



	When you enter @ file_name.ext, SQL*Plus searches for a file with the file 
name and extension you specify in the current default directory. If SQL*Plus 
does not find such a file, SQL*Plus will search a system-dependent path to 
find the file. Some operating systems may not support the path-search. 
Consult the Oracle installation and user's manual(s) provided for your 
operating system for specific information related to your operating system 
environment. 



	Note that you can omit the space between the \"at\" sign (@) and the 
command-file name. 



arg1 arg2 ...	Represent data items you wish to pass to parameters in the command file.  
If you enter one or more arguments, SQL*Plus substitutes the values into 
the parameters (&1, &2, and so forth) in the command file. The first 
argument replaces each occurrence of &1, the second replaces each 
occurrence of &2, and so forth. 



	The \"at\" sign@ command DEFINEs the parameters with the values of the 
arguments; if you run the command file again in this session, you can enter 
new arguments or omit the arguments to use the old values. 



	For more information on using parameters, refer to the subsection \"Passing 
Parameters through the START Command\" under \"Writing Interactive 
Commands\" in Chapter 3.  


 Usage Notes	

 You can include in a command file any command you would normally enter interactively (typically, 
SQL or SQL*Plus commands). 

The \"at\" sign command functions the same as START. 

 Example	

 To run a command file named PRINTRPT with the extension SQL, enter: 



SQL> @PRINTRPT 

To run a command file named WKRPT with the extension QRY, enter: 



SQL> @WKRPT.QRY  

 ________________________________________



Copyright (c) 1994, Oracle Corporation."))
("@@" . (nil "SQL*Plus command" "@@ 



 Purpose	

 Runs a nested command file. This command is identical to the @ (\"at\" sign) command except that it 
looks for the specified command file in the same path as the command file from which it was called. 

 Syntax	

 @@ file_name[.ext] 

 Terms and Clauses	

 Refer to the following list for a description of each term or clause:


file_name[.ext]	Represents the nested command file you wish to run. If you omit ext, 
SQL*Plus assumes the default command-file extension (normally SQL). 
For information on changing the default extension, see the SUFFIX variable 
of the SET command in this chapter. 



	When you enter @@file_name.ext within a command file, SQL*Plus 
searches for a file with the file name and extension you specify in the same 
path as the command file. If SQL*Plus does not find such a file, SQL*Plus 
will search a system-dependent path to find the file. Some operating 
systems may not support the path-search. Consult the Oracle installation 
and user's manual(s) provided for your operating system for specific 
information related to your operating system environment. 



	Note that you can omit the space between the double \"at\" sign (@@) and 
the command-file name. 


 Usage Notes	

 You can include in a command file any command you would normally enter interactively (typically, 
SQL or SQL*Plus commands). 

 Example	

 Suppose that you have the following command file named PRINTRPT:



SELECT * FROM EMP 

@EMPRPT 
@@ WKRPT 

When you run PRINTRPT and it reaches the @ command, it looks for the command file named 
EMPRPT in the current working directory and runs it. When PRINTRPT reaches the @@command, it 
looks for the command file named WKRPT in the same path as PRINTRPT and runs it."))
("/" . (nil "SQL*Plus command" "/


 Purpose	

 Executes the SQL command or PL/SQL block currently stored in the SQL buffer. 

 Syntax	

 / 

 Usage Notes	

 You can enter a slash (/) at the command prompt or at a line number prompt for a continuing 
command or block in the SQL buffer. 

The slash command functions similarly to RUN, but does not list the command in the buffer on your 
screen. 

Executing a SQL command or PL/SQL block using the slash command will not cause the current line 
number in the SQL buffer to change unless the command in the buffer contains an error. In that case 
SQL*Plus changes the current line number to the number of the line containing the error. 


 Example	

 To see the SQL command(s) you will execute, you can list the contents of the buffer: 



SQL> LIST

  1* SELECT ENAME, JOB FROM EMP WHERE ENAME = 'JAMES' 

Enter a slash (/) to the command prompt to execute the command(s) in the buffer: 



SQL> / 

For the above query, SQL*Plus displays the following output: 



ENAME      JOB

---------- --------- 
JAMES      CLERK  

 ________________________________________



Copyright (c) 1994, Oracle Corporation."))
("accept" . (nil "SQL*Plus command" "ACCEPT 



 Purpose	

 Reads a line of input and stores it in a given user variable. 

 Syntax	

 



ACC[EPT] variable [NUM[BER]|CHAR]



	[PROMPT text|NOPR[OMPT]]

	[HIDE]




 Terms and Clauses	

 Refer to the following list for a description of each term or clause:


variable	Represents the name of the variable in which you wish to store a value. If 
variable does not exist, SQL*Plus creates it. 



NUM[BER]	Restricts the datatype of variable to the datatype NUMBER. If the reply 
does not match the datatype, ACCEPT gives an error message and 
terminates.  



CHAR	Restricts the datatype of variable to the datatype CHAR. If the reply does 
not match the datatype, ACCEPT gives an error message and terminates.  



PROMPT text 	Displays text on-screen before accepting the value of variable from the 
user. 



NOPR[OMPT] 	Skips a line and waits for input without  displaying a prompt. 



HIDE 	Suppresses the display as you type the reply. 


 Examples	

 To display the prompt, \"Salary:  \" and place the reply in a NUMBER variable named SALARY, enter: 



SQL> ACCEPT salary NUMBER PROMPT 'Salary:  '  

To display the prompt, \"Password:  \", to place the reply in a CHAR variable named PSWD, and to 
suppress the display, enter: 



SQL> ACCEPT pswd CHAR PROMPT 'Password:  ' HIDE 

 Usage Notes	

 To display a percent sign (%) in your PROMPT string, you need to specify %%. For example: 



SQL> ACCEPT mystr CHAR PROMPT 'mystr (%%):'  

 ________________________________________



Copyright (c) 1994, Oracle Corporation."))
("append" . (nil "SQL*Plus command" "APPEND 



 Purpose	

 Adds specified text to the end of the current line in the buffer. 

 Syntax	

 A[PPEND] text  

 Terms and Clauses	

 Refer to the following list for a description of each term or clause: 


text	Represents the text you wish to append. If you wish to separate text from 
the preceding characters with a space, enter two spaces between APPEND 
and text. 



	To APPEND text that ends with a semicolon, end the command with two 
semicolons (SQL*Plus interprets a single semicolon as an optional 
command terminator). 


 Examples	

 To append a space and the column name DEPT to the second line of the buffer, make that line the 
current line by listing the line as follows: 



SQL> 2

  2* FROM EMP, 

Now enter APPEND: 



SQL> APPEND  DEPT

SQL> 2
  2* FROM EMP, DEPT 

Notice the double space between APPEND and DEPT. The first space separates APPEND from the 
characters to be appended; the second space becomes the first appended character. 

To append a semicolon to the line, enter: 



SQL> APPEND ;;  

SQL*Plus appends the first semicolon to the line and interprets the second as the terminator for the 
APPEND command.  

 ________________________________________



Copyright (c) 1994, Oracle Corporation."))
("break" . (nil "SQL*Plus command" "BREAK 



 Purpose	

 Specifies where and how formatting will change in a report, such as:


	ú	suppressing display of duplicate values for a given column 



	ú	skipping a line each time a given column value changes 



	ú	printing COMPUTEd figures each time a given column value changes or at the end of the 
report (see also the COMPUTE command) 


Also lists the current BREAK definition. 

 Syntax	

 BRE[AK] [ON report_element [action [action]]] ...

where: 


report_element	Requires the following syntax: 



	{column|expr|ROW|REPORT} 



action	Requires the following syntax: 



	[SKI[P] n|[SKI[P]] PAGE]  [NODUP[LICATES]|DUP[LICATES]] 


 Terms and Clauses	

 Refer to the following list for a description of each term or clause:


ON column [action [action]] 	 When you include action(s), specifies action(s) for 
SQL*Plus to take whenever a break occurs in the specified column (called 
the break column). (column cannot have a table or view prepended to it. 
To achieve this, you can alias the column in the SQL statement.)  A break is 
one of three events: 



	ú	a change in the value of a column or expression 



	ú	the output of a row 



	ú	the end of a report 



	When you omit action(s), BREAK ON column suppresses printing of 
duplicate values in column and marks a place in the report where 
SQL*Plus will perform the computation you specify in a corresponding 
COMPUTE command. 



	You can specify ON column one or more times. If you specify multiple ON 
clauses, as in:




				SQL> BREAK ON DEPTNO SKIP PAGE ON  JOB SKIP 1 -

				> ON SAL SKIP 1


	The first ON clause represents the outermost break (in this case, ON 
DEPTNO) and the last ON clause represents the innermost break (in this 
case, ON SAL). SQL*Plus searches each row of output for the specified 
break(s), starting with the outermost break and proceeding--in the order you 
enter the clauses--to the innermost. In the example, SQL*Plus searches for 
a change in the value of DEPTNO, then JOB, then SAL. 



	Next, SQL*Plus executes actions beginning with the action specified for the 
innermost break and proceeding in reverse order toward the outermost 
break (in this case from SKIP 1 for ON SAL toward SKIP PAGE for ON 
DEPTNO). SQL*Plus executes each action up to and including the action 
specified for the first occurring break encountered in the initial search. 



	If, for example, in a given row the value of JOB changes--but the values of 
DEPTNO and SAL remain the same--SQL*Plus skips two lines before 
printing the row (one as a result of SKIP 1 in the ON SAL clause and one as 
a result of SKIP 1 in the ON JOB clause). 



	Whenever you use ON column, you should also use an ORDER BY clause 
in the SQL SELECT command. Typically, the columns used in the BREAK 
command should appear in the same order in the ORDER BY clause 
(although all columns specified in the ORDER BY clause need not appear 
in the BREAK command).  This prevents breaks from occurring at 
meaningless points in the report. 



	With the above BREAK command, the following SELECT command 
produces meaningful results: 




				SQL> SELECT DEPTNO, JOB, SAL, ENAME

				  2  FROM EMP   
				  3  ORDER BY DEPTNO, JOB, SAL, ENAME; 


	All rows with the same DEPTNO print together on one page, and within that 
page all rows with the same JOB print in groups. Within each group of jobs, 
jobs with the same SAL print in groups. Breaks in ENAME cause no action, 
because ENAME does not appear in the BREAK command. 



ON expr [action [action]]  	 When you include action(s), specifies action(s) for 
SQL*Plus to take when the value of the expression changes. 



	When you omit action(s), BREAK ON expr suppresses printing of 
duplicate values of expr and marks a place in the report where SQL*Plus 
will perform the computation you specify in a corresponding COMPUTE 
command. 



	You can use an expression involving one or more table columns or an alias 
assigned to a report column in a SQL SELECT or SQL*Plus COLUMN 
command. If you use an expression in a BREAK command, you must enter 
expr exactly as it appears in the SELECT command. If the expression in 
the SELECT command is a+b, for example, you cannot use b+a or (a+b) in 
a BREAK command to refer to the expression in the SELECT command. 



	The information given above for ON column also applies to ON expr. 



ON ROW [action [action]] 	 When you include action(s), specifies action(s) for SQL*Plus to 
take when a SQL SELECT command returns a row. The ROW break 
becomes the innermost break regardless of where you specify it in the 
BREAK command. You should always specify an action when you BREAK 
on a row. 



ON REPORT 	Marks a place in the report where SQL*Plus will perform the computation 
you specify in a corresponding COMPUTE command. Use BREAK ON 
REPORT in conjunction with COMPUTE to print grand totals or other 
\"grand\" computed values. 



	The REPORT break becomes the outermost break regardless of where you 
specify it in the BREAK command. 


Refer to the following list for a description of each action: 


SKI[P] n	Skips n lines before printing the row where the break occurred.  



[SKI[P]] PAGE 	Skips the number of lines that are defined to be a page before printing the 
row where the break occurred. The number of lines per page can be set via 
the PAGESIZE clause of the SET command. Note that PAGESIZE only 
changes the number of lines that SQL*Plus considers to be a page. Thus 
SKIP PAGE may not always cause a physical page break, unless you have 
also specified NEWPAGE 0. 



NODUP[LICATES]	Prints blanks rather than the value of a break column when the value is a 
duplicate of the column's value in the preceding row. 



DUP[LICATES] 	Prints the value of a break column in every selected row. 


Enter BREAK with no clauses to list the current break definition. 

 Usage Notes	

 Each new BREAK command you enter replaces the preceding one. 

When you use COMPUTE with BREAK, the label for the computed value normally appears in the first 
column. However, if the COMPUTE is being performed on the first column, you should create a 
dummy first column for the label using the COLUMN command.  Otherwise, the label will not appear. 

 Example	

 To produce a report that prints duplicate job values, prints the average of SAL and inserts one blank 
line when the value of JOB changes, and additionally prints the sum of SAL and inserts another 
blank line when the value of DEPTNO changes, you could enter the following commands. (The 
example selects departments 10 and 30 and the jobs of clerk and salesman only.)




SQL> BREAK ON DEPTNO SKIP 1 ON JOB SKIP 1 DUPLICATES 

SQL> COMPUTE SUM OF SAL ON DEPTNO 
SQL> COMPUTE AVG OF SAL ON JOB 
SQL> SELECT DEPTNO, JOB, ENAME, SAL FROM EMP 
 2   WHERE JOB IN ('CLERK', 'SALESMAN') 
 3   AND DEPTNO IN (10, 30) 
 4   ORDER BY DEPTNO, JOB;  

The following output results: 



DEPTNO	JOB	ENAME	SAL

-----------	---------	---------	---------

10	CLERK	MILLER	1300

	*********		---------

	avg		1300

			

**********			----------

sum			1300

			

30	CLERK	JAMES	1045

	*********		----------

	avg		1045

			

	SALESMAN	ALLEN	1760

	SALESMAN	MARTIN	1375

	SALESMAN	TURNER	1650


	SALESMAN	WARD	1375

	*********		----------

	avg		1540

			

**********			----------

sum			7205




 ________________________________________



Copyright (c) 1994, Oracle Corporation."))
("btitle" . (nil "SQL*Plus command" "BTITLE 



 Purpose	

Places and formats a specified title at the bottom of each report page, or lists the current BTITLE 
definition. 

Note:	For a description of the old form of BTITLE, see BTITLE (old form) in Appendix F. 

 Syntax	

BTI[TLE] [printspec [text|variable] ...] | 	    [OFF|ON] 

 Terms and Clauses	

Refer to the TTITLE command for additional information on terms and clauses in the BTITLE 
command syntax.

Enter BTITLE with no clauses to list the current BTITLE definition. 


 Usage Notes	

 SQL*Plus interprets BTITLE in the new form if a valid printspec clause (LEFT, SKIP, COL, etc) 
immediately follows the command name. 

For information on printing page numbers in the title, see TTITLE. 

 Examples	

To set a bottom title with CORPORATE PLANNING DEPARTMENT on the left and a date on the 
right, enter:



SQL> BTITLE LEFT 'CORPORATE PLANNING DEPARTMENT' - 

> RIGHT '11 Mar 1988' 

To set a bottom title with CONFIDENTIAL in column 50, followed by 6 spaces and a date, enter: 



SQL> BTITLE COL 50 'CONFIDENTIAL' TAB 6 '11 Mar 88'  

 ________________________________________



Copyright (c) 1994, Oracle Corporation."))
("change" . (nil "SQL*Plus command" "CHANGE 



 Purpose	

Changes text on the current line in the buffer. 

 Syntax	

C[HANGE] sepchar old [sepchar [new [sepchar]]] 

 Terms and Clauses	

Refer to the following list for a description of each term or clause:


sepchar	Represents any non-alphanumeric character such as \"/\" or \"!\". Use a 
sepchar that does not appear in old or new. You can omit the space 
between CHANGE and the first sepchar. 



old	Represents the text you wish to change. CHANGE ignores case in 
searching for old. For example, 

 				CHANGE /aq/aw

	will find the first occurrence of \"aq\", \"AQ\", \"aQ\", or \"Aq\" and change it to 
\"aw\". SQL*Plus inserts the new text exactly as you specify it. 



	If old is prefixed with \"...\", it matches everything up to and including the first 
occurrence of old. If it is suffixed with \"...\", it matches the first occurrence 
of old and everything that follows on that line. If it contains an embedded 
\"...\", it matches everything from the preceding part of old through the 
following part of old. 



new	Represents the text with which you wish to replace old. If you omit new 
and, optionally, the second and third sepchars, CHANGE deletes old 
from the current line of the buffer.  


 Usage Notes	

CHANGE changes the existing text you specify from the current line of the buffer to the new text you 
specify. The current line is marked with an asterisk (*) in the LIST output. 

You can also use CHANGE to modify a line in the buffer that has generated an ORACLE error. 
SQL*Plus sets the buffer's current line to the line containing the error so that you can make 
modifications. 

To re-enter an entire line, you can type the line number followed by the new contents of the line. If you 
specify a line number larger than the number of lines in the buffer, and follow the number with text, 
SQL*Plus adds the text in a new line at the end of the buffer. If you specify zero (\"0\") for the line 
number and follow the zero with text, then SQL*Plus inserts the line at the beginning of the buffer 
(that line becomes line 1). 


 Examples	

Assume the current line of the buffer contains the following text:



4* WHERE JOB IS IN ('CLERK','SECRETARY','RECEPTIONIST') 

Enter the following command: 



SQL> C /RECEPTIONIST/GUARD/ 

The text in the buffer changes as follows: 



4* WHERE JOB IS IN ('CLERK','SECRETARY','GUARD') 

Or enter the following command: 



SQL> C /'CLERK',.../'CLERK')/ 

The original line changes to: 



4* WHERE JOB IS IN ('CLERK') 

Or enter the following command: 



SQL> C /(...)/('COOK','BUTLER')/ 

The original line changes to: 



4* WHERE JOB IS IN ('COOK','BUTLER') 

You can replace the contents of an entire line using the line number. This entry 



SQL> 2  FROM EMP e1 

causes the second line of the buffer to be replaced with: 



FROM EMP e1 

Note: Entering a line number followed by a string will replace the line regardless of what text follows 
the line number. Thus, 



SQL> 2  c/old/new/ 

will change the second line of the buffer to be: 



SQL> c/old/new 

 ________________________________________



Copyright (c) 1994, Oracle Corporation."))
("clear" . (nil "SQL*Plus command" "CLEAR 



 Purpose	

Resets or erases the current value or setting for the specified option. 

 Syntax	

CL[EAR] option 

where option represents one of the following clauses: 



BRE[AKS]

BUFF[ER]
COL[UMNS]
COMP[UTES]
SCR[EEN]
SQL
TIMI[NG] 

 Terms and Clauses	

Refer to the following list for a description of each term or clause: 


BRE[AKS] 	Removes the break definition set by the BREAK command. 



BUFF[ER] 	Clears text from the buffer. CLEAR BUFFER has the same effect as 
CLEAR SQL, unless you are using multiple buffers (see SET BUFFER in 
Appendix F). 



COL[UMNS] 	Resets column display attributes set by the COLUMN command to default 
settings for all columns. To reset display attributes for a single column, use 
the CLEAR clause of the COLUMN command. 



COMP[UTES] 	Removes all COMPUTE definitions set by the COMPUTE command. 



SCR[EEN] 	Clears your screen. 



SQL 	Clears the text from SQL buffer. CLEAR SQL has the same effect as 
CLEAR BUFFER, unless you are using multiple buffers (see SET BUFFER 
in Appendix F). 



TIMI[NG] 	Deletes all timing areas created by the TIMING command. 


 Examples	

To clear breaks, enter:



SQL> CLEAR BREAKS 

To clear column definitions, enter: 



SQL> CLEAR COLUMNS 

 ________________________________________



Copyright (c) 1994, Oracle Corporation."))
("column" . (nil "SQL*Plus command" "COLUMN 



 Purpose	

Specifies display attributes for a given column, such as:


	ú	text for the column heading 



	ú	alignment of the column heading 



	ú	format for NUMBER data 



	ú	wrapping of column data 


Also lists the current display attributes for a single column or all columns. 

 Syntax	

COL[UMN] [{column|expr} [option ...]] 

where option represents one of the following clauses: 



ALI[AS] alias

CLE[AR]
COLOR {color|color_variable}
DEF[AULT]
FOLD_A[FTER]
FOLD_B[EFORE]
FOR[MAT] format
HEA[DING] text
JUS[TIFY] {L[EFT]|C[ENTER]|C[ENTRE]|R[IGHT]}
LIKE {expr|alias}
LINEAPP {LINE|MARK|BOTH}
NEWL[INE]
NEW_V[ALUE] variable
NOPRI[NT]|PRI[NT] 
NUL[L] char
OLD_V[ALUE] variable
ON|OFF
PATTERN {pattern_number|pattern_variable} WRA[PPED]|WOR[D_WRAPPED]|TRU[NCATED] 

 Terms and Clauses	

Enter COLUMN followed by column or expr and no other clauses to list the current display 
attributes for only the specified column or expression. Enter COLUMN with no clauses to list all 
current column display attributes.


Refer to the following list for a description of each term or clause: 


{column|expr} 	Identifies the data item (typically, the name of a column) in a SQL SELECT 
command to which the column command refers. If you use an expression in 
a COLUMN command, you must enter expr exactly as it appears in the 
SELECT command. If the expression in the SELECT command is a+b, for 
example, you cannot use b+a or (a+b) in a COLUMN command to refer to 
the expression in the SELECT command. 



	If you select columns with the same name from different tables, a COLUMN 
command for that column name will apply to both columns. That is, a 
COLUMN command for the column ENAME applies to all columns named 
ENAME that you reference in this session. COLUMN ignores table name 
prefixes in SELECT commands. Also, spaces are ignored unless the name 
is placed in double quotes. 



	To format the columns differently, assign a unique alias to each column 
within the SELECT command itself (do not use the ALIAS clause of the 
COLUMN command) and enter a COLUMN command for each column's 
alias. 



ALI[AS] alias  	Assigns a specified alias to a column, which can be used to refer to the 
column in BREAK, COMPUTE, and other COLUMN commands. 



CLE[AR]  	Resets the display attributes for the column to default values. 



COLOR {color|color_variable}  	 Is described in the SQL*Graph User's Guide. 



FOLD_A[FTER] 	Inserts a carriage return after the column heading and after each row in the 
column. 



FOLD_B[EFORE]	Inserts a carriage return before the column heading and before each row of 
the column. 



FOR[MAT] format 	Specifies the display format of the column. The format specification must 
be a text constant such as A10 or $9,999--not a variable.  



	Character Columns A CHAR or VARCHAR2 (VARCHAR) column's 
width defaults to the column's width as defined in the database or to the 
length of the column's heading, whichever is longer. SQL*Plus formats 
CHAR and VARCHAR2 (VARCHAR) data left-justified. If a value does not 
fit within the column width, SQL*Plus wraps or truncates the character 
string depending on the setting of SET WRAP. The width cannot exceed 
32,767 or the value set with SET MAXDATA. (VARCHAR2 requires 
ORACLE7.) 



	A LONG column's width defaults to the value of SET LONGCHUNKSIZE or 
SET LONG, whichever one is smaller. 



	A Trusted ORACLE column of datatype MLSLABEL or RAW MLSLABEL 
defaults to the width defined for the column in the database or the length of 
the column's heading, whichever is longer. The default display width for a 
Trusted ORACLE column of dataype ROWLABEL is 15. 



	To change the width of a CHAR, VARCHAR2 (VARCHAR), LONG, or 
Trusted ORACLE column to n, use FORMAT An. (A stands for 
alphanumeric.)  If you specify a width shorter than the column heading, 
SQL*Plus truncates the heading.  If you make the width of a LONG column 
greater than LONGCHUNKSIZE, LONGCHUNKSIZE is automatically 
increased to equal the column's width. 



	DATE Columns For ORACLE7, the default width for unformatted DATE 
columns in SQL*Plus is derived from the default format specified via 
initialization parameter in a parameter file. Otherwise, the default width is 
A9. 



	To change the format of a DATE column, use the SQL function TO_CHAR 
in your SQL SELECT command. When you use TO_CHAR, ORACLE 
automatically allows for a very wide column, so SQL*Plus automatically 
sets the column width to 80 characters. To reset the width of the column, 
use the COLUMN command with FORMAT An, where n is the desired 
display width. If n is shorter than the column heading, SQL*Plus truncates 
the heading.  For more information on TO_CHAR, see your ORACLE7 
Server SQL Language Reference Manual. 


Note: SQL calculations may cause a column to become very wide; in such cases you should also 
use the SQL*Plus COLUMN command to reset the column width. 


	NUMBER Columns    A NUMBER column's width defaults to the value of 
SET NUMWIDTH. To change the width, use FORMAT followed by an 
element as specified in Table 6-1. 




Element	Example(s)	Description
------------------------------------------------------------------------
9	9999	Determines the display 
		width by the number of 
		digits entered. Does not 
		display leading zeroes.

0	0999	Displays leading zeroes.

	9990	Displays zero instead of 
		a blank when a value is 
		zero.

$	$9999	Prefixes a dollar sign to 
		a value.

B	B9999	Displays a zero value as 

		blank.

MI	9999MI	Displays \"-\" after a neg-
		ative value. 

PR	9999PR	Displays a negative val-
		ue in angle brackets.

comma	9,999	Displays a comma in the 
		position indicated.

period	99.99	Aligns the decimal point 
		in the position indicated.

V	999V99	Multiplies value by 10n, 
		where n is the number of 
		\"9's\" after the \"V.\"

EEEE	9.999EEEE	Displays in scientific 

		notation (format must 
		contain exactly four 
		\"E's\").

DATE	DATE	Displays value as a date 
		in MM/DD/YY format; 
		used to format NUM-
		BER columns that repre-
		sent Julian dates.


Table 6 - 1.  
Number Formats





	SQL*Plus formats NUMBER data right-justified. The field width equals the 
width of the heading or the format plus one space for the sign, whichever is 
greater. If you specify a width shorter than the column heading, SQL*Plus 
truncates the heading. If a value does not fit within the column width, 
SQL*Plus displays an asterisk (*) in place of each digit the width allows to 
indicate overflow. 



	With all number formats, SQL*Plus rounds a number to the specified 
number of significant digits. When no format is given, a number's width 
defaults to the value of NUMWIDTH (see the SET command in this 
chapter). 



HEA[DING] text  	Defines a column heading. If you do not use a HEADING clause, the 
column's heading defaults to column or expr. If text contains blanks or 
punctuation characters, you must enclose it with single or double quotes. 
Each occurrence of the HEADSEP character (by default, '|') begins a new 
line. For example, 

 				COLUMN ENAME HEADING 'Employee |Name' 

	would produce a two-line column heading. See the HEADSEP variable of 
the SET command in this chapter for information on changing the 
HEADSEP character. 



JUS[TIFY] {L[EFT]|C[ENTER]|C[ENTRE]|R[IGHT]} 	 Aligns the heading.  If you do not use a 
JUSTIFY clause, NUMBER columns default to RIGHT and other column 
types default to LEFT. 



LIKE {expr|alias} 	 Copies the display attributes of another column or expression (whose 
attributes you have already defined with another COLUMN command). LIKE 
copies only attributes not defined by another clause in the current 
COLUMN command. 



LINEAPP {LINE|MARK|BOTH} 	 Is described in the SQL*Graph User's Guide. 



NEWL[INE] 	Starts a new line before displaying the column's value. NEWLINE has the 
same effect as FOLD_BEFORE n. 



NEW_V[ALUE] variable  	 Specifies a variable to hold a column value. You can reference the 
variable in TTITLE commands. Use NEW_VALUE to display column values 
or the date in the top title. You must include the column in a BREAK 
command with the SKIP PAGE action. The variable name cannot contain a 
pound sign (#). 



	NEW_VALUE is useful for master/detail reports in which there is a new 
master record for each page. For master/detail reporting, you must also 
include the column in the ORDER BY clause. See the example at the end of 
this command description. 



	For information on displaying a column value in the bottom title, see 
COLUMN OLD_VALUE. Refer to TTITLE for more information on 
referencing variables in titles. See COLUMN FORMAT for details on 
formatting and valid format models. 



NOPRI[NT]|PRI[NT]	 Controls the printing of the column (the column heading and all the 
selected values). NOPRINT turns the printing of the column off. PRINT 
turns the printing of the column on. 



NUL[L] char	Controls the text SQL*Plus displays for null values in the given column. If 
you do not use a NULL clause in the COLUMN command, SQL*Plus 
displays blanks for embedded null values, nothing for trailing null values, or 
the text to which you have set NULL using the SET command for all null 
values. (SET NULL controls the text displayed for all null values for all 
columns, unless overridden for a specific column by the NULL clause of the 
COLUMN command.) 



OLD_V[ALUE] variable  	 Specifies a variable to hold a column value.   You can reference the 
variable in BTITLE commands. Use OLD_VALUE to display column values 
or the date in the bottom title. You must include the column in a BREAK 
command with the SKIP PAGE action. 



	OLD_VALUE is useful for master/detail reports in which there is a new 
master record for each page. For master/detail reporting, you must also 
include the column in the ORDER BY clause. 



	For information on displaying a column value in the top title, see COLUMN 
NEW_VALUE. Refer to TTITLE for more information on referencing 
variables in titles. See COLUMN FORMAT for details on formatting and 
valid format models. 



ON|OFF   	Controls the status of display attributes for a column. OFF disables the 
attributes for a column without affecting the attributes' definition. ON 
reinstates the attributes. 



PATTERN {pattern_number|pattern_variable} 	 Is described in the SQL*Graph User's 
Guide.  



WRA[PPED]| WOR[D_WRAPPED]|TRU[NCATED] 	 Specifies how SQL*Plus will treat a CHAR string 
that is too wide for a column. WRAPPED wraps the end of the string to the 
next line. WORD_WRAP functions similarly to WRAPPED, but moves an 
entire word to the next line rather than splitting the word between two lines. 
TRUNCATED truncates the string at the end of the first line of display. 


 Usage Notes	

You can enter any number of COLUMN commands for one or more columns. All column attributes 
set for each column remain in effect for the remainder of the session, or until you turn the column 
OFF. Thus, the COLUMN commands you enter can control a column's display attributes for multiple 
SQL SELECT commands.

When you enter multiple COLUMN commands for the same column, SQL*Plus applies their clauses 
collectively. If several COLUMN commands apply the same clause to the same column, the last one 
entered will control the output.


 Examples	

To make the ENAME column 20 characters wide and display EMPLOYEE NAME on two lines at the 
top, enter: 



SQL> COLUMN ENAME FORMAT A20 HEADING 'EMPLOYEE |NAME' 

To format the SAL column so that it shows millions of dollars, rounds to cents, uses commas to 
separate thousands, and displays $0.00 when a value is zero, you would enter: 



SQL> COLUMN SAL FORMAT $9,999,990.99 

To assign the alias NET to a column containing a long expression, to display the result in a dollar 
format, and to display <NULL> for null values, you might enter: 



SQL> COLUMN SAL+COMM+BONUS-EXPENSES-INS-TAX ALIAS NET  SQL> COLUMN NET FORMAT 
$9,999,999.99 NULL '<NULL>' 

Note that the example divides this column specification into two commands. The first defines the 
alias NET, and the second uses NET to define the format. 

Also note that in the first command you must enter the expression exactly as you entered it (or will 
enter it) in the SELECT command. Otherwise, SQL*Plus cannot match the COLUMN command to 
the appropriate column. 


To wrap long values in a column named REMARKS, you can enter: 



SQL> COLUMN REMARKS FORMAT A20 WRAP 

For example: 



CUSTOMER	DATE	QUANTITY	REMARKS

----------	----------	----------	----------

123	25-AUG-86	144	This order 
			must be s
			hipped by 
			air freigh
			t to ORD




If you replace WRAP with WORD_WRAP, REMARKS looks like this: 



CUSTOMER	DATE	QUANTITY	REMARKS

----------	----------	----------	----------

123	25-AUG-86	144	This order 
			must be 
			shipped by 
			air freight 
			to ORD




If you specify TRUNCATE, REMARKS looks like this: 



CUSTOMER	DATE	QUANTITY	REMARKS

----------	----------	----------	----------

123	25-AUG-86	144	This order must be s




In order to print the current date and the name of each job in the top title, enter the following. (For 
details on creating a date variable through your SQL*Plus LOGIN file, see \"Displaying the Current 
Date in Titles\" under \"Defining Page Titles and Dimensions\" in Chapter 4.) 



SQL> COLUMN JOB NOPRINT NEW_VALUE JOBVAR

SQL> COLUMN TODAY  NOPRINT NEW_VALUE DATEVAR
SQL> BREAK ON JOB SKIP PAGE ON TODAY 
SQL> TTITLE CENTER 'Job Report' RIGHT DATEVAR  SKIP 2 -
> LEFT 'Job:     ' JOBVAR SKIP 2
SQL> SELECT TO_CHAR(SYSDATE, 'MM/DD/YY') TODAY,
  2  ENAME, JOB, MGR, HIREDATE, SAL, DEPTNO
  3  FROM EMP WHERE JOB IN ('CLERK', 'SALESMAN')
  4  ORDER BY JOB, ENAME;  

Your 2-page report would look similar to the following report, with \"Job Report\" centered within your 
current linesize: 




						Job Report	

05/01/88 
Job:     CLERK




ENAME	MGR	HIREDATE	SAL	DEPTNO

-------	----	--------------	------	-------

ADAMS	7788	14-JAN-87	1100	20

JAMES	7698	03-DEC-81	950	30

MILLER	7782	23-JAN-82	1300	10

SMITH	7902	17-DEC-80	800	20






						Job Report	

05/01/88 
Job:     CLERK




ENAME	MGR	HIREDATE	SAL	DEPTNO

-------	----	--------------	------	-------

ALLEN	7698	20-JAN-81	1600	20

MARTIN	7698	03-DEC-81	950	30

MILLER	7782	23-JAN-82	1300	10

SMITH	7902	17-DEC-80	800	20




 ________________________________________



Copyright (c) 1994, Oracle Corporation."))
("compute" . (nil "SQL*Plus command" "COMPUTE 



 Purpose	

Calculates and prints summary lines, using various standard computations, on subsets of selected 
rows. Or, lists all COMPUTE definitions. (For details on how to create summaries, see \"Clarifying 
Your Report with Spacing and Summary Lines\" in Chapter 4.) 

 Syntax	

COMP[UTE]	[function ... 		OF {expr|column|alias}...ON 
{expr|column|alias|REPORT|ROW}] 

 Terms and Clauses	

Refer to the following list for a description of each term or clause: 



function ...	Represents one of the functions listed in Table 6-2. If you specify more than 
one function, use spaces to separate the functions. (VARCHAR2 requires 
ORACLE7.) 




Function	Computes	Applies to Datatypes

AVG	Average of non-null values	NUMBER

COU[NT]	Count of non-null values 	all types

MAX[IMUM]	Maximum value	NUMBER, CHAR, 
		VARCHAR2
		(VARCHAR)

MIN[IMUM]	Minimum value	NUMBER, CHAR, 
		VARCHAR2
		(VARCHAR)

NUM(BER)	Count of rows	all types

STD	Standard diviation of non-null val-	NUMBER
	ues	

SUM	Sum of non-null values	NUMBER

VAR(IANCE)	Variance of non-null values	NUMBER


OF {expr|column|alias}...	

	Specifies the column(s) or expression(s) you wish to 
	use in the computation. (column cannot have a table 
	or view prepended to it. To achieve this, you can alias 
	the column in the SQL statement.)  You must also 
	specify these columns in the SQL SELECT command, 
	or SQL*Plus will ignore the COMPUTE command.
	
	If you do not want the computed values of a given 
	column to appear in the output of a SELECT com-

	mand, specify that column in a COLUMN command 
	with a NOPRINT clause. Use spaces to separate mul-
	tiple expressions, columns, or aliases within the OF 
	clause. 
	
	In the OF clause, you can refer to an expression or 
	function reference in the SELECT statement by plac-
	ing the expression or function reference in double 
	quotes. Column names and aliases do not need 
	quotes.

ON {expr|column|alias|REPORT|ROW}]       	

	Specifies the event SQL*Plus will use as a break. (col-

	umn cannot have a table or view prepended to it. To 
	achieve this, you can alias the column in the SQL 
	statement.)  COMPUTE prints the computed value 
	and restarts the computation when the event occurs 
	(i.e., when the value of the expression changes, a new 
	ROW is fetched, or the end of the report is reached). 
	If multiple COMPUTE commands reference the same 
	column in the ON clause, only the last COMPUTE 
	command applies. To reference a SQL SELECT ex-

	pression or function reference in an ON clause, place 
	the expression or function reference in quotes. Col-
	umn names and aliases do not need quotes.


Table 6 - 2.  
COMPUTE Functions




Enter COMPUTE without clauses to list all COMPUTE definitions. 

 Usage Notes	

In order for the computations to occur, the following must all be true: 


	ú	The expression, column, or column alias you reference in the ON clause must occur in the 
SELECT command. 



	ú	The expression, column, or column alias you reference in the ON clause must also occur in 
the most recent BREAK command. 



	ú	If you reference either ROW or REPORT in the ON clause, also reference ROW or REPORT 
in the most recent BREAK command. 



	ú	One or more of the expressions, columns, or column aliases you reference in the OF clause 
must also occur in the SELECT command. 


When you use COMPUTE with BREAK, the label for the computed value normally appears in the first 
column. However, if the COMPUTE is being done on the first column, you should create a dummy 
first column for the label with the COLUMN command. Otherwise, the label will not print. 

 Examples	

To subtotal the salary for the \"clerk,\" \"analyst,\" and \"salesman\" job classifications, enter: 



SQL> BREAK ON JOB SKIP 1 

SQL> COMPUTE SUM OF SAL ON JOB  
SQL> SELECT JOB, ENAME, SAL  
  2  FROM EMP
  3  WHERE JOB IN ('CLERK', 'ANALYST', 'SALESMAN')
  4  ORDER BY JOB, SAL;  

The following output results: 



JOB	ENAME	SAL

-----------	-----------	----------

ANALYST	SCOTT	3000

	FORD	3000

***********		----------

sum		6000

		

CLERK	SMITH	800

	JAMES	950

	ADAMS	1100

	MILLER	1300

***********		----------

sum		4150

		

SALESMAN	WARD	1250

	MARTIN	1250

	TURNER	1500

	ALLEN	1600

	WILSON	3000

***********		----------

sum		8600




To compute the average and maximum salary for the accounting and sales departments, enter: 



SQL> BREAK ON DNAME SKIP 1 

SQL> COMPUTE AVG MAX OF SAL ON DNAME 
SQL> SELECT DNAME, ENAME, SAL 
  2  FROM DEPT, EMP 
  3  WHERE DEPT.DEPTNO=EMP.DEPTNO 
  4  AND DNAME IN ('ACCOUNTING', 'SALES') 
  5  ORDER BY DNAME; 

The following output results: 



DNAME	ENAME	SAL

-----------	-----------	----------

ACCOUNTING	CLARK	2450

	KING	5000

	MILLER	1300

***********		----------

avg		2916.66667

maximum		5000

		

SALES	ALLEN	1600

	WARD	1250

	MARTIN	1250

	TURNER	1500

	JAMES	950

	BLAKE	2850

***********		----------

avg		1566.66667

maximum		2850




 ________________________________________



Copyright (c) 1994, Oracle Corporation."))
("connect" . (nil "SQL*Plus command" "CONNECT 



 Purpose	

Connects a given username to ORACLE.

 Syntax	

CONN[ECT] [logon] 

where: 


logon	Requires the following syntax: 
username[/password][@database_specification] | / 


 Terms and Clauses	

Refer to the following list for a description of each term or clause:


username [/password]	Represent the username and password with which you wish to connect to 
ORACLE. If you omit username and password, SQL*Plus prompts you 
for them. If you enter a slash (/) or simply enter [Return] to the prompt for 
username, SQL*Plus logs you on using a default logon (see \"/\" below). 



	If you omit only password, SQL*Plus prompts you for password. When 
prompting, SQL*Plus does not display password on your terminal screen. 



/	Represents a default (ops$) logon. You cannot enter a 
database_specification if you use a default logon. In a default logon 
SQL*Plus attempts to log you on using the username OPS$name, where 
name is your operating system username. 



database _specification 	Consists of a SQL*Net connection string. The exact syntax depends 
upon the SQL*Net communications protocol your Oracle installation uses. 
For more information, refer to the SQL*Net manual appropriate for your 
protocol or contact your DBA. SQL*Plus does not prompt for a database 
specification, but uses your default database if you do not include a 
specification. 


 Usage Notes	

CONNECT commits the current transaction to the database, disconnects the current username from 
ORACLE, and reconnects with the specified username.

 Examples	

To connect using username SCOTT and password TIGER to the default  database on the DECnet 
node \"corp\", enter: 



SQL> CONNECT SCOTT/TIGER@d:corp 

To connect using username SCOTT and let SQL*Plus prompt you for the password, enter: 



SQL> CONNECT SCOTT   

 ________________________________________



Copyright (c) 1994, Oracle Corporation."))
("copy" . (nil "SQL*Plus command" "COPY 



 Purpose	

Copies the data from a query to a table in a local or remote database. 

 Syntax	

COPY {FROM username[/password]@database_specification |     TO 
username[/password]@database_specification |     FROM 
username[/password]@database_specification TO      
username[/password]@database_specification}     {APPEND|CREATE|INSERT|REPLACE} 
destination_table      [(column, column, column ...)] USING query 

 Terms and Clauses	


Refer to the following list for a description of each term or clause: 


username[/password]	 Represent the ORACLE username/password you wish to COPY FROM 
and TO. In the FROM clause, username/password identifies the source of 
the data; in the TO clause, username/password  identifies the destination. 
If you do not specify password in either the FROM clause or the TO 
clause, SQL*Plus will prompt you for it. SQL*Plus suppresses the display 
of your response to these prompts. 



database_specification 	 Consists of a SQL*Net connection string. In the FROM clause, 
database_specification represents the database at the source; in the TO 
clause, database_specification  represents the database at the 
destination. The exact syntax depends upon the SQL*Net communications 
protocol your Oracle installation uses. For more information, refer to the 
SQL*Net manual appropriate for your protocol or contact your DBA. 



destination_table	 Represents the table you wish to create or to which you wish to add data. 



(column, column, column, <+>...) 	 Specifies the names of the columns in  destination_table. 
You must enclose a name in double quotes if it contains lower case letters 
or blanks. 



	If you specify columns, the number of columns must equal the number of 
columns selected by the query. If you do not specify any columns, the 
copied columns will have the same names in the destination table as they 
had in the source, if COPY creates destination_table. 



USING query	Specifies a SQL query (SELECT command) determining which rows and 
columns COPY copies. 



FROM username[/password]database_specification	 Specifies the username, password, and 
database  that contains the data to be copied. If you omit the FROM clause, 
the source defaults to the database SQL*Plus is connected to (i.e., the 
database that other commands address). You must include a FROM clause 
to specify a source database other than the default. 



TO username[/password]database_specification	 Specifies the database containing the 
destination table. If you omit the TO clause, the destination defaults to the 
database SQL*Plus is connected to (i.e., the database that other 
commands address). You must include a TO clause to specify a destination 
database other than the default. 



APPEND	Inserts the rows from query into destination_table if the table exists. If 
destination_table does not exist, COPY creates it. 



CREATE	Inserts the rows from query into destination_table after creating the table 
first. If destination_table already exists, COPY returns an error. 



INSERT	Inserts the rows from query into destination_table if the table exists. If 
destination_table does not exist, COPY returns an error. When using 
INSERT, the USING query  must select one column for each column in the 
destination_table. 



REPLACE 	Replaces destination_table and its contents with the rows from query.  If 
destination_table does not exist, COPY creates it. If destinationt_table 
does already exist, COPY drops the existing table and replaces it with a 
table containing the copied data. 


 Usage Notes	

To enable the copying of data between ORACLE and non-ORACLE databases, NUMBER columns 
are changed to DECIMAL columns in the destination table. Hence, if you are copying between 
ORACLE databases, a NUMBER column with no precision will be changed to a DECIMAL(38) 
column. When copying between ORACLE databases, you should use SQL commands (CREATE 
TABLE AS and INSERT) or you should ensure that your columns have a precision specified. 

The SQL*Plus SET variable LONG limits the length of LONG columns that you copy. If any LONG 
columns contain data longer than the value of LONG, COPY truncates the data. 


SQL*Plus performs a commit at the end of each successful COPY. If you set the SQL*Plus SET 
variable COPYCOMMIT to a positive value n, SQL*Plus performs a commit after copying every n 
batches of records. (The SQL*Plus SET variable ARRAYSIZE determines the size of the batch.) 

Some operating environments require that database specifications be placed in double quotes. 

 Examples	

The following command copies the entire EMP table to a table named WESTEMP. Note that the 
tables are located in two different databases. If WESTEMP already exists, SQL*Plus replaces the 
table and its contents. The columns in WESTEMP have the same names as the columns in the 
source table, EMP. 




SQL> COPY FROM SCOTT/TIGER@HQ TO JOHN/CHROME@WEST -  

> REPLACE WESTEMP -  
> USING SELECT * FROM EMP 

The following command copies selected records from EMP to the database to which SQL*Plus is 
connected. SQL*Plus creates SALESMEN through the copy. SQL*Plus copies only the columns 
EMPNO and ENAME and at the destination names them EMPNO and SALESMAN.  



SQL> COPY FROM SCOTT/TIGER@HQ -  

> CREATE SALESMEN (EMPNO,SALESMAN) - 
> USING SELECT EMPNO, ENAME FROM EMP -  
> WHERE JOB='SALESMAN'      



 ________________________________________



Copyright (c) 1994, Oracle Corporation."))
("define" . (nil "SQL*Plus command" "DEFINE 



 Purpose	

Specifies a user variable and assigns it a CHAR value. Or, lists the value and variable type of a single 
variable or all variables. 

 Syntax		

DEF[INE] [variable] |  [variable = text] 

 Terms and Clauses	

Refer to the following list for a description of each term or clause: 


variable	Represents the user variable whose value you wish to assign or list. 



text	Represents the CHAR value you wish to assign to variable. Enclose text 
in single quotes if it contains punctuation or blanks. 



variable = text	Defines (names) a user variable and assigns it a CHAR value. 


Enter DEFINE followed by variable to list the value and type of variable.  Enter DEFINE with no 
clauses to list the values and types of all user variables. 

 Usage Notes	

DEFINEd variables retain their values until one of the following events occurs: 


	ú	you enter a new DEFINE command referencing the variable 



	ú	you enter an UNDEFINE command referencing the variable 



	ú	you enter an ACCEPT command referencing the variable 



	ú	you reference the variable in the NEW_VALUE or OLD_VALUE clause of the COLUMN 
command and reference the column in a subsequent SQL SELECT command 



	ú	you EXIT SQL*Plus 


Whenever you run a stored query or command file, SQL*Plus substitutes the value of variable for 
each substitution variable referencing variable (in the form &variable or &&variable). SQL*Plus 
will not prompt you for the value of variable in this session until you UNDEFINE variable. 

You can DEFINE a maximum of 1024 variables. 

Note that you can use DEFINE to define the variable, _EDITOR, which establishes the host system 
editor invoked by the SQL*Plus EDIT command.  


If you continue the value of a DEFINEd variable on multiple lines (using the SQL*Plus command 
continuation character), SQL*Plus replaces each continuation character and carriage return you 
enter with a space in the resulting variable. For example, SQL*Plus interprets 



SQL> DEFINE TEXT = 'ONE- 

> TWO- 
> THREE' 

as: 



SQL> DEFINE TEXT = 'ONE TWO THREE' 

 Examples	

To assign the value MANAGER to the variable POS, type: 



SQL> DEFINE POS = MANAGER 

If you execute a command that contains a reference to &POS, SQL*Plus will substitute the value 
MANAGER for &POS and will not prompt you for a POS value. 

To assign the CHAR value 20 to the variable DEPTNO, type: 



SQL> DEFINE DEPTNO = 20 

Even though you enter the number 20, SQL*Plus assigns a CHAR value to DEPTNO consisting of 
two characters, 2 and 0. 

To list the definition of DEPTNO, enter: 



SQL> DEFINE DEPTNO

DEFINE DEPTNO          = \"20\" (CHAR) 

This result shows that the value of DEPTNO is 20.  

 ________________________________________



Copyright (c) 1994, Oracle Corporation."))
("del" . (nil "SQL*Plus command" "DEL 



 Purpose	

Deletes the current line of the buffer. 

 Syntax	

DEL 

 Usage Notes	

DEL makes the following line of the buffer (if any) the current line. To delete several consecutive 
lines, enter DEL several times. 

 Examples	

Assume the SQL buffer contains the following query: 



1  SELECT ENAME, DEPTNO 

2  FROM EMP 
3  WHERE JOB = 'SALESMAN' 
4* ORDER BY DEPTNO 

To make the line containing the WHERE clause the current line, you would enter: 



SQL> LIST 3 

  3* WHERE JOB = 'SALESMAN' 

To delete the WHERE clause, enter: 



SQL> DEL 

The SQL buffer now contains the following lines: 



1  SELECT ENAME, DEPTNO 

2  FROM EMP 
3* ORDER BY DEPTNO  

 ________________________________________



Copyright (c) 1994, Oracle Corporation."))
("describe" . (nil "SQL*Plus command" "DESCRIBE 



 Purpose	

Lists the column definitions for the specified table, view, or synonym or the specifications for the 
specified function, procedure, package, or package contents. 

 Syntax	

DESC[RIBE] {[user.]table[@database_link_name] [column] | 
[user.]object[.subobject]} 

 Terms and Clauses	

Refer to the following list for a description of each term or clause: 


user	Represents the user who owns table or object. If you omit user, 
SQL*Plus assumes you own table or object. 



table	Represents the table, view, or synonym you wish to describe. 



database_link_name	 Consists of the database link name corresponding to the database where 
table exists. For more information on which privileges allow access to 
another table in a different schema, refer to the ORACLE7 Server SQL 
Language Reference Manual. 



column	Represents the column in table that you wish to describe. 



object	Represents the function, procedure, or package you wish to describe. 



subobject	Represents the function or procedure in the package that you wish to 
describe. 


 Usage Notes	

The description for tables, views, and synonyms contains the following information: 


	ú	each column's name 



	ú	whether or not null values are allowed (NULL or NOT NULL) for each column 



	ú	datatype of columns--NUMBER, CHAR, VARCHAR2 (VARCHAR), LONG, DATE, 
MLSLABEL, or RAW MLSLABEL 



	ú	precision of columns (and scale, if any, for a numeric column) 


When you do a DESCRIBE, VARCHAR columns are returned with a type of VARCHAR2.

The description for functions, procedures, and packages contains the following: 


	ú	the type of PL/SQL (function, procedure, or package) 



	ú	the name of the function, procedure, or package 



	ú	the type of value returned (for functions) 



	ú	the argument names, types, whether they are input or output, and default values, if any 



	ú	the package contents, if describing a package 


 Example	

To describe the table EMP, enter: 



SQL> DESCRIBE EMP 

DESCRIBE lists the following information: 



Name	Null?	Type

------------------------------	--------	------------

EMPNO	NOT NULL	NUMBER(4)

ENAME		CHAR(10)

JOB		JOB(9)

MGR		NUMBER(4)

HIREDATE		DATE

SAL		NUMBER(7,2)

COMM		NUMBER(7,2)

DEPTNO		NUMBER(2)




To describe the package APACK, enter: 



SQL> DESCRIBE apack 

DESCRIBE lists the following information: 



PACKAGE apack AS PROCEDURE aproc (p1 varchar2) ; END apack; 

To describe the procedure APROC in the package APACK, enter: 



SQL> DESCRIBE apack.aproc 

DESCRIBE lists the following information: 



PROCEDURE apack.aproc 



Argument Name	Type	In/Out	Default?

------------------	--------	--------	---------

P1	CHAR	IN

P2	NUMBER	IN




 ________________________________________



Copyright (c) 1994, Oracle Corporation."))
("disconnect" . (nil "SQL*Plus command" "DISCONNECT 



 Purpose	

Commits pending changes to the database and logs the current username off ORACLE, but does not 
exit SQL*Plus. 

 Syntax	

DISC[ONNECT] 

 Usage Notes	

Use DISCONNECT within a command file to prevent user access to the database when you want to log 
the user off ORACLE but have the user remain in SQL*Plus. Use EXIT or QUIT to log off ORACLE and 
return control to your host computer's operating system. 

 Example	


Your command file might begin with a CONNECT command and end with a DISCONNECT, as shown 
below. 



SQL> GET MYFILE 

  1  CONNECT ... 
     . 
     . 
     . 
     . 
15* DISCONNECT  

 ________________________________________



Copyright (c) 1994, Oracle Corporation."))
("edit" . (nil "SQL*Plus command" "EDIT 



 Purpose	

Invokes a host operating system text editor on the contents of the specified file or on the contents of 
the buffer. 

 Syntax	

EDIT [file_name[.ext]]  

 Terms and Clauses	

Refer to the following list for a description of each term or clause: 


file_name[.ext] 	Represents the file you wish to edit (typically a command file). If you omit 
ext, SQL*Plus assumes the default command-file extension (normally 
SQL). For information on changing the default extension, see the SUFFIX 
variable of the SET command in this chapter. 


Enter EDIT with no file name to edit the contents of the SQL buffer with the host operating system 
text editor. 

 Usage Notes	

The user variable, _EDITOR, contains the name of the text editor invoked by EDIT. You can change 
the text editor by changing the value of _EDITOR. See DEFINE for information about changing the 
value of a user variable. If _EDITOR is undefined, EDIT attempts to invoke the default host operating 
system editor. 

EDIT alone places the contents of the buffer in a file named AFIEDT with the extension BUF (located 
in your current working directory) and invokes the text editor on the contents of the file. If the 
AFIEDT.BUF file already exists, it is overwritten with the contents of the buffer. If you do not specify a 
file name and the buffer is empty, EDIT returns an error message. When you save edited text, the 
text  is saved back into the buffer. 


To leave the editing session and return to SQL*Plus, terminate the editing session in the way 
customary for the text editor. 

 Example	

To edit the file REPORT with the extension SQL using your host operating system text editor, enter: 



SQL> EDIT REPORT  

 ________________________________________



Copyright (c) 1994, Oracle Corporation."))
("execute" . (nil "SQL*Plus command" "EXECUTE 



 Purpose	

Executes a single PL/SQL statement. The EXECUTE command is often useful when you want to 
execute a PL/SQL statement that references a stored procedure. For more information on PL/SQL, 
see your PL/SQL User's Guide and Reference. 

 Syntax	

EXE[CUTE] statement 

 Terms and Clauses	

Refer to the following list for a description of each term or clause: 


statement	Represents a PL/SQL statement. 


 Usage Notes	

If your EXECUTE command cannot fit on one line because of the PL/SQL statement, use the 
SQL*Plus continuation character (a hyphen) as shown in the example below. 

The length of the command and the PL/SQL statement cannot exceed the length defined by SET 
LINESIZE. 

 Examples	

The following EXECUTE command assigns a value to a variable: 



SQL> EXECUTE :n := 1 

The following EXECUTE command runs a PL/SQL statement which references a stored procedure: 



SQL> EXECUTE - 

:ID := EMP_MANAGEMENT.HIRE('BLAKE','MANAGER','KING',2990,'SALES') 

Note that the value returned by the stored procedure is being placed in a bind variable, :ID. For 
information on how to create a bind variable, see the VARIABLE command in this chapter.  

 ________________________________________



Copyright (c) 1994, Oracle Corporation."))
("exit" . (nil "SQL*Plus command" "EXIT 



 Purpose	

Commits all pending database changes, terminates SQL*Plus, and returns control to the operating 
system. 

 Syntax	

{EXIT|QUIT} [SUCCESS|FAILURE|WARNING|n|variable] 

 Terms and Clauses	

Refer to the following list for a description of each term or clause: 


{EXIT|QUIT}	Can be used interchangeably (QUIT is a synonym for EXIT). 



n	Represents an integer you specify as the return code. 



variable 	Represents a user-defined or system variable, such as SQL.SQLCODE. 
EXIT variable exits with the value of variable as the return code. 



SUCCESS	Exits normally. 



FAILURE	Exits with a return code indicating failure. 



WARNING	Exits with a return code indicating warning. 


EXIT with no clauses exits with a value of SUCCESS. 

 Usage Notes	

EXIT allows you to specify an operating system return code. This allows you to run SQL*Plus 
command files in batch mode and to detect programmatically the occurrence of an unexpected event. 
The manner of detection is operating system specific. See the Oracle installation and user's 
manual(s) provided for your operating system for details. 

The key words SUCCESS, WARNING, and FAILURE represent operating-system dependent values. 
On some systems, WARNING and FAILURE may be indistinguishable. 


Note: SUCCESS, FAILURE, and WARNING are not reserved words. 

For information on exiting conditionally, see the WHENEVER SQLERROR and WHENEVER 
OSERROR commands later in this chapter. 

 Example	

The following returns the error code of the last executed SQL command or PL/SQL block: 



SQL> EXIT SQL.SQLCODE 

The location of the return code depends on your system. Consult your DBA for information 
concerning how your operating system retrieves data from a program. 

 ________________________________________



Copyright (c) 1994, Oracle Corporation."))
("get" . (nil "SQL*Plus command" "GET 



 Purpose	

Loads a host operating system file into the buffer. 

 Syntax	

GET file_name[.ext] [LIS[T]|NOL[IST]] 

 Terms and Clauses	

Refer to the following list for a description of each term or clause: 


file_name[.ext] 	Represents the file you wish to load (typically a command file). If you do not 
specify a file extension, SQL*Plus assumes the default command-file 
extension (normally SQL). For information on changing the default 
extension, see the SUFFIX variable of the SET command in this chapter. 



LIS[T] 	Lists the contents of the file. 



NOL[IST] 	Suppresses the listing. 


 Usage Note	

If part of the filename you are specifying contains the word list or the word file, you need to put the 
name in double quotes because LIST and FILE are reserved words in this instance. 

 Example	

To load a file called YEARENDRPT with the extension SQL into the buffer, type: 



SQL> GET YEARENDRPT  

 ________________________________________



Copyright (c) 1994, Oracle Corporation."))
("help" . (nil "SQL*Plus command" "HELP 



 Purpose:	

Accesses the SQL*Plus help system. 

 Syntax	

HELP [topic] 

 Terms and Clauses	

Refer to the following list for a description of each term or clause: 


topic	Represents a SQL*Plus help topic. This can be a SQL*Plus command (e.g., 
COLUMN), a SQL statement (e.g., INSERT), a PL/SQL statement (e.g., IF), 
or another topic in the help system (e.g., comparison operators). 


Enter HELP without topic to get help on the help system. 

 Usage Notes	

You can only enter one topic after HELP. You can abbreviate the topic (e.g., COL for COLUMN). 
However, if you enter only an abbreviated topic and the abbreviation is ambiguous, SQL*Plus will 
display help for all topics that match the abbreviation. For example, if you entered: 



SQL> HELP COMP 

SQL*Plus would display help on COMPUTE followed by help on comparison operators. 

If you get a response indicating that help is not available, consult your database administrator. 

 Example	

To see a list of SQL*Plus commands and PL/SQL and SQL statements enter: 



SQL> HELP COMMANDS 

 ________________________________________



Copyright (c) 1994, Oracle Corporation."))
("host" . (nil "SQL*Plus command" "HOST 



 Purpose	

Executes a host operating system command without leaving SQL*Plus. 

 Syntax	

HO[ST] [command] 

 Terms and Clauses	

Refer to the following list for a description of each term or clause: 


command	Represents a host operating system command. 


Enter HOST without command to display an operating system prompt. You can then enter multiple 
operating system commands. For information on returning to SQL*Plus, refer to the Oracle 
installation and user's manual(s) provided for your operating system. 

 Usage Notes	

With some operating systems, you can use a \"$\" or another character instead of HOST. See the 
Oracle installation and user's manual(s) provided for your operating system for details. 


You may not have access to the HOST command, depending on your operating system. See the 
Oracle installation and user's manual(s) provided for your operating system or ask your DBA for 
more information. 

 Example	

To execute an operating system command, ls *.sql, enter: 



SQL> HOST ls *.sql  

 ________________________________________



Copyright (c) 1994, Oracle Corporation."))
("input" . (nil "SQL*Plus command" "INPUT 



 Purpose	

Adds one or more new lines of text after the current line in the buffer. 

 Syntax	

I[NPUT] [text] 

 Terms and Clauses	

Refer to the following list for a description of each term or clause: 


text	Represents the text you wish to add. To add a single line, enter the text of 
the line after the command INPUT, separating the text from the command 
with a space. To begin the line with one or more spaces, enter two or more 
spaces between INPUT and the first non-blank character of text. 


To add several lines, enter INPUT with no text. INPUT prompts you for each line. To leave INPUT, 
enter a null (empty) line. 

 Usage Notes	

If you enter at the command prompt a line number larger than the number of lines in the buffer, and 
follow the number with text, SQL*Plus adds the text in a new line at the end of the buffer. If you specify 
zero (0) for the line number and follow the zero with text, then SQL*Plus inserts the line at the 
beginning of the buffer (that line becomes line 1). 


 Examples	

Assume the SQL buffer contains the following command: 



1  SELECT ENAME, DEPTNO, SAL, COMM 

2  FROM EMP 

To add an ORDER BY clause to the query, enter: 



SQL> LIST 2  

  2* FROM   EMP 
SQL> INPUT ORDER BY ENAME 

LIST 2 ensures that line 2 is the current line. INPUT adds a new line containing the ORDER BY 
clause after the current line. The SQL buffer now contains the following lines: 



1  SELECT ENAME, DEPTNO, SAL, COMM  

2  FROM  EMP  
3* ORDER BY ENAME 
 

To add a two-line WHERE clause, enter: 



SQL> LIST 2  

  2* FROM EMP  
SQL> INPUT  
  3  WHERE JOB = 'SALESMAN'  
  4  AND COMM  500  
  5 

INPUT prompts you for new lines until you enter an empty line. The SQL buffer now contains the 
following lines: 



1  SELECT ENAME, DEPTNO, SAL, COMM  

2  FROM EMP  
3  WHERE JOB = 'SALESMAN'  
4  AND COMM  500  
5  ORDER BY ENAME  

 ________________________________________



Copyright (c) 1994, Oracle Corporation."))
("list" . (nil "SQL*Plus command" "LIST 



 Purpose	

Lists one or more lines of the buffer. 

 Syntax	

L[IST] [n|n m|n *|n LAST|*|* n|* LAST|LAST] 

 Terms and Clauses	

Refer to the following list for a description of each term or clause: 


n	Lists line n. 



n m	Lists lines n through m. 



n *	Lists line n through the current line. 



n LAST	Lists line n through the last line. 



*	Lists the current line. 



* n	Lists the current line through line n. 



* LAST	Lists the current line through the last line. 



LAST	Lists the last line. 


Enter LIST with no clauses to list all lines. 

 Usage Notes	

You can omit the space between LIST and n or *, but not between LIST and LAST. 

The last line listed becomes the new current line (marked by an asterisk). 

 Example	

To list the contents of the buffer, enter: 



SQL> L 

You will see a listing of all lines in the buffer, similar in form to the following: 



  1  SELECT ENAME, DEPTNO, JOB  

  2  FROM EMP  
  3  WHERE JOB = 'CLERK' 
  4* ORDER BY DEPTNO 

The asterisk indicates that line 4 is the current line. 

To list the second line only, enter: 



SQL> L 2 

You will then see this: 



  2* FROM EMP 

 

To list the current line (now line 2) to the last line, enter: 



SQL> L * LAST 

You will then see this: 



  2  FROM EMP 

  3  WHERE JOB = 'CLERK' 
  4* ORDER BY DEPTNO  

 ________________________________________



Copyright (c) 1994, Oracle Corporation."))
("pause" . (nil "SQL*Plus command" "PAUSE 



 Purpose	

Displays an empty line followed by a line containing text, then waits for the user to press [Return]. Or, 
displays two empty lines and waits for the user's response.  

 Syntax	

PAU[SE] [text]  

 Terms and Clauses	

Refer to the following list for a description of each clause or term: 


text	Represents the text you wish to display. 


Enter PAUSE followed by no text to display two empty lines. 

 Usage Notes	

Because PAUSE always waits for the user's response, it is best to use a message that tells the user 
explicitly to press [Return]. 

PAUSE reads input from the terminal (if a terminal is available) even when you have designated the 
source of the command input as a file. 

For information on pausing between pages of a report, see the PAUSE variable of the SET command 
later in this chapter. 


 Example	

To print \"Adjust paper and press RETURN to continue.\", and to have SQL*Plus wait for the user to 
press [Return], you might include the following PAUSE command in a command file: 



SQL> GET MYFILE 

1  SET PAUSE OFF 
2  PAUSE Adjust paper and press RETURN to continue. 
3  SELECT ...  

 ________________________________________



Copyright (c) 1994, Oracle Corporation."))
("print" . (nil "SQL*Plus command" "PRINT 



 Purpose	

Displays the current value of a bind variable. For more information on bind variables, see your 
PL/SQL User's Guide and Reference. 

 Syntax	

PRI[NT] variable

 Terms and Clauses	

Refer to the following list for a description of each clause or term: 


variable	Represents the name of the bind variable whose value you wish to display. 


 Usage Notes	

Bind variables are created using the VARIABLE command. For more information, see the VARIABLE 
command in this chapter. 

You can control the formatting of the PRINT output just as you would query output. See the formatting 
techniques described in Chapter 4. 

 Example	

The following is an example of a PRINT command: 



SQL> VARIABLE n NUMBER 

SQL> BEGIN 
  2   :n := 1; 
  3  END; 
SQL> PRINT n 
         N 
---------- 
         1  

 ________________________________________



Copyright (c) 1994, Oracle Corporation."))
("prompt" . (nil "SQL*Plus command" "PROMPT 



 Purpose	

Sends the specified message or a blank line to the user's screen. 

 Syntax	

PROMPT [text] 

 Terms and Clauses	

Refer to the following list for a description of each term or clause: 


text	Represents the text of the message you wish to display. If you omit text, 
PROMPT displays a blank line on the user's screen. 


 Usage Notes	

You can use this command in command files to give information to the user. 

 Example	

The following example shows the use of PROMPT in conjunction with ACCEPT in a command file 
called ASKFORDEPT. ASKFORDEPT contains the following SQL*Plus and SQL commands: 



PROMPT 

PROMPT Please enter a valid department 
PROMPT For example:  10, 20, 30, 40 
ACCEPT NEWDEPT NUMBER PROMPT 'DEPT:> ' 
SELECT DNAME FROM DEPT 
WHERE DEPTNO = &NEWDEPT 

Assume you run the file using START or @: 



SQL> @ASKFORDEPT 

SQL*Plus displays the following prompts: 



Please enter a valid department 

For example:  10, 20, 30, 40 
DEPT:> 

The end user enters a department number to the prompt DEPT:>. SQL*Plus lists the line containing 
&NEWDEPT before and after substitution, and then displays the department name corresponding to the 
number entered at the DEPT:> prompt.  

 ________________________________________



Copyright (c) 1994, Oracle Corporation."))
("remark" . (nil "SQL*Plus command" "REMARK 



 Purpose	

Begins a comment in a command file.  SQL*Plus does not interpret the comment as a command.

 Syntax	

REM[ARK] 

 Usage Notes	

The REMARK command must appear at the beginning of a line, and the comment ends at the end of 
the line. A line cannot contain both a comment and a command. 

For details on entering comments in command files using the SQL comment delimiters, /* ... */, or the 
ANSI/ISO comment delimiter, -- ..., refer to \"Placing Comments in Command Files\" in Chapter 3. 


 Example	

The following command file contains some typical comments: 



SQL> GET EMPSUM 

1  REM COMPUTE uses BREAK ON REPORT to break on end of table. 
2  BREAK ON REPORT 
3  COMPUTE SUM OF \"DEPARTMENT 10\" \"DEPARTMENT 20\" - 
4  \"DEPARTMENT 30\" \"TOTAL BY JOB\" ON REPORT 
5  REM Each column displays the sums of salaries by job for 
6  REM one of the departments 10, 20, 30. 
7  SELECT JOB, 
8         SUM( DECODE( DEPTNO, 10, SAL, 0)) \"DEPARTMENT 10\",
9         SUM( DECODE( DEPTNO, 20, SAL, 0)) \"DEPARTMENT 20\",

10        SUM( DECODE( DEPTNO, 30, SAL, 0)) \"DEPARTMENT 30\",
11        SUM(SAL) \"TOTAL BY JOB\" 
12  FROM EMP 
13* GROUP BY JOB  

 ________________________________________



Copyright (c) 1994, Oracle Corporation."))
("run" . (nil "SQL*Plus command" "RUN 



 Purpose	

Lists and executes the SQL command or PL/SQL block currently stored in the SQL buffer. 

 Syntax	

R[UN] 

 Usage Notes	

RUN causes the last line of the SQL buffer to become the current line. 

The slash command (/) functions similarly to RUN, but does not list the command in the SQL buffer 
on your screen. 

 Example	

Assume the SQL buffer contains the following query: 



SELECT DEPTNO FROM DEPT 

To RUN the query, enter: 



SQL> RUN 

The following output results: 



1* SELECT DEPTNO FROM DEPT  


    DEPTNO 
---------- 
        10 
        20 
        30 
        40  

 ________________________________________



Copyright (c) 1994, Oracle Corporation."))
("runform" . (nil "SQL*Plus command" "RUNFORM 



 Purpose	

Invokes a SQL*Forms application from within SQL*Plus. 

Note: You have access to this command only if your site chose this option while installing SQL*Plus. 

 Syntax	

RUNFORM [options] form_name 

 Usage Notes	

The RUNFORM syntax is the same in both SQL*Plus and SQL*Forms.  If you are already in SQL*Plus, 
you can invoke a form more quickly in this manner than by invoking a form from the system prompt, 
because you avoid a separate ORACLE logon. See your SQL*Forms Operator's Guide for details on 
the correct syntax. 


Note that when you use RUNFORM from within SQL*Plus, you may not specify a username/password 
(you retain your current connection to ORACLE). If you wish to use a different username/password, use 
the SQL*Plus CONNECT command to connect to the desired ORACLE username prior to issuing the 
RUNFORM command. 

 Example	

To run a form named MYFORM, enter: 



SQL> RUNFORM MYFORM  

 ________________________________________



Copyright (c) 1994, Oracle Corporation."))
("save" . (nil "SQL*Plus command" "SAVE 



 Purpose	

Saves the contents of the buffer in a host operating system file (a command file). 

 Syntax	

SAV[E] file_name[.ext] [CRE[ATE]|REP[LACE]|APP[END]]  

 Terms and Clauses	

Refer to the following list for a description of each term or clause: 


file_name[.ext] 	Specifies the command file in which you wish to save the buffer's contents. 
If you do not specify an extension, SQL*Plus assumes the default 
command-file extension (normally SQL). For information on changing this 
default extension, see the  SUFFIX variable of the SET command in this 
chapter. 



	If you wish to SAVE a file under a name identical to a SAVE command 
clause (CREATE, REPLACE, or APPEND), you must specify a file 
extension. 



CRE[ATE]	Creates the file if the file does not exist. 



REP[LACE]	Replaces the contents of an existing file. If the file does not exist, 
REPLACE creates the file. 



APP[END]	Adds the contents of the buffer to the end of the file you specify. 


 Usage Notes	

When you SAVE the contents of the SQL buffer, SAVE adds a line containing a slash (/) to the end of 
the file. 

If part of the filename you are specifying contains the word file, you need to put the name in double 
quotes because FILE is a reserved word in this instance. 

 Example	

To save the contents of the buffer in a file named DEPTSALRPT with the extension SQL, enter: 



SQL> SAVE DEPTSALRPT 

To save the contents of the buffer in a file named DEPTSALRPT with the extension OLD, enter: 



SQL> SAVE DEPTSALRPT.OLD  



 ________________________________________



Copyright (c) 1994, Oracle Corporation."))
("set" . (nil "SQL*Plus command" "SET 



 Purpose	

Establishes an aspect of the SQL*Plus environment for your current session, such as: 


	ú	setting the display width for NUMBER data 



	ú	setting the display width for LONG data 



	ú	enabling or disabling the printing of column headings 



	ú	setting the number of lines per page 


 Syntax	

SET system_variable value 

where system_variable value represents a system variable followed by a value as shown below: 



ARRAY[SIZE] {20|n} 

AUTO[COMMIT] {OFF|ON|IMM[EDIATE]} 
BLO[CKTERMINATOR] {.|c} 
CMDS[EP] {;|c|OFF|ON} 
COM[PATIBILITY] {V5|V6|V7|NATIVE} 
CON[CAT] {.|c|OFF|ON} 
COPYC[OMMIT] {0|n} 
CRT crt 
DEF[INE] {'&'|c|OFF|ON} 
ECHO {OFF|ON} 
EMBEDDED {OFF|ON} 
ESC[APE] {\|c|OFF|ON} 
FEED[BACK] {6|n|OFF|ON} 
FLU[SH] {OFF|ON} 
HEA[DING] {OFF|ON} 
HEADS[EP] {||c|OFF|ON} 
LIN[ESIZE] {80|n} 
LONG {80|n} 
LONGC[HUNKSIZE] {80|n} 
MAXD[ATA] n 
NEWP[AGE] {1|n} NULL text 
NULL text
NUMF[ORMAT] format 

NUM[WIDTH] {10|n} 
PAGES[IZE] {14|n} 
PAU[SE] {OFF|ON|text} 
RECSEP {WR[APPED]|EA[CH]|OFF} 
RECSEPCHAR {_|c} 
SCAN {OFF|ON} 
SERVEROUT[PUT] {OFF|ON} [SIZE n] 
SHOW[MODE] {OFF|ON} 
SPA[CE] {1|n} 
SQLC[ASE] {MIX[ED]|LO[WER]|UP[PER]} 
SQLCO[NTINUE] {> |text} 
SQLN[UMBER] {OFF|ON} 
SQLPRE[FIX] {#|c} 
SQLP[ROMPT] {SQL>|text} 
SQLT[ERMINATOR] {;|c|OFF|ON}| 
SUF[FIX] {SQL|text} 
TAB {OFF|ON} 
TERM[OUT] {OFF|ON} 
TI[ME] {OFF|ON} 
TIMI[NG] {OFF|ON} 
TRIM[OUT] {OFF|ON} 

UND[ERLINE] {-|c|ON|OFF} 
VER[IFY] {OFF|ON} 
WRA[P] {OFF|ON} 


 Terms and Clauses	

Refer to the following list for a description of each term, clause, or system variable: 


ARRAY[SIZE] {20|n}	 Sets the number of rows--called a batch--that SQL*Plus will fetch from 
the database at one time. Valid values are 1 to 5000. A large value 
increases the efficiency of queries and subqueries that fetch many rows, 
but requires more main memory in the host computer. Values over 
approximately 100 provide little added performance. ARRAYSIZE has no 
effect on the results of SQL*Plus operations other than increasing 
efficiency. 



AUTO[COMMIT] {OFF|ON|IMM[EDIATE]}	 Controls when ORACLE commits pending changes to the 
database. ON commits pending changes to the database after ORACLE 
executes each SQL command or PL/SQL block. OFF suppresses 
automatic committing, so that you must commit changes manually (for 
example, with the SQL command COMMIT). IMMEDIATE functions in the 
same manner as the ON option. 



BLO[CKTERMINATOR] {.|c}	 Sets the non-alphanumeric character used to end PL/SQL blocks to 
c. To execute the block, you must issue a RUN or / (slash) command. 



CMDS[EP] {;|c|OFF|ON}  	 Sets the non-alphanumeric character used to separate multiple 
SQL*Plus commands entered on one line to c. ON or OFF controls 
whether you can enter multiple commands on a line; ON automatically sets 
the command separator character to a semicolon (;).  



COM[PATIBILITY] {V5|V6|V7|NATIVE}  	 Specifies the version of ORACLE to which you are 
currently connected. Set COMPATIBILITY to V5 for ORACLE Version 5; set 
it to V6 for ORACLE Version 6; set it to V7 (or V6) for ORACLE7; set it to 
NATIVE if you wish the database to determine the setting (e.g., if connected 
to ORACLE7, COMPATIBILITY would default to V7). COMPATIBILITY must 
be correctly set for the version of ORACLE to which you are connected, 
otherwise you will be unable to run any SQL commands. Note that you can 
set COMPATIBILITY to V6 or V7 when connected to ORACLE7. (This 
enables you to run ORACLE Version 6 SQL against ORACLE7.) 



	COMPATIBILITY also controls whether SQL*Plus stores the COMMIT and 
ROLLBACK commands in the SQL buffer. V5 does not store COMMIT and 
ROLLBACK in the SQL buffer; V6 and V7 do. Refer to the ORACLE7 
Server SQL Language Reference Manual for information on COMMIT and 
ROLLBACK. 



	Setting COMPATIBILITY to V6 and V7 affects how SQL*Plus handles 
character data. Setting COMPATIBILITY to V6 causes SQL*Plus to treat 
CHAR column values as variable length character strings. Setting 
COMPATIBILITY to V7 causes SQL*Plus to treat CHAR column values as 
fixed length character strings and VARCHAR2 (VARCHAR) column values 
as variable length character strings. 



CON[CAT] {.|c|OFF|ON}  	 Sets the character you can use to terminate a substitution variable 
reference if you wish to immediately follow the variable with a character that 
SQL*Plus would otherwise interpret as a part of the substitution variable 
name. SQL*Plus resets the value of CONCAT to a period when you switch 
CONCAT on. 



COPYC[OMMIT] {0|n} 	 Controls the number of batches after which the COPY command commits 
changes to the database.  COPY commits rows to the destination database 
each time it copies n row batches. Valid values are 0 to 5000. You can set 
the size of a batch with the ARRAYSIZE variable. If you set COPYCOMMIT 
to 0, COPY performs a commit only at the end of a copy operation. 



CRT crt 	Changes the default CRT file used in the SQL*Plus RUNFORM command. 
To return to the original default (before CRT was set), set CRT to nothing by 
entering two double quotes (\"\") for crt. 



	If you want to use NEW.CRT during a form invocation on a system where 
the default CRT is OLD.CRT, you can either invoke the form by: 



	SQL> RUNFORM -c NEW form_name 



	or 



	SQL> SET CRT NEW  SQL> RUNFORM  form_name 



	The second method stores the CRT option so that you do not need to 
re-specify it for subsequent RUNFORM commands during the same 
SQL*Plus session. 



DEF[INE] {'&'|c|OFF|ON}  	 Sets the character used to prefix substitution variables to c. ON or 
OFF controls whether SQL*Plus will scan commands for substitution 
variables and replace them with their values. The setting of DEFINE to ON 
or OFF overrides the setting of the SCAN variable. 



ECHO {OFF|ON} 	Controls whether the START command lists each command in a command 
file as the command is executed. ON lists the commands; OFF suppresses 
the listing. 



EMBEDDED {OFF|ON} 	 Controls where on a page each report begins. OFF forces each report to 
start at the top of a new page. ON allows a report to begin anywhere on a 
page. Set EMBEDDED to ON when you want a report to begin printing 
immediately following the end of the previously run report. 



ESC[APE] {\|c|OFF|ON} 	 Defines the character you enter as the escape character. OFF 
undefines the escape character. ON enables the escape character. 



	You can use the escape character before the substitution character (set 
through SET DEFINE) to indicate that SQL*Plus should treat the 
substitution character as an ordinary character rather than as a request for 
variable substitution.  



FEED[BACK] {6|n|OFF|ON} 	 Displays the number of records returned by a query when a query 
selects at least n records. ON or OFF turns this display on or off. Turning 
feedback ON sets n to 1. Setting feedback to 0 is equivalent to turning it 
OFF. 



FLU[SH] {OFF|ON} 	 Controls when output is sent to the user's display device. OFF allows the 
host operating system to buffer output. ON disables buffering. 



	Use OFF only when you run a command file non-interactively (i.e., when you 
do not need to see output and/or prompts until the command file finishes 
running). The use of FLUSH OFF may improve performance by reducing 
the amount of program I/O. 



HEA[DING] {OFF|ON}	 Controls printing of column headings in reports. ON prints column 
headings in reports; OFF suppresses column headings. 



HEADS[EP] {||c|OFF|ON} 	 Defines the character you enter as the heading separator 
character. You can use the heading separator character in the COLUMN 
command and in the old forms of BTITLE and TTITLE to divide a column 
heading or title onto more than one line. ON or OFF turns heading 
separation on or off. When heading separation is OFF, SQL*Plus prints a 
heading separator character like any other character. 



LIN[ESIZE] {80|n}	 Sets the total number of characters that SQL*Plus displays on one line 
before beginning a new line. It also controls the position of centered and 
right-aligned text in TTITLE and BTITLE. You can define LINESIZE as a 
value from 1 to a maximum that is system dependent. Refer to the Oracle 
installation and user's manual(s) provided for your operating system. 



LONG {80|n}	Sets maximum width (in characters) for displaying and copying LONG 
values. For ORACLE7, the maximum value of n is two gigabytes. For 
ORACLE Version 6, the maximum is 32,767.  



LONGC[HUNKSIZE] {80|n}    	 Sets the size (in characters) of the increments in which SQL*Plus 
retrieves a LONG value. When retrieving a LONG value, you may want to 
retrieve it in increments rather than all at once because of memory size 
restrictions. Valid values are 1 to whatever has been set with MAXDATA. 
LONGCHUNKSIZE applies only to ORACLE7. 



MAXD[ATA] n 	Sets the maximum total row width that SQL*Plus can process. The default 
and maximum values of n vary in different operating systems. Consult the 
ORACLE installation and user's manual(s) provided for your operating 
system or your DBA for details.  Note that MAXDATA has no effect when 
using ORACLE7. 



NEWP[AGE] {1|n}	Sets the number of blank lines to be printed between the beginning of each 
page and the top title. A value of 0 sends a formfeed between pages and 
clears the screen on most terminals. 



NULL text	Sets the text that represents a null value in the result of a SQL SELECT 
command. NULL without text displays the default for null values--a blank 
for embedded null values and nothing for trailing null values. Use the NULL 
clause of the COLUMN command to override the setting of the NULL 
variable for a given column. 



NUMF[ORMAT] format	 Sets the default format for displaying numbers. Enter a number format for 
format. For number format descriptions, see the FORMAT clause of the 
COLUMN command in this chapter. 



NUM[WIDTH] {10|n}	 Sets the default width for displaying numbers. 



PAGES[IZE] {14|n}	 Sets the number of lines from the top title to the end of the page. For 
reports printed on paper 11 inches long, a value of 54 (plus a NEWPAGE 
value of 6) leaves one-inch margins above and below the output. 



	You can set PAGESIZE to 0 to suppress all headings, page breaks, titles, 
the initial blank line, and other formatting information.  



PAU[SE] {OFF|ON|text}	 Allows you to control scrolling of your terminal when running 
reports. You must press [Return] after seeing each pause. ON causes 
SQL*Plus to pause at the beginning of each page of report output.  The 
text you enter specifies the text to be displayed each time SQL*Plus 
pauses. If you enter multiple words, you must enclose text in single 
quotes. 



	You can embed terminal-dependent escape sequences in the PAUSE 
command. These sequences allow you to create inverse video messages 
or other effects on terminals that support such characteristics. 



RECSEP {WR[APPED]|EA[CH]|OFF} and RECSEPCHAR { |c} 	  Display or print record separators. 
A record separator consists of a single line of the RECSEPCHAR (record 
separating character) repeated LINESIZE times. 



	RECSEPCHAR defines the record separating character. A  blank space is 
the default for RECSEPCHAR. 



	RECSEP tells SQL*Plus where to make the record separation. For 
example, if you set RECSEP to WRAPPED, SQL*Plus prints a record 
separator only after wrapped lines. If you set RECSEP to EACH, SQL*Plus 
prints a record separator following every row. If you set RECSEP to OFF, 
SQL*Plus does not print a record separator. 



SCAN {OFF|ON}	Controls scanning for the presence of substitution variables and 
parameters. OFF suppresses processing of substitution variables and 
parameters; ON allows normal processing. 



SERVEROUT[PUT] {OFF|ON} [SIZE n]	 Controls whether to display the output (i.e., 
DBMS_OUTPUT.PUT_LINE) of stored procedures in SQL*Plus. OFF 
suppresses the output of DBMS_OUTPUT.PUT_LINE;  ON displays the 
output. 



	SIZE sets the number of bytes of the output that can be buffered within the 
ORACLE7 Server. The default for n is 2000. n cannot be less than 2000 
or greater than 1,000,000. 



	For more information on DBMS_OUTPUT.PUT_LINE, see your ORACLE7 
Server Application Developer's Guide. 



SHOW[MODE] {OFF|ON}	 Controls whether SQL*Plus lists the old and new settings of a SQL*Plus 
system variable when you change the setting with SET. ON lists the 
settings; OFF suppresses the listing. BOTH functions in the same manner 
as ON. 



SPA[CE] {1|n} 	Sets the number of spaces between columns in output. The maximum value 
of n is 10. 



SQLC[ASE] {MIX[ED]|LO[WER]|UP[PER]} 	 Converts the case of SQL commands and PL/SQL 
blocks just prior to execution. SQL*Plus converts all text within the 
command, including quoted literals and identifiers, as follows: 



	ú	uppercase if SQLCASE equals UPPER 



	ú	lowercase if SQLCASE equals LOWER 



	ú	unchanged if SQLCASE equals MIXED 



	SQLCASE does not change the SQL buffer itself. 



SQLCO[NTINUE] {> |text} 	 Sets the character sequence SQL*Plus displays as a prompt after 
you continue a SQL*Plus command on an additional line using a hyphen (-). 



SQLN[UMBER] {OFF|ON}	 Sets the prompt for the second and subsequent lines of a SQL command 
or PL/SQL block. ON sets the prompt to be the line number. OFF sets the 
prompt to the value of SQLPROMPT. 



SQLPRE[FIX] {#|c}	 Sets the SQL*Plus prefix character. While you are entering a SQL 
command or PL/SQL block, you can enter a SQL*Plus command on a 
separate line, prefixed by the SQL*Plus prefix character. SQL*Plus will 
execute the command immediately without affecting the SQL command or 
PL/SQL block that you are entering. The prefix character must be a 
non-alphanumeric character. 



SQLP[ROMPT] {SQL>|text}	 Sets the SQL*Plus command prompt.  



SQLT[ERMINATOR] {;|c|OFF|ON}	 Sets the character used to end and execute SQL 
commands to c. OFF means that SQL*Plus recognizes no command 
terminator; you  terminate a SQL command by entering an empty line. ON 
resets the terminator to the default semicolon (;). 



SUF[FIX] {SQL|text}	 Sets the default file suffix (extension) that SQL*Plus uses in commands 
that refer to command files. SUFFIX does not control extensions for output 
(spool) files. 



TAB {OFF|ON} 	Determines how SQL*Plus formats white space in terminal output.  OFF 
uses spaces to format white space in the output. ON uses  the TAB 
character. The default value for TAB is system-dependent. Note that this 
option applies only to terminal output. Tabs will not be placed in output files. 
Enter SHOW TAB to see the default value. 



TERM[OUT] {OFF|ON}	 Controls the display of output generated by commands executed from a 
file. OFF suppresses the display so that you can spool output from a 
command file without seeing the output on the screen. ON displays the 
output. TERMOUT OFF does not affect output from commands you enter 
interactively. 



TI[ME] {OFF|ON}	Controls the display of the current time. ON displays the current time before 
each command prompt. OFF suppresses the time display. 



TIMI[NG] {OFF|ON}	 Controls the display of timing statistics. ON displays timing statistics on 
each SQL command or PL/SQL block run. OFF suppresses timing of each 
command. For information about the data SET TIMING ON displays, see 
the Oracle installation and user's manual(s) provided for your operating 
system.  Refer to the TIMING command for information on timing multiple 
commands. 



TRIM[OUT] {OFF|ON}	 Determines whether SQL*Plus allows trailing blanks at the end of each 
displayed line. ON removes blanks at the end of each line, improving 
performance especially when you access SQL*Plus from a slow 
communications device. OFF allows SQL*Plus to display trailing blanks. 
TRIMOUT ON does not affect spooled output; SQL*Plus ignores TRIMOUT 
ON unless you set TAB ON. 



UND[ERLINE] {-|c|ON|OFF}	 Sets the character used to underline column headings in SQL*Plus 
reports to c. ON or OFF turns underlining on or off without affecting the 
value of c. 



VER[IFY] {OFF|ON}	 Controls whether SQL*Plus lists the text of a command before and after 
SQL*Plus replaces substitution variables with values. ON lists the text; 
OFF suppresses the listing. 



WRA[P] {OFF|ON}	Controls whether SQL*Plus truncates the display of a data item if it is too 
long for the current line width. OFF truncates the data item; ON allows the 
data item to wrap to the next line. 



	Use the WRAPPED and TRUNCATED clauses of the COLUMN command 
to override the setting of WRAP for specific columns. 


 Usage Notes	

SQL*Plus maintains system variables (also called SET command variables) to allow you to establish 
a particular environment for a SQL*Plus session. You can change these system variables with the 
SET command and list them with the SHOW command. 

SET ROLE and SET TRANSACTION are SQL commands (see the  ORACLE7 Server SQL 
Language Reference Manual for more information). When not followed by the keywords 
TRANSACTION or ROLE, SET is assumed to be a SQL*Plus command. 


 Examples	

The following examples show sample uses of selected SET command variables. 

 COMPATIBILITY	

To run a command file, SALARY.SQL, created with Version 5 of ORACLE, enter: 



SQL> SET COMPATIBILITY V5 

SQL> START SALARY 

After running the file, reset compatibility to V7 to run command files created with ORACLE7: 



SQL> SET COMPATIBILITY V7 

Alternatively, you can add the command SET COMPATIBILITY V5 to the beginning of the command 
file, and reset COMPATIBILITY to V7 at the end of the file. 

 ESCAPE	

If you define the escape character as an exclamation point (!), then 



SQL> ACCEPT v1 PROMPT 'Enter !&1:' 

displays this prompt: 



Enter &1: 

 HEADING	

To suppress the display of column headings in a report, enter: 



SQL> SET HEADING OFF 

If you then run a SQL SELECT command, 



SQL> SELECT ENAME, SAL FROM EMP   2  WHERE JOB = 'CLERK'; 

the following output results: 



ADAMS			1100 

JAMES			950 
MILLER		1300 

 LONG	

To set the maximum width for displaying and copying LONG values to 500, enter: 



SQL> SET LONG 500 

The LONG data will wrap on your screen; SQL*Plus will not truncate until the 501st character. 

 LONGCHUNKSIZE	

To set the size of the increments in which SQL*Plus retrieves LONG values to 100 characters, enter: 



SQL> SET LONGCHUNKSIZE 100 

The LONG data will be retrieved in increments of 100 characters until the entire value is retrieved. 

 SERVEROUTPUT	

To enable the display of DBMS_OUTPUT.PUT_LINE, enter: 



SQL> SET SERVEROUTPUT ON 

The following shows what happens when you execute an anonymous procedure with SET 
SERVEROUTPUT ON: 



SQL> BEGIN 

2    DBMS_OUTPUT.PUT_LINE('Task is complete'); 
3    END; 
4    / 
Task is complete. 

PL/SQL procedure successfully completed. 

The following shows what happens when you create a trigger with SET SERVEROUTPUT ON: 



SQL> CREATE TRIGGER SERVER_TRIG BEFORE INSERT OR UPDATE OR DELETE 

2    ON SERVER_TAB 
3    BEGIN 
4    DBMS_OUTPUT.PUT_LINE('Task is complete.'); 
5    END; 
6    / 
Trigger created. 
SQL> INSERT INTO SERVER_TAB VALUES ('TEXT'); 
Task is complete. 
1 row created. 

 SQLCONTINUE	

To set the SQL*Plus command continuation prompt to an exclamation point followed by a space, 
enter: 



SQL> SET SQLCONTINUE '! ' 

SQL*Plus will prompt for continuation as follows: 



SQL> TTITLE 'YEARLY INCOME' - 

! RIGHT SQL.PNO SKIP 2 - 
! CENTER 'PC DIVISION' 
SQL> 

 SUFFIX	

To set the default command-file extension to UFI, enter: 



SQL> SET SUFFIX UFI 

If you then enter 



SQL> GET EXAMPLE 

SQL*Plus will look for a file named EXAMPLE with an extension of UFI instead of EXAMPLE with an 
extension of SQL..

 ________________________________________



Copyright (c) 1994, Oracle Corporation."))
("show" . (nil "SQL*Plus command" "SHOW 



 Purpose	

Lists the value of a SQL*Plus system variable. 

 Syntax	

SHO[W] option 

where option represents one of the following terms or clauses: 



system_variable 

ALL 
BTI[TLE] 
ERR[ORS] [{FUNCTION|PROCEDURE|PACKAGE|PACKAGE BODY| 
   TRIGGER|VIEW} name] 
LABEL 
LNO 
PNO 
REL[EASE] 
SPOO[L] 
SQLCODE 
TTI[TLE] 
USER 


 Terms and Clauses	

Refer to the following list for a description of each term or clause: 


system_variable	Represents any system variable set by the SET command. 



ALL	Lists the settings of all SHOW options. 



BTI[TLE] 	Shows the current BTITLE definition. 



ERR[ORS] [{FUNCTION|PROCEDURE|PACKAGE|PACKAGE BODY|     TRIGGER|VIEW} name]	

Shows the compilation errors of a stored procedure (includes stored functions, procedures, and 
packages). After you use the CREATE command to create a stored 
procedure, a message is displayed if the stored procedure has any 
compilation errors. To see the errors, you use SHOW ERRORS. 



	When you specify SHOW ERRORS with no arguments, SQL*Plus shows 
compilation errors for the most recently created or altered stored 
procedure. When you specify the type (function, procedure, package, 
package body, trigger, or view) and the name of the PL/SQL stored 
procedure, SQL*Plus shows errors for that stored procedure. For more 
information on compilation errors, see your PL/SQL User's Guide and 
Reference.  



	SHOW ERRORS ouput displays the line and column number of the error 
(LINE/COL) as well as the error itself (ERROR). LINE/COL and ERROR 
have default widths of 8 and 65, respectively. You can alter these widths 
using the COLUMN command. 



LABEL	Shows the security level for the current session. For more information, see 
your Trusted ORACLE Administrator's Guide. 



LNO 	Shows the current line number (the position in the current page of the 
display and/or spooled output). 



PNO 	Shows the current page number. 



REL[EASE] 	Shows the release number of ORACLE that SQL*Plus is accessing. 



SPOO[L] 	Shows whether output is being spooled. 



SQLCODE 	Shows the value of SQL.SQLCODE (for example, the SQL return code of 
the most recent operation). 



TTI[TLE] 	Shows the current TTITLE definition. 



USER 	Shows the username under which you are currently accessing SQL*Plus. 


 Usage Notes	

SHOW ERRORS is translated into a SQL command and, as a result, overwrites the contents of the 
SQL buffer. This means the previously entered SQL statement is overwritten. 

 Example	

To list the current LINESIZE, enter: 



SQL> SHOW LINESIZE 

If the current linesize equals 80 characters, SQL*Plus will give the following response: 



linesize 80  

Following is an example of how to create a stored procedure and then show its compilation errors: 



SQL> CREATE PROCEDURE ASSIGNVL AS BEGIN zzzzzzz; END; 

  2 / 
Warning: Procedure created with compilation errors. 
SQL> SHOW ERRORS PROCEDURE ASSIGNVL 



LINE/COL	ERROR

--------	-------------------------------------------------

1/26	PL/SQL: Statement ignored

1/26	PLS-00201: identifier 'ZZZZZZZ' must be declared




Note: Since the procedure ASSIGNVL was the  most recently created/altered stored procedure, you 
could just type SHOW ERRORS with no arguments to see its compilation errors.  

 ________________________________________



Copyright (c) 1994, Oracle Corporation."))
("spool" . (nil "SQL*Plus command" "SPOOL 



 Purpose	

Stores query results in an operating system file and, optionally, sends the file to a default printer.  
Also lists the current spooling status. 

 Syntax	

SPO[OL] [file_name[.ext]|OFF|OUT] 

 Terms and Clauses	

Refer to the following list for a description of each term or clause: 


file_name[.ext]	Represents the name of the file to which you wish to spool. SPOOL 
followed by file_name begins spooling displayed output to the named file. If 
you do not specify an extension, SPOOL uses a default extension (LST or 
LIS on most systems). 



OFF	Stops spooling. 



OUT	Stops spooling and sends the file to your host computer's standard 
(default) printer. 


Enter SPOOL with no clauses to list the current spooling status. 

 Usage Notes	

To spool output generated by commands in a command file without displaying the output on the 
screen, use SET TERMOUT OFF. SET TERMOUT OFF does not affect output from commands run 
interactively. 

The filename for SPOOL cannot contain the caret (^) character. 

 Examples	

To record your displayed output in a file named DIARY using the default file extension, enter: 




SQL> SPOOL DIARY 

To stop spooling and print the file on your default printer, type: 



SQL> SPOOL OUT  

 ________________________________________



Copyright (c) 1994, Oracle Corporation."))
("sqlplus" . (nil "SQL*Plus command" "SQLPLUS 



 Purpose	

Starts SQL*Plus from the operating system prompt. 

 Syntax	

SQLPLUS [[-S[ILENT]] [logon] [start]] |          -? 

where: 


logon	Requires the following syntax: 



	username[/password][@database_specification]| /| /NOLOG 



start	Allows you to enter the name of a command file and arguments. SQL*Plus 
passes the arguments to the command file as though you executed the file 
using the SQL*Plus START command. The start clause requires the 
following syntax: 



	file_name[.ext][arg1 arg2 . . .] 



	See the START command in this chapter for more information. 


 Terms and Clauses	

You have the option of entering logon. If you do not specify logon, and do specify start, SQL*Plus 
assumes that the first line of the command file contains a valid logon. If neither start nor logon are 
specified, SQL*Plus prompts for logon information. 

Refer to the following list for a description of each term or clause: 


username[/password]	 Represent the username and password with which you wish to start 
SQL*Plus and connect to ORACLE. If you omit username and password, 
SQL*Plus prompts you for them. If you enter a slash (/) or simply enter 
[Return] to the prompt for username, SQL*Plus logs you on using a default 
logon (see \"/\" below). 



	If you omit only password, SQL*Plus prompts you for password. When 
prompting, SQL*Plus does not display password on your terminal screen. 



/	Represents a default (ops$) logon. You cannot enter a 
database_specification if you use a default logon. In a default logon 
SQL*Plus attempts to log you on using the username OPS$name, where 
name is your operating system username. 



/NOLOG 	Establishes no initial connection to ORACLE. Before issuing any SQL 
commands, you must issue a CONNECT command to establish a valid 
logon. Use /NOLOG when you want to have a SQL*Plus command file 
prompt the user for the name of a database. 



database_specification	 Consists of a SQL*Net connection string. The exact syntax 
depends upon the SQL*Net communications protocol your Oracle 
installation uses. For more information, refer to the SQL*Net manual 
appropriate for your protocol or contact your DBA. 



-S[ILENT] 	Suppresses all SQL*Plus information and prompt messages, including the 
command prompt, the echoing of commands, and the banner normally 
displayed when you start SQL*Plus. Use SILENT to invoke SQL*Plus within 
another program so that the use of SQL*Plus is invisible to the user. 



-? 	Makes SQLPLUS display its current version and level number and then 
returns control to the operating system. Do not enter a space between the 
hyphen (-) and the question mark (?). 


 Usage Notes	

SQL*Plus supports a Site Profile, a SQL*Plus command file created by the database administrator. 
SQL*Plus executes this command file whenever any user starts SQL*Plus and SQL*Plus 
establishes the ORACLE connection. The Site Profile allows the DBA to set up SQL*Plus 
environment defaults for all users at a particular site; users cannot directly access the Site Profile. 
The default name and location of the Site Profile depend on your system. Site Profiles are described 
in more detail in the Oracle installation and user's manual(s) provided for your operating system. 


SQL*Plus also supports a User Profile, executed after the Site Profile. SQL*Plus searches for a file 
named LOGIN with the extension SQL in your current directory. If SQL*Plus does not find the file 
there, SQL*Plus will search a system-dependent path to find the file. Some operating systems may 
not support this path-search. If SQL*Plus does not find the LOGIN file in the paths, SQL*Plus prints a 
warning message and continues the logon process.  

 Examples	


To start SQL*Plus with username SCOTT and password TIGER, enter: 



SQLPLUS SCOTT/TIGER 

To start SQL*Plus, as above, and to make POLICY the default database, enter: 



SQLPLUS SCOTT/TIGER@POLICY 

To start SQL*Plus and run a command file named STARTUP with the extension SQL, enter: 



SQLPLUS SCOTT/TIGER @STARTUP  

Note the space between TIGER and @STARTUP.  

 ________________________________________



Copyright (c) 1994, Oracle Corporation."))
("start" . (nil "SQL*Plus command" "START 



 Purpose	

Executes the contents of the specified command file. 

 Syntax	

STA[RT] file_name[.ext] [arg1 arg2 ... ] 

 Terms and Clauses	

Refer to the following list for a description of each term or clause: 


file_name[.ext]	Represents the command file you wish to execute. The file can contain any 
command that you can run interactively. 



	If you do not specify an extension, SQL*Plus assumes the default 
command-file extension (normally SQL). For information on changing this 
default extension, see the  SUFFIX variable of the SET command in this 
chapter. 



	When you enter START file_name.ext, SQL*Plus searches for a file with 
the file name and extension you specify in the current default directory. If 
SQL*Plus does not find such a file, SQL*Plus will search a 
system-dependent path to find the file. Some operating systems may not 
support the path-search. Consult the Oracle installation and user's 
manual(s) provided for your operating system for specific information 
related to your operating system environment. 



arg1 arg2 ...	Represent data items you wish to pass to parameters in the command file.  
If you enter one or more arguments, SQL*Plus substitutes the values into 
the parameters (&1, &2, and so forth) in the command file. The first 
argument replaces each occurrence of &1, the second replaces each 
occurrence of &2, and so forth. 



	The START command DEFINEs the parameters with the values of the 
arguments; if you START the command file again in this session, you can 
enter new arguments or omit the arguments to use the old values. 



	For more information on using parameters, refer to the subsection \"Passing 
Parameters through the START Command\" under \"Writing Interactive 
Commands\" in Chapter 3.  


 Usage Notes	

 The @ (\"at\" sign) command functions similarly to START. 

The (double \"at\" sign) command functions similarly to START, but does not allow the passing of 
values to parameters. 

 Example	

 A file named PROMOTE with the extension SQL, used to promote employees, might contain the 
following command: 



SELECT *  FROM EMP  

WHERE MGR=&1 AND JOB='&2' AND SAL>&3; 

To run this command file, enter: 



SQL> START PROMOTE 7280 CLERK 950 

SQL*Plus then executes the following command: 



SELECT *  FROM EMP 

WHERE MGR=7280 AND JOB='CLERK' AND SAL>950;  

 ________________________________________



Copyright (c) 1994, Oracle Corporation."))
("timing" . (nil "SQL*Plus command" "TIMING 



 Purpose	

Records timing data for an elapsed period of time, lists the current timing area's title and timing data, 
or lists the number of active timing areas. 

 Syntax	

TIMI[NG] [START text|SHOW|STOP] 

 Terms and Clauses	

Refer to the following list for a description of each term or clause: 


START text	Sets up a timing area and makes text the title of the timing area. You can 
have more than one active timing area by STARTing additional areas before 
STOPping the first; SQL*Plus nests each new area within the preceding 
one. The area most recently STARTed becomes the current timing area. 



SHOW 	Lists the current timing area's title and timing data. 



STOP 	Lists the current timing area's title and timing data, and then deletes the 
timing area. If any other timing areas are active, the next most recently 
STARTed area becomes the current timing area. Use the TIMING clause of 
the CLEAR command to delete all timing areas. 


Enter TIMING with no clauses to list the number of active timing areas. 

 Usage Notes	

You can use this data to do a performance analysis on any commands or blocks run during the 
period. 

For information about the data TIMING displays, see the Oracle installation and user's manual(s) 
provided for your operating system.  Refer to SET TIMING ON for information on automatically 
displaying timing data after each SQL command or PL/SQL block you run. 

 Examples	


To create a timing area named SQL_AREA, enter: 



SQL> TIMING START SQL_AREA 

To list the current timing area's title and accumulated time, enter: 



SQL> TIMING SHOW 

To list the current timing area's title and accumulated time and to remove the timing area, enter: 



SQL> TIMING STOP 

 ________________________________________



Copyright (c) 1994, Oracle Corporation."))
("ttitle" . (nil "SQL*Plus command" "TTITLE 



 Purpose	

Places and formats a specified title at the top of each report page, or lists the current TTITLE 
definition. 

Note: For a description of the old form of TTITLE, which is compatible with UFI (a predecessor of 
SQL*Plus), see TTITLE (old form) in Appendix F. 

 Syntax	

TTI[TLE] [printspec [text|variable] ...] |          [OFF|ON] 

where printspec represents one or more of the following clauses used to place and format the text: 




COL n 

S[KIP] [n] 
TAB n 
LE[FT] 
CE[NTER] 
R[IGHT] 
BOLD 
FORMAT char 

 Terms and Clauses	

If you do not enter a printspec clause before the first occurrence of text, TTITLE left justifies the 
text. Enter TTITLE with no clauses to list the current TTITLE definition. 

Refer to the following list for a description of each term or clause. These terms and clauses also 
apply to the BTITLE command. 


text	Represents the title text. Enter text in single quotes if you wish to place 
more than one word on a single line. 



variable	Represents a user variable or any of the following system-maintained 
values:  



	ú	SQL.LNO  (current line number) 



	ú	SQL.PNO  (current page number) 



	ú	SQL.RELEASE  (current ORACLE release number) 



	ú	SQL.SQLCODE  (current error code) 



	ú	SQL.USER  (current username)  



	To print one of these values, reference the appropriate variable in the title. 
You can format variable with the FORMAT clause. 



OFF 	Turns the title off (suppresses its display) without affecting its definition. 



ON	Turns the title on (restores its display). When you define a top title, 
SQL*Plus automatically sets TTITLE to ON. 



COL n 	Indents to column n of the current line (backward if column n has been 
passed). \"Column\" in this context means print position, not table column. 



S[KIP] [n] 	Skips to the start of a new line n times; if you omit n, one time; if you enter 
zero for n, backward to the start of the current line. 



TAB n 	Skips forward n columns (backward if you enter a negative value for n). 
\"Column\" in this context means print position, not table column. 



LE[FT], CE[NTER], and R[IGHT] 	Left-align, center, and right-align data on the current line 
respectively. SQL*Plus aligns following data items as a group, up to the end 
of the printspec or the next LEFT, CENTER, RIGHT, or COL command. 
CENTER and RIGHT use the SET LINESIZE value to calculate the position 
of the data item that follows. 



BOLD 	Prints data in bold print. SQL*Plus represents bold print on your terminal by 
repeating the data on three consecutive lines. 



FORMAT char 	Specifies a format model that determines the format of following data items, 
up to the next FORMAT clause or the end of the command. The format 
model must be a char constant such as A10 or $999--not a variable. See 
COLUMN FORMAT for more information on formatting and valid format 
models. 



	If the datatype of the format model does not match the datatype of a given 
data item, the FORMAT clause has no effect on that item. 



	If no appropriate FORMAT model precedes a given data item, SQL*Plus 
prints NUMBER values according to the format specified by SET 
NUMFORMAT or, if you have not used SET NUMFORMAT, the default 
format. SQL*Plus prints DATE values according to the default format. 



	Refer to the FORMAT clause of the COLUMN command in this chapter for 
more information on default formats.  


 Usage Notes	

SQL*Plus interprets TTITLE in the new form if a valid printspec clause (LEFT, SKIP, COL, etc) 
immediately follows the command name. See COLUMN NEW_VALUE for information on printing 
column and DATE values in the top title. 

You can use any number of constants and variables in a printspec. SQL*Plus displays the constants 
and variables in the order you specify them, positioning and formatting each constant or variable as 
specified by the printspec clauses that precede it. 


The length of the title you specify with TTITLE cannot exceed 2400 characters. 

The continuation character (a hyphen) will not be recognized inside a single-quoted title text string. 
To be recognized, the continuation character must appear outside of the quotes, as follows: 



SQL> TTITLE CENTER 'Summary Report for' - 

> 'the Month of May' 

 Examples	

To define \"Monthly Analysis\" as the top title and to left-align it, to center the date, to right-align the 
page number with a  three-digit format, and to display \"Date in Thousands\" in bold in the center of the 
next line, enter: 



SQL> TTITLE LEFT 'Monthly Analysis' CENTER '11 Mar 88' -  

> RIGHT 'Page:' FORMAT 999 SQL.PNO SKIP CENTER BOLD - 
> 'Data in Thousands' 

The following title results: 



Monthly Analysis                11 Mar 88          Page:   
1                              Data in Thousands 

To suppress the top title display without changing its definition, enter: 



SQL> TTITLE OFF  

 ________________________________________



Copyright (c) 1994, Oracle Corporation."))
("undefine" . (nil "SQL*Plus command" "UNDEFINE 



 Purpose	

Deletes a given user variable that you defined either explicitly (with the DEFINE command) or implicitly 
(with an argument to the START command). 

 Syntax	

UNDEF[INE] variable 

 Terms and Clauses	

Refer to the following list for a description of each term or clause: 


variable	Represents the name of the user variable you wish to delete. 


 Example	

To undefine a user variable named POS, enter: 



SQL> UNDEFINE POS  

 ________________________________________



Copyright (c) 1994, Oracle Corporation."))
("variable" . (nil "SQL*Plus command" "VARIABLE 



 Purpose	

Declares a bind variable which can then be referenced in PL/SQL. For more information on bind 
variables, see \"Using Bind Variables\" in Chapter 3. For more information about PL/SQL, see your 
PL/SQL User's Guide and Reference. 

VARIABLE without arguments displays a list of all the variables declared in the session. VARIABLE 
followed only by a variable name lists that variable. 

 Syntax	

VAR[IABLE] [variable {NUMBER|CHAR|CHAR (n)}] 


 Terms and Clauses	

Refer to the following list for a description of each term or clause: 


variable	Represents the name of the bind variable you wish to create. 



NUMBER	Creates a variable of type NUMBER with a fixed length. 



CHAR	Creates a variable of type CHAR (character) with a length of one. 



CHAR (n)	Creates a variable of type CHAR with a maximum length of n. 


 Usage Notes	

To display the value of a bind variable created with VARIABLE, use the PRINT command. For more 
information, see the PRINT command in this chapter. 

 Examples	

Following is an example of creating a bind variable and then setting it to the value returned by a 
function: 



SQL> VARIABLE id NUMBER 

SQL> BEGIN 
  1    :id := emp_management.hire 
  2      ('BLAKE','MANAGER','KING',2990,'SALES');
  3  END; 

The bind variable named id could also be displayed with the PRINT command or used in subsequent 
PL/SQL subprograms.  

Following is an example of creating some variables and then listing some or all of them: 



SQL> VARIABLE id NUMBER 

SQL> VARIABLE txt CHAR (20) 
SQL> VARIABLE 
variable id 
datatype NUMBER 
 
variable txt 
datatype CHAR(20) 
SQL> VARIABLE txt 
variable txt 
datatype CHAR(20)  

 ________________________________________



Copyright (c) 1994, Oracle Corporation."))
("whenever oserror" . (nil "SQL*Plus command" "WHENEVER OSERROR 



 Purpose	

Exits SQL*Plus if an operating system error occurs, (such as a file I/O error). 

 Syntax	

WHENEVER OSERROR {EXIT [SUCCESS|FAILURE|OSCODE|n]    [COMMIT|ROLLBACK] | CONTINUE 
[COMMIT|ROLLBACK|NONE]} 

 Terms and Clauses	

Refer to the following list for a description of each term or clause: 


EXIT [SUCCESS|FAILURE|OSCODE|n|variable]	 Directs SQL*Plus to exit as soon as an 
operating system error is detected. You can also specify that SQL*Plus 
return a success or failure code, the operating system failure code, or a 
number or variable of your choice. See also EXIT in this chapter for details. 



	The EXIT clause will not exit if a SQL*Plus command generates an error. 



CONTINUE 	Turns off the EXIT option. 



COMMIT	Directs SQL*Plus to execute a COMMIT before exiting or continuing and 
save pending changes to the database. 



ROLLBACK	Directs SQL*Plus to execute a ROLLBACK before exiting or continuing and 
abandon pending changes to the database. 



NONE	Directs SQL*Plus to take no action before exiting or continuing. 


 Usage Notes	

 If you do not enter the WHENEVER OSERROR command, the default behavior of SQL*Plus is to 
continue and take no action when an operating system error occurs. 

 Examples	

 The commands in the following command file cause SQL*Plus to exit and COMMIT any pending 
changes if a failure occurs when writing to the output file: 



SQL> GET RAISE 

  1  WHENEVER OSERROR EXIT OSCODE COMMIT
  2  UPDATE EMP SET SAL = SAL*1.1 
  3  COPY TO SCOTT/TIGER@D:BETHESDA - 
  4  REPLACE EMP - 
  5  USING SELECT * FROM EMP 
  6  SPOOL OUT 
  7  SELECT SAL FROM EMP 

 ________________________________________



Copyright (c) 1994, Oracle Corporation."))
("whenever sqlerror" . (nil "SQL*Plus command" "WHENEVER SQLERROR 



 Purpose	

Exits SQL*Plus if a SQL command or PL/SQL block generates an error. 

 Syntax	

WHENEVER SQLERROR {EXIT [SUCCESS|FAILURE|WARNING|n|variable][COMMIT|ROLLBACK] | 
CONTINUE [COMMIT|ROLLBACK|NONE]} 

 Terms and Clauses	

Refer to the following list for a description of each term or clause: 


EXIT [SUCCESS|FAILURE|WARNING|n|variable]	Directs SQL*Plus exit as soon as it detects 
any SQL error (but after printing the SQL error message). The EXIT clause 
of WHENEVER SQLERROR follows the same syntax as the EXIT 
command. See EXIT in this chapter for details. 



	The EXIT clause will not exit if a SQL*Plus command generates an error. 



CONTINUE	Turns off the EXIT option. 



COMMIT	Directs SQL*Plus to execute a COMMIT before exiting or continuing and 
save pending changes to the database. 



ROLLBACK	Directs SQL*Plus to execute a ROLLBACK before exiting or continuing and 
abandon pending changes to the database. 



NONE	Directs SQL*Plus to take no action before exiting or continuing. 


 Usage Notes	

If you do not enter the WHENEVER SQLERROR command, the default behavior of SQL*Plus is to 
continue and take no action when a SQL error occurs. 

 Examples	

The commands in the following command file cause SQL*Plus to exit and display the SQL error code 
if a SQL UPDATE command fails and skips the COPY command: 



SQL> GET RAISE 

  1  WHENEVER SQLERROR EXIT SQL.SQLCODE 
  2  UPDATE EMP SET SAL = SAL*1.1 
  3  COPY TO SCOTT/TIGER@D:BETHESDA - 
  4  REPLACE EMP - 
  5  USING SELECT * FROM EMP 
  6  WHENEVER SQLERROR CONTINUE 
Copyright (c) 1994, Oracle Corporation."))
("document" . (nil "SQL*Plus command" "DOCUMENT 



 Purpose	

 Begins a block of documentation in a command file. 

 Syntax	

 DOC[UMENT]

 Usage Notes	

 For information on the current method of inserting comments in a command file, refer to the subsection 
\"Placing Comments in Command Files\" under \"Saving Commands for Later Use\" in Chapter 3 and to 
REMARK in Chapter 6. 

After you type DOCUMENT and enter [Return], SQL*Plus displays the prompt DOC> in place of SQL> 
until you end the documentation. The \"pound\" character (#) on a line by itself ends the documentation. 


If you have set DOCUMENT to OFF, SQL*Plus suppresses the display of the block of documentation 
created by the DOCUMENT command. (See SET DOCUMENT later in this appendix.)  

 ________________________________________



Copyright (c) 1994, Oracle Corporation."))
("newpage" . (nil "SQL*Plus command" "NEWPAGE 



 Purpose	

 Advances spooled output n lines beyond the beginning of the next page. 

 Syntax	

 NEWPAGE [1|n] 

 Usage Notes	

 Refer to the NEWPAGE variable of the SET command in Chapter 6 for information on the current 
method for advancing spooled output. 

 ________________________________________



Copyright (c) 1994, Oracle Corporation."))
("set buffer" . (nil "SQL*Plus command" "SET BUFFER 



 Purpose	

 Makes the specified buffer the current buffer.

 Syntax	

 SET BUF[FER] {buffer|SQL} 

 Usage Notes	

 Initially, the SQL buffer is the current buffer. SQL*Plus does not require the use of multiple buffers; the 
SQL buffer alone should meet your needs. 

If the buffer name you enter does not already exist, SET BUFFER defines (creates and names) the 
buffer. SQL*Plus deletes the buffer and its contents when you exit SQL*Plus. 


Running a query automatically makes the SQL buffer the current buffer. To copy text from one buffer to 
another, use the GET and SAVE commands. To clear text from the current buffer, use CLEAR BUFFER. 
To clear text from the SQL buffer while using a different buffer, use CLEAR SQL. 

 ________________________________________



Copyright (c) 1994, Oracle Corporation."))
("set document" . (nil "SQL*Plus command" "SET DOCUMENT 



 Purpose	

 Displays or suppresses blocks of documentation created by the DOCUMENT command. 

 Syntax	

 SET DOC[UMENT] {OFF|ON} 

 Usage Notes	

 SET DOCUMENT ON causes blocks of documentation to be echoed to the screen. Set DOCUMENT 
OFF suppresses the display of blocks of documentation. 

See DOCUMENT in this appendix for information on the DOCUMENT command. 

 ________________________________________



Copyright (c) 1994, Oracle Corporation."))
("set truncate" . (nil "SQL*Plus command" "SET TRUNCATE 



 Purpose	

 Controls whether SQL*Plus truncates or wraps a data item that is too long for the current line width. 

 Syntax	

 SET TRU[NCATE] {OFF|ON} 

 Usage Notes	

 ON functions in the same manner as SET WRAP OFF, and vice versa. You may prefer to use WRAP 
because the SHOW command recognizes WRAP and does not recognize TRUNCATE. 

 ________________________________________



Copyright (c) 1994, Oracle Corporation."))
))

;; End of sql-oracle-doc.el
