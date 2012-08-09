==============
Error Messages
==============

The errors reported when CouchDB is unable to read a required file have
been updated so that explicit information about the files and problem
can now be identified from the error message. The errors report file
permission access either when reading or writing to configuration and
database files.

The error is raised both through the log file and the error message
returned through the API call as a JSON error message. For example, when
setting configuration values:

::

    shell> 
    {"error":"file_permission_error","reason":"/etc/couchdb/local.ini"}
        

Errors will always be reported using the ``file_permission_error`` error
type.

During startup permissions errors on key files are also reported in the
log with a descriptive error message and file location so that
permissions can be fixed before restart.
