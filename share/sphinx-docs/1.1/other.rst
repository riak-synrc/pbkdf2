More granular ETag support for views
====================================

ETags have been assigned to a map/reduce group (the collection of views
in a single design document). Any change to any of the indexes for those
views would generate a new ETag for all view URL's in a single design
doc, even if that specific view's results had not changed.

In CouchDB 1.1 each ``_view`` URL has it's own ETag which only gets
updated when changes are made to the database that effect that index. If
the index for that specific view does not change, that view keeps the
original ETag head (therefore sending back 304 Not Modified more often).

Added built-in filters for ``_changes``: ``_doc_ids`` and ``_design``.
======================================================================

The ``_changes`` feed can now be used to watch changes to specific
document ID's or the list of ``_design`` documents in a database. If the
``filters`` parameter is set to ``_doc_ids`` a list of doc IDs can be
passed in the "doc_ids" as a JSON array.

Allow wildcards in vhosts definitions
=====================================

Similar to the rewrites section of a ``_design`` document, the new
``vhosts`` system uses variables in the form of ``:varname`` or wildcards in
the form of asterisks. The variable results can be output into the
resulting path as they are in the rewriter.

OS Daemons
==========

CouchDB now supports starting external processes. The support is simple
and enables CouchDB to start each configured OS daemon. If the daemon
stops at any point, CouchDB will restart it (with protection to ensure
regularly failing daemons are not repeatedly restarted).

The daemon starting process is one-to-one; for each each configured
daemon in the configuration file, CouchDB will start exactly one
instance. If you need to run multiple instances, then you must create
separate individual configurations. Daemons are configured within the
``[os_daemons]`` section of your configuration file (``local.ini``). The
format of each configured daemon is:

::

    NAME = PATH ARGS
        

Where ``NAME`` is an arbitrary (and unique) name to identify the daemon;
``PATH`` is the full path to the daemon to be executed; ``ARGS`` are any
required arguments to the daemon.

For example:

.. code-block:: ini

    [os_daemons]
    basic_responder = /usr/local/bin/responsder.js

There is no interactivity between CouchDB and the running process, but
you can use the OS Daemons service to create new HTTP servers and
responders and then use the new proxy service to redirect requests and
output to the CouchDB managed service. For more information on proxying,
see :ref:`proxying`. For further background on the OS Daemon service, see
`CouchDB Externals API`_.

Stale views and ``update_after``
================================

Currently a view request can include the ``stale=ok`` query argument,
which allows the contents of a stale view index to be used to produce
the view output. In order to trigger a build of the outdated view index,
a second view request must be made.

To simplify this process, the ``update_after`` value can be supplied to
the ``stale`` query argument. This triggers a rebuild of the view index
after the results of the view have been retrieved.

Socket Options Configuration Setting
====================================

The socket options for the listening socket in CouchDB can now be set
within the CouchDB configuration file. The setting should be added to
the ``[httpd]`` section of the file using the option name
``socket_options``. The specification is as a list of tuples. For
example:

::

    [httpd]
    socket_options = [{recbuf, 262144}, {sndbuf, 262144}, {nodelay, true}]

The options supported are a subset of full options supported by the
TCP/IP stack. A list of the supported options are provided in the
`Erlang inet`_ documentation.

Server Options Configuration Setting
====================================

Server options for the MochiWeb component of CouchDB can now be added to
the configuration file. Settings should be added to the
``server_options`` option of the ``[httpd]`` section of ``local.ini``.
For example:

.. code-block:: ini

    [httpd]
    server_options = [{backlog, 128}, {acceptor_pool_size, 16}]
           

Improved Error Messages
=======================

The errors reported when CouchDB is unable to read a required file have
been updated so that explicit information about the files and problem
can now be identified from the error message. The errors report file
permission access either when reading or writing to configuration and
database files.

The error is raised both through the log file and the error message
returned through the API call as a JSON error message. For example, when
setting configuration values:

.. code-block:: bash

    shell> curl -X PUT http://couchdb:5984/_config/couchdb/delayed_commits \
                -H 'X-Couch-Persist: true' -d '"false"'
    {"error":"file_permission_error","reason":"/etc/couchdb/local.ini"}

Errors will always be reported using the ``file_permission_error`` error
type.

During startup permissions errors on key files are also reported in the
log with a descriptive error message and file location so that
permissions can be fixed before restart.

Multiple micro-optimizations when reading data.
===============================================

We found a number of places where CouchDB wouldn't do the absolute
optimal thing when reading data and got rid of quite a few
inefficiencies. The problem with small optimizations all over the place
is that you may not notice them with every use-case, but we sure hope
you can see an improvement overall.

.. _CouchDB Externals API: http://davispj.com/2010/09/26/new-couchdb-externals-api.html
.. _Erlang inet: http://www.erlang.org/doc/man/inet.html#setopts-2
