=====================
Miscellaneous Methods
=====================

The CouchDB Miscellaneous interface provides the basic interface to a
CouchDB server for obtaining CouchDB information and getting and setting
configuration information.

A list of the available methods and URL paths are provided below:

Miscellaneous API Calls

``GET /``
=========

Accessing the root of a CouchDB instance returns meta information about
the instance. The response is a JSON structure containing information
about the server, including a welcome message and the version of the
server.

::

    {
       "couchdb" : "Welcome",
       "version" : "1.0.1"
    }

``GET /_active_tasks``
======================

You can obtain a list of active tasks by using the ``/_active_tasks``
URL. The result is a JSON array of the currently running tasks, with
each task being described with a single object. For example:

::



    [
       {
        "pid" : "<0.11599.0>",
        "status" : "Copied 0 of 18369 changes (0%)",
        "task" : "recipes",
        "type" : "Database Compaction"
        }
    ]


        

The returned structure includes the following fields for each task:

For operation type, valid values include:

-  ``Database Compaction``

-  ``Replication``

-  ``View Group Compaction``

-  ``View Group Indexer``

``GET /_all_dbs``
=================

Returns a list of all the databases in the CouchDB instance. For
example:

::

    GET http://couchdb:5984/_all_dbs
    Accept: application/json

The return is a JSON array:

::

    [
       "_users",
       "contacts",
       "docs",
       "invoices",
       "locations"
    ]

``GET /_log``
=============

Gets the CouchDB log, equivalent to accessing the local log file of the
corresponding CouchDB instance.

When you request the log, the response is returned as plain (UTF-8)
text, with an HTTP ``Content-type`` header as ``text/plain``.

For example, the request:

::

    GET http://couchdb:5984/_log
    Accept: */*

The raw text is returned:

::


    [Wed, 27 Oct 2010 10:49:42 GMT] [info] [<0.23338.2>] 192.168.0.2 - - 'PUT' /authdb 401

    [Wed, 27 Oct 2010 11:02:19 GMT] [info] [<0.23428.2>] 192.168.0.116 - - 'GET' /recipes/FishStew 200

    [Wed, 27 Oct 2010 11:02:19 GMT] [info] [<0.23428.2>] 192.168.0.116 - - 'GET' /_session 200

    [Wed, 27 Oct 2010 11:02:19 GMT] [info] [<0.24199.2>] 192.168.0.116 - - 'GET' / 200

    [Wed, 27 Oct 2010 13:03:38 GMT] [info] [<0.24207.2>] 192.168.0.116 - - 'GET' /_log?offset=5 200

If you want to pick out specific parts of the log information you can
use the ``bytes`` argument, which specifies the number of bytes to be
returned, and ``offset``, which specifies where the reading of the log
should start, counted back from the end. For example, if you use the
following request:

::

    GET /_log?bytes=500&offset=2000

Reading of the log will start at 2000 bytes from the end of the log, and
500 bytes will be shown.

``POST /_replicate``
====================

Request, configure, or stop, a replication operation.

The specification of the replication request is controlled through the
JSON content of the request. The JSON should be an object with the
fields defining the source, target and other options. The fields of the
JSON request are shown in the table below:

Replication Operation
---------------------

The aim of the replication is that at the end of the process, all active
documents on the source database are also in the destination database
and all documents that were deleted in the source databases are also
deleted (if they exist) on the destination database.

Replication can be described as either push or pull replication:

-  *Pull replication* is where the ``source`` is the remote CouchDB
   instance, and the ``destination`` is the local database.

   Pull replication is the most useful solution to use if your source
   database has a permanent IP address, and your destination (local)
   database may have a dynamically assigned IP address (for example,
   through DHCP). This is particularly important if you are replicating
   to a mobile or other device from a central server.

-  *Push replication* is where the ``source`` is a local database, and
   ``destination`` is a remote database.

Specifying the Source and Target Database
-----------------------------------------

You must use the URL specification of the CouchDB database if you want
to perform replication in either of the following two situations:

-  Replication with a remote database (i.e. another instance of CouchDB
   on the same host, or a different host)

-  Replication with a database that requires authentication

For example, to request replication between a database local to the
CouchDB instance to which you send the request, and a remote database
you might use the following request:

::

    POST http://couchdb:5984/_replicate
    Content-Type: application/json
    Accept: application/json

    {
       "source" : "recipes",
       "target" : "http://coucdb-remote:5984/recipes",
    }
          

In all cases, the requested databases in the ``source`` and ``target``
specification must exist. If they do not, an error will be returned
within the JSON object:

::

    {
       "error" : "db_not_found"
       "reason" : "could not open http://couchdb-remote:5984/ol1ka/",
    }
          

You can create the target database (providing your user credentials
allow it) by adding the ``create_target`` field to the request object:

::

    POST http://couchdb:5984/_replicate
    Content-Type: application/json
    Accept: application/json

    {
       "create_target" : true
       "source" : "recipes",
       "target" : "http://couchdb-remote:5984/recipes",
    }

The ``create_target`` field is not destructive. If the database already
exists, the replication proceeds as normal.

Single Replication
------------------

You can request replication of a database so that the two databases can
be synchronized. By default, the replication process occurs one time and
synchronizes the two databases together. For example, you can request a
single synchronization between two databases by supplying the ``source``
and ``target`` fields within the request JSON content.

::

    POST http://couchdb:5984/_replicate
    Content-Type: application/json
    Accept: application/json

    {
       "source" : "recipes",
       "target" : "recipes-snapshot",
    }

In the above example, the databases ``recipes`` and ``recipes-snapshot``
will be synchronized. These databases are local to the CouchDB instance
where the request was made. The response will be a JSON structure
containing the success (or failure) of the synchronization process, and
statistics about the process:

::

    {
       "ok" : true,
       "history" : [
          {
             "docs_read" : 1000,
             "session_id" : "52c2370f5027043d286daca4de247db0",
             "recorded_seq" : 1000,
             "end_last_seq" : 1000,
             "doc_write_failures" : 0,
             "start_time" : "Thu, 28 Oct 2010 10:24:13 GMT",
             "start_last_seq" : 0,
             "end_time" : "Thu, 28 Oct 2010 10:24:14 GMT",
             "missing_checked" : 0,
             "docs_written" : 1000,
             "missing_found" : 1000
          }
       ],
       "session_id" : "52c2370f5027043d286daca4de247db0",
       "source_last_seq" : 1000
    }

The structure defines the replication status, as described in the table
below:

Continuous Replication
----------------------

Synchronization of a database with the previously noted methods happens
only once, at the time the replicate request is made. To have the target
database permanently replicated from the source, you must set the
``continuous`` field of the JSON object within the request to true.

With continuous replication changes in the source database are
replicated to the target database in perpetuity until you specifically
request that replication ceases.

::

    POST http://couchdb:5984/_replicate
    Content-Type: application/json
    Accept: application/json

    {
       "continuous" : true
       "source" : "recipes",
       "target" : "http://couchdb-remote:5984/recipes",
    }

Changes will be replicated between the two databases as long as a
network connection is available between the two instances.

    **Note**

    Two keep two databases synchronized with each other, you need to set
    replication in both directions; that is, you must replicate from
    ``databasea`` to ``databaseb``, and separately from ``databaseb`` to
    ``databasea``.

Canceling Continuous Replication
--------------------------------

You can cancel continuous replication by adding the ``cancel`` field to
the JSON request object and setting the value to true. Note that the
structure of the request must be identical to the original for the
cancelation request to be honoured. For example, if you requested
continuous replication, the cancellation request must also contain the
``continuous`` field.

For example, the replication request:

::

    POST http://couchdb:5984/_replicate
    Content-Type: application/json
    Accept: application/json

    {
       "source" : "recipes",
       "target" : "http://couchdb-remote:5984/recipes",
       "create_target" : true,
       "continuous" : true
    }

Must be canceled using the request:

::

    POST http://couchdb:5984/_replicate
    Content-Type: application/json
    Accept: application/json

    {
        "cancel" : true,
        "continuous" : true
        "create_target" : true,
        "source" : "recipes",
        "target" : "http://couchdb-remote:5984/recipes",
    }

Requesting cancellation of a replication that does not exist results in
a 404 error.

``POST /_restart``
==================

Restarts the CouchDB instance. You must be authenticated as a user with
administration privileges for this to work.

For example:

::

    POST http://admin:password@couchdb:5984/_restart

The return value (if the server has not already restarted) is a JSON
status object indicating that the request has been received:

::

    {
       "ok" : true,
    }

If the server has already restarted, the header may be returned, but no
actual data is contained in the response.

``GET /_stats``
===============

The ``_stats`` method returns a JSON object containting the statistics
for the running server. The object is structured with top-level sections
collating the statistics for a range of entries, with each individual
statistic being easily identified, and the content of each statistic is
self-describing. For example, the request time statistics, within the
``couchdb`` section are structured as follows:

::

    {
       "couchdb" : {
    ...
          "request_time" : {
             "stddev" : "27.509",
             "min" : "0.333333333333333",
             "max" : "152",
             "current" : "400.976",
             "mean" : "10.837",
             "sum" : "400.976",
             "description" : "length of a request inside CouchDB without MochiWeb"
          },
    ...
        }
    }
          

The fields provide the current, minimum and maximum, and a collection of
statistical means and quantities. The quantity in each case is not
defined, but the descriptions below provide

The statistics are divided into the following top-level sections:

-  ``couchdb``

   Describes statistics specific to the internals of CouchDB.

   +-------------------------+-------------------------------------------------------+----------------+
   | Statistic ID            | Description                                           | Unit           |
   +=========================+=======================================================+================+
   | ``auth_cache_hits``     | Number of authentication cache hits                   | number         |
   +-------------------------+-------------------------------------------------------+----------------+
   | ``auth_cache_misses``   | Number of authentication cache misses                 | number         |
   +-------------------------+-------------------------------------------------------+----------------+
   | ``database_reads``      | Number of times a document was read from a database   | number         |
   +-------------------------+-------------------------------------------------------+----------------+
   | ``database_writes``     | Number of times a database was changed                | number         |
   +-------------------------+-------------------------------------------------------+----------------+
   | ``open_databases``      | Number of open databases                              | number         |
   +-------------------------+-------------------------------------------------------+----------------+
   | ``open_os_files``       | Number of file descriptors CouchDB has open           | number         |
   +-------------------------+-------------------------------------------------------+----------------+
   | ``request_time``        | Length of a request inside CouchDB without MochiWeb   | milliseconds   |
   +-------------------------+-------------------------------------------------------+----------------+

   Table: ``couchdb`` statistics

-  ``httpd_request_methods``

   +----------------+----------------------------------+----------+
   | Statistic ID   | Description                      | Unit     |
   +================+==================================+==========+
   | ``COPY``       | Number of HTTP COPY requests     | number   |
   +----------------+----------------------------------+----------+
   | ``DELETE``     | Number of HTTP DELETE requests   | number   |
   +----------------+----------------------------------+----------+
   | ``GET``        | Number of HTTP GET requests      | number   |
   +----------------+----------------------------------+----------+
   | ``HEAD``       | Number of HTTP HEAD requests     | number   |
   +----------------+----------------------------------+----------+
   | ``POST``       | Number of HTTP POST requests     | number   |
   +----------------+----------------------------------+----------+
   | ``PUT``        | Number of HTTP PUT requests      | number   |
   +----------------+----------------------------------+----------+

   Table: ``httpd_request_methods`` statistics

-  ``httpd_status_codes``

   +----------------+------------------------------------------------------+----------+
   | Statistic ID   | Description                                          | Unit     |
   +================+======================================================+==========+
   | ``200``        | Number of HTTP 200 OK responses                      | number   |
   +----------------+------------------------------------------------------+----------+
   | ``201``        | Number of HTTP 201 Created responses                 | number   |
   +----------------+------------------------------------------------------+----------+
   | ``202``        | Number of HTTP 202 Accepted responses                | number   |
   +----------------+------------------------------------------------------+----------+
   | ``301``        | Number of HTTP 301 Moved Permanently responses       | number   |
   +----------------+------------------------------------------------------+----------+
   | ``304``        | Number of HTTP 304 Not Modified responses            | number   |
   +----------------+------------------------------------------------------+----------+
   | ``400``        | Number of HTTP 400 Bad Request responses             | number   |
   +----------------+------------------------------------------------------+----------+
   | ``401``        | Number of HTTP 401 Unauthorized responses            | number   |
   +----------------+------------------------------------------------------+----------+
   | ``403``        | Number of HTTP 403 Forbidden responses               | number   |
   +----------------+------------------------------------------------------+----------+
   | ``404``        | Number of HTTP 404 Not Found responses               | number   |
   +----------------+------------------------------------------------------+----------+
   | ``405``        | Number of HTTP 405 Method Not Allowed responses      | number   |
   +----------------+------------------------------------------------------+----------+
   | ``409``        | Number of HTTP 409 Conflict responses                | number   |
   +----------------+------------------------------------------------------+----------+
   | ``412``        | Number of HTTP 412 Precondition Failed responses     | number   |
   +----------------+------------------------------------------------------+----------+
   | ``500``        | Number of HTTP 500 Internal Server Error responses   | number   |
   +----------------+------------------------------------------------------+----------+

   Table: ``httpd_status_codes`` statistics

-  ``httpd``

   +----------------------------------+----------------------------------------------+----------+
   | Statistic ID                     | Description                                  | Unit     |
   +==================================+==============================================+==========+
   | ``bulk_requests``                | Number of bulk requests                      | number   |
   +----------------------------------+----------------------------------------------+----------+
   | ``clients_requesting_changes``   | Number of clients for continuous \_changes   | number   |
   +----------------------------------+----------------------------------------------+----------+
   | ``requests``                     | Number of HTTP requests                      | number   |
   +----------------------------------+----------------------------------------------+----------+
   | ``temporary_view_reads``         | Number of temporary view reads               | number   |
   +----------------------------------+----------------------------------------------+----------+
   | ``view_reads``                   | Number of view reads                         | number   |
   +----------------------------------+----------------------------------------------+----------+

   Table: ``httpd`` statistics

You can also access individual statistics by quoting the statistics
sections and statistic ID as part of the URL path. For example, to get
the ``request_time`` statistics, you can use:

::

    GET /_stats/couchdb/request_time
        

This returns an entire statistics object, as with the full request, but
containining only the request individual statistic. Hence, the returned
structure is as follows:

::

    {
       "couchdb" : {
          "request_time" : {
             "stddev" : 7454.305,
             "min" : 1,
             "max" : 34185,
             "current" : 34697.803,
             "mean" : 1652.276,
             "sum" : 34697.803,
             "description" : "length of a request inside CouchDB without MochiWeb"
          }
       }
    }
        

``GET /_utils``
===============

Accesses the built-in Futon administration interface for CouchDB.

``GET /_uuids``
===============

Requests one or more Universally Unique Identifiers (UUIDs) from the
CouchDB instance. The response is a JSON object providing a list of
UUIDs. For example:

::

    {
       "uuids" : [
          "7e4b5a14b22ec1cf8e58b9cdd0000da3"
       ]
    }

You can use the ``count`` argument to specify the number of UUIDs to be
returned. For example:

::

        GET http://couchdb:5984/_uuids?count=5

Returns:

::

    {
       "uuids" : [
          "c9df0cdf4442f993fc5570225b405a80",
          "c9df0cdf4442f993fc5570225b405bd2",
          "c9df0cdf4442f993fc5570225b405e42",
          "c9df0cdf4442f993fc5570225b4061a0",
          "c9df0cdf4442f993fc5570225b406a20"
       ]
    }

The UUID type is determined by the UUID type setting in the CouchDB
configuration. See ?.

For example, changing the UUID type to ``random``:

::

    PUT http://couchdb:5984/_config/uuids/algorithm
    Content-Type: application/json
    Accept: */*

    "random"

When obtaining a list of UUIDs:

::

    {
       "uuids" : [
          "031aad7b469956cf2826fcb2a9260492",
          "6ec875e15e6b385120938df18ee8e496",
          "cff9e881516483911aa2f0e98949092d",
          "b89d37509d39dd712546f9510d4a9271",
          "2e0dbf7f6c4ad716f21938a016e4e59f"
       ]
    }

``GET /favicon.ico``
====================

Returns the site icon. The return ``Content-type`` header is
``image/x-icon``, and the content stream is the image data.
