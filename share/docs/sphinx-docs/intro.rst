============
Introduction
============

There are two interfaces to CouchDB, the built-in Futon web-based
interface and the CouchDB API accessed through the HTTP REST interface.
The former is the simplest way to view and monitor your CouchDB
installation and perform a number of basic database and system
operations. More information on using the Futon interface can be found
in ?.

The primary way to interact with the CouchDB API is to use a client
library or other interface that provides access to the underlying
functionality through your chosen language or platform. However, since
the API is supported through HTTP REST, you can interact with your
CouchDB with any solution that supports the HTTP protocol.

There are a number of different tools that talk the HTTP protocol and
allow you to set and configure the necessary information. One tool for
this that allows for access from the command-line is ``curl``. See ?.

Using Futon
===========

Futon is a native web-based interface built into CouchDB. It provides a
basic interface to the majority of the functionality, including the
ability to create, update, delete and view documents and views, provides
access to the configuration parameters, and an interface for initiating
replication.

The default view is the Overview page which provides you with a list of
the databases. The basic structure of the page is consistent regardless
of the section you are in. The main panel on the left provides the main
interface to the databases, configuration or replication systems. The
side panel on the right provides navigation to the main areas of Futon
interface:

.. figure:: images/futon-overview.png
   :align: center
   :alt:  Futon Overview

    Futon Overview
The main sections are:

-  Overview

   The main overview page, which provides a list of the databases and
   provides the interface for querying the database and creating and
   updating documents. See ?.

-  Configuration

   An interface into the configuration of your CouchDB installation. The
   interface allows you to edit the different configurable parameters.
   For more details on configuration, see ?.

-  Replicator

   An interface to the replication system, enabling you to initiate
   replication between local and remote databases. See ?.

-  Status

   Displays a list of the running background tasks on the server.
   Background tasks include view index building, compaction and
   replication. The Status page is an interface to the `Active Tasks`_
   API call. See ?.

-  Verify Installation

   The Verify Installation allows you to check whether all of the
   components of your CouchDB installation are correctly installed.

-  Test Suite

   The Test Suite section allows you to run the built-in test suite.
   This executes a number of test routines entirely within your browser
   to test the API and functionality of your CouchDB installation. If
   you select this page, you can run the tests by using the Run All
   button. This will execute all the tests, which may take some time.

Managing Databases and Documents
--------------------------------

You can manage databases and documents within Futon using the main
Overview section of the Futon interface.

To create a new database, click the Create Database ELLIPSIS button. You
will be prompted for the database name, as shown in the figure below.

.. figure:: images/futon-createdb.png
   :align: center
   :alt:  Creating a Database

    Creating a Database
Once you have created the database (or selected an existing one), you
will be shown a list of the current documents. If you create a new
document, or select an existing document, you will be presented with the
edit document display.

Editing documents within Futon requires selecting the document and then
editing (and setting) the fields for the document individually before
saving the document back into the database.

For example, the figure below shows the editor for a single document, a
newly created document with a single ID, the document ``_id`` field.

.. figure:: images/futon-editdoc.png
   :align: center
   :alt:  Editing a Document

    Editing a Document
To add a field to the document:

1. Click Add Field.

2. In the fieldname box, enter the name of the field you want to create.
   For example, “company”.

3. Click the green tick next to the field name to confirm the field name
   change.

4. Double-click the corresponding Value cell.

5. Enter a company name, for example “Example”.

6. Click the green tick next to the field value to confirm the field
   value.

7. The document is still not saved as this point. You must explicitly
   save the document by clicking the Save Document button at the top of
   the page. This will save the document, and then display the new
   document with the saved revision information (the ``_rev`` field).

   .. figure:: images/futon-editeddoc.png
      :align: center
      :alt:  Edited Document

       Edited Document

The same basic interface is used for all editng operations within Futon.
You *must* rememmbr to save the individual element (fieldname, value)
using the green tick button, before then saving the document.

Configuring Replication
-----------------------

When you click the Replicator option within the Tools menu you are
presented with the Replicator screen. This allows you to start
replication between two databases by filling in or select the
appropriate options within the form provided.

.. figure:: images/futon-replform.png
   :align: center
   :alt:  Replication Form

    Replication Form
To start a replication process, either the select the local database or
enter a remote database name into the corresponding areas of the form.
Replication occurs from the database on the left to the database on the
right.

If you are specifying a remote database name, you must specify the full
URL of the remote database (including the host, port number and database
name). If the remote instance requires authentication, you can specify
the username and password as part of the URL, for example
``http://username:pass@remotehost:5984/demo``.

To enable continuous replication, click the Continuous checkbox.

To start the replication process, click the Replicate button. The
replication process should start and will continue in the background. If
the replication process will take a long time, you can monitor the
status of the replication using the Status option under the Tools menu.

Once replication has been completed, the page will show the information
returned when the replication process completes by the API.

The Replicator tool is an interface to the underlying replication API.
For more information, see ?. For more information on replication, see ?.

Using ``curl``
==============

The ``curl`` utility is a command line tool available on Unix, Linux,
Mac OS X and Windows and many other platforms. ``curl`` provides easy
access to the HTTP protocol (among others) directly from the
command-line and is therefore an ideal way of interacting with CouchDB
over the HTTP REST API.

For simple ``GET`` requests you can supply the URL of the request. For
example, to get the database information:

::

    shell> 

This returns the database information (formatted in the output below for
clarity):

::

    {
       "modules" : {
          "geocouch" : "7fd793c10f3aa667a1088a937398bc5b51472b7f"
       },
       "couchdb" : "Welcome",
       "version" : "1.1.0",
    }

    **Note**

    For some URLs, especially those that include special characters such
    as ampersand, exclamation mark, or question mark, you should quote
    the URL you are specifying on the command line. For example:

    ::

        shell> 

You can explicitly set the HTTP command using the ``-X`` command line
option. For example, when creating a database, you set the name of the
database in the URL you send using a PUT request:

::

    shell> 
    {"ok":true}

But to obtain the database information you use a ``GET`` request (with
the return information formatted for clarity):

::

    shell> 
    {
       "compact_running" : false,
       "doc_count" : 0,
       "db_name" : "demo",
       "purge_seq" : 0,
       "committed_update_seq" : 0,
       "doc_del_count" : 0,
       "disk_format_version" : 5,
       "update_seq" : 0,
       "instance_start_time" : "1306421773496000",
       "disk_size" : 79
    }

For certain operations, you must specify the content type of request,
which you do by specifying the ``Content-Type`` header using the ``-H``
command-line option:

::

    shell> 

You can also submit 'payload' data, that is, data in the body of the
HTTP request using the ``-d`` option. This is useful if you need to
submit JSON structures, for example document data, as part of the
request. For example, to submit a simple document to the ``demo``
database:

::

    shell> 
    {"ok":true,"id":"8843faaf0b831d364278331bc3001bd8",
     "rev":"1-33b9fbce46930280dab37d672bbc8bb9"}

In the above example, the argument after the ``-d`` option is the JSON
of the document we want to submit.

The document can be accessed by using the automatically generated
document ID that was returned:

::

    shell> 
    {"_id":"8843faaf0b831d364278331bc3001bd8",
     "_rev":"1-33b9fbce46930280dab37d672bbc8bb9",
     "company":"Example, Inc."}

The API samples in the ? show the HTTP command, URL and any payload
information that needs to be submitted (and the expected return value).
All of these examples can be reproduced using ``curl`` with the
command-line examples shown above.
