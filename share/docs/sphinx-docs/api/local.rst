========================================
Local (non-replicating) Document Methods
========================================

The Local (non-replicating) document interface allows you to create
local documents that are not replicated to other databases. These
documents can be used to hold configuration or other information that is
required specifically on the local CouchDB instance.

Local documents have the following limitations:

-  Local documents are not replicated to other databases.

-  The ID of the local document must be known for the document to
   accessed. You cannot obtain a list of local documents from the
   database.

-  Local documents are not output by views, or the ``_all_docs`` view.

Local documents can be used when you want to store configuration or
other information for the curent (local) instance of a given database.

A list of the available methods and URL paths are provided below:

Local (non-replicating) Document API Calls

``GET /db/_local/local-doc``
============================

Gets the specified local document. The semantics are identical to
accessing a standard document in the specified database, except that the
document is not replicated. See ?.

``PUT /db/_local/local-doc``
============================

Stores the specified local document. The semantics are identical to
storing a standard document in the specified database, except that the
document is not replicated. See ?.

``DELETE /db/_local/local-doc``
===============================

Deletes the specified local document. The semantics are identical to
deleting a standard document in the specified database, except that the
document is not replicated. See ?.

``COPY /db/_local/local-doc``
=============================

Copies the specified local document. The semantics are identical to
copying a standard document in the specified database, except that the
document is not replicated. See ?.
