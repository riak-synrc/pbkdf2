================
Document Methods
================

The CouchDB API Server Document methods detail how to create, read,
update and delete documents within a database.

A list of the available methods and URL paths are provided below:

Document API Calls

``POST /db``
============

Create a new document in the specified database, using the supplied JSON
document structure. If the JSON structure includes the ``_id`` field,
then the document will be created with the specified document ID. If the
``_id`` field is not specified, a new unique ID will be generated.

For example, you can generate a new document with a generated UUID using
the following request:

::

    POST http://couchdb:5984/recipes/
    Content-Type: application/json

    {
       "servings" : 4,
       "subtitle" : "Delicious with fresh bread",
       "title" : "Fish Stew"
    }

The return JSON will specify the automatically enerated ID and revision
information:

::

    {
       "id" : "64575eef70ab90a2b8d55fc09e00440d",
       "ok" : true,
       "rev" : "1-9c65296036141e575d32ba9c034dd3ee"
    }

Specifying the Document ID
--------------------------

The document ID can be specified by including the ``_id`` field in the
JSON of the submitted record. The following request will create the same
document with the ID ``FishStew``:

::

    POST http://couchdb:5984/recipes/
    Content-Type: application/json

    {
       "_id" : "FishStew",
       "servings" : 4,
       "subtitle" : "Delicious with fresh bread",
       "title" : "Fish Stew"
    }

The structure of the submitted document is as shown in the table below:

In either case, the returned JSON will specify the document ID, revision
ID, and status message:

::

    {
       "id" : "FishStew",
       "ok" : true,
       "rev" : "1-9c65296036141e575d32ba9c034dd3ee"
    }
        

Batch Mode Writes
-----------------

You can write documents to the database at a higher rate by using the
batch option. This collects document writes together in memory (on a
user-by-user basis) before they are committed to disk. This increases
the risk of the documents not being stored in the event of a failure,
since the documents are not written to disk immediately.

To use the batched mode, append the ``batch=ok`` query argument to the
URL of the ``PUT`` or ``POST`` request. The CouchDB server will respond
with a 202 HTTP response code immediately.

Including Attachments
---------------------

You can include one or more attachments with a given document by
incorporating the attachment information within the JSON of the
document. This provides a simpler alternative to loading documents with
attachments than making a separate call (see ?).

The ``filename`` will be the attachment name. For example, when sending
the JSON structure below:

::

    {
       "_id" : "FishStew",
       "servings" : 4,
       "subtitle" : "Delicious with fresh bread",
       "title" : "Fish Stew"
       "_attachments" : {
          "styling.css" : {
             "content-type" : "text/css",
             "data" : "cCB7IGZvbnQtc2l6ZTogMTJwdDsgfQo=",
             },
       },
    }
        

The attachment ``styling.css`` can be accessed using
``/recipes/FishStew/styling.css``. For more information on attachments,
see ?.

The document data embedded in to the structure must be encoded using
base64.

``GET /db/doc``
===============

Returns the specified ``doc`` from the specified ``db``. For example, to
retrieve the document with the id ``FishStew`` you would send the
following request:

::

    GET http://couchdb:5984/recipes/FishStew
    Content-Type: application/json
    Accept: application/json

The returned JSON is the JSON of the document, including the document ID
and revision number:

::

    {
       "_id" : "FishStew",
       "_rev" : "3-a1a9b39ee3cc39181b796a69cb48521c",
       "servings" : 4,
       "subtitle" : "Delicious with a green salad",
       "title" : "Irish Fish Stew"
    }
        

Unless you request a specific revision, the latest revision of the
document will always be returned.

Attachments
-----------

If the document includes attachments, then the returned structure will
contain a summary of the attachments associatd with the document, but
not the attachment data itself.

The JSON for the returned document will include the ``_attachments``
field, with one or more attachment definitions. For example:

::

    {
       "_id" : "FishStew",
       "servings" : 4,
       "subtitle" : "Delicious with fresh bread",
       "title" : "Fish Stew"
       "_attachments" : {
          "styling.css" : {
             "stub" : true,
             "content-type" : "text/css",
             "length" : 783426,
             },
       },
    }

The format of the returned JSON is shown in the table below:

Getting a List of Revisions
---------------------------

You can obtain a list of the revisions for a given document by adding
the ``revs=true`` parameter to the request URL. For example:

::

    GET http://couchdb:5984/recipes/FishStew?revs=true
    Accept: application/json

The returned JSON structure includes the original document, including a
``_revisions`` structure that includes the revision information:

::

    {
       "servings" : 4,
       "subtitle" : "Delicious with a green salad",
       "_id" : "FishStew",
       "title" : "Irish Fish Stew",
       "_revisions" : {
          "ids" : [
             "a1a9b39ee3cc39181b796a69cb48521c",
             "7c4740b4dcf26683e941d6641c00c39d",
             "9c65296036141e575d32ba9c034dd3ee"
          ],
          "start" : 3
       },
       "_rev" : "3-a1a9b39ee3cc39181b796a69cb48521c"
    }

Obtaining an Extended Revision History
--------------------------------------

You can get additional information about the revisions for a given
document by supplying the ``revs_info`` argument to the query:

::

    GET http://couchdb:5984/recipes/FishStew?revs_info=true
    Accept: application/json

This returns extended revision information, including the availability
and status of each revision:

::

    {
       "servings" : 4,
       "subtitle" : "Delicious with a green salad",
       "_id" : "FishStew",
       "_revs_info" : [
          {
             "status" : "available",
             "rev" : "3-a1a9b39ee3cc39181b796a69cb48521c"
          },
          {
             "status" : "available",
             "rev" : "2-7c4740b4dcf26683e941d6641c00c39d"
          },
          {
             "status" : "available",
             "rev" : "1-9c65296036141e575d32ba9c034dd3ee"
          }
       ],
       "title" : "Irish Fish Stew",
       "_rev" : "3-a1a9b39ee3cc39181b796a69cb48521c"
    }

Obtaining a Specific Revision
-----------------------------

To get a specific revision, use the ``rev`` argument to the request, and
specify the full revision number:

::

    GET http://couchdb:5984/recipes/FishStew?rev=2-7c4740b4dcf26683e941d6641c00c39d
    Accept: application/json

The specified revision of the document will be returned, including a
``_rev`` field specifying the revision that was requested:

::

    {
       "_id" : "FishStew",
       "_rev" : "2-7c4740b4dcf26683e941d6641c00c39d",
       "servings" : 4,
       "subtitle" : "Delicious with a green salad",
       "title" : "Fish Stew"
    }

``HEAD /db/doc``
================

Returns the HTTP Headers containing a minimal amount of information
about the specified document. The method supports the same query
arguments as the ``GET`` method, but only the header information
(including document size, and the revision as an ETag), is returned. For
example, a simple ``HEAD`` request:

::

    HEAD http://couchdb:5984/recipes/FishStew
    Content-Type: application/json
        

Returns the following HTTP Headers:

::

    HTTP/1.1 200 OK
    Server: CouchDB/1.0.1 (Erlang OTP/R13B)
    Etag: "7-a19a1a5ecd946dad70e85233ba039ab2"
    Date: Fri, 05 Nov 2010 14:54:43 GMT
    Content-Type: text/plain;charset=utf-8
    Content-Length: 136
    Cache-Control: must-revalidate

The ``Etag`` header shows the current revision for the requested
document, and the ``Content-Length`` specifies the length of the data,
if the document were requested in full.

Adding any of the query arguments (as supported by ```GET```_ method),
then the resulting HTTP Headers will correspond to what would be
returned. Note that the current revision is not returned when the
``refs_info`` argument is used. For example:

::

    HTTP/1.1 200 OK
    Server: CouchDB/1.0.1 (Erlang OTP/R13B)
    Date: Fri, 05 Nov 2010 14:57:16 GMT
    Content-Type: text/plain;charset=utf-8
    Content-Length: 609
    Cache-Control: must-revalidate

``PUT /db/doc``
===============

The ``PUT`` method creates a new named document, or creates a new
revision of the existing document. Unlike the ```POST```_ method, you
must specify the document ID in the request URL.

For example, to create the docment ``FishStew``, you would send the
following request:

::

    PUT http://couchdb:5984/recipes/FishStew
    Content-Type: application/json

    {
      "servings" : 4,
      "subtitle" : "Delicious with fresh bread",
      "title" : "Fish Stew"
    }

The return type is JSON of the status, document ID,and revision number:

::

    {
       "id" : "FishStew",
       "ok" : true,
       "rev" : "1-9c65296036141e575d32ba9c034dd3ee"
    }

Updating an Existing Document
-----------------------------

To update an existing document you must specify the current revision
number within the ``_rev`` parameter. For example:

::

    PUT http://couchdb:5984/recipes/FishStew
    Content-Type: application/json

    {
      "_rev" : "1-9c65296036141e575d32ba9c034dd3ee",
      "servings" : 4,
      "subtitle" : "Delicious with fresh salad",
      "title" : "Fish Stew"
    }

Alternatively, you can supply the current revision number in the
``If-Match`` HTTP header of the request. For example:

::

    PUT http://couchdb:5984/recipes/FishStew
    If-Match: 2-d953b18035b76f2a5b1d1d93f25d3aea
    Content-Type: application/json

    {
       "servings" : 4,
       "subtitle" : "Delicious with fresh salad",
       "title" : "Fish Stew"
    }

The JSON returned will include the updated revision number:

::

    {
       "id" : "FishStew99",
       "ok" : true,
       "rev" : "2-d953b18035b76f2a5b1d1d93f25d3aea"
    }

For information on batched writes, which can provide improved
performance, see ?.

``DELETE /db/doc``
==================

Deletes the specified document from the database. You must supply the
current (latest) revision, either by using the ``rev`` parameter to
specify the revision:

::

    DELETE http://couchdb:5984/recipes/FishStew?rev=3-a1a9b39ee3cc39181b796a69cb48521c
    Content-Type: application/json

Alternatively, you can use ETags with the ``If-Match`` field:

::

    DELETE http://couchdb:5984/recipes/FishStew
    If-Match: 3-a1a9b39ee3cc39181b796a69cb48521c
    Content-Type: application/json
        

The returned JSON contains the document ID, revision and status:

::

    {
       "id" : "FishStew",
       "ok" : true,
       "rev" : "4-2719fd41187c60762ff584761b714cfb"
    }

    **Note**

    Note that deletion of a record increments the revision number. The
    use of a revision for deletion of the record allows replication of
    the database to correctly track the deletion in synchronized copies.

``COPY /db/doc``
================

The ``COPY`` command (which is non-standard HTTP) copies an existing
document to a new or existing document.

The source document is specified on the request line, with the
``Destination`` HTTP Header of the request specifying the target
document.

Copying a Document
------------------

You can copy the latest version of a document to a new document by
specifying the current document and target document:

::

    COPY http://couchdb:5984/recipes/FishStew
    Content-Type: application/json
    Destination: IrishFishStew

The above request copies the document ``FishStew`` to the new document
``IrishFishStew``. The response is the ID and revision of the new
document.

::

    {
       "id" : "IrishFishStew",
       "rev" : "1-9c65296036141e575d32ba9c034dd3ee"
    }

Copying from a Specific Revision
--------------------------------

To copy *from* a specific version, use the ``rev`` argument to the query
string:

::

    COPY http://couchdb:5984/recipes/FishStew?rev=5-acfd32d233f07cea4b4f37daaacc0082
    Content-Type: application/json
    Destination: IrishFishStew

The new document will be created using the information in the specified
revision of the source document.

Copying to an Existing Document
-------------------------------

To copy to an existing document, you must specify the current revision
string for the target document, using the ``rev`` parameter to the
``Destination`` HTTP Header string. For example:

::

    COPY http://couchdb:5984/recipes/FishStew
    Content-Type: application/json
    Destination: IrishFishStew?rev=1-9c65296036141e575d32ba9c034dd3ee

The return value will be the new revision of the copied document:

::

    {
       "id" : "IrishFishStew",
       "rev" : "2-55b6a1b251902a2c249b667dab1c6692"
    }

``GET /db/doc/attachment``
==========================

Returns the file attachment ``attachment`` associated with the document
``doc``. The raw data of the associated attachment is returned (just as
if you were accessing a static file. The returned HTTP ``Content-type``
will be the same as the content type set when the document attachment
was submitted into the database.

``PUT /db/doc/attachment``
==========================

Upload the supplied content as an attachment to the specified document
(``doc``). The ``attachment`` name provided must be a URL encoded
string. You must also supply either the ``rev`` query argument or the
``If-Match`` HTTP header for validation, and the HTTP headers (to set
the attacment content type). The content type is used when the
attachment is requested as the corresponding content-type in the
returned document header.

For example, you could upload a simple text document using the following
request:

::

    PUT http://couchdb:5984/recipes/FishStew/basic?rev=8-a94cb7e50ded1e06f943be5bfbddf8ca
    Content-Length: 10
    Content-Type: text/plain

    Roast it

Or by using the ``If-Match`` HTTP header:

::

    PUT http://couchdb:5984/recipes/FishStew/basic
    If-Match: 8-a94cb7e50ded1e06f943be5bfbddf8ca
    Content-Length: 10
    Content-Type: text/plain

    Roast it

The returned JSON contains the new document information:

::

    {
       "id" : "FishStew",
       "ok" : true,
       "rev" : "9-247bb19a41bfd9bfdaf5ee6e2e05be74"
    }

    **Note**

    Uploading an attachment updates the corresponding document revision.
    Revisions are tracked for the parent document, not individual
    attachments.

Updating an Existing Attachment
-------------------------------

Uploading an attachment using an existing attachment name will update
the corresponding stored content of the database. Since you must supply
the revision information to add an attachment to a document, this serves
as validation to update the existing attachment.

``DELETE /db/doc/attachment``
=============================

Deletes the attachment ``attachment`` to the specified ``doc``. You must
supply the ``rev`` argument with the current revision to delete the
attachment.

For example to delete the attachment ``basic`` from the recipe
``FishStew``:

::

    DELETE http://couchdb:5984/recipes/FishStew/basic?rev=9-247bb19a41bfd9bfdaf5ee6e2e05be74
    Content-Type: application/json

        

The returned JSON contains the updated revision information:

::

    {
       "id" : "FishStew",
       "ok" : true,
       "rev" : "10-561bf6b1e27615cee83d1f48fa65dd3e"
    }

.. _JSON object: #table-couchdb-api-db_db-json-changes
.. _``POST``: #couchdb-api-dbdoc_db_post
