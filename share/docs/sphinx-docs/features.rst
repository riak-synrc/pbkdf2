==========================
Features and Functionality
==========================

 

HTTP Range Requests
===================

HTTP allows you to specify byte ranges for requests. This allows the
implementation of resumable downloads and skippable audio and video
streams alike. The following example uses a text file to make the range
request process easier.

::

    shell> 
    My hovercraft is full of eels!

Uploading this as an attachment to a ``text`` database using ``curl``:

::

    shell> 
    {"ok":true,"id":"doc","rev":"1-287a28fa680ae0c7fb4729bf0c6e0cf2"}

Requesting the whole file works as normal:

::

    shell> 
    My hovercraft is full of eels!

But to retrieve only the first 13 bytes using ``curl``:

::

    shell> 
    My hovercraft

HTTP supports many ways to specify single and even multiple byte
rangers. See `RFC 2616`_.

    **Note**

    Databases that have been created with CouchDB 1.0.2 or earlier will
    support range requests in 1.1.0, but they are using a less-optimal
    algorithm. If you plan to make heavy use of this feature, make sure
    to compact your database with CouchDB 1.1.0 to take advantage of a
    better algorithm to find byte ranges.

HTTP Proxying
=============

The HTTP proxy feature makes it easy to map and redirect different
content through your CouchDB URL. The proxy works by mapping a pathname
and passing all content after that prefix through to the configured
proxy address.

Configuration of the proxy redirect is handled through the
``[httpd_global_handlers]`` section of the CouchDB configuration file
(typically ``local.ini``). The format is:

::

    [httpd_global_handlers]
    PREFIX = {couch_httpd_proxy, handle_proxy_req, <<"DESTINATION">>}
      

Where:

-  ``PREFIX``

   Is the string that will be matched. The string can be any valid
   qualifier, although to ensure that existing database names are not
   overridden by a proxy configuration, you can use an underscore
   prefix.

-  ``DESTINATION``

   The fully-qualified URL to which the request should be sent. The
   destination must include the ``http`` prefix. The content is used
   verbatim in the original request, so you can also forward to servers
   on different ports and to specific paths on the target host.

The proxy process then translates requests of the form:

::

    http://couchdb:5984/PREFIX/path

To:

::

    DESTINATION/path

    **Note**

    Everything after ``PREFIX`` including the required forward slash
    will be appended to the ``DESTINATION``.

The response is then communicated back to the original client.

For example, the following configuration:

::


    _google = {couch_httpd_proxy, handle_proxy_req, <<"http://www.google.com">>}

Would forward all requests for ``http://couchdb:5984/_google`` to the
Google website.

The service can also be used to forward to related CouchDB services,
such as Lucene:

::

      
    [httpd_global_handlers]
    _fti = {couch_httpd_proxy, handle_proxy_req, <<"http://127.0.0.1:5985">>}

    **Note**

    The proxy service is basic. If the request is not identified by the
    ``DESTINATION``, or the remainder of the ``PATH`` specification is
    incomplete, the original request URL is interpreted as if the
    ``PREFIX`` component of that URL does not exist.

    For example, requesting ``http://couchdb:5984/_intranet/media`` when
    ``/media`` on the proxy destination does not exist, will cause the
    request URL to be interpreted as ``http://couchdb:5984/media``. Care
    should be taken to ensure that both requested URLs and destination
    URLs are able to cope

CommonJS support for map functions
==================================

CommonJS support allows you to use CommonJS notation inside map and
reduce functions, but only of libraries that are stored inside the views
part of the design doc.

So you could continue to access CommonJS code in design\_doc.foo, from
your list functions etc, but we'd add the ability to require CommonJS
modules within map and reduce, but only from ``design_doc.views.lib``.

There's no worry here about namespace collisions, as Couch just plucks
``views.*.map`` and ``views.*.reduce`` out of the design doc. So you
could have a view called ``lib`` if you wanted, and still have CommonJS
stored in ``views.lib.sha1`` and ``views.lib.stemmer`` if you wanted.

The implementation is simplified by enforcing that CommonJS modules to
be used in map functions be stored in views.lib.

A sample design doc (taken from the test suite in Futon) is below:

::

    {
       "views" : {
          "lib" : {
             "baz" : "exports.baz = 'bam';",
             "foo" : {
                "zoom" : "exports.zoom = 'yeah';",
                "boom" : "exports.boom = 'ok';",
                "foo" : "exports.foo = 'bar';"
             }
          },
          "commonjs" : {
             "map" : "function(doc) { emit(null, require('views/lib/foo/boom').boom)}"
          }
       },
       "_id" : "_design/test"
    }

The ``require()`` statement is relative to the design document, but
anything loaded form outside of ``views/lib`` will fail.

Granular ETag support
=====================

ETags have been assigned to a map/reduce group (the collection of views
in a single design document). Any change to any of the indexes for those
views would generate a new ETag for all view URL's in a single design
doc, even if that specific view's results had not changed.

In CouchDB 1.1 each ``_view`` URL has it's own ETag which only gets
updated when changes are made to the database that effect that index. If
the index for that specific view does not change, that view keeps the
original ETag head (therefore sending back 304 Not Modified more often).

.. _RFC 2616: http://tools.ietf.org/html/rfc2616#section-14.27
