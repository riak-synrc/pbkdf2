Added CommonJS support to map functions
=======================================

We didn't have CommonJS require in map functions because the current
CommonJS implementation is scoped to the whole design doc, and giving
views access to load code from anywhere in the design doc would mean
we'd have to blow away your view index any time you changed anything.
Having to rebuild views from scratch just because you changed some CSS
or a show function isn't fun, so we avoided the issue by keeping
CommonJS require out of map and reduce altogether.

The solution we came up with is to allow CommonJS inside map and reduce
funs, but only of libraries that are stored inside the views part of the
design doc.

So you could continue to access CommonJS code in design\_doc.foo, from
your list functions etc, but we'd add the ability to require CommonJS
modules within map and reduce, but only from design\_doc.views.lib

There's no worry here about namespace collisions, as Couch just plucks
``views.*.map`` and ``views.*.reduce`` out of the design doc. So you
could have a view called ``lib`` if you wanted, and still have CommonJS
stored in ``views.lib.sha1`` and ``views.lib.stemmer`` if you wanted.

We simplified the implementation by enforcing that CommonJS modules to
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
