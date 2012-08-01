=================================
CouchDB Release 1.1 Feature Guide
=================================

Upgrading to CouchDB 1.1
========================

You can upgrade your existing CouchDB 1.0.x installation to CouchDB 1.1
without any specific steps or migration. When you run CouchDB 1.1 the
existing data and index files will be opened and used as normal.

The first time you run a compaction routine on your database within
CouchDB 1.1, the data structure and indexes will be updated to the new
version of the CouchDB database format that can only be read by CouchDB
1.1 and later. This step is not reversible. Once the data files have
been updated and migrated to the new version the data files will no
longer work with a CouchDB 1.0.x release.

.. warning::
   If you want to retain support for opening the data files in
   CouchDB 1.0.x you must back up your data files before performing the
   upgrade and compaction process.

New features in CouchDB 1.1
===========================

.. toctree::
   :maxdepth: 2
   
   replicator
   ssl
   range
   proxy
   commonjs
   other
