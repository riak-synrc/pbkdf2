.. Licensed under the Apache License, Version 2.0 (the "License"); you may not
.. use this file except in compliance with the License. You may obtain a copy of
.. the License at
..
..   http://www.apache.org/licenses/LICENSE-2.0
..
.. Unless required by applicable law or agreed to in writing, software
.. distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
.. WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
.. License for the specific language governing permissions and limitations under
.. the License.

========================
JSON Structure Reference
========================

The following appendix provides a quick reference to all the JSON structures
that you can supply to CouchDB, or get in return to requests.

All Database Documents
======================

+--------------------------------+---------------------------------------------+
| Field                          | Description                                 |
+================================+=============================================+
| total_rows                     | Number of documents in the database/view    |
+--------------------------------+---------------------------------------------+
| offset                         | Offset where the document list started      |
+--------------------------------+---------------------------------------------+
| update_seq (optional)          | Current update sequence for the database    |
+--------------------------------+---------------------------------------------+
| rows [array]                   | Array of document object                    |
+--------------------------------+---------------------------------------------+

Bulk Document Response
======================

+--------------------------------+---------------------------------------------+
| Field                          | Description                                 |
+================================+=============================================+
| docs [array]                   | Bulk Docs Returned Documents                |
+--------------------------------+---------------------------------------------+
|         id                     | Document ID                                 |
+--------------------------------+---------------------------------------------+
|         error                  | Error type                                  |
+--------------------------------+---------------------------------------------+
|         reason                 | Error string with extended reason           |
+--------------------------------+---------------------------------------------+

Bulk Documents
==============

+--------------------------------+---------------------------------------------+
| Field                          | Description                                 |
+================================+=============================================+
| all_or_nothing (optional)      | Sets the database commit mode to use        |
|                                | all-or-nothing semantics                    |
+--------------------------------+---------------------------------------------+
| docs [array]                   | Bulk Documents Document                     |
+--------------------------------+---------------------------------------------+
|         _id (optional)         | Document ID                                 |
+--------------------------------+---------------------------------------------+
|         _rev (optional)        | Revision ID (when updating an existing      |
|                                | document)                                   |
+--------------------------------+---------------------------------------------+
|         _deleted (optional)    | Whether the document should be deleted      |
+--------------------------------+---------------------------------------------+

Changes information for a database
==================================

+--------------------------------+---------------------------------------------+
| Field                          | Description                                 |
+================================+=============================================+
| last_seq                       | Last change sequence number                 |
+--------------------------------+---------------------------------------------+
| results [array]                | Changes made to a database                  |
+--------------------------------+---------------------------------------------+
|         seq                    | Update sequence number                      |
+--------------------------------+---------------------------------------------+
|         id                     | Document ID                                 |
+--------------------------------+---------------------------------------------+
|         changes [array]        | List of changes, field-by-field, for this   |
|                                | document                                    |
+--------------------------------+---------------------------------------------+

CouchDB Document
================

+--------------------------------+---------------------------------------------+
| Field                          | Description                                 |
+================================+=============================================+
| _id (optional)                 | Document ID                                 |
+--------------------------------+---------------------------------------------+
| _rev (optional)                | Revision ID (when updating an existing      |
|                                | document)                                   |
+--------------------------------+---------------------------------------------+

CouchDB Error Status
====================

+--------------------------------+---------------------------------------------+
| Field                          | Description                                 |
+================================+=============================================+
| id                             | Document ID                                 |
+--------------------------------+---------------------------------------------+
| error                          | Error type                                  |
+--------------------------------+---------------------------------------------+
| reason                         | Error string with extended reason           |
+--------------------------------+---------------------------------------------+

CouchDB database information object
===================================

+--------------------------------+---------------------------------------------+
| Field                          | Description                                 |
+================================+=============================================+
| db_name                        | The name of the database.                   |
+--------------------------------+---------------------------------------------+
| committed_update_seq           | The number of committed update.             |
+--------------------------------+---------------------------------------------+
| doc_count                      | A count of the documents in the specified   |
|                                | database.                                   |
+--------------------------------+---------------------------------------------+
| doc_del_count                  | Number of deleted documents                 |
+--------------------------------+---------------------------------------------+
| compact_running                | Set to true if the database compaction      |
|                                | routine is operating on this database.      |
+--------------------------------+---------------------------------------------+
| disk_format_version            | The version of the physical format used for |
|                                | the data when it is stored on disk.         |
+--------------------------------+---------------------------------------------+
| disk_size                      | Size in bytes of the data as stored on the  |
|                                | disk. Views indexes are not included in the |
|                                | calculation.                                |
+--------------------------------+---------------------------------------------+
| instance_start_time            | Timestamp of when the database was created, |
|                                | expressed in milliseconds since the epoch.  |
+--------------------------------+---------------------------------------------+
| purge_seq                      | The number of purge operations on the       |
|                                | database.                                   |
+--------------------------------+---------------------------------------------+
| update_seq                     | The current number of updates to the        |
|                                | database.                                   |
+--------------------------------+---------------------------------------------+

Design Document
===============

+--------------------------------+---------------------------------------------+
| Field                          | Description                                 |
+================================+=============================================+
| _id                            | Design Document ID                          |
+--------------------------------+---------------------------------------------+
| _rev                           | Design Document Revision                    |
+--------------------------------+---------------------------------------------+
| views                          | View                                        |
+--------------------------------+---------------------------------------------+
|     viewname                   | View Definition                             |
+--------------------------------+---------------------------------------------+
|         map                    | Map Function for View                       |
+--------------------------------+---------------------------------------------+
|         reduce (optional)      | Reduce Function for View                    |
+--------------------------------+---------------------------------------------+

Design Document Information
===========================

+--------------------------------+---------------------------------------------+
| Field                          | Description                                 |
+================================+=============================================+
| name                           | Name/ID of Design Document                  |
+--------------------------------+---------------------------------------------+
| view_index                     | View Index                                  |
+--------------------------------+---------------------------------------------+
|     compact_running            | Indicates whether a compaction routine is   |
|                                | currently running on the view               |
+--------------------------------+---------------------------------------------+
|     disk_size                  | Size in bytes of the view as stored on disk |
+--------------------------------+---------------------------------------------+
|     language                   | Language for the defined views              |
+--------------------------------+---------------------------------------------+
|     purge_seq                  | The purge sequence that has been processed  |
+--------------------------------+---------------------------------------------+
|     signature                  | MD5 signature of the views for the design   |
|                                | document                                    |
+--------------------------------+---------------------------------------------+
|     update_seq                 | The update sequence of the corresponding    |
|                                | database that has been indexed              |
+--------------------------------+---------------------------------------------+
|     updater_running            | Indicates if the view is currently being    |
|                                | updated                                     |
+--------------------------------+---------------------------------------------+
|     waiting_clients            | Number of clients waiting on views from this|
|                                | design document                             |
+--------------------------------+---------------------------------------------+
|     waiting_commit             | Indicates if there are outstanding commits  |
|                                | to the underlying database that need to     |
|                                | processed                                   |
+--------------------------------+---------------------------------------------+

Design Document spatial index Information
=========================================

+--------------------------------+---------------------------------------------+
| Field                          | Description                                 |
+================================+=============================================+
| name                           | Name/ID of Design Document                  |
+--------------------------------+---------------------------------------------+
| spatial_index                  | View Index                                  |
+--------------------------------+---------------------------------------------+
|     compact_running            | Indicates whether a compaction routine is   |
|                                | currently running on the view               |
+--------------------------------+---------------------------------------------+
|     disk_size                  | Size in bytes of the view as stored on disk |
+--------------------------------+---------------------------------------------+
|     language                   | Language for the defined views              |
+--------------------------------+---------------------------------------------+
|     purge_seq                  | The purge sequence that has been processed  |
+--------------------------------+---------------------------------------------+
|     signature                  | MD5 signature of the views for the design   |
|                                | document                                    |
+--------------------------------+---------------------------------------------+
|     update_seq                 | The update sequence of the corresponding    |
|                                | database that has been indexed              |
+--------------------------------+---------------------------------------------+
|     updater_running            | Indicates if the view is currently being    |
|                                | updated                                     |
+--------------------------------+---------------------------------------------+
|     waiting_clients            | Number of clients waiting on views from this|
|                                | design document                             |
+--------------------------------+---------------------------------------------+
|     waiting_commit             | Indicates if there are outstanding commits  |
|                                | to the underlying database that need to     |
|                                | processed                                   |
+--------------------------------+---------------------------------------------+

Document with Attachments
=========================

+--------------------------------+---------------------------------------------+
| Field                          | Description                                 |
+================================+=============================================+
| _id (optional)                 | Document ID                                 |
+--------------------------------+---------------------------------------------+
| _rev (optional)                | Revision ID (when updating an existing      |
|                                | document)                                   |
+--------------------------------+---------------------------------------------+
| _attachments (optional)        | Document Attachment                         |
+--------------------------------+---------------------------------------------+
|     filename                   | Attachment information                      |
+--------------------------------+---------------------------------------------+
|         content_type           | MIME Content type string                    |
+--------------------------------+---------------------------------------------+
|         data                   | File attachment content, Base64 encoded     |
+--------------------------------+---------------------------------------------+

List of Active Tasks
====================

+--------------------------------+---------------------------------------------+
| Field                          | Description                                 |
+================================+=============================================+
| tasks [array]                  | Active Task                                 |
+--------------------------------+---------------------------------------------+
|     pid                        | Process ID                                  |
+--------------------------------+---------------------------------------------+
|     status                     | Task status message                         |
+--------------------------------+---------------------------------------------+
|     task                       | Task name                                   |
+--------------------------------+---------------------------------------------+
|     type                       | Operation Type                              |
+--------------------------------+---------------------------------------------+

Replication Settings
====================

+--------------------------------+---------------------------------------------+
| Field                          | Description                                 |
+================================+=============================================+
| source                         | Source database name or URL                 |
+--------------------------------+---------------------------------------------+
| target                         | Target database name or URL                 |
+--------------------------------+---------------------------------------------+
| create_target (optional)       | Creates the target database                 |
+--------------------------------+---------------------------------------------+
| continuous (optional)          | Configure the replication to be continuous  |
+--------------------------------+---------------------------------------------+
| cancel (optional)              | Cancels the replication                     |
+--------------------------------+---------------------------------------------+
| doc_ids (optional)             | Array of document IDs to be synchronized    |
+--------------------------------+---------------------------------------------+
| proxy (optional)               | Address of a proxy server through which     |
|                                | replication should occur                    |
+--------------------------------+---------------------------------------------+

Replication Status
==================

+--------------------------------+---------------------------------------------+
| Field                          | Description                                 |
+================================+=============================================+
| ok                             | Replication status                          |
+--------------------------------+---------------------------------------------+
| session_id                     | Unique session ID                           |
+--------------------------------+---------------------------------------------+
| source_last_seq                | Last sequence number read from source       |
|                                | database                                    |
+--------------------------------+---------------------------------------------+
| history [array]                | Replication History                         |
+--------------------------------+---------------------------------------------+
|     session_id                 | Session ID for this replication operation   |
+--------------------------------+---------------------------------------------+
|     recorded_seq               | Last recorded sequence number               |
+--------------------------------+---------------------------------------------+
|     docs_read                  | Number of documents read                    |
+--------------------------------+---------------------------------------------+
|     docs_written               | Number of documents written to target       |
+--------------------------------+---------------------------------------------+
|     doc_write_failures         | Number of document write failures           |
+--------------------------------+---------------------------------------------+
|     start_time                 | Date/Time replication operation started     |
+--------------------------------+---------------------------------------------+
|     start_last_seq             | First sequence number in changes stream     |
+--------------------------------+---------------------------------------------+
|     end_time                   | Date/Time replication operation completed   |
+--------------------------------+---------------------------------------------+
|     end_last_seq               | Last sequence number in changes stream      |
+--------------------------------+---------------------------------------------+
|     missing_checked            | Number of missing documents checked         |
+--------------------------------+---------------------------------------------+
|     missing_found              | Number of missing documents found           |
+--------------------------------+---------------------------------------------+

Returned CouchDB Document with Detailed Revision Info
=====================================================

+--------------------------------+---------------------------------------------+
| Field                          | Description                                 |
+================================+=============================================+
| _id (optional)                 | Document ID                                 |
+--------------------------------+---------------------------------------------+
| _rev (optional)                | Revision ID (when updating an existing      |
|                                | document)                                   |
+--------------------------------+---------------------------------------------+
| _revs_info [array]             | CouchDB Document Extended Revision Info     |
+--------------------------------+---------------------------------------------+
|         rev                    | Full revision string                        |
+--------------------------------+---------------------------------------------+
|         status                 | Status of the revision                      |
+--------------------------------+---------------------------------------------+

Returned CouchDB Document with Revision Info
============================================

+--------------------------------+---------------------------------------------+
| Field                          | Description                                 |
+================================+=============================================+
| _id (optional)                 | Document ID                                 |
+--------------------------------+---------------------------------------------+
| _rev (optional)                | Revision ID (when updating an existing      |
|                                | document)                                   |
+--------------------------------+---------------------------------------------+
| _revisions                     | CouchDB Document Revisions                  |
+--------------------------------+---------------------------------------------+
|     ids [array]                | Array of valid revision IDs, in reverse     |
|                                | order (latest first)                        |
+--------------------------------+---------------------------------------------+
|     start                      | Prefix number for the latest revision       |
+--------------------------------+---------------------------------------------+

Returned Document with Attachments
==================================

+--------------------------------+---------------------------------------------+
| Field                          | Description                                 |
+================================+=============================================+
| _id (optional)                 | Document ID                                 |
+--------------------------------+---------------------------------------------+
| _rev (optional)                | Revision ID (when updating an existing      |
|                                | document)                                   |
+--------------------------------+---------------------------------------------+
| _attachments (optional)        | Document Attachment                         |
+--------------------------------+---------------------------------------------+
|     filename                   | Attachment                                  |
+--------------------------------+---------------------------------------------+
|         stub                   | Indicates whether the attachment is a stub  |
+--------------------------------+---------------------------------------------+
|         content_type           | MIME Content type string                    |
+--------------------------------+---------------------------------------------+
|         length                 | Length (bytes) of the attachment data       |
+--------------------------------+---------------------------------------------+
|         revpos                 | Revision where this attachment exists       |
+--------------------------------+---------------------------------------------+

Security Object
===============

+--------------------------------+---------------------------------------------+
| Field                          | Description                                 |
+================================+=============================================+
| admins                         | Roles/Users with admin privileges           |
+--------------------------------+---------------------------------------------+
|         roles [array]          | List of roles with parent privilege         |
+--------------------------------+---------------------------------------------+
|         users [array]          | List of users with parent privilege         |
+--------------------------------+---------------------------------------------+
| readers                        | Roles/Users with reader privileges          |
+--------------------------------+---------------------------------------------+
|         roles [array]          | List of roles with parent privilege         |
+--------------------------------+---------------------------------------------+
|         users [array]          | List of users with parent privilege         |
+--------------------------------+---------------------------------------------+
