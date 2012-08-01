======================
Authentication Methods
======================

.. todo:: Authentication Methods

The CouchDB Authentication methods provide an interface for obtaining
session and authorization data.

A list of the available methods and URL paths are provided below:

+--------+-------------------------+-------------------------------------------+
| Method | Path                    | Description                               |
+========+=========================+===========================================+
| GET    | /_oauth/access_token    | TBC                                       |
+--------+-------------------------+-------------------------------------------+
| GET    | /_oauth/authorize       | TBC                                       |
+--------+-------------------------+-------------------------------------------+
| POST   | /_oauth/authorize       | TBC                                       |
+--------+-------------------------+-------------------------------------------+
| GET    | /_oauth/request_token   | TBC                                       |
+--------+-------------------------+-------------------------------------------+
| GET    | /_session               | Returns cookie based login user           |
|        |                         | information                               |
+--------+-------------------------+-------------------------------------------+
| POST   | /_session               | Do cookie based user login                |
+--------+-------------------------+-------------------------------------------+
| DELETE | /_session               | Logout cookie based user                  |
+--------+-------------------------+-------------------------------------------+
