.. _configuring:

===================
Configuring CouchDB
===================

.. todo:: Configuring CouchDB

CouchDB Configuration Files
===========================

.. todo:: CouchDB Configuration Files

Configuration File Locations
============================

CouchDB reads files from the following locations, in the following
order.

1. ``PREFIX/default.ini``

2. ``PREFIX/default.d/*``

3. ``PREFIX/local.ini``

4. ``PREFIX/local.d/*``

Settings in successive documents override the settings in earlier
entries. For example, setting the ``bind_address`` parameter in
``local.ini`` would override any setting in ``default.ini``.

.. warning::
   The ``default.ini`` file may be overwritten during an upgrade or
   re-installation, so localised changes should be made to the
   ``local.ini`` file or files within the ``local.d`` directory.

MochiWeb Server Options
=======================

Server options for the MochiWeb component of CouchDB can be added to the
configuration files. Settings should be added to the ``server_options``
option of the ``[httpd]`` section of ``local.ini``. For example:

.. code-block:: ini

    [httpd]
    server_options = [{backlog, 128}, {acceptor_pool_size, 16}]
           

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

.. code-block:: ini

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

.. _update-notifications:

Update Notifications
====================

.. todo:: Update Notifications

Socket Options Configuration Setting
====================================

The socket options for the listening socket in CouchDB can now be set
within the CouchDB configuration file. The setting should be added to
the ``[httpd]`` section of the file using the option name
``socket_options``. The specification is as a list of tuples. For
example:

.. code-block:: ini

    [httpd]
    socket_options = [{recbuf, 262144}, {sndbuf, 262144}, {nodelay, true}]

The options supported are a subset of full options supported by the
TCP/IP stack. A list of the supported options are provided in the
`Erlang inet`_ documentation.

``vhosts`` definitions
======================

Similar to the rewrites section of a ``_design`` document, the
``vhosts`` system uses variables in the form of ``:varname`` or wildcards in
the form of asterisks. The variable results can be output into the
resulting path as they are in the rewriter.

Configuring SSL Network Sockets
===============================

SSL configuration in CouchDB was designed to be as easy as possible. All
you need is two files; a certificate and a private key. If you bought an
official SSL certificate from a certificate authority, both should be in
your possession already.

If you just want to try this out and don't want to pay anything upfront,
you can create a self-signed certificate. Everything will work the same,
but clients will get a warning about an insecure certificate.

You will need the OpenSSL command line tool installed. It probably
already is.

.. code-block:: bash

    shell> mkdir cert && cd cert
    shell> openssl genrsa > privkey.pem
    shell> openssl req -new -x509 -key privkey.pem -out mycert.pem -days 1095
    shell> ls
    mycert.pem privkey.pem

Now, you need to edit CouchDB's configuration, either by editing your
``local.ini`` file or using the ``/_config`` API calls or the
configuration screen in Futon. Here is what you need to do in
``local.ini``, you can infer what needs doing in the other places.

Be sure to make these edits. Under ``[daemons]`` you should see:

.. code-block:: ini

    ; enable SSL support by uncommenting the following line and supply the PEM's below.
    ; the default ssl port CouchDB listens on is 6984
    ;httpsd = {couch_httpd, start_link, [https]}

Here uncomment the last line:

.. code-block:: ini

    httpsd = {couch_httpd, start_link, [https]}

Next, under ``[ssl]`` you will see:

.. code-block:: ini

    ;cert_file = /full/path/to/server_cert.pem
    ;key_file = /full/path/to/server_key.pem

Uncomment and adjust the paths so it matches your system's paths:

.. code-block:: ini

    cert_file = /home/jan/cert/mycert.pem
    key_file = /home/jan/cert/privkey.pem

For more information please read
`http://www.openssl.org/docs/HOWTO/certificates.txt`_.

Now start (or restart) CouchDB. You should be able to connect to it
using HTTPS on port 6984:

.. code-block:: bash

    shell> curl https://127.0.0.1:6984/
    curl: (60) SSL certificate problem, verify that the CA cert is OK. Details:
    error:14090086:SSL routines:SSL3_GET_SERVER_CERTIFICATE:certificate verify failed
    More details here: http://curl.haxx.se/docs/sslcerts.html

    curl performs SSL certificate verification by default, using a "bundle"
    of Certificate Authority (CA) public keys (CA certs). If the default
    bundle file isn't adequate, you can specify an alternate file
    using the --cacert option.
    If this HTTPS server uses a certificate signed by a CA represented in
    the bundle, the certificate verification probably failed due to a
    problem with the certificate (it might be expired, or the name might
    not match the domain name in the URL).
    If you'd like to turn off curl's verification of the certificate, use
    the -k (or --insecure) option.

Oh no what happened?! — Remember, clients will notify their users that
your certificate is self signed. ``curl`` is the client in this case and
it notifies you. Luckily you trust yourself (don't you?) and you can
specify the ``-k`` option as the message reads:

.. code-block:: bash

    shell> curl -k https://127.0.0.1:6984/
    {"couchdb":"Welcome","version":"1.1.0"}

.. _CouchDB Externals API: http://davispj.com/2010/09/26/new-couchdb-externals-api.html
.. _Erlang inet: http://www.erlang.org/doc/man/inet.html#setopts-2
.. _`http://www.openssl.org/docs/HOWTO/certificates.txt`: http://www.openssl.org/docs/HOWTO/certificates.txt


Configuration options reference
===============================


Configuration Groups
--------------------

+----------------------------------+-------------------------------------------+
| Section                          | Description                               |
+==================================+===========================================+
| attachments                      | Attachment options                        |
+----------------------------------+-------------------------------------------+
| couchdb                          | CouchDB specific options                  |
+----------------------------------+-------------------------------------------+
| couch_httpd_auth                 | HTTPD Authentication options              |
+----------------------------------+-------------------------------------------+
| daemons                          | Daemons and background processes          |
+----------------------------------+-------------------------------------------+
| httpd                            | HTTPD Server options                      |
+----------------------------------+-------------------------------------------+
| httpd_db_handlers                | Database Operation handlers               |
+----------------------------------+-------------------------------------------+
| httpd_design_handlers            | Handlers for design document operations   |
+----------------------------------+-------------------------------------------+
| httpd_global_handlers            | Handlers for global operations            |
+----------------------------------+-------------------------------------------+
| log                              | Logging options                           |
+----------------------------------+-------------------------------------------+
| query_servers                    | Query Server options                      |
+----------------------------------+-------------------------------------------+
| query_server_config              | Query server options                      |
+----------------------------------+-------------------------------------------+
| replicator                       | Replicator Options                        |
+----------------------------------+-------------------------------------------+
| ssl                              | SSL (Secure Sockets Layer) Options        |
+----------------------------------+-------------------------------------------+
| stats                            | Statistics options                        |
+----------------------------------+-------------------------------------------+
| uuids                            | UUID generation options                   |
+----------------------------------+-------------------------------------------+

attachments Configuration Options
---------------------------------

+--------------------------------------+---------------------------------------+
| Option                               | Description                           |
+======================================+=======================================+
| compressible_types                   | compressible_types                    |
+--------------------------------------+---------------------------------------+
| compression_level                    | compression_level                     |
+--------------------------------------+---------------------------------------+

couchdb Configuration Options
-----------------------------

+--------------------------------------+---------------------------------------+
| Option                               | Description                           |
+======================================+=======================================+
| database_dir                         | database_dir                          |
+--------------------------------------+---------------------------------------+
| delayed_commits                      | delayed_commits                       |
+--------------------------------------+---------------------------------------+
| max_attachment_chunk_size            | max_attachment_chunk_size             |
+--------------------------------------+---------------------------------------+
| max_dbs_open                         | max_dbs_open                          |
+--------------------------------------+---------------------------------------+
| max_document_size                    | max_document_size                     |
+--------------------------------------+---------------------------------------+
| os_process_timeout                   | os_process_timeout                    |
+--------------------------------------+---------------------------------------+
| uri_file                             | uri_file                              |
+--------------------------------------+---------------------------------------+
| util_driver_dir                      | util_driver_dir                       |
+--------------------------------------+---------------------------------------+
| view_index_dir                       | view_index_dir                        |
+--------------------------------------+---------------------------------------+

daemons Configuration Options
-----------------------------

+--------------------------------------+---------------------------------------+
| Option                               | Description                           |
+======================================+=======================================+
| auth_cache                           | auth_cache                            |
+--------------------------------------+---------------------------------------+
| db_update_notifier                   | db_update_notifier                    |
+--------------------------------------+---------------------------------------+
| external_manager                     | external_manager                      |
+--------------------------------------+---------------------------------------+
| httpd                                | httpd                                 |
+--------------------------------------+---------------------------------------+
| httpsd                               | Enabled HTTPS service                 |
+--------------------------------------+---------------------------------------+
| query_servers                        | query_servers                         |
+--------------------------------------+---------------------------------------+
| stats_aggregator                     | stats_aggregator                      |
+--------------------------------------+---------------------------------------+
| stats_collector                      | stats_collector                       |
+--------------------------------------+---------------------------------------+
| uuids                                | uuids                                 |
+--------------------------------------+---------------------------------------+
| view_manager                         | view_manager                          |
+--------------------------------------+---------------------------------------+

httpd_db_handlers Configuration Options
---------------------------------------

+--------------------------------------+---------------------------------------+
| Option                               | Description                           |
+======================================+=======================================+
| _changes                             | _changes                              |
+--------------------------------------+---------------------------------------+
| _compact                             | _compact                              |
+--------------------------------------+---------------------------------------+
| _design                              | _design                               |
+--------------------------------------+---------------------------------------+
| _temp_view                           | _temp_view                            |
+--------------------------------------+---------------------------------------+
| _view_cleanup                        | _view_cleanup                         |
+--------------------------------------+---------------------------------------+

couch_httpd_auth Configuration Options
--------------------------------------

+--------------------------------------+---------------------------------------+
| Option                               | Description                           |
+======================================+=======================================+
| auth_cache_size                      | auth_cache_size                       |
+--------------------------------------+---------------------------------------+
| authentication_db                    | authentication_db                     |
+--------------------------------------+---------------------------------------+
| authentication_redirect              | authentication_redirect               |
+--------------------------------------+---------------------------------------+
| require_valid_user                   | require_valid_user                    |
+--------------------------------------+---------------------------------------+
| timeout                              | timeout                               |
+--------------------------------------+---------------------------------------+

httpd Configuration Options
---------------------------

+--------------------------------------+---------------------------------------+
| Option                               | Description                           |
+======================================+=======================================+
| allow_jsonp                          | allow_jsonp                           |
+--------------------------------------+---------------------------------------+
| authentication_handlers              | authentication_handlers               |
+--------------------------------------+---------------------------------------+
| bind_address                         | bind_address                          |
+--------------------------------------+---------------------------------------+
| default_handler                      | default_handler                       |
+--------------------------------------+---------------------------------------+
| max_connections                      | max_connections                       |
+--------------------------------------+---------------------------------------+
| nodelay                              | Enable TCP_NODELAY                    |
+--------------------------------------+---------------------------------------+
| port                                 | port                                  |
+--------------------------------------+---------------------------------------+
| secure_rewrites                      | secure_rewrites                       |
+--------------------------------------+---------------------------------------+
| vhost_global_handlers                | vhost_global_handlers                 |
+--------------------------------------+---------------------------------------+

httpd_design_handlers Configuration Options
-------------------------------------------

+--------------------------------------+---------------------------------------+
| Option                               | Description                           |
+======================================+=======================================+
| _info                                | _info                                 |
+--------------------------------------+---------------------------------------+
| _list                                | _list                                 |
+--------------------------------------+---------------------------------------+
| _rewrite                             | _rewrite                              |
+--------------------------------------+---------------------------------------+
| _show                                | _show                                 |
+--------------------------------------+---------------------------------------+
| _update                              | _update                               |
+--------------------------------------+---------------------------------------+
| _view                                | _view                                 |
+--------------------------------------+---------------------------------------+

httpd_global_handlers Configuration Options
-------------------------------------------

+--------------------------------------+---------------------------------------+
| Option                               | Description                           |
+======================================+=======================================+
| /                                    | /                                     |
+--------------------------------------+---------------------------------------+
| _active_tasks                        | _active_tasks                         |
+--------------------------------------+---------------------------------------+
| _all_dbs                             | _all_dbs                              |
+--------------------------------------+---------------------------------------+
| _config                              | _config                               |
+--------------------------------------+---------------------------------------+
| _log                                 | _log                                  |
+--------------------------------------+---------------------------------------+
| _oauth                               | _oauth                                |
+--------------------------------------+---------------------------------------+
| _replicate                           | _replicate                            |
+--------------------------------------+---------------------------------------+
| _restart                             | _restart                              |
+--------------------------------------+---------------------------------------+
| _session                             | _session                              |
+--------------------------------------+---------------------------------------+
| _stats                               | _stats                                |
+--------------------------------------+---------------------------------------+
| _utils                               | _utils                                |
+--------------------------------------+---------------------------------------+
| _uuids                               | _uuids                                |
+--------------------------------------+---------------------------------------+
| favicon.ico                          | favicon.ico                           |
+--------------------------------------+---------------------------------------+

log Configuration Options
-------------------------

+--------------------------------------+---------------------------------------+
| Option                               | Description                           |
+======================================+=======================================+
| file                                 | file                                  |
+--------------------------------------+---------------------------------------+
| include_sasl                         | include_sasl                          |
+--------------------------------------+---------------------------------------+
| level                                | level                                 |
+--------------------------------------+---------------------------------------+

query_servers Configuration Options
-----------------------------------

+--------------------------------------+---------------------------------------+
| Option                               | Description                           |
+======================================+=======================================+
| javascript                           | javascript                            |
+--------------------------------------+---------------------------------------+

query_server_config Configuration Options
-----------------------------------------

+--------------------------------------+---------------------------------------+
| Option                               | Description                           |
+======================================+=======================================+
| reduce_limit                         | reduce_limit                          |
+--------------------------------------+---------------------------------------+

replicator Configuration Options
--------------------------------

+--------------------------------------+---------------------------------------+
| Option                               | Description                           |
+======================================+=======================================+
| max_http_pipeline_size               | max_http_pipeline_size                |
+--------------------------------------+---------------------------------------+
| max_http_sessions                    | max_http_sessions                     |
+--------------------------------------+---------------------------------------+

stats Configuration Options
---------------------------

+--------------------------------------+---------------------------------------+
| Option                               | Description                           |
+======================================+=======================================+
| rate                                 | rate                                  |
+--------------------------------------+---------------------------------------+
| samples                              | samples                               |
+--------------------------------------+---------------------------------------+

uuids Configuration Options
---------------------------

+--------------------------------------+---------------------------------------+
| Option                               | Description                           |
+======================================+=======================================+
| algorithm                            | algorithm                             |
+--------------------------------------+---------------------------------------+
