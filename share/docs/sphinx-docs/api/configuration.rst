=====================
Configuration Methods
=====================

The CouchDB API Server Configuration Methods provide an interface to
query and update the various configuration values within a running
CouchDB instance.

``GET /_config``
================

Returns the entire CouchDB server configuration as a JSON structure. The
structure is organized by different configuration sections, with
individual values.

For example, to get the configuration for a server:

::

    GET http://couchdb:5984/_config
    Accept: application/json

The response is the JSON structure:

::

    {
       "query_server_config" : {
          "reduce_limit" : "true"
       },
       "couchdb" : {
          "os_process_timeout" : "5000",
          "max_attachment_chunk_size" : "4294967296",
          "max_document_size" : "4294967296",
          "uri_file" : "/var/lib/couchdb/couch.uri",
          "max_dbs_open" : "100",
          "view_index_dir" : "/var/lib/couchdb",
          "util_driver_dir" : "/usr/lib64/couchdb/erlang/lib/couch-1.0.1/priv/lib",
          "database_dir" : "/var/lib/couchdb",
          "delayed_commits" : "true"
       },
       "attachments" : {
          "compressible_types" : "text/*, application/javascript, application/json,  application/xml",
          "compression_level" : "8"
       },
       "uuids" : {
          "algorithm" : "utc_random"
       },
       "daemons" : {
          "view_manager" : "{couch_view, start_link, []}",
          "auth_cache" : "{couch_auth_cache, start_link, []}",
          "uuids" : "{couch_uuids, start, []}",
          "stats_aggregator" : "{couch_stats_aggregator, start, []}",
          "query_servers" : "{couch_query_servers, start_link, []}",
          "httpd" : "{couch_httpd, start_link, []}",
          "stats_collector" : "{couch_stats_collector, start, []}",
          "db_update_notifier" : "{couch_db_update_notifier_sup, start_link, []}",
          "external_manager" : "{couch_external_manager, start_link, []}"
       },
       "stats" : {
          "samples" : "[0, 60, 300, 900]",
          "rate" : "1000"
       },
       "httpd" : {
          "vhost_global_handlers" : "_utils, _uuids, _session, _oauth, _users",
          "secure_rewrites" : "true",
          "authentication_handlers" : "{couch_httpd_oauth, oauth_authentication_handler},
                                       {couch_httpd_auth, cookie_authentication_handler},
                                       {couch_httpd_auth, default_authentication_handler}",
          "port" : "5984",
          "default_handler" : "{couch_httpd_db, handle_request}",
          "allow_jsonp" : "false",
          "bind_address" : "192.168.0.2",
          "max_connections" : "2048"
       },
       "query_servers" : {
          "javascript" : "/usr/bin/couchjs /usr/share/couchdb/server/main.js"
       },
       "couch_httpd_auth" : {
          "authentication_db" : "_users",
          "require_valid_user" : "false",
          "authentication_redirect" : "/_utils/session.html",
          "timeout" : "600",
          "auth_cache_size" : "50"
       },
       "httpd_db_handlers" : {
          "_design" : "{couch_httpd_db, handle_design_req}",
          "_compact" : "{couch_httpd_db, handle_compact_req}",
          "_view_cleanup" : "{couch_httpd_db, handle_view_cleanup_req}",
          "_temp_view" : "{couch_httpd_view, handle_temp_view_req}",
          "_changes" : "{couch_httpd_db, handle_changes_req}"
       },
       "replicator" : {
          "max_http_sessions" : "10",
          "max_http_pipeline_size" : "10"
       },
       "log" : {
          "include_sasl" : "true",
          "level" : "info",
          "file" : "/var/log/couchdb/couch.log"
       },
       "httpd_design_handlers" : {
          "_update" : "{couch_httpd_show, handle_doc_update_req}",
          "_show" : "{couch_httpd_show, handle_doc_show_req}",
          "_info" : "{couch_httpd_db,   handle_design_info_req}",
          "_list" : "{couch_httpd_show, handle_view_list_req}",
          "_view" : "{couch_httpd_view, handle_view_req}",
          "_rewrite" : "{couch_httpd_rewrite, handle_rewrite_req}"
       },
       "httpd_global_handlers" : {
          "_replicate" : "{couch_httpd_misc_handlers, handle_replicate_req}",
          "/" : "{couch_httpd_misc_handlers, handle_welcome_req, <<\"Welcome\">>}",
          "_config" : "{couch_httpd_misc_handlers, handle_config_req}",
          "_utils" : "{couch_httpd_misc_handlers, handle_utils_dir_req, \"/usr/share/couchdb/www\"}",
          "_active_tasks" : "{couch_httpd_misc_handlers, handle_task_status_req}",
          "_session" : "{couch_httpd_auth, handle_session_req}",
          "_log" : "{couch_httpd_misc_handlers, handle_log_req}",
          "favicon.ico" : "{couch_httpd_misc_handlers, handle_favicon_req, \"/usr/share/couchdb/www\"}",
          "_all_dbs" : "{couch_httpd_misc_handlers, handle_all_dbs_req}",
          "_oauth" : "{couch_httpd_oauth, handle_oauth_req}",
          "_restart" : "{couch_httpd_misc_handlers, handle_restart_req}",
          "_uuids" : "{couch_httpd_misc_handlers, handle_uuids_req}",
          "_stats" : "{couch_httpd_stats_handlers, handle_stats_req}"
       }
    }
        

``GET /_config/section``
========================

Gets the configuration structure for a single section. For example, to
retrieve the CouchDB configuration section values:

::

    GET http://couchdb:5984/_config/couchdb
    Accept: application/json

The returned JSON contains just the configuration values for this
section:

::

    {
       "os_process_timeout" : "5000",
       "max_attachment_chunk_size" : "4294967296",
       "max_document_size" : "4294967296",
       "uri_file" : "/var/lib/couchdb/couch.uri",
       "max_dbs_open" : "100",
       "view_index_dir" : "/var/lib/couchdb",
       "util_driver_dir" : "/usr/lib64/couchdb/erlang/lib/couch-1.0.1/priv/lib",
       "database_dir" : "/var/lib/couchdb",
       "delayed_commits" : "true"
    }

``GET /_config/section/key``
============================

Gets a single configuration value from within a specific configuration
section. For example, to obtain the current log level:

::

    GET http://couchdb:5984/_config/log/level
    Accept: application/json

Returns the string of the log level:

::

    "info"

    **Note**

    The returned value will be the JSON of the value, which may be a
    string or numeric value, or an array or object. Some client
    environments may not parse simple strings or numeric values as valid
    JSON.

``PUT /_config/section/key``
============================

Updates a configuration value. The new value should be supplied in the
request body in the corresponding JSON format. For example, if you are
setting a string value, you must supply a valid JSON string.

For example, to set the function used to generate UUIDs by the
``GET /_uuids`` API call to use the ``utc_random`` generator:

::

    PUT http://couchdb:5984/_config/uuids/algorithm
    Content-Type: application/json

    "utc_random"

The return value will be empty, with the response code indicating the
success or failure of the configuration setting.

``DELETE /_config/section/key``
===============================

Deletes a configuration value. The returned JSON will be the value of
the configuration parameter before it was deleted. For example, to
delete the UUID parameter:

::

    DELETE http://couchdb:5984/_config/uuids/algorithm
    Content-Type: application/json

The returned value is the last configured UUID function:

::

    "random"
