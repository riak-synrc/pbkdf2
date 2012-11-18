## Licensed under the Apache License, Version 2.0 (the "License"); you may not
## use this file except in compliance with the License. You may obtain a copy of
## the License at
##
##   http://www.apache.org/licenses/LICENSE-2.0
##
## Unless required by applicable law or agreed to in writing, software
## distributed under the License is distributed on an "AS IS" BASIS, WITHOUT
## WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied. See the
## License for the specific language governing permissions and limitations under
## the License.

SUBDIRS = \
    couch_index \
    couch_mrview \
    couch_replicator \
    couchdb \
    ejson \
    erlang-oauth \
    etap \
    ibrowse \
    snappy

mochiweb_file_collection = \
    mochiweb/src/mochifmt.erl \
    mochiweb/src/mochifmt_records.erl \
    mochiweb/src/mochifmt_std.erl \
    mochiweb/src/mochiglobal.erl \
    mochiweb/src/mochihex.erl \
    mochiweb/src/mochijson.erl \
    mochiweb/src/mochijson2.erl \
    mochiweb/src/mochilists.erl \
    mochiweb/src/mochilogfile2.erl \
    mochiweb/src/mochinum.erl \
    mochiweb/src/mochitemp.erl \
    mochiweb/src/mochiutf8.erl \
    mochiweb/src/mochiweb.app.src \
    mochiweb/src/mochiweb.erl \
    mochiweb/src/mochiweb_acceptor.erl \
    mochiweb/src/mochiweb_charref.erl \
    mochiweb/src/mochiweb_cookies.erl \
    mochiweb/src/mochiweb_cover.erl \
    mochiweb/src/mochiweb_echo.erl \
    mochiweb/src/mochiweb_headers.erl \
    mochiweb/src/mochiweb_html.erl \
    mochiweb/src/mochiweb_http.erl \
    mochiweb/src/mochiweb_io.erl \
    mochiweb/src/mochiweb_mime.erl \
    mochiweb/src/mochiweb_multipart.erl \
    mochiweb/src/mochiweb_request.erl \
    mochiweb/src/mochiweb_request_tests.erl \
    mochiweb/src/mochiweb_response.erl \
    mochiweb/src/mochiweb_socket.erl \
    mochiweb/src/mochiweb_socket_server.erl \
    mochiweb/src/mochiweb_util.erl \
    mochiweb/src/reloader.erl

mochiwebebin_make_generated_file_list = \
    mochiweb/ebin/mochifmt.beam \
    mochiweb/ebin/mochifmt_records.beam \
    mochiweb/ebin/mochifmt_std.beam \
    mochiweb/ebin/mochiglobal.beam \
    mochiweb/ebin/mochihex.beam \
    mochiweb/ebin/mochijson.beam \
    mochiweb/ebin/mochijson2.beam \
    mochiweb/ebin/mochilists.beam \
    mochiweb/ebin/mochilogfile2.beam \
    mochiweb/ebin/mochinum.beam \
    mochiweb/ebin/mochitemp.beam \
    mochiweb/ebin/mochiutf8.beam \
    mochiweb/ebin/mochiweb.app \
    mochiweb/ebin/mochiweb.beam \
    mochiweb/ebin/mochiweb_acceptor.beam \
    mochiweb/ebin/mochiweb_charref.beam \
    mochiweb/ebin/mochiweb_cookies.beam \
    mochiweb/ebin/mochiweb_cover.beam \
    mochiweb/ebin/mochiweb_echo.beam \
    mochiweb/ebin/mochiweb_headers.beam \
    mochiweb/ebin/mochiweb_html.beam \
    mochiweb/ebin/mochiweb_http.beam \
    mochiweb/ebin/mochiweb_io.beam \
    mochiweb/ebin/mochiweb_mime.beam \
    mochiweb/ebin/mochiweb_multipart.beam \
    mochiweb/ebin/mochiweb_request.beam \
    mochiweb/ebin/mochiweb_response.beam \
    mochiweb/ebin/mochiweb_socket.beam \
    mochiweb/ebin/mochiweb_socket_server.beam \
    mochiweb/ebin/mochiweb_util.beam \
    mochiweb/ebin/reloader.beam

mochiwebebindir = $(localerlanglibdir)/mochiweb-2.3.2/ebin
mochiwebebin_DATA = \
    $(mochiwebebin_make_generated_file_list)


$(mochiwebebin_make_generated_file_list): mochiweb.stamp
	@if test -f $@; then :; else \
		rm -f mochiweb.stamp; \
		$(MAKE) $(AM_MAKEFLAGS) mochiweb.stamp; \
	fi

mochiweb.stamp: $(mochiweb_file_collection)
	@rm -f mochiweb.tmp
	@touch mochiweb.tmp
	make -C mochiweb all
	@mv -f mochiweb.tmp $@

DISTCLEANFILES = \
	mochiweb.stamp
