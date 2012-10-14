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

# -----------------------------------------------------------------------------

# @@ TODO LIST BEFORE WE CAN MERGE

# @@ add RM checks to configure.ac

# @@ improve conf.py settings

# @@ replace "test !" in configure.ac with "test -f X; then :; else" for portability

# @@ stamps break parallel builds and "make -n" promise

# @@ check that maintainer-clean works for help2man

# @@ can we remove _static?

# @@ need to explicitly put the license in the readable part of the doc

# @@ fix package name which turns out as "apache-couchdb"

# @@ sort out sphinx variables in this file

# -----------------------------------------------------------------------------

SPHINXOPTS = \
    -d doctrees \
    -n -c $(srcdir)/.. \
    $(srcdir)/../src

# -----------------------------------------------------------------------------

texinfo_files = texinfo/CouchDB.texi 

pdf_files = latex/CouchDB.pdf.gz

html_files = \
    html/_images/futon-createdb.png \
    html/_images/futon-editdoc.png \
    html/_images/futon-editeddoc.png \
    html/_images/futon-overview.png \
    html/_images/futon-replform.png \
    html/_sources/api/authn.txt \
    html/_sources/api/configuration.txt \
    html/_sources/api/database.txt \
    html/_sources/api/dbmaint.txt \
    html/_sources/api/design.txt \
    html/_sources/api/documents.txt \
    html/_sources/api/local.txt \
    html/_sources/api/misc.txt \
    html/_sources/api/reference.txt \
    html/_sources/api-basics.txt \
    html/_sources/changes.txt \
    html/_sources/commonjs.txt \
    html/_sources/config_reference.txt \
    html/_sources/configuring.txt \
    html/_sources/ddocs.txt \
    html/_sources/errors.txt \
    html/_sources/http-proxying.txt \
    html/_sources/index.txt \
    html/_sources/intro.txt \
    html/_sources/json-structure.txt \
    html/_sources/os-daemons.txt \
    html/_sources/range.txt \
    html/_sources/release.txt \
    html/_sources/replication.txt \
    html/_sources/ssl.txt \
    html/_static/ajax-loader.gif \
    html/_static/basic.css \
    html/_static/comment-bright.png \
    html/_static/comment-close.png \
    html/_static/comment.png \
    html/_static/default.css \
    html/_static/doctools.js \
    html/_static/down-pressed.png \
    html/_static/down.png \
    html/_static/file.png \
    html/_static/jquery.js \
    html/_static/minus.png \
    html/_static/plus.png \
    html/_static/pygments.css \
    html/_static/searchtools.js \
    html/_static/sidebar.js \
    html/_static/underscore.js \
    html/_static/up-pressed.png \
    html/_static/up.png \
    html/_static/websupport.js \
    html/api/authn.html \
    html/api/configuration.html \
    html/api/database.html \
    html/api/dbmaint.html \
    html/api/design.html \
    html/api/documents.html \
    html/api/local.html \
    html/api/misc.html \
    html/api/reference.html \
    html/api-basics.html \
    html/changes.html \
    html/commonjs.html \
    html/config_reference.html \
    html/configuring.html \
    html/ddocs.html \
    html/errors.html \
    html/genindex.html \
    html/http-proxying.html \
    html/index.html \
    html/intro.html \
    html/json-structure.html \
    html/objects.inv \
    html/os-daemons.html \
    html/range.html \
    html/release.html \
    html/replication.html \
    html/search.html \
    html/searchindex.js \
    html/ssl.html

image_files = \
	../images/epub-icon.png \
	../images/futon-createdb.png \
	../images/futon-editdoc.png \
	../images/futon-editeddoc.png \
	../images/futon-overview.png \
	../images/futon-replform.png
	
src_files = \
	../src/api/authn.rst \
	../src/api/configuration.rst \
	../src/api/database.rst \
	../src/api/dbmaint.rst \
	../src/api/design.rst \
	../src/api/documents.rst \
	../src/api/local.rst \
	../src/api/misc.rst \
	../src/api/reference.rst \
	../src/api-basics.rst \
	../src/changes.rst \
	../src/commonjs.rst \
	../src/config_reference.rst \
	../src/configuring.rst \
	../src/ddocs.rst \
	../src/errors.rst \
	../src/http-proxying.rst \
	../src/index.rst \
	../src/intro.rst \
	../src/json-structure.rst \
	../src/os-daemons.rst \
	../src/range.rst \
	../src/release.rst \
	../src/replication.rst \
	../src/ssl.rst

dist_localdoc_DATA = \
    $(texinfo_files) \
    $(pdf_files)

nobase_dist_localdoc_DATA = $(html_files)

EXTRA_DIST = \
    ../conf.py \
	../demo.mk \
	../make.bat \
	../Makefile.am \
	$(image_files) \
	$(src_files) \
	html/.buildinfo

$(pdf_files): pdf

$(html_files): html

$(texinfo_files): texinfo.stamp
	@if test -f $@; then :; else \
	    rm -f texinfo.stamp; \
	    $(MAKE) $(AM_MAKEFLAGS) texinfo.stamp; \
	fi

texinfo.stamp: $(image_files) $(src_files)
	@rm -f texinfo.tmp
	@touch texinfo.tmp
	if test -n "`which makeinfo`"; then \
	    $(top_srcdir)/build-aux/missing makeinfo; \
	fi
	if test -n "`which sphinx-build`"; then \
	    sphinx-build -b texinfo $(SPHINXOPTS) $(builddir)/texinfo; \
	else \
	    $(top_srcdir)/build-aux/missing sphinx-build; \
	fi
	@mv -f texinfo.tmp $@

$(pdf_files): pdf.stamp
	@if test -f $@; then :; else \
	    rm -f pdf.stamp; \
	    $(MAKE) $(AM_MAKEFLAGS) pdf.stamp; \
	fi

pdf.stamp: $(image_files) $(src_files)
	@rm -f pdf.tmp
	@touch pdf.tmp
	if test -n "`which pdflatex`"; then \
	    $(top_srcdir)/build-aux/missing pdflatex; \
	fi
	if test -n "`which sphinx-build`"; then \
	    sphinx-build -b latex $(SPHINXOPTS) $(builddir)/latex; \
	    $(MAKE) -C latex all-pdf; \
	    gzip -9 < latex/CouchDB.pdf > latex/CouchDB.pdf.gz; \
	else \
	    $(top_srcdir)/build-aux/missing sphinx-build; \
	fi
	@mv -f pdf.tmp $@

$(html_files): html.stamp
	@if test -f $@; then :; else \
	    rm -f html.stamp; \
	    $(MAKE) $(AM_MAKEFLAGS) html.stamp; \
	fi

html.stamp: $(image_files) $(src_files)
	@rm -f html.tmp
	@touch html.tmp
	if test -n "`which sphinx-build`"; then \
	    sphinx-build -b html $(SPHINXOPTS) $(builddir)/html; \
	else
	    $(top_srcdir)/build-aux/missing sphinx-build; \
	fi
	@mv -f html.tmp $@

pdf-local:
	$(MAKE) $(AM_MAKEFLAGS) pdf.stamp

html-local:
	$(MAKE) $(AM_MAKEFLAGS) html.stamp

install-data-local:
	$(MAKE) -C texinfo install-info;

uninstall-local:
	$(MAKE) -C texinfo uninstall-info;

DISTCLEANFILES = \
    texinfo.stamp \
    pdf.stamp \
    html.stamp

distclean-local:
	rm -fr doctrees
	rm -fr html
	rm -fr latex
	rm -fr texinfo
