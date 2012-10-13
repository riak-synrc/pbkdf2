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

# You can set these variables from the command line.
SPHINXOPTS    =
SPHINXBUILD   = sphinx-build
PAPER         =

# Internal variables.
PAPEROPT_a4     = -D latex_paper_size=a4
PAPEROPT_letter = -D latex_paper_size=letter
ALLSPHINXOPTS   = -d doctrees $(PAPEROPT_$(PAPER)) $(SPHINXOPTS) ../src
# the i18n builder cannot share the environment and doctrees with the others
I18NSPHINXOPTS  = $(PAPEROPT_$(PAPER)) $(SPHINXOPTS) source

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
	../src/conf.py \
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

info_TEXINFOS = $(texinfo_files)

dist_localdoc_DATA = $(pdf_files)

nobase_dist_localdoc_DATA = $(html_files)

EXTRA_DIST = \
	../demo.mk \
	../make.bat \
	../Makefile.am \
	$(image_files) \
	$(src_files) \
	html/.buildinfo

$(dist_localdoc_DATA): pdf

$(nobase_dist_localdoc_DATA): html

# @@ convert both targets to use stamp files
# http://www.gnu.org/software/automake/manual/html_node/Multiple-Outputs.html

# @@ move conf.py and use and set variables

# @@ figure out if we should distribute the PDF in the dist dir

# @@ seems to get called twice during the build
# @@ use stamp files, this gets called twice during install too

# @@ replace "test !" in configure.ac with "test -f X; then :; else" for portability

# @@ stamps break parallel builds and "make -n" promise

$(texinfo_files): texinfo.stamp
	@if test -f $@; then :; else \
	    rm -f texinfo.stamp; \
	    $(MAKE) $(AM_MAKEFLAGS) texinfo.stamp; \
	fi

texinfo.stamp: $(image_files) $(src_files)
	@rm -f texinfo.tmp
	@touch texinfo.tmp
	$(SPHINXBUILD) -b texinfo $(ALLSPHINXOPTS) texinfo
	@mv -f texinfo.tmp $@

$(pdf_files): pdf.stamp
	@if test -f $@; then :; else \
	    rm -f pdf.stamp; \
	    $(MAKE) $(AM_MAKEFLAGS) pdf.stamp; \
	fi

pdf.stamp: $(image_files) $(src_files)
	@rm -f pdf.tmp
	@touch pdf.tmp
	$(SPHINXBUILD) -b latex $(ALLSPHINXOPTS) latex
	$(MAKE) -C latex all-pdf
	-gzip -9 < latex/CouchDB.pdf > latex/CouchDB.pdf.gz
	@mv -f pdf.tmp $@

$(html_files): html.stamp
	@if test -f $@; then :; else \
	    rm -f html.stamp; \
	    $(MAKE) $(AM_MAKEFLAGS) html.stamp; \
	fi

html.stamp: $(image_files) $(src_files)
	@rm -f html.tmp
	@touch html.tmp
	$(SPHINXBUILD) -b html $(ALLSPHINXOPTS) html
	@mv -f html.tmp $@

dvi:
	@echo "This target is not supported."

ps:
	@echo "This target is not supported."

pdf:
	$(MAKE) $(AM_MAKEFLAGS) pdf.stamp

html:
	$(MAKE) $(AM_MAKEFLAGS) html.stamp

install-dvi:
	@echo "This target is not supported."

install-ps:
	@echo "This target is not supported."

install-pdf:
	@echo "This target is not supported. Try \`install-data' instead."

install-html:
	@echo "This target is not supported. Try \`install-data' instead."

# @@ figure out how to move conf.py up a dir
# http://sphinx.pocoo.org/glossary.html#term-configuration-directory

# @@ check that maintainer-clean works for help2man
maintainer-clean-local:
	rmdir ../src/_static
	rm -fr doctrees
	rm -fr html
	rm -fr latex
	rm -fr texinfo

# @@ can we remove _static?

# @@ what is the license for conf.py?

# @@ need to explicitly put the license in the readable part of the doc

# @@ tweak conf.py
# http://sphinx.pocoo.org/config.html

# "make dvi" fails
# "make ps" fails

# @@ fix package name which turns out as "apache-couchdb"
