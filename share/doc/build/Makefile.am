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

info_TEXINFOS = texinfo/CouchDB.texi

info_node = texinfo/node.texi

dist_localdoc_DATA = latex/CouchDB.pdf.gz

nobase_dist_localdoc_DATA = \
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

EXTRA_DIST = \
	html/.buildinfo \
	$(info_node)

# Total bodge. Automake expects Texinfo documents to be hand authored, so there
# is no supported way talk about Texinfo files that need to be built. Instead,
# we're hooking on to a generated target we're not supposed to know about.
$(srcdir)/texinfo/CouchDB: $(info_node)

$(info_node): texinfo

$(dist_localdoc_DATA): pdf

$(nobase_dist_localdoc_DATA): html

# @@ convert both targets to use stamp files
# http://www.gnu.org/software/automake/manual/html_node/Multiple-Outputs.html

# @@ move conf.py and use and set variables

# @@ figure out if we should distribute the PDF in the dist dir

# @@ seems to get called twice during the build
# @@ use stamp files, this gets called twice during install too

texinfo:
	$(SPHINXBUILD) -b texinfo $(ALLSPHINXOPTS) texinfo

dvi:
	@echo "This target is not supported."

ps:
	@echo "This target is not supported."

pdf:
	$(SPHINXBUILD) -b latex $(ALLSPHINXOPTS) latex
	$(MAKE) -C latex all-pdf
	-gzip -9 < latex/CouchDB.pdf > latex/CouchDB.pdf.gz

html: ../src/_static
	$(SPHINXBUILD) -b html $(ALLSPHINXOPTS) html

install-dvi:
	@echo "This target is not supported."

install-ps:
	@echo "This target is not supported."

install-pdf:
	@echo "This target is not supported. Use \`install-data\` instead."

install-html:
	@echo "This target is not supported. Use \`install-data\` instead."

# @@ figure out how to remove this _static dir
../src/_static:
	mkdir -p $@

# @@ figure out how to move conf.py up a dir
# http://sphinx.pocoo.org/glossary.html#term-configuration-directory

# @@ check that maintainer-clean works for help2man
maintainer-clean-local:
	rmdir ../src/_static
	rm -fr doctrees
	rm -fr html
	rm -fr latex

# @@ can we remove _static?

# @@ what is the license for conf.py?

# @@ need to explicitly put the license in the readable part of the doc

# @@ tweak conf.py
# http://sphinx.pocoo.org/config.html

# "make dvi" fails
# "make ps" fails

# @@ fix package name which turns out as "apache-couchdb"
