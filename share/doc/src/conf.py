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

import sys, os

extensions = ["sphinx.ext.todo"]

source_suffix = ".rst"

master_doc = "index"

nitpicky = True

version = "1.3"

release = "1.3.0"

project = u"Apache CouchDB"

copyright = u"2012, The Apache Software Foundation"

highlight_language = "json"

pygments_style = "sphinx"

html_theme = "default"

html_theme_options = {

	"footerbgcolor": "#465158", #
	"footertextcolor": "#ffffff",
	"sidebarbgcolor": "#E8ECEF", #
	"sidebarbtncolor": "#3c6e83",
	"sidebartextcolor": "#444444", #
	"sidebarlinkcolor": "#444444", #
	"relbarbgcolor": "#465158", #
	"relbartextcolor": "#ffffff",
	"relbarlinkcolor": "#ffffff",
	"bgcolor": "#ffffff",
	"textcolor": "#000000",
	"headbgcolor": "inherit", #
	"headtextcolor": "#111111", #
	"headlinkcolor": "#111111", #
	"linkcolor": "#355f7c",
	"visitedlinkcolor": "#355f7c",
	"codebgcolor": "#eeffcc",
	"codetextcolor": "#333333",

}

# 
# [theme]
# inherit = basic
# stylesheet = default.css
# pygments_style = sphinx
# 
# [options]
# rightsidebar = false
# stickysidebar = false
# collapsiblesidebar = false
# externalrefs = false
# 
# footerbgcolor    = #11303d
# footertextcolor  = #ffffff
# sidebarbgcolor   = #1c4e63
# sidebarbtncolor  = #3c6e83
# sidebartextcolor = #ffffff
# sidebarlinkcolor = #98dbcc
# relbarbgcolor    = #133f52
# relbartextcolor  = #ffffff
# relbarlinkcolor  = #ffffff
# bgcolor          = #ffffff
# textcolor        = #000000
# headbgcolor      = #f2f2f2
# headtextcolor    = #20435c
# headlinkcolor    = #c60f0f
# linkcolor        = #355f7c
# visitedlinkcolor = #355f7c
# codebgcolor      = #eeffcc
# codetextcolor    = #333333
# 
# bodyfont = sans-serif
# headfont = 'Trebuchet MS', sans-serif

html_logo = "../images/logo.png" # @@ waiting on logo

html_favicon = "../images/favicon.ico"

text_newlines = "native"

latex_documents = [(
       "index",
       "CouchDB.tex",
       project,
       "",
       "manual",
       True
)]

latex_elements = {
    "papersize":"a4paper"
}

texinfo_documents = [(
       "index",
       "CouchDB",
       project,
       "",
       "CouchDB",
       "The Apache CouchDB database",
       "Databases",
       True
)]
