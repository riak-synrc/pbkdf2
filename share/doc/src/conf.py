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

# http://sphinx.pocoo.org/config.html

import sys, os

extensions = ["sphinx.ext.todo"]

source_suffix = ".rst"

master_doc = "index"

nitpicky = True

project = "@@"

copyright = "@@"

version = "@@"

release = "@@"

highlight_language = "json"

pygments_style = "sphinx"

# @@ design CouchDB theme

# @@ check out http://sphinx.pocoo.org/theming.html

html_theme = "default"

html_short_title = "@@"

html_logo = ""

html_favicon = ""

html_use_opensearch = ""

text_newlines = "native"

latex_documents = [(
	"index",
	"CouchDB.tex",
	"",
	"",
	"manual",
	True
)]

texinfo_documents = [(
	"index",
	"content",
	"",
	"",
	"dir_entry",
	"description",
	"category",
	"manual",
	True
)]