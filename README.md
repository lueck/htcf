[![License GPLv3](https://img.shields.io/badge/license-GPL_3-green.svg)](http://www.gnu.org/licenses/gpl-3.0.txt)

# htfc - Haskell library and executable for generating and parsing files in Text Corpus Format (TCF) #

The
[`Text Corpus Format (TCF)`](http://weblicht.sfs.uni-tuebingen.de/weblichtwiki/index.php/The_TCF_Format)
is an XML data exchange format that has been developed within the
[`WebLicht`](http://weblicht.sfs.uni-tuebingen.de/weblichtwiki/index.php/Main_Page)
architecture to facilitate efficient interoperability of the WebLicht
services. `htcf` is a library for parsing and writing TCF data in the
haskell programming language and also a configurable tokenizer for XML
input.

`htcf` provides the `xml2tcf` commandline program that takes an XML
input file, e.g. in TEI P5 format, and generates TCF layers from
it. `xml2tcf` has a fancy feature: It does not only provide a start
and end character offsets of the tokens in relation to the text layer,
but also in relation to the source file form the input. These offsets
are provided in all layers, where it makes sense, e.g. in the tokens
layer and in the text structure layer. This makes it possible to
interrelate the TCF data to semantic annotations that were made to the
input file in a standoff manner.

`htcf` also provides a taxi for setting TCF data over to JSON, SQL or
raw haskell data: the `readtcf` commandline program. Currently it only
outputs raw haskell data.

# Layers currently supported #

<pre><code>
| Layer          | read (library) | write (library) | xml2tcf (exec) | readtcf (exec) |
|----------------+----------------+-----------------+----------------+----------------|
| text           | yes            | yes             | yes            | yes            |
| tokens         | yes            | yes             | yes            | yes            |
| text structure | no             | yes             | yes            | no             |
</code></pre>


# Installation #

[`stack`](https://docs.haskellstack.org), the haskell build tool, is
required for installation. After cloning `htcf` from github, `cd` into
the working directory and run `stack` like follows.

	$ stack setup
	$ stack build

Have fun with

	$ stack exec -- xml2tcf <INPUT.XML>
    $ stack exec -- readtcf <INPUT.TCF>

# Tests #

Some `QuickCheck` tests only make sense with real world XML input. They
need a TEI file from Deutsches Textarchiv which is not included in
this repository due to license conditions. But you can download
[Kants *Was ist Aufklaerung?*](http://www.deutschestextarchiv.de/book/download_xml/kant_aufklaerung_1784)
and put it into the `doc/examples` directory. Then run the tests.

    $ stack test


# xml2tcf #


<pre><code>
xml2tcf - generate TCF from XML input.

Usage: xml2tcf [-c|--config CONFIGFILE] [-a|--abbrevs ABBREVFILE]
               [-S|--no-structure] INFILE
  xml2tcf generates a TCF file from XML input. TCF is the Text Corpus Format
  defined for WebLicht.

Available options:
  -h,--help                Show this help text
  -c,--config CONFIGFILE   Specify a config file. Defaults to config.xml in the
                           working directory.
  -a,--abbrevs ABBREVFILE  Specify a abbreviations file. The file is expected to
                           be plain text with one abbreviation per line. Dots
                           shoult not be in there. Defaults to abbrevs.txt in
                           the working directory.
  -S,--no-structure        Do not output structure layer.
</code></pre>

`htcf` provides a [`config.xml`](config.xml) as a reasonable config
for parsing a TEI file and [`abbrevs.txt`](abbrevs.txt) as a starting
point for abbreviations, used to configure the tokenizer.

# License #

[GPL V3](http://www.gnu.org/licenses/gpl-3.0.txt)
