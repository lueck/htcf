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
input file (manually) in a standoff manner.

`htcf` also provides a taxi for setting TCF data over to JSON, SQL or
raw haskell data: the `tcflayer` and `tcftokens` commandline
programs. They let you specify the output format and which layer to
get out of the file. It is useful for preparing bulk inserts into a
database. While `tcflayer` reads a single layer, `tcftokens` collects
information about tokens from all layers.



## Layers currently supported ##

<pre><code>
| Layer          | read (library) | write (library) | xml2tcf (exec) | tcflayer (exec) |
|----------------+----------------+-----------------+----------------+-----------------|
| text           | yes            | yes             | yes            | yes             |
| tokens         | yes            | yes             | yes            | yes             |
| sentences      | yes            | yes             |                | yes             |
| POStags        | yes            | yes             |                | yes             |
| lemmas         | yes            | yes             |                | yes             |
| text structure | no             | yes             | yes            | no              |
</code></pre>

Roadmap: Frequencies in the output of `tcftokens`

## Installation ##

[`stack`](https://docs.haskellstack.org), the haskell build tool, is
required for installation. After cloning `htcf` from github, `cd` into
the working directory and run `stack` like follows.

	$ cd <path-to-htcf>
	$ stack setup
	$ stack build

Have fun with

	$ stack exec -- xml2tcf [options] <INPUT.XML>
    $ stack exec -- tcflayer [options] <INPUT.TCF>

Running these programs with the `-h` option gives you a help message.

If you like, install the binaries within your system path by calling
the `stack install htcf` in the `htcf` working directory.


## Tests ##

Some `QuickCheck` tests only make sense with real world XML input. They
need a TEI file from Deutsches Textarchiv which is not included in
this repository due to license conditions. But you can download
[Kants *Was ist Aufklaerung?*](http://www.deutschestextarchiv.de/book/download_xml/kant_aufklaerung_1784)
and put it into the `doc/examples` directory. Then run the tests.

    $ stack test


## xml2tcf ##


<pre><code>
xml2tcf - generate TCF from XML input.

Usage: xml2tcf [-c|--config CONFIGFILE] [-a|--abbrevs ABBREVFILE]
               [-S|--no-structure] [-o|--output OUTFILE] INFILE
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
  -o,--output OUTFILE      Output file. If left, the TCF is printed to stdout.
</code></pre>

`htcf` provides a [`config.xml`](config.xml) as a reasonable config
for parsing a TEI file and [`abbrevs.txt`](abbrevs.txt) as a starting
point for abbreviations, used to configure the tokenizer.

Here is an example of the character offsets `xml2tcf` generates:

<pre><code>
    &lt;token>
      ...
      &lt;token id="w37b" start="5036" end="5045" srcStart="18726" srcEnd="18759">Aufklärung&lt;/token>
      &lt;token id="w37c" start="5047" end="5049" srcStart="18766" srcEnd="18775">iſt&lt;/token>
      &lt;token id="w37d" start="5051" end="5053" srcStart="18777" srcEnd="18779">der&lt;/token>
      &lt;token id="w37e" start="5055" end="5061" srcStart="18781" srcEnd="18787">Ausgang&lt;/token>
      &lt;token id="w37f" start="5063" end="5065" srcStart="18789" srcEnd="18791">des&lt;/token>
      &lt;token id="w380" start="5067" end="5074" srcStart="18793" srcEnd="18814">Menſchen&lt;/token>
	  ...
    &lt;/token>
	&lt;textstructure>
	  ...
	  &lt;textspan type="p" namespace="http://www.tei-c.org/ns/1.0" start="w37b" end="w3c8" textStart="5035" textEnd="5534" srcStart="18699" srcEnd="19556"/>
      &lt;textspan type="hi" namespace="http://www.tei-c.org/ns/1.0" start="w37b" end="w37b" textStart="5036" textEnd="5037" srcStart="18703" srcEnd="18731"/>
      &lt;textspan type="hi" namespace="http://www.tei-c.org/ns/1.0" start="w37b" end="w37b" textStart="5037" textEnd="5046" srcStart="18732" srcEnd="18764"/>
	  ...
    &lt;/textstructure>
</code></pre>

`xml2tcf` does not generate a source layer where the opening tags are
escaped to `&lt;`. This escaping isn't a bijective mapping from the
xml source to the string of the source layer. So this layer is worth
nothing in regard to the character offsets generated by
`xml2tcf`. Only a link to the source file and a md5 hash of it would
make sense here.

## Caveat ##

When reading TCF data with `tcflayer`, it automatically parses the IDs
of tokens, sentences etc. into integer counts. For that purpose the
common prefix of the tokens is stripped and the base of the resting
numerical part is calculated from the length of the "alphabet" of
digits in the numerical parts. This works for prefixed IDs encoded in
binary, octet, decimal, hexadecimal etc. format. But it only *works as
long as the count of tokens is not lesser than the base of their
IDs*. So you might run into problems with short texts. And there is an
additional constraint: The alphabet must utilize characters out of 0-9
first and a-z then, in ascending order.

## License ##

[GPL V3](http://www.gnu.org/licenses/gpl-3.0.txt)
