## Kappa to RDF converter

This package contains a proof-of-concept tool to convert annotated
Kappa files to RDF. It goes along with the paper "Annotation of
rule-based models with formal semantics to enable creation, analysis,
reuse and visualisation", to appear.

It contains a fork of the Kappa parser from
https://github.com/wwaites/reaction

### Quick start:

 * [optional] Make sure you have a recent version of the cabal tool
   that supports "sandbox" and do
```
% cabal sandbox init
```
 * Install dependencies
```
% cabal install --only-dependencies
% cabal install hunit
```
 * Check that everything is working properly
```
% cabal build
% cabal test
...
1 of 1 test suites (1 of 1 test cases) passed.
```
 * Investigate things in the REPL. For example,
```
% cabal repl
Flow.Kappa> declare [complex| A(x!1), A(x~2!1,y~p), A(x~1), B(u,v,w~1) |]
[A(x~1~2,y),B(w,u,v)]
```
 * Look at a materialised RDF version of an example model
```
% ./dist/build/krdf/krdf -f ../examples/tcs.kappa -a -n -m
```

### The krdf tool

The krdf tool transforms annotated Kappa files into RDF. By default it
will produce an empty graph as output. It has command line flags to
extract the annotations from the comments in the file (see the
`examples/tcs.kappa` file), to normalise agent patterns in rules
according to the declarations, and to materialise the rules into
RDF. All three of these options used together will extract the maximum
information from the source file. Using the normalise flag only makes
sense together with materialise since it operates on the information
latent in the Kappa rules.

```
Kappa -> RDF

Usage: krdf (-f|--filename FILENAME) [-a|--annotations] [-m|--materialise]
            [-n|--normalise]
  Transform Kappa rules to RDF

Available options:
  -h,--help                Show this help text
  -f,--filename FILENAME   Kappa file to read
  -a,--annotations         Extract annotations
  -m,--materialise         Materialise Kappa statements to RDF
  -n,--normalise           Normalise agent patterns according to declarations
```

The serialiser that this tool uses does not produce the prettiest
RDF/turtle serialisation. For a nicer looking version, the
[rapper](http://librdf.org) tool does a much better job. This tool,
and its very useful query counterpart [roqet](http://librdf.org) are
available in most recent operating systems.

To prettify the `examples/tcs.kappa` file, it can be used in
conjunction with `krdf` like so:
```
krdf -f ../examples/tcs.kappa -a -m -n | \
    rapper -i turtle -o turtle - file://examples/tcs.kappa
```

This representation is particularly useful for understanding the
pattern of the output in order to write queries against it.

### Queries

Using [roqet](http://librdf.org/) it is possible to run queries
in the [SPARQL](http://www.w3.org/TR/sparql11-query/) language
angainst the RDF graph produced by `krdf`. The
`examples/binding.sparql` file contains a query to extract the
forward part of the contact graph -- which agents and sites become
bound under a particular rule. An example command line is,
```
krdf -f ../examples/tcs.kappa -a -m -n | \
    roqet -D - -r table examples/binding.sparql
```
[roqet](http://librdf.org) can produce output in a variety of
machine-readable formats (json, xml, csv) as well as some human
readable ones (html, table).
