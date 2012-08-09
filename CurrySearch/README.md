Curr(y)gle
===========

Conctact : sad@informatik.uni-kiel.de

About
------

Curr(y)gle is an API search engine for the functional logic
programming language Curry
(http://www-ps.informatik.uni-kiel.de/currywiki/).  It can search for
modules, functions, types and signatures. Additionally Curr(y)gle
searches in description and author information.  Other search
restrictions cover non-deterministic or deterministic as well as rigid
or flexible functions.  For more information about the syntax of
queries, read the how-to-search section.

Curr(y)gle uses the Holumbus framework (http://holumbus.fh-wedel.de)
that specializes to build and configure highly-flexible search
engines.


Installation
-------------

This directory contains cabal-file, for the installation you simply
have to run cabal.

> $ cabal configure cabal build cabal install

Alternatively you can use the provided *Setup.hs*.  
> $ runhaskell Setup.hs configure 
> $ runhaskell Setup.hs build 
> $ runhaskell Setup.hs install


How-to-use
-----------

The installation process creates to binaries: *curryIndexer* and
*curryServer*.  The first creates the index for a given directory that
contains at least one .cdoc-file and a corresponding uri. The
.cdoc-file can be generated with the extended CurryDoc
(http://www-ps.informatik.uni-kiel.de/currywiki/tools/currydoc) for a
given Curry module, the uri is the corresponding HTML documentation
that also can be generated with CurryDoc.  This package provides an
example-directory with three Curry modules generated with CurryDoc
that provide some .cdoc-files. So you can generate an index with these
examples and the documentation provided by PAKCS
(http://www.informatik.uni-kiel.de/~pakcs/lib/CDOC/).  The
curryIndexer can either generate a new index or update the existing
one, to distinguish between these options, you can use the flag *--n*
for generating a new index and *--u* for updating the index.

> $ curryIndexer ./example/CDOC_HTML http://www.informatik.uni-kiel.de/~pakcs/lib/CDOC/ --n 
> $ curryIndexer ./example/CDOC_URL http://www.informatik.uni-kiel.de/~pakcs/lib/CDOC/ --u 
> $ curryIndexer ./example/CDOC_XML http://www.informatik.uni-kiel.de/~pakcs/lib/CDOC/ --u

Since it can take a very long time to add each single module, you can
pass a .txt-file as argument that consists of pairs of .cdoc-file and
uri to generate the index.

> $ curryIndexer ./example/test.txt --n

With a generated index, you can run the web application on your
localhost on a given port with the curryServer.

> $ curryServer -p 1337


How-to-search
--------------

There are several identifier available to
restrict your search.

* :module io - searches for modules starting with _io_ (also :m io), without a following name, it searches for all modules
* :function map - searches for functions starting with _map_ (also :f map), without a following name, it searches for all functions
* :type either - searches for types starting with either (also :t either), without a following name, it searches for all types
* :nondet [name] - searches for non-deterministic functions (also :nd), a following name is optional
* :det [name] - searches for deterministic functions (also :d), a following name is optional
* :flexible [name] - searches for flexible functions (also :fl), a following name is optional
* :rigid [name] - searches for rigid functions (also :ri), a following name is optional
* :signature a - searches for signature starting with _a_ (also :s a)
* upper character, _->_, tuples, lists are all identifiers to search for signatures
* :function map AND maybe - searches for a function starting with map and searches for maybe in all contexts (like description or signature)
* :function map maybe - does the same thing, because all parts of an query are applied by AND
* :type either OR :function either - OR can be used as binary operator
* io NOT :module io - searches for io in all contexts except for modules


Have fun searching, but remember: keep calm and curry on!