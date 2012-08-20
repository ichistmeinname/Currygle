\chapter{CurryDoc Instruction}\label{a:currydoc}

\chapter{Installation and usage}\label{a:currysearch}

\section{Installation}

This directory contains cabal-file, for the installation you simply
have to run cabal.

\begin{verbatim}
$ cabal configure 
$ cabal build 
$ cabal install
\end{verbatim}

Alternatively you can use the provided \emph{Setup.hs}.
\begin{verbatim}
$ runhaskell Setup.hs configure
$ runhaskell Setup.hs build
$ runhaskell Setup.hs install
\end{verbatim}

\section{How-to-use}

The installation process creates to binaries: *curryIndexer* and
*curryServer*. %
The first creates the index for a given directory that contains at
least one \emph{.cdoc}-file and a corresponding URI. %
The \emph{.cdoc}-file can be generated with the extended CurryDoc for
a given Curry module, the URI is the corresponding HTML documentation
that also can be generated with CurryDoc. %
This package provides an example-directory with three Curry modules
generated with CurryDoc that provide some \emph{.cdoc}-files. %
So you can generate an index with these examples and the documentation
provided by PAKCS. %
The curryIndexer can either generate a new index or update the
existing one, to distinguish between these options, you can use the
flag \emph{- -n} for generating a new index and \emph{- -u} for
updating the index.

\begin{verbatim}
$ curryIndexer ./example/CDOC_HTML 
   http://www.informatik.uni-kiel.de/~pakcs/lib/CDOC/ --n
$ curryIndexer ./example/CDOC_URL 
   http://www.informatik.uni-kiel.de/~pakcs/lib/CDOC/ --u
$ curryIndexer ./example/CDOC_XML 
   http://www.informatik.uni-kiel.de/~pakcs/lib/CDOC/ --u
\end{verbatim}

Since it can take a very long time to add each single module, you can
pass a \emph{.txt}-file as argument that consists of pairs of \emph{.cdoc-file} and
URI to generate the index.

\begin{verbatim}
$ curryIndexer ./example/test.txt --n
\end{verbatim}

If you have generated an index, you can run the web application on your
localhost on a given port with the curryServer.

\begin{verbatim}
$ curryServer -p 1337
\end{verbatim}

\chapter{User-Query Syntax}\label{a:syntax}
