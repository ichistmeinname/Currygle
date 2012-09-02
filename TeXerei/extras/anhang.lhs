\chapter{CurryDoc Instruction}\label{currydoc}
\section{Installation}

The installation requires a running Curry and curry2prolog system, we
recommend the PAKCS and
KICS2\footnote{\url{http://www-ps.informatik.uni-kiel.de/kics2/}}
implementation. \\

If you have a running Curry system, you can go on and modify the
included \emph{Makefile}. %
The first variable defines the path to your Curry implementation and
has to be modified. %
The following three stand for the provided libraries and the analysis
tool. %

\begin{verbatim}
CURRY=/path/to/your/CurryImplementation
LIB=${CURRY}/lib
META=${LIB}/meta
ANADIR=${CURRY}/tools/analysis
\end{verbatim}

The next path must link to the executable of your Curry implementation
and the last path must be modified to the directory of
the\emph{Makefile}.

\section{How-to-use}

You can either generate the \emph{.cdoc}-file itself or the HTML
representation, since the latter generates the \emph{.cdoc}-file as well.
 
\begin{verbatim}
$ currydoc [--nomarkdown] [--html|--tex|--cdoc] <module_name>
$ currydoc [--nomarkdown] [--html|--tex|--cdoc] 
   <doc directory> <module_name>
$ currydoc [--nomarkdown] --noindexhtml 
   <doc directory> <module_name>
$ currydoc --onlyindexhtml <doc directory> <module_names>
\end{verbatim}

\chapter{Installation and Usage of Curr(y)gle}\label{currysearch}

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

The installation process creates to binaries: \emph{curryIndexer} and
\emph{curryServer}. %
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

\chapter{User-Query Syntax}\label{syntax}

\section{EBNF}\label{a:ebnf}
%include ../extras/ebnf.lhs

\section{Rail-Road Diagram}\label{a:railroad}
\vspace{-0.6cm}
\begin{minipage}{0.85\textwidth}
\begin{minipage}{0.55\textwidth}
\includegraphics{bilder/Query}
\end{minipage}
\hspace{1.3cm}
\begin{minipage}{0.45\textwidth}
\includegraphics{bilder/Expr}
\end{minipage}

\begin{minipage}[t]{0.5\textwidth}
\vspace{1cm}

\includegraphics{bilder/String}

\vspace{1cm}
\includegraphics{bilder/Signature}

\vspace{1cm}
\includegraphics{bilder/ConsArgumentType} 

\end{minipage}
\begin{minipage}[t]{0.5\textwidth}

\vspace{1cm}

\includegraphics{bilder/identStart}
\includegraphics{bilder/identLetter}

\vspace{1.68cm}
\includegraphics{bilder/ConstructorType}

\vspace{1cm}
\includegraphics{bilder/Identifier}

\vspace{1cm}
\includegraphics{bilder/TypeVariable}
\includegraphics{bilder/ListType}
\end{minipage}

\vspace{0.5cm}
\begin{minipage}{0.5\textwidth}
\includegraphics{bilder/TupleType}
\end{minipage}
\hspace{3.25cm}
\begin{minipage}{0.5\textwidth}
\vspace{0.25cm}
\includegraphics{bilder/PrimitiveType}
\end{minipage}

\end{minipage}
\begin{minipage}{0.15\textwidth}
\vspace{0.5cm}
{\hspace{-0.5cm}
\includegraphics{bilder/Bool}}

\vspace{0.4cm}

\includegraphics{bilder/Operator}
\end{minipage}

\begin{center}
\includegraphics{bilder/Specifier}
\end{center}
