\chapter{Introduction}

This thesis examines the development of an API search engine for the
functional logic programming language Curry. %
Currently, online documentations are the only way to get an overview
of the existing functions and data types Curry provides. %
More precisely, PAKCS and KICS2, two popular implementations of the
Curry system, offer such online documentations for their provided
Curry modules. %
Unfortunately, the documentation is presented as list of modules;
under this circumstances, the search for a specific function is
complicated, unless one scans every modules or knows, where to start
the searching. %

The community of the functional programming language Haskell\cite{haskellreport} offers a
similar documentation system. %
The
hackageDB\footnote{\url{http://hackage.haskell.org/packages/hackage.html}}
is a collection of released Haskell packages; such packages mostly
consist of several modules that fulfill a specific functionality. %
In the course of time, the amount of released and uploaded packages
increased; currently, the database consists of more than 4.000
packages and 40.000 functions\footnote{Source:
  \url{http://holumbus.fh-wedel.de/hayoo}}. %
In 2004, Neil Mitchell started to work on a search engine for Haskell
packages and Hoogle\footnote{\url{http://haskell.org/hoogle}} was
born; the implementation was also done in Haskell. %
Hoogle browses all modules that are part of the Glasgow Haskell
Compiler\footnote{\url{http://haskell.org/ghc}}\footnote{\url{http://www.haskell.org/haskellwiki/GHC}}
(GHC), an open source compiler and interactive environment for
Haskell. %
Unfortunately, Hoogle browses \emph{only} these modules, but as we
learned, there are about 4000 of these packages that consist of even
more modules. %
Hence, in 2008, the FH Wedel decided to build
\emph{Hayoo!}\footnote{\url{http://holumbus.fh-wedel.de/hayoo/hayoo.html}},
a new search engine, written in Haskell, that gathers information
about all available packages and corresponding modules on hackageDB. %
Since we had the privilege to look into Haskell in the past year, we
are frequent users of both search engines. %
Hence, we think that such a search engine simplifies the work with a
language like Haskell because we can get a quick overview of functions
that already exist and use them in our implementations. %
In connection with Hayoo!, the FH Wedel also implemented a framework
to build highly-flexible search engines. %
And this is the starting point of our idea and this thesis: inspired
by Hayoo! and due to the existing framework, we decided that Curry
needs its own customized search engine. %
We think, this service can improve the work with Curry and forms a
good addition to the existing documentation. %
Furthermore, we are very curious to use Haskell in connection with a
complex project that exceeds our previous use-cases and to learn more
about functional programming techniques in general %

\begin{figure}[h]
\begin{center}
\includegraphics[width=12.5cm]{bilder/hackage.png}
\end{center}
\caption{The increasing data stock of hackageDB over the past five years}
\end{figure}

% currently just documentations accessible online

% \section{Motivation}

% doing a more complex project written in Hakell
% Hoogle, Hayoo as reference for Haskell functions
% Curr(y)gle > HayooI


% \section{Structure}

The further chapters of this theses are organized as follows. %
At first, \hyperref[preliminaries]{Chapter \ref*{preliminaries}} gives
the preliminiaries to understand this thesis. %
This includes basic information about the programming language Curry,
the representation of CurryDoc -- a tool to generate documentation of
a given Curry program,
and we introduce the Holumbus framework that is used to build the
search engine in connection with this thesis. %
In \hyperref[analysis]{Chapter \ref*{analysis}}, we analyze the
requirements to create an API search engine for Curry. %
It outlines the first ideas for the following implementation which is
given in \hyperref[implementation]{Chapter \ref*{implementation}}. %
Thereby, we focus on the most important implementation ideas and
decisions. %
At the very end, we discuss the results of this development. %
Also, we give a short outlook on features and ideas to expand the
given result. %
