\chapter{Analysis}\label{analysis}
This chaper looks into the requirements to build and run an API search
engine for Curry. The first section deals with the creation of an
index, whereas the second section addresses the process of searching
for a query. In this context, we take a closer look of the Holumbus
framework and its features concering search and evaluate the criteria
to accomplish a user-friendy search mechanism. Above all we need a web
application to handle user queries, which is analyzed in the last
section.

% Give a short summary about the following sections.\\

% What do we need to create this search engine? 
% The possibility to generate and update the index.\\
% The possibility to search for these informations in the index. At
% best: user-friendly \\

% What do we want above all?
% A web application for queries.

% \section{Currydoc}
\section{Creating the Index}
At first, we have to decide what kind of data we want to store in
the index. Secondly the data structure of the index and documents
provided by the Holumbus framework, that hold
these information, is discussed.\\

The data we want to extract from a given Curry module are at least the
defined functions and data structures as well as general information
about the module like the name and author. Since Curry is currently
organized by the module documentation generated by CurryDoc, we
already have a mechanism to gain these information about a Curry
module. In fact, we even have function related information like a
non-deterministic definition or a flexible evaluation as we
stated in the \hyperref[preliminaries:curryInfo]{previous chapter}.

Currently CurryDoc processes Curry modules and generates documentation
as HTML or \LaTeX~ output. For our index, we are not interested in any
document markup language, but the pure information about the Curry
module. This leads to the idea of generating a new readable data
structure as an extension to CurryDoc. We discuss the actual
implementation in \hyperref[implementation:currydoc]{Chapter
  \ref{implementation} Section \ref{implementation:currydoc}}.\\

% CurryDoc uses the meta-programming language FlatCurry to gain an
% intermediate data structure. We can use this data structure for our
% purposes. But at first, we have to discuss the which information to
% provide in our data structures. We already introduced \emph{CurryInfo}
% as structure for a Curry module in the
% \hyperref[preliminaries:currydoc:curryInfo]{previous chapter}.
% As next step we want to describe the information this data
% structure holds. The information about a module covers its name,
% author and description.

% Thanks to FlatCurry, we get quite more details about functions and
% types. Besides basic information like the name, description and the
% corresponding module, we have access to function characteristics like
% its flexible or rigid status and non- or deterministic property. In addition
% FlatCurry provides a data structure |TypeExpr| to describe type
% signatures (see \hyperref[fig:typeExpr]{Figure \ref{fig:typeExpr}}),
% which we use in both data structures. All described data structures to
% provide a representation for a Curry module can be seen in
% \hyperref[fig:curryInfo]{Figure \ref{fig:curryInfo}}.

% \begin{figure}[h]
% \begin{code}
% -- || The CurryInfo data holds information about the module, and
% --  corresponding  functions, data and type declaration of a given 
% --  Curry module.
% data CurryInfo = 
%   CurryInfo ModuleInfo [FunctionInfo] [TypeInfo]


% -- || ModuleInfo holds information about the name, author,
% --    and the description of a given module.
% data ModuleInfo = 
%   ModuleInfo String String String

% -- || FunctionInfo holds information about the name, signature, corresponding 
% -- module, description and flexible/rigid status of a function and 
% -- its non-/deterministic behaviour.
% data FunctionInfo = 
%   FunctionInfo String (QName, TypeExpr) String String Bool
%   FlexRigidResult

% -- || TypeInfo holds information about the name, signature, type variables,
% -- corresponding module, and description of a given type.
% data TypeInfo = 
%   TypeInfo String [(QName, [TypeExpr])] [Int] String String
% \end{code}
% \caption{The data structures for a Curry module}
% \label{fig:curryInfo}
% \end{figure}


% What do we need to create an index that can be used for the Curry
% search engine?\\

% First: Curry specific information (CurryDoc)\\

% First start with the idea of the extension: instead of generating a
% document markup language, generate a readable data structure.\\

% Introduce the TypeExpr data structure that is part of the FlatCurry
% feature.\\

\begin{figure}
\begin{code}
data TypeExpr =
     TVar TVarIndex                 -- type variable
   | FuncType TypeExpr TypeExpr     -- function type t1->t2
   | TCons QName [TypeExpr]         -- type constructor application
                                    -- TCons (module, name) typeargs

type QName = (String, String)     -- (module, type or constructor  name)
type TVarIndex = Int
\end{code}
\caption{FlatCurry data structure for type signatures}
\label{fig:typeExpr}
\end{figure}
% Second: a data structure to hold the information (index and document
% structure from the Holumbus framework)\\

% \begin{itemize}
% \item HolDocuments - Stores the documents that correspond to the
%   index. A mapping is provided.
% \item HolIndex - Data structure to store the information, that is
%   traversed in the search process.
% \item HolumbusState a - the combination of index and document,
%   polymorph by the data the HolDocuments holds.
% \end{itemize}

\section{Searching}
How do we search for the information in the index?

Holumbus provides search mechanism with a special syntax.
\begin{code}
data Query = 
             Word String |
             Phrase String |
             CaseWord String |
             CasePhrase String |
             FuzzyWord String |
             Specifier [Context] Query |
             Negation Query |
             BinQuery BinOp Query Query
data BinOp = And | Or | But
\end{code}

And this data structure can be processed by processQuery (Holumbus.Query.Processor).\\

Holumbus also provides a data structure that is returned after a query

\begin{code}
data Result a  = Result        
                { docHits  :: (DocHits a)
                , wordHits :: WordHits
                } deriving (Eq, Show)

data DocInfo a = DocInfo 
                { document :: (Document a)
                , docScore :: Score
                } deriving (Eq, Show)

data WordInfo  = WordInfo 
                { terms     :: Terms
                , wordScore :: Score                
                } deriving (Eq, Show)

type DocHits a       = DocIdMap (DocInfo a, DocContextHits)

type DocContextHits  = Map Context DocWordHits

type DocWordHits     = Map Word Positions

type WordHits        = Map Word (WordInfo, WordContextHits)

type WordContextHits = Map Context WordDocHits

type WordDocHits     = Occurrences

type Score           = Float
type Terms           = [String]
\end{code}

But first the user input has to be parsed into the query structure to
start the processing.

\subsection{Parsing User-Queries}
Which criteria do we want to search for? Modules, functions, types,
signatures, and  det./non-det., flexible/rigid functions. \\

First describe the idea, that the use of a specific language increases
the usability. But it also restricts the user in her usage of the
search engine, if this language gets more complex. So this results in
a compromise between a simply to use language and a language that can
be parsed.  Show the example of searching IO, where the restriction to
modules minimizes the result.\\

% After that list all language components that describe the restriction
% to contexts (module, function, type, signature and  non-/deterministic,
% flexible, rigid functions.\\

Set the focus on signatures. Because Hayoo! does not find signatures with
redundant parentheses, Curr(y)gle supports parenthesized signatures
and parenthesized query parts in general. \\

In addition: binary operations/conjunctions. \\
Explain that in most cases, a combination of more search words is
desirable, because first popular search engines like Google\texttrademark~use this
feature so it's common knowledge (the user expects this features) and
second it's easier to search for more search words, if the desired
result is still vague.

The parser becomes a complex, but very important matter.