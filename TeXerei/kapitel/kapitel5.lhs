\chapter{Conclusion}\label{conclusion}

In the last chapter of this thesis, we complete this thesis with a
conclusion. %
At first, we review the progress of our implementation and tie in with
our introduction and the goals, requirements and ideas we had in the
beginning, respectively. %
Furthermore, we give an insight about lessons we learned and provide
an overview of ideas for future works. %

\section{Summary and Results}

The main goal was to build a search engine for the functional logic
programming language Curry in order to provide a simple way to look up
functions, modules and data types of Curry libraries, e.g.,
the libraries distributed with PAKCS. %
Therefore, we presented the CurryDoc tool to generate informative
documentations for a given Curry program. %
In \hyperref[implementation:currydoc]{chapter
  \ref{implementation:currydoc}} we demonstrated an extension of
CurryDoc in order to create a source of our information of Curry
libraries. %
This source produces a data structure that represents the given Curry
program that is eventually analysed in
\hyperref[implementation:index]{the following section} in order to
create an index for the search engine. %
The index consists of the data we want to search for, i.e., in our
case the API of the mentioned Curry libraries. %
The data we are indexing can be paired with a \textss{context} or,
more precisely, every information we are indexing is categorized
corresponding to its \textss{role} in the Curry program, e.g., a
function's name, the description of the module, a data type's
signature. %
These pairs of context and word allow us to improve our search engine
because we can restrict a search term to one of these contexts. %
This restrictions can minimize unwanted results and, therefore, make a
contribution to a user-friendly search engine. %
In order to provide the usage of such specifiers to restrict a search
term, we need a non-ambiguous language for the search queries. %
Therefore, we present a parser to analyze the search term in
\hyperref[implementation:parser]{the last section of chapter
  \ref{implementation}}. %
In the end, we have build a search engine to run as web
application. \\ %

In retrospective, we spent the most time studying the Holumbus
framework in detail and all the features it provides. %
After we consult the master's theses concerning the framework, the
main idea and general usage was clear pretty fast. %
Unfortunately, we discovered that these documentations of the
framework are quite out-of-date. %
Since the theses were written in 2008, the framework had many
improvements and new ideas were implemented. %
Thus, the fact, that it takes a lot of time to familiarise oneself
with an existing complex framework, was the first lesson that we
learned. %
However, after first approaches and a lot of time of progressive
implementation, we increased our knowledge of the structures that the
Holumbus framework provides. %
Eventually, we think that we interpenetrated how to use the framework
correctly. %
Furthermore, we entered a new field of analytics and semantics when we
studied the general idea of parser and eventually implemented our own
parser for the search queries. %
In this context, we used the Haskell library Parsec and had to
confront ourselves with a new complex library all over again. %
Fortunately, after a short time, we achieved good results and had
first successes with the implementation of our parser. %
In the end, it took several testing phases to pass all the
requirements and we often found improvements to simplify the
implementation but, all in all, we had very positive experiences with
Parsec and its functionality.\\ %

\todo[inline]{
  more?
}

\section{Outlook}

Last but not least, we want to discuss some ideas to extend this
project. %
One functionality we do not provide is an \emph{intelligent} way to
search for polymorphic types, like Hoogle, the search engine for
Haskell, does. %
For example, let's assume we search for a function that concatenates
two lists of integer. %
The best idea is to search for the signature of this function, i.e.,
|"[Int] -> [Int]"|. %
Unfortunately, we will not find any results, since there is no
function with this specific type signature. %
But it exist a function to concatenate two lists of any type, i.e.,
|[a] -> [a]|. %
Like we mentioned, Hoogle uses a mechanism to search for more general
types in order to find the convenient function in our example. %
In our opinion, there is one simple approach to gain this
functionality and another one that requires more resources. %
The first approach just replaces each appearance of a type with a
fixed type variable. %
For our example, we can transform |[Int] -> [Int]| to |[a] -> [a]| if
we look at the first type |Int| and replace this and all following
occurrences with the type variable |a|. %
Another idea is to unify each type signature, that we have in the
index, with the searched signature and also return the successful
unifications. %
In the development period of this search engine, we simply did not
find the time to concern ourselves with this topic. %
Albeit, we think such a feature would be an excellent addition of the
already existing functionality. \\ %

Furthermore, we think that the widely-used "did you mean?" suggestion
can make our search engine even more user-friendly. %
In case of misspellings, this feature helps to find words that are
lexicographical similar with the search term in order to proceed the
search. %
The actual implementation will not be too complicated, since the
Holumbus framework already provides a fuzzy search that can help to
find the best result for misspelled words. \\ %

At last, we want to mention, that the interaction between the CurryDoc
extension and the Haskell implementation could be more modular. %
We tried to accomodate the search engine with the existing CurryDoc
and its function of generating documentation. %
For example, we compose the URI to provide the reference between a
result entry and the documentation in our search engine
implementation. %
Another approach would be to provide these kind of information in the
CurryDoc extension because, then, we can automatically consider
adjustments in the documentation process, like changes in the URI
construction. %
Another example can be the contexts; maybe a more flexible adjustment
of the context names is desirable. % 
One method could be to use construct the |CurryInfo| data structures
with several tuples, where the first entry represents the context and
the second entry the value. %
For example, |ModuleInfo| could look like this:

\begin{code}
data ModuleInfo = 
  (String, String)  -- (context, name)
  (String, String)  -- (context, author)
  (String, String)  -- (context, description)
\end{code}

These are just some ideas we had in the end of our development. %
In the end, we had to differ between requirements and features and, of
course, we decided to focus on the requirements. %
We think, the features, that we just mentioned, are purely optional
and our search engine meets the requirements we made, provides a good
user-experience and is a good starting point for further
developments. %
During the implementation of the CurryDoc extension, we wanted to look
up a lot of functions and suffered from the lack of a
Hoogle-equivalent. %
We got really frustrated when browsing the online documentation and
reading each module in search of the convenient function. %
Hence, we hope to do some good with this search engine and help to
provide a more efficient work-environment for Curry. \\ %

\todo[inline]{something missing here?}

