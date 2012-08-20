# Makefile for generating CurryDoc tool

# Required:
# - installed curry2prolog system

# the following paths must link to the directory of your Curry implementation.
# the library directories META, LIB and the analysis tool
CURRY=/Users/ichistmeinname/Downloads/pakcs
LIB=${CURRY}/lib
META=${LIB}/meta
ANADIR=/Users/ichistmeinname/Downloads/pakcs/tools/analysis

# generate saved state for currydoc program:
CurryDoc.state: CurryDoc.curry CurryDocRead.curry CurryDocHtml.curry \
	        CurryDocTeX.curry CurryDocParams.curry CurryDocCDoc.curry\
	        ${LIB}/Markdown.curry \
	        ${META}/FlexRigid.curry \
	        ${LIB}/CategorizedHtmlList.curry \
	        ${LIB}/Distribution.curry ${LIB}/PropertyFile.curry \
	        ${META}/FlatCurry.curry ${LIB}/HTML.curry \
	        ${ANADIR}/AnaOverlapping.curry ${ANADIR}/AnaIndeterminism.curry \
	        ${ANADIR}/AnaCompleteness.curry ${ANADIR}/AnaOpComplete.curry \
	        ${ANADIR}/AnaDependency.curry
	(echo ":set path ${ANADIR}" ; echo ":l CurryDoc" ; echo ":set path" ; \
	 echo ":save main" ) | /Users/ichistmeinname/Downloads/pakcs/bin/pakcs
# the path above must link to your curry implementation 
	(cd /Users/ichistmeinname/Dropbox/Uni/6/Bachelorarbeit/2012-sad-ba/cdoc ; \
# the path above must link to the current director 
	 rm -f currydoc ; \
		ln -s ./CurryDoc.state currydoc)

.PHONY: clean
clean:
	rm -f ./currydoc ; rm -f ./CurryDoc.state ; rm -R ./.curry
