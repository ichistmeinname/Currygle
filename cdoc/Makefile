# Makefile for generating CurryDoc tool

# Required:
# - installed curry2prolog system

PAKCS=../../../../../../Downloads/pakcs
LIB=${PAKCS}/lib
META=${LIB}/meta
ANADIR=${PAKCS}/tools/analysis

# generate saved state for currydoc program:
CurryDoc.state: CurryDoc.curry CurryDocRead.curry CurryDocHtml.curry \
	        CurryDocTeX.curry CurryDocParams.curry CurryDocCDoc.curry \
	        ${LIB}/Markdown.curry \
	        ${META}/FlexRigid.curry \
	        ${LIB}/CategorizedHtmlList.curry \
	        ${LIB}/Distribution.curry ${LIB}/PropertyFile.curry \
	        ${META}/FlatCurry.curry ${LIB}/HTML.curry \
	        ${ANADIR}/AnaOverlapping.curry ${ANADIR}/AnaIndeterminism.curry \
	        ${ANADIR}/AnaCompleteness.curry ${ANADIR}/AnaOpComplete.curry \
	        ${ANADIR}/AnaDependency.curry 
	(echo ":set path ${ANADIR}" ; echo ":l CurryDoc" ; echo ":set path" ; \
	 echo ":save main" ) | ~/Downloads/pakcs/bin/pakcs
	(cd ~/Downloads/pakcs/bin ; rm -f currydoc ; \
	                ln -s ~/Downloads/pakcs/tools/cdoc/CurryDoc.state currydoc)


.PHONY: clean
clean: 
	~/Downloads/pakcs/bin/cleancurry
	rm -f ~/Downloads/pakcs/bin/currydoc
