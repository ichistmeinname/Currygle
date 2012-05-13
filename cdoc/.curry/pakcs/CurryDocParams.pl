%PAKCS1.10 sicstus4 VARIABLESHARING

:-noSingletonWarnings.
:-noRedefineWarnings.
:-noDiscontiguousWarnings.

:-importModule('Prelude').
:-importModule('System').


:-curryModule('CurryDocParams').

%%%%%%%%%%%% function types %%%%%%%%%%%%%%%%%%%
:-multifile functiontype/6.
:-dynamic functiontype/6.
functiontype('CurryDocParams.docType',docType,1,'CurryDocParams.docType',nofix,'FuncType'('TCons'('CurryDocParams.DocParams',[]),'TCons'('CurryDocParams.DocType',[]))).
functiontype('CurryDocParams.setDocType',setDocType,2,'CurryDocParams.setDocType',nofix,'FuncType'('TCons'('CurryDocParams.DocType',[]),'FuncType'('TCons'('CurryDocParams.DocParams',[]),'TCons'('CurryDocParams.DocParams',[])))).
functiontype('CurryDocParams.withIndex',withIndex,1,'CurryDocParams.withIndex',nofix,'FuncType'('TCons'('CurryDocParams.DocParams',[]),'TCons'('Prelude.Bool',[]))).
functiontype('CurryDocParams.setIndex',setIndex,2,'CurryDocParams.setIndex',nofix,'FuncType'('TCons'('Prelude.Bool',[]),'FuncType'('TCons'('CurryDocParams.DocParams',[]),'TCons'('CurryDocParams.DocParams',[])))).
functiontype('CurryDocParams.withMarkdown',withMarkdown,1,'CurryDocParams.withMarkdown',nofix,'FuncType'('TCons'('CurryDocParams.DocParams',[]),'TCons'('Prelude.Bool',[]))).
functiontype('CurryDocParams.setMarkDown',setMarkDown,2,'CurryDocParams.setMarkDown',nofix,'FuncType'('TCons'('Prelude.Bool',[]),'FuncType'('TCons'('CurryDocParams.DocParams',[]),'TCons'('CurryDocParams.DocParams',[])))).
functiontype('CurryDocParams.defaultCurryDocParams',defaultCurryDocParams,0,'CurryDocParams.defaultCurryDocParams',nofix,'TCons'('CurryDocParams.DocParams',[])).

%%%%%%%%%%%% constructor types %%%%%%%%%%%%%%%%%%%
:-multifile constructortype/6.
:-dynamic constructortype/6.
constructortype('CurryDocParams.HtmlDoc','HtmlDoc',0,'HtmlDoc',0,'TCons'('CurryDocParams.DocType',[])).
constructortype('CurryDocParams.TexDoc','TexDoc',0,'TexDoc',1,'TCons'('CurryDocParams.DocType',[])).
constructortype('CurryDocParams.DocParams','DocParams',3,'DocParams',0,'FuncType'('TCons'('CurryDocParams.DocType',[]),'FuncType'('TCons'('Prelude.Bool',[]),'FuncType'('TCons'('Prelude.Bool',[]),'TCons'('CurryDocParams.DocParams',[]))))).

%%%%%%%%%%%% function definitions %%%%%%%%%%%%%%%%%%%
:-block 'CurryDocParams.docType'(?,?,-,?).
'CurryDocParams.docType'(A,B,C,D):-hnf(A,E,C,F),'CurryDocParams.docType_1'(E,B,F,D).

:-block 'CurryDocParams.docType_1'(?,?,-,?).
'CurryDocParams.docType_1'('CurryDocParams.DocParams'(A,B,C),D,E,F):-!,hnf(A,D,E,F).
'CurryDocParams.docType_1'('FAIL'(A),'FAIL'(A),B,B):-nonvar(A).

:-block 'CurryDocParams.setDocType'(?,?,?,-,?).
'CurryDocParams.setDocType'(A,B,C,D,E):-hnf(B,F,D,G),'CurryDocParams.setDocType_2'(F,A,C,G,E).

:-block 'CurryDocParams.setDocType_2'(?,?,?,-,?).
'CurryDocParams.setDocType_2'('CurryDocParams.DocParams'(A,B,C),D,'CurryDocParams.DocParams'(E,'Prelude.=='(E,'CurryDocParams.HtmlDoc'),C),F,G):-!,makeShare(D,E),F=G.
'CurryDocParams.setDocType_2'('FAIL'(A),B,'FAIL'(A),C,C):-nonvar(A).

:-block 'CurryDocParams.withIndex'(?,?,-,?).
'CurryDocParams.withIndex'(A,B,C,D):-hnf(A,E,C,F),'CurryDocParams.withIndex_1'(E,B,F,D).

:-block 'CurryDocParams.withIndex_1'(?,?,-,?).
'CurryDocParams.withIndex_1'('CurryDocParams.DocParams'(A,B,C),D,E,F):-!,hnf(B,D,E,F).
'CurryDocParams.withIndex_1'('FAIL'(A),'FAIL'(A),B,B):-nonvar(A).

:-block 'CurryDocParams.setIndex'(?,?,?,-,?).
'CurryDocParams.setIndex'(A,B,C,D,E):-hnf(B,F,D,G),'CurryDocParams.setIndex_2'(F,A,C,G,E).

:-block 'CurryDocParams.setIndex_2'(?,?,?,-,?).
'CurryDocParams.setIndex_2'('CurryDocParams.DocParams'(A,B,C),D,'CurryDocParams.DocParams'(A,D,C),E,E):-!.
'CurryDocParams.setIndex_2'('FAIL'(A),B,'FAIL'(A),C,C):-nonvar(A).

:-block 'CurryDocParams.withMarkdown'(?,?,-,?).
'CurryDocParams.withMarkdown'(A,B,C,D):-hnf(A,E,C,F),'CurryDocParams.withMarkdown_1'(E,B,F,D).

:-block 'CurryDocParams.withMarkdown_1'(?,?,-,?).
'CurryDocParams.withMarkdown_1'('CurryDocParams.DocParams'(A,B,C),D,E,F):-!,hnf(C,D,E,F).
'CurryDocParams.withMarkdown_1'('FAIL'(A),'FAIL'(A),B,B):-nonvar(A).

:-block 'CurryDocParams.setMarkDown'(?,?,?,-,?).
'CurryDocParams.setMarkDown'(A,B,C,D,E):-hnf(B,F,D,G),'CurryDocParams.setMarkDown_2'(F,A,C,G,E).

:-block 'CurryDocParams.setMarkDown_2'(?,?,?,-,?).
'CurryDocParams.setMarkDown_2'('CurryDocParams.DocParams'(A,B,C),D,'CurryDocParams.DocParams'(A,B,D),E,E):-!.
'CurryDocParams.setMarkDown_2'('FAIL'(A),B,'FAIL'(A),C,C):-nonvar(A).

:-block 'CurryDocParams.defaultCurryDocParams'(?,-,?).
'CurryDocParams.defaultCurryDocParams'('CurryDocParams.DocParams'('CurryDocParams.HtmlDoc','Prelude.True','Prelude.True'),A,A).

:-costCenters(['']).




%%%%% Number of shared variables: 1
