%PAKCS1.10 sicstus4 VARIABLESHARING

:-noSingletonWarnings.
:-noRedefineWarnings.
:-noDiscontiguousWarnings.

:-importModule('CurryDocRead').
:-importModule('FlatCurry').
:-importModule('Prelude').
:-importModule('ReadShowTerm').
:-importModule('CurryDocParams').


:-curryModule('CurryDocCDoc').

%%%%%%%%%%%% function types %%%%%%%%%%%%%%%%%%%
:-multifile functiontype/6.
:-dynamic functiontype/6.
functiontype('CurryDocCDoc.generateCDoc',generateCDoc,4,'CurryDocCDoc.generateCDoc',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'([],['TCons'('Prelude.(,)',['TCons'('CurryDocRead.SourceLine',[]),'TCons'([],['TCons'('Prelude.Char',[])])])]),'FuncType'('TCons'('CurryDocRead.AnaInfo',[]),'TCons'('Prelude.IO',['TCons'([],['TCons'('Prelude.Char',[])])])))))).
functiontype('CurryDocCDoc.generateCDoc\'2E_\'23lambda1','CurryDocCDoc.generateCDoc._#lambda1',2,'CurryDocCDoc.generateCDoc\'2E_\'23lambda1',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'('FlatCurry.Prog',[]),'TCons'('Prelude.IO',['TCons'([],['TCons'('Prelude.Char',[])])])))).
functiontype('CurryDocCDoc.generateCDoc\'2E_\'23lambda1\'2E_\'23lambda2','CurryDocCDoc.generateCDoc._#lambda1._#lambda2',3,'CurryDocCDoc.generateCDoc\'2E_\'23lambda1\'2E_\'23lambda2',nofix,'FuncType'('TCons'([],['TCons'([],['TCons'('Prelude.Char',[])])]),'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])])])]),'TCons'('Prelude.(,,,,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'([],['TCons'('Prelude.Char',[])])]),'TCons'([],['TCons'('Prelude.Char',[])])]))))).
functiontype('CurryDocCDoc.versionOrAuthor',versionOrAuthor,2,'CurryDocCDoc.versionOrAuthor',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'([],['TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])])]),'TCons'([],['TCons'('Prelude.Char',[])])))).

%%%%%%%%%%%% constructor types %%%%%%%%%%%%%%%%%%%
:-multifile constructortype/6.
:-dynamic constructortype/6.

%%%%%%%%%%%% function definitions %%%%%%%%%%%%%%%%%%%
:-block 'CurryDocCDoc.generateCDoc'(?,?,?,?,?,-,?).
'CurryDocCDoc.generateCDoc'(A,B,C,D,E,F,G):-makeShare(H,I),hnf('Prelude.cond'('Prelude.letrec'(I,'FlatCurry.flatCurryFileName'(A)),'Prelude.>>'('Prelude.$'(partcall(1,'Prelude.putStrLn',[]),'Prelude.++'(['^R','^e','^a','^d','^i','^n','^g','^ ','^F','^l','^a','^t','^C','^u','^r','^r','^y','^ ','^p','^r','^o','^g','^r','^a','^m','^ ','^"'],'Prelude.++'(I,['^"',^.,^.,^.]))),'Prelude.>>='('FlatCurry.readFlatCurryFile'(I),partcall(1,'CurryDocCDoc.generateCDoc\'2E_\'23lambda1',[B])))),E,F,G).

:-block 'CurryDocCDoc.generateCDoc\'2E_\'23lambda1'(?,?,?,-,?).
'CurryDocCDoc.generateCDoc\'2E_\'23lambda1'(A,B,C,D,E):-hnf(B,F,D,G),'CurryDocCDoc.generateCDoc\'2E_\'23lambda1_2'(F,A,C,G,E).

:-block 'CurryDocCDoc.generateCDoc\'2E_\'23lambda1_2'(?,?,?,-,?).
'CurryDocCDoc.generateCDoc\'2E_\'23lambda1_2'('FlatCurry.Prog'(A,B,C,D,E),F,G,H,I):-!,hnf('Prelude.$'(partcall(1,'Prelude.return',[]),'ReadShowTerm.showTerm'('Prelude.$'(partcall(1,'CurryDocCDoc.generateCDoc\'2E_\'23lambda1\'2E_\'23lambda2',[A,B]),'CurryDocRead.splitComment'(F)))),G,H,I).
'CurryDocCDoc.generateCDoc\'2E_\'23lambda1_2'('FAIL'(A),B,'FAIL'(A),C,C):-nonvar(A).

:-block 'CurryDocCDoc.generateCDoc\'2E_\'23lambda1\'2E_\'23lambda2'(?,?,?,?,-,?).
'CurryDocCDoc.generateCDoc\'2E_\'23lambda1\'2E_\'23lambda2'(A,B,C,D,E,F):-hnf(C,G,E,H),'CurryDocCDoc.generateCDoc\'2E_\'23lambda1\'2E_\'23lambda2_3'(G,A,B,D,H,F).

:-block 'CurryDocCDoc.generateCDoc\'2E_\'23lambda1\'2E_\'23lambda2_3'(?,?,?,?,-,?).
'CurryDocCDoc.generateCDoc\'2E_\'23lambda1\'2E_\'23lambda2_3'('Prelude.(,)'(A,B),C,D,'Prelude.(,,,,)'(D,'CurryDocCDoc.versionOrAuthor'(['^v','^e','^r','^s','^i','^o','^n'],E),'CurryDocCDoc.versionOrAuthor'(['^a','^u','^t','^h','^o','^r'],E),C,A),F,G):-!,makeShare(B,E),F=G.
'CurryDocCDoc.generateCDoc\'2E_\'23lambda1\'2E_\'23lambda2_3'('FAIL'(A),B,C,'FAIL'(A),D,D):-nonvar(A).

:-block 'CurryDocCDoc.versionOrAuthor'(?,?,?,-,?).
'CurryDocCDoc.versionOrAuthor'(A,B,C,D,E):-hnf('Prelude.$'(partcall(1,'Prelude.concat',[]),'CurryDocRead.getCommentType'(A,B)),C,D,E).

:-costCenters(['']).




%%%%% Number of shared variables: 2
