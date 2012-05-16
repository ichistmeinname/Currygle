%PAKCS1.10 sicstus4 VARIABLESHARING

:-noSingletonWarnings.
:-noRedefineWarnings.
:-noDiscontiguousWarnings.

:-importModule('CurryDocRead').
:-importModule('FlatCurry').
:-importModule('FlexRigid').
:-importModule('Prelude').
:-importModule('ReadShowTerm').
:-importModule('CurryDocParams').
:-importModule('List').


:-curryModule('CurryDocCDoc').

%%%%%%%%%%%% function types %%%%%%%%%%%%%%%%%%%
:-multifile functiontype/6.
:-dynamic functiontype/6.
functiontype('CurryDocCDoc.generateCDoc',generateCDoc,4,'CurryDocCDoc.generateCDoc',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'([],['TCons'('Prelude.(,)',['TCons'('CurryDocRead.SourceLine',[]),'TCons'([],['TCons'('Prelude.Char',[])])])]),'FuncType'('TCons'('CurryDocRead.AnaInfo',[]),'TCons'('Prelude.IO',['TCons'([],['TCons'('Prelude.Char',[])])])))))).
functiontype('CurryDocCDoc.generateCDoc\'2E_\'23lambda1','CurryDocCDoc.generateCDoc._#lambda1',4,'CurryDocCDoc.generateCDoc\'2E_\'23lambda1',nofix,'FuncType'('TCons'('CurryDocRead.AnaInfo',[]),'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'([],['TCons'('Prelude.(,)',['TCons'('CurryDocRead.SourceLine',[]),'TCons'([],['TCons'('Prelude.Char',[])])])]),'FuncType'('TCons'('FlatCurry.Prog',[]),'TCons'('Prelude.IO',['TCons'([],['TCons'('Prelude.Char',[])])])))))).
functiontype('CurryDocCDoc.generateCDoc\'2E_\'23lambda1\'2E_\'23lambda2','CurryDocCDoc.generateCDoc._#lambda1._#lambda2',3,'CurryDocCDoc.generateCDoc\'2E_\'23lambda1\'2E_\'23lambda2',nofix,'FuncType'('TCons'([],['TCons'([],['TCons'('Prelude.Char',[])])]),'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])])])]),'TCons'('Prelude.(,,,,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'([],['TCons'('Prelude.Char',[])])]),'TCons'([],['TCons'('Prelude.Char',[])])]))))).
functiontype('CurryDocCDoc.versionOrAuthor',versionOrAuthor,2,'CurryDocCDoc.versionOrAuthor',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'([],['TCons'('Prelude.(,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])])])]),'TCons'([],['TCons'('Prelude.Char',[])])))).
functiontype('CurryDocCDoc.funcInfo',funcInfo,3,'CurryDocCDoc.funcInfo',nofix,'FuncType'('TCons'('CurryDocRead.AnaInfo',[]),'FuncType'('TCons'([],['TCons'('Prelude.(,)',['TCons'('CurryDocRead.SourceLine',[]),'TCons'([],['TCons'('Prelude.Char',[])])])]),'FuncType'('TCons'('FlatCurry.FuncDecl',[]),'TCons'('Prelude.(,,,,,)',['TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])]),'TCons'([],['TCons'('Prelude.Char',[])]),'TCons'('Prelude.Bool',[]),'TCons'('FlexRigid.FlexRigidResult',[])]))))).
functiontype('CurryDocCDoc.splitTypeExpr',splitTypeExpr,1,'CurryDocCDoc.splitTypeExpr',nofix,'FuncType'('TCons'('FlatCurry.TypeExpr',[]),'TCons'([],['TCons'('Prelude.Char',[])]))).
functiontype('CurryDocCDoc.unwrap',unwrap,2,'CurryDocCDoc.unwrap',nofix,'FuncType'('TCons'([],['TCons'('Prelude.Char',[])]),'FuncType'('TCons'([],['TCons'('FlatCurry.TypeExpr',[])]),'TCons'([],['TCons'('Prelude.Char',[])])))).
functiontype('CurryDocCDoc.unwrap\'2E_\'23caseor0','CurryDocCDoc.unwrap._#caseor0',2,'CurryDocCDoc.unwrap\'2E_\'23caseor0',nofix,'FuncType'('TCons'('Prelude.Bool',[]),'FuncType'('TCons'([],['TCons'('FlatCurry.TypeExpr',[])]),'TCons'([],['TCons'('Prelude.Char',[])])))).
functiontype('CurryDocCDoc.unwrap\'2E_\'23caseor0\'2E_\'23caseor0','CurryDocCDoc.unwrap._#caseor0._#caseor0',3,'CurryDocCDoc.unwrap\'2E_\'23caseor0\'2E_\'23caseor0',nofix,'FuncType'('TCons'('Prelude.Bool',[]),'FuncType'(A,'FuncType'(A,A)))).

%%%%%%%%%%%% constructor types %%%%%%%%%%%%%%%%%%%
:-multifile constructortype/6.
:-dynamic constructortype/6.

%%%%%%%%%%%% function definitions %%%%%%%%%%%%%%%%%%%
:-block 'CurryDocCDoc.generateCDoc'(?,?,?,?,?,-,?).
'CurryDocCDoc.generateCDoc'(A,B,C,D,E,F,G):-makeShare(H,I),hnf('Prelude.cond'('Prelude.letrec'(I,'FlatCurry.flatCurryFileName'(A)),'Prelude.>>'('Prelude.$'(partcall(1,'Prelude.putStrLn',[]),'Prelude.++'(['^R','^e','^a','^d','^i','^n','^g','^ ','^F','^l','^a','^t','^C','^u','^r','^r','^y','^ ','^p','^r','^o','^g','^r','^a','^m','^ ','^"'],'Prelude.++'(I,['^"',^.,^.,^.]))),'Prelude.>>='('FlatCurry.readFlatCurryFile'(I),partcall(1,'CurryDocCDoc.generateCDoc\'2E_\'23lambda1',[C,B,D])))),E,F,G).

:-block 'CurryDocCDoc.generateCDoc\'2E_\'23lambda1'(?,?,?,?,?,-,?).
'CurryDocCDoc.generateCDoc\'2E_\'23lambda1'(A,B,C,D,E,F,G):-hnf(D,H,F,I),'CurryDocCDoc.generateCDoc\'2E_\'23lambda1_4'(H,A,B,C,E,I,G).

:-block 'CurryDocCDoc.generateCDoc\'2E_\'23lambda1_4'(?,?,?,?,?,-,?).
'CurryDocCDoc.generateCDoc\'2E_\'23lambda1_4'('FlatCurry.Prog'(A,B,C,D,E),F,G,H,I,J,K):-!,hnf('Prelude.$'(partcall(1,'Prelude.return',[]),'ReadShowTerm.showTerm'('Prelude.(,)'('Prelude.$'(partcall(1,'CurryDocCDoc.generateCDoc\'2E_\'23lambda1\'2E_\'23lambda2',[A,B]),'CurryDocRead.splitComment'(G)),'Prelude.map'(partcall(1,'CurryDocCDoc.funcInfo',[H,F]),D)))),I,J,K).
'CurryDocCDoc.generateCDoc\'2E_\'23lambda1_4'('FAIL'(A),B,C,D,'FAIL'(A),E,E):-nonvar(A).

:-block 'CurryDocCDoc.generateCDoc\'2E_\'23lambda1\'2E_\'23lambda2'(?,?,?,?,-,?).
'CurryDocCDoc.generateCDoc\'2E_\'23lambda1\'2E_\'23lambda2'(A,B,C,D,E,F):-hnf(C,G,E,H),'CurryDocCDoc.generateCDoc\'2E_\'23lambda1\'2E_\'23lambda2_3'(G,A,B,D,H,F).

:-block 'CurryDocCDoc.generateCDoc\'2E_\'23lambda1\'2E_\'23lambda2_3'(?,?,?,?,-,?).
'CurryDocCDoc.generateCDoc\'2E_\'23lambda1\'2E_\'23lambda2_3'('Prelude.(,)'(A,B),C,D,'Prelude.(,,,,)'(D,'CurryDocCDoc.versionOrAuthor'(['^v','^e','^r','^s','^i','^o','^n'],E),'CurryDocCDoc.versionOrAuthor'(['^a','^u','^t','^h','^o','^r'],E),C,A),F,G):-!,makeShare(B,E),F=G.
'CurryDocCDoc.generateCDoc\'2E_\'23lambda1\'2E_\'23lambda2_3'('FAIL'(A),B,C,'FAIL'(A),D,D):-nonvar(A).

:-block 'CurryDocCDoc.versionOrAuthor'(?,?,?,-,?).
'CurryDocCDoc.versionOrAuthor'(A,B,C,D,E):-hnf('Prelude.$'(partcall(1,'Prelude.concat',[]),'CurryDocRead.getCommentType'(A,B)),C,D,E).

:-block 'CurryDocCDoc.funcInfo'(?,?,?,?,-,?).
'CurryDocCDoc.funcInfo'(A,B,C,D,E,F):-hnf(C,G,E,H),'CurryDocCDoc.funcInfo_3'(G,A,B,D,H,F).

:-block 'CurryDocCDoc.funcInfo_3'(?,?,?,?,-,?).
'CurryDocCDoc.funcInfo_3'('FlatCurry.Func'(A,B,C,D,E),F,G,H,I,J):-!,hnf(A,K,I,L),'CurryDocCDoc.funcInfo_3_FlatCurry.Func_1'(K,B,C,D,E,F,G,H,L,J).

:-block 'CurryDocCDoc.funcInfo_3_FlatCurry.Func_1'(?,?,?,?,?,?,?,?,-,?).
'CurryDocCDoc.funcInfo_3_FlatCurry.Func_1'('Prelude.(,)'(A,B),C,D,E,F,G,H,I,J,K):-!,hnf(F,L,J,M),'CurryDocCDoc.funcInfo_3_FlatCurry.Func_1_Prelude.(,)_6'(L,A,B,C,D,E,G,H,I,M,K).

:-block 'CurryDocCDoc.funcInfo_3_FlatCurry.Func_1_Prelude.(,)_6'(?,?,?,?,?,?,?,?,?,-,?).
'CurryDocCDoc.funcInfo_3_FlatCurry.Func_1_Prelude.(,)_6'('FlatCurry.Rule'(A,B),C,D,E,F,G,H,I,'Prelude.(,,,,,)'(J,'CurryDocCDoc.splitTypeExpr'(G),K,'CurryDocRead.getFuncComment'(J,I),'Prelude.apply'('CurryDocRead.getOverlappingInfo'(H),'Prelude.(,)'(K,J)),'FlexRigid.getFlexRigid'(B)),L,M):-!,makeShare(D,J),makeShare(C,K),L=M.
'CurryDocCDoc.funcInfo_3_FlatCurry.Func_1_Prelude.(,)_6'('FlatCurry.External'(A),B,C,D,E,F,G,H,I,J,K):-!,hnf('Prelude.failure'('CurryDocCDoc.funcInfo',['FlatCurry.External'(A)]),I,J,K).
'CurryDocCDoc.funcInfo_3_FlatCurry.Func_1_Prelude.(,)_6'('FAIL'(A),B,C,D,E,F,G,H,'FAIL'(A),I,I).
'CurryDocCDoc.funcInfo_3_FlatCurry.Func_1'('FAIL'(A),B,C,D,E,F,G,'FAIL'(A),H,H):-nonvar(A).
'CurryDocCDoc.funcInfo_3'('FAIL'(A),B,C,'FAIL'(A),D,D):-nonvar(A).

:-block 'CurryDocCDoc.splitTypeExpr'(?,?,-,?).
'CurryDocCDoc.splitTypeExpr'(A,B,C,D):-hnf(A,E,C,F),'CurryDocCDoc.splitTypeExpr_1'(E,B,F,D).

:-block 'CurryDocCDoc.splitTypeExpr_1'(?,?,-,?).
'CurryDocCDoc.splitTypeExpr_1'('FlatCurry.TCons'(A,B),C,D,E):-hnf(A,F,D,G),'CurryDocCDoc.splitTypeExpr_1_FlatCurry.TCons_1'(F,B,C,G,E).

:-block 'CurryDocCDoc.splitTypeExpr_1_FlatCurry.TCons_1'(?,?,?,-,?).
'CurryDocCDoc.splitTypeExpr_1_FlatCurry.TCons_1'('Prelude.(,)'(A,B),C,D,E,F):-!,hnf('CurryDocCDoc.unwrap'(B,C),D,E,F).
'CurryDocCDoc.splitTypeExpr_1_FlatCurry.TCons_1'('FAIL'(A),B,'FAIL'(A),C,C):-nonvar(A).
'CurryDocCDoc.splitTypeExpr_1'('FlatCurry.FuncType'(A,B),C,D,E):-!,hnf(A,F,D,G),'CurryDocCDoc.splitTypeExpr_1_FlatCurry.FuncType_1'(F,B,C,G,E).

:-block 'CurryDocCDoc.splitTypeExpr_1_FlatCurry.FuncType_1'(?,?,?,-,?).
'CurryDocCDoc.splitTypeExpr_1_FlatCurry.FuncType_1'('FlatCurry.TCons'(A,B),C,D,E,F):-!,hnf(A,G,E,H),'CurryDocCDoc.splitTypeExpr_1_FlatCurry.FuncType_1_FlatCurry.TCons_1'(G,B,C,D,H,F).

:-block 'CurryDocCDoc.splitTypeExpr_1_FlatCurry.FuncType_1_FlatCurry.TCons_1'(?,?,?,?,-,?).
'CurryDocCDoc.splitTypeExpr_1_FlatCurry.FuncType_1_FlatCurry.TCons_1'('Prelude.(,)'(A,B),C,D,E,F,G):-!,hnf('Prelude.++'('CurryDocCDoc.unwrap'(B,C),'Prelude.++'([^-,^>],'CurryDocCDoc.splitTypeExpr'(D))),E,F,G).
'CurryDocCDoc.splitTypeExpr_1_FlatCurry.FuncType_1_FlatCurry.TCons_1'('FAIL'(A),B,C,'FAIL'(A),D,D):-nonvar(A).
'CurryDocCDoc.splitTypeExpr_1_FlatCurry.FuncType_1'('FlatCurry.TVar'(A),B,C,D,E):-!,hnf('Prelude.failure'('CurryDocCDoc.splitTypeExpr',['FlatCurry.TVar'(A)]),C,D,E).
'CurryDocCDoc.splitTypeExpr_1_FlatCurry.FuncType_1'('FlatCurry.FuncType'(A,B),C,D,E,F):-!,hnf('Prelude.failure'('CurryDocCDoc.splitTypeExpr',['FlatCurry.FuncType'(A,B)]),D,E,F).
'CurryDocCDoc.splitTypeExpr_1_FlatCurry.FuncType_1'('FAIL'(A),B,'FAIL'(A),C,C).
'CurryDocCDoc.splitTypeExpr_1'('FlatCurry.TVar'(A),B,C,D):-!,hnf('Prelude.failure'('CurryDocCDoc.splitTypeExpr',['FlatCurry.TVar'(A)]),B,C,D).
'CurryDocCDoc.splitTypeExpr_1'('FAIL'(A),'FAIL'(A),B,B).

:-block 'CurryDocCDoc.unwrap'(?,?,?,-,?).
'CurryDocCDoc.unwrap'(A,B,C,D,E):-makeShare(F,G),makeShare(A,H),makeShare(B,I),hnf('Prelude.cond'('Prelude.letrec'(G,'CurryDocCDoc.unwrap\'2E_\'23caseor0'('Prelude.=='(H,['^[','^]']),I)),'CurryDocCDoc.unwrap\'2E_\'23caseor0\'2E_\'23caseor0'('Prelude.null'(I),H,G)),C,D,E).

:-block 'CurryDocCDoc.unwrap\'2E_\'23caseor0'(?,?,?,-,?).
'CurryDocCDoc.unwrap\'2E_\'23caseor0'(A,B,C,D,E):-hnf(A,F,D,G),'CurryDocCDoc.unwrap\'2E_\'23caseor0_1'(F,B,C,G,E).

:-block 'CurryDocCDoc.unwrap\'2E_\'23caseor0_1'(-,?,?,?,?),'CurryDocCDoc.unwrap\'2E_\'23caseor0_1'(?,?,?,-,?).
'CurryDocCDoc.unwrap\'2E_\'23caseor0_1'('Prelude.True',A,B,C,D):-hnf('Prelude.++'(['^['],'Prelude.++'('Prelude.apply'('Prelude.concatMap'(partcall(1,'CurryDocCDoc.splitTypeExpr',[])),A),['^]'])),B,C,D).
'CurryDocCDoc.unwrap\'2E_\'23caseor0_1'('Prelude.False',A,B,C,D):-!,hnf('Prelude.++'(['^('],'Prelude.++'('Prelude.apply'('Prelude.concatMap'(partcall(1,'CurryDocCDoc.splitTypeExpr',[])),A),['^)'])),B,C,D).
'CurryDocCDoc.unwrap\'2E_\'23caseor0_1'('FAIL'(A),B,'FAIL'(A),C,C).

:-block 'CurryDocCDoc.unwrap\'2E_\'23caseor0\'2E_\'23caseor0'(?,?,?,?,-,?).
'CurryDocCDoc.unwrap\'2E_\'23caseor0\'2E_\'23caseor0'(A,B,C,D,E,F):-hnf(A,G,E,H),'CurryDocCDoc.unwrap\'2E_\'23caseor0\'2E_\'23caseor0_1'(G,B,C,D,H,F).

:-block 'CurryDocCDoc.unwrap\'2E_\'23caseor0\'2E_\'23caseor0_1'(-,?,?,?,?,?),'CurryDocCDoc.unwrap\'2E_\'23caseor0\'2E_\'23caseor0_1'(?,?,?,?,-,?).
'CurryDocCDoc.unwrap\'2E_\'23caseor0\'2E_\'23caseor0_1'('Prelude.True',A,B,C,D,E):-hnf(A,C,D,E).
'CurryDocCDoc.unwrap\'2E_\'23caseor0\'2E_\'23caseor0_1'('Prelude.False',A,B,C,D,E):-!,hnf(B,C,D,E).
'CurryDocCDoc.unwrap\'2E_\'23caseor0\'2E_\'23caseor0_1'('FAIL'(A),B,C,'FAIL'(A),D,D).

:-costCenters(['']).




%%%%% Number of shared variables: 7
