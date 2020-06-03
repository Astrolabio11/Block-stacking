stack := {N,U,A,L}
table := {S,I,R,V,E}
correctword := {U,N,I,V,E,R,S,A,L}

(* SENSORS *)
cs := If[MatchQ[stack,{}],
	False,
	stack[[-1]]
	]

tb := Module[{counter=0},
	If[stack==={},
		False,
		For[i=0,i<Length[stack],i++,
			If[stack[[counter+1]]===correctword[[counter+1]],counter+=1,Break[] ]
			];
		If[counter===0,False,stack[[counter]] ]
		]
	]

nn := Which[tb===correctword[[-1]],
	False,
	tb===False,
	correctword[[1]],
	True,
	pos = Position[correctword,tb];
	correctword[[pos[[1,1]]+1]]
	]

(* FUNCTIONS *)
ms[x_] := If[MemberQ[table,x],
	stack = Append[stack,x];
	table = DeleteCases[table,x];
	True,
	False
	]

mt[x_] := If[MemberQ[stack,x],
	table = Append[table,stack[[-1]] ];
	stack = Delete[stack,-1];
	True,
	False
	]

SetAttributes[du, HoldAll]
du[x_,y_] := Module[{iterations=0,a,b},
	While[iterations<20 && y===False,
		iterations+=1;
		Evaluate[x]
		]
	]

not[x_] := If[x===False, True, False]

eq[x_,y_] := If[x===y, True]

(* LISTS OF COMMANDS *)
commands := {xcs, xtb, xnn, xms[arg], xmt[arg], xdu[arg,arg], xnot[arg], xeq[arg,arg]}
crossoverCommands := {xcs, xtb, xnn, xms, xmt, xdu, xnot, xeq}

(* RANDOM GENERATORS *)
rand18 := Random[Integer,{1,8}];
rand13 := Random[Integer,{1,3}];
randlist[list_] := Random[Integer,{1,Length[list]}];

(* GENERATING RANDOM POPULATION *)
randomPrograms := Module[{counter=1,prog=commands[[8]]},
	While[FreeQ[prog,arg]===False,
		If[counter<5,
			prog = prog /. xms[arg] :> xms[commands[[rand13 ]] ];
			prog = prog /. xmt[arg] :> xmt[commands[[rand13 ]] ];
			prog = prog /. arg :> commands[[rand18 ]],
			prog = prog /. arg :> commands[[rand13 ]]
			];
		counter+=1
		];
	prog
	]

population := Table[randomPrograms,{10}];

(* EVALUATION OF PROGRAMS AND FITNESS *)
fitness[prog_] := Module[{evaluate=prog},
	evaluate = evaluate /. {xcs :> cs, xtb :> tb, xnn :> nn, xms :> ms, xmt :> mt, xdu :> du,
	xnot :> not, xeq :> eq};
	evaluate
	]

(* EVOLVING POPULATION *)
crossover[parents_] := Module[{poscomm1,poscomm2,prev1,prev2},
	While[
		(* Selecting branch from parent 1 *)
		While[poscomm1 = rand18;
			pos1 = Position[parents[[1]],crossoverCommands[[poscomm1]] ];
			pos1=={}
			];
		pos1 = pos1[[randlist[pos1] ]];

		If[pos1[[-1]]==0,
			pos1 = Drop[pos1,-1],
			prev1 = Head[parents[[1,Sequence @@ Drop[pos1,-1] ]] ]
			];

		(* Selecting branch from parent 2 *)
		While[poscomm2 = rand18;
			pos2 = Position[parents[[2]],crossoverCommands[[poscomm2]] ];
			pos2=={}
			];
		pos2 = pos2[[randlist[pos2] ]];

		If[pos2[[-1]]==0,
			pos2 = Drop[pos2,-1],
			prev2 = Head[parents[[2,Sequence @@ Drop[pos2,-1] ]] ]
			];

		(* Condition of compatibility *)
		(poscomm1<4 && MatchQ[prev1,xms | xmt] && poscomm2>3) || (poscomm2<4 && MatchQ[prev2,xms | xmt] && poscomm1>3)
		];

	(* Crossover *)
	If[pos1=={},
		prog1 = parents[[2]][[Sequence @@ pos2]],
		prog1 = ReplacePart[parents[[1]],parents[[2]][[Sequence @@ pos2]],pos1]
		];
	If[pos2=={},
		prog2 = parents[[1]][[Sequence @@ pos1]],
		prog2 = ReplacePart[parents[[2]],parents[[1]][[Sequence @@ pos1]],pos2]
		];

	newprogs = {prog1, prog2};
	newprogs
	]

evolution[selectedparents_] := Module[{pairs=Partition[selectedparents,2]},
	offspring = Map[crossover,pairs];
	offspring = Flatten[offspring];
	offspring
	]

sol := xeq[ xdu[xmt[xcs], xnot[xcs]], xdu[xms[xnn], xnot[xnn]] ];
Print[stack]
