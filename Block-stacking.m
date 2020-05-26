stack := {N,U,A,L}
table := {S,I,R,V,E}
correctword := {U,N,I,V,E,R,S,A,L}

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

commands := {xcs, xtb, xnn, xms[arg], xmt[arg], xdu[arg,arg], xnot[arg], xeq[arg,arg]}

randomprograms := Module[{counter=1,prog=commands[[8]]},
	While[FreeQ[prog,arg]===False,
		If[counter<5,
			prog = prog /. xms[arg] :> xms[commands[[Random[Integer,{1,3}] ]] ];
			prog = prog /. xmt[arg] :> xmt[commands[[Random[Integer,{1,3}] ]] ];
			prog = prog /. arg :> commands[[Random[Integer,{1,8}] ]],
			prog = prog /. arg :> commands[[Random[Integer,{1,3}] ]]
			];
		counter+=1
		];
	prog
	]

population := Table[randomprograms,{10}];

fitness[prog_] := Module[{evaluate=prog},
	For[i=Depth[evaluate]-1,i>0,i--,
		evaluate = Replace[evaluate,{xcs :> cs,xtb :> tb,xnn :> nn,xms[x_] :> ms[x],
		xmt[x_] :> mt[x],xdu[x_,y_] :> du[x,y],xnot[x_] :> not[x],xeq[x_,y_] :> eq[x,y]},i]
		];
	evaluate = ReplacePart[evaluate,eq,{{0}}];
	evaluate
	]

crossover[parents_] := Module[
	{pos1=Position[parents[[1]],commands[[Random[Integer,{4,8}]]][[0]] ],
	pos2=Position[parents[[2]],commands[[Random[Integer,{4,8}]]][[0]] ],
	seg1=parents[[1]],seg2=parents[[2]]},

	While[pos1 == {},
		pos1 = Position[prog1,commands[[Random[Integer,{4,8}]]][[0]] ]
		];
	pos1 = Drop[pos1[[Random[Integer,{1,Length[pos1]}] ]],-1];

	While[pos2 == {},
		pos2 = Position[prog2,commands[[Random[Integer,{4,8}]]][[0]] ]
		];
	pos2 = Drop[pos2[[Random[Integer,{1,Length[pos2]}] ]],-1];

	For[i=1,i<=Length[pos1],i++,seg1 = seg1[[pos1[[i]] ]] ];
	For[i=1,i<=Length[pos2],i++,seg2 = seg2[[pos2[[i]] ]] ];

	prog1 = ReplacePart[prog1,seg2,pos1];
	prog2 = ReplacePart[prog2,seg1,pos2];
	newprogs = {prog1, prog2}
	]

evolution[selectedparents_] := Module[{pairs=Partition[selectedparents,2]},
	offspring = Map[crossover,pairs];
	offspring = Flatten[offspring]
	]

sol = xeq[ xdu[xmt[xcs], xnot[xcs]], xdu[xms[xnn], xnot[xnn]] ];
Print[stack]