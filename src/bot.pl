/**
* This file implements everything related to the bots
*/

:- use_module(library(random)).
:- use_module(library(sets)).
:- ensure_loaded('board.pl').

thinkMove(Board, Player, 0, Move):-
	getAllValidMoves(Board, Player, Plays),
	length(Plays, NPlays),
	random(0, NPlays, ChosenPlay),
	find(ChosenPlay, Plays, Move).
thinkMove(Board, Player, Difficulty, Move) :-
	minimax(Board, max, Player, 0, Difficulty, Move, _).

getAllValidMoves(Board, Player, Plays) :-
	findall(
		OOCol-OORow-NNCol-NNRow,
		validatePlay(Board, Player, OOCol, OORow, NNCol, NNRow),
		Plays
	).
	
getScore(_, atkplayer, Score) :- Score = 1.
getScore(_, defplayer, Score) :- Score = -1.

maxValue(_, [], _, _, _, E, V, E, V).

maxValue(Board, [OCol-ORow-NCol-NRow|OEs], Depth, Difficulty, Player, Eant, Vant, Eres, Vres):-
	Depth1 is Depth + 1,
	move(Board, OCol, ORow, NCol, NRow, B1),
	switchPlayer(Player, NP),
	minimax(B1, min, NP, Depth1, Difficulty, _, V1),
	((V1 > Vant, Vaux = V1, Eaux = OCol-ORow-NCol-NRow)
	; (Vaux = Vant, Eaux = Eant)),
	maxValue(Board, OEs, Depth, Difficulty, Player, Eaux, Vaux, Eres, Vres).

minValue(_, [], _, _, _, E, V, E, V).

minValue(Board, [OCol-ORow-NCol-NRow|OEs], Depth, Difficulty, NP, Eant, Vant, Eres, Vres):-
	Depth1 is Depth + 1,
	move(Board, OCol, ORow, NCol, NRow, B1),
	switchPlayer(NP, Player),
	minimax(B1, max, Player, Depth1, Difficulty, _, V1),
	((V1 < Vant, Vaux = V1, Eaux = OCol-ORow-NCol-NRow)
	; (Vaux = Vant, Eaux = Eant)),
	minValue(B1, OEs, Depth, Difficulty, NP, Eaux, Vaux, Eres, Vres).	

minimax(Board, max, Player, Depth, Difficulty, Move, Value):-
	Depth \= Difficulty,
	getAllValidMoves(Board, Player, ListMoves),
	maxValue(Board, ListMoves, Depth, Difficulty, Player, _, -9999, Move, Value).
	
minimax(Board, min, NextPlayer, Depth, Difficulty, Move, Value):-
	Depth \= Difficulty,
	getAllValidMoves(Board, NextPlayer, ListMoves),
	minValue(Board, ListMoves, Depth, Difficulty, NextPlayer, _, 9999, Move, Value).
	
minimax(Board, _, Player, _, _, OCol-ORow-NCol-NRow, Value) :-
	move(Board, OCol, ORow, NCol, NRow, NewBoard),
	getScore(NewBoard, Player, Value).