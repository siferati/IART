/**
* This file implements everything related to the bots
*/

:- use_module(library(random)).
:- use_module(library(sets)).
:- ensure_loaded('board.pl').

/**
* Predicts best move for current player
*
* @param +Board: Current board
* @param +Player: Current player
* @param +Difficulty: 0 -> random move, 1+ -> depth of minimax algorithm
* @param -Move: Best move determined by thinkMove
*/
thinkMove(Board, Player, 0, Move):-
	getAllValidMoves(Board, Player, Plays),
	length(Plays, NPlays),
	random(0, NPlays, ChosenPlay),
	find(ChosenPlay, Plays, Move).
thinkMove(Board, Player, Difficulty, Move) :-
	minimax(Board, min, Player, 0, Difficulty, Move, _).

/**
* Gets list of valid moves
*
* @param +Board: Current board
* @param +Player: Current player
* @param -Plays: List of valid moves
*/

getAllValidMoves(Board, Player, Plays) :-
	findall(
		OOCol-OORow-NNCol-NNRow,
		validatePlay(Board, Player, OOCol, OORow, NNCol, NNRow),
		Plays
	).

/**
* Gets score of player on current game state
*
* @param +Board: Current game state
* @param +Player: Current player
* @param -Score: Player score
*/

getScore(Board, Score) :- 
	numberOfEscapes(Board,Escapes),
	%write('Escapes: '),write(Escapes),nl,
	%Atackers = 0,
	%Defenders = 0,
	%NCoveredLines = 0,
	%NCoveredDiag = 0,
	%getCaptured(Board,Captured),
	%countRemovedPieces(Captured,Atackers,Defenders),
	%countCoveredLines(Board,NCoveredLines),
	%countCoveredDiag(Board,NCoveredDiag),
	Score is Escapes*10 %+ ( Atackers - Defenders ) + ( NCoveredLines * 2 ) + NCoveredDiag
	%write('Score: '), write(Score), nl
.

%getScore(_, atkplayer, Score) :- Score = 1.
%getScore(_, defplayer, Score) :- Score = -1.

/**
* Gets number of possible king escapes
*
* @param +Board: Current board
* @param -Escapes: Number of escapes
*/
numberOfEscapes(Board,Escapes):-
  	findPos(Col, Row, Board, king),
	( % right
	  clearPath(Board, Col, Row, 8, Row),
	  Right = 1
	|
	  Right = 0
	),
	( % left
	  clearPath(Board, Col, Row, 0, Row),
	  Left = 1
	|
	  Left = 0
	),
	( % top
	  clearPath(Board, Col, Row, Col, 0),
	  Top = 1
	|
	  Top = 0
	),
	( % down
	  clearPath(Board, Col, Row, Col, 8),
	  Down = 1
	|
	  Down = 0
	),
	Escapes is (Right + Left + Top + Down) 
.

/**
* Gets each side ingame count of removed pieces
*
* @param +[Piece | Rest]: List of Captured pieces 
* @param -Atackers: Number of Atackers pieces
* @param -Defenders: Number of Defenders pieces
*/
countRemovedPieces([],_,_).
countRemovedPieces([Piece|Rest],Atackers,Defenders):-
	(
		ownPiece(atkplayer,Piece),
		NAtackers is Atackers + 1
	|
		ownPiece(defplayer,Piece),
		NDefenders is Defenders + 1
	),
	countPieces(Rest,NAtackers,NDefenders)
.
/**
* Gets number of lines that the king is protected
*
* @param +Board: Current board
* @param -NCoveredLines: Number of lines protected
*/
countCoveredLines(Board,NCoveredLines).
/**
* Gets number of adjacent diagonals that king is "protected" (offers a better likely hood to protect the king)
*
* @param +Board: Current board
* @param -NCoveredDiag: Number of diagonals protected
*/
countCoveredDiag(Board,NCoveredDiag):-
	findPos(Col, Row, Board, king),
	(%down&right
		NCol is Col+1,
		NRow is Row+1,
		findPos(NCol,NRow,Board,Piece),
		ownPiece(defplayer,Piece),
		DownRight = 1
	|
		DownRight = 0
	),
	(%top&right
		NCol is Col+1,
		NRow is Row-1,
		findPos(NCol,NRow,Board,Piece),
		ownPiece(defplayer,Piece),
		TopRight = 1
	|
		TopRight = 0
	),
	(%down&left
		NCol is Col-1,
		NRow is Row+1,
		findPos(NCol,NRow,Board,Piece),
		ownPiece(defplayer,Piece),
		DownLeft = 1
	|
		DownLeft = 0
	),
	(%top&left
		NCol is Col-1,
		NRow is Row-1,
		findPos(NCol,NRow,Board,Piece),
		ownPiece(defplayer,Piece),
		TopLeft = 1
	|
		TopLeft = 0
	),
	NCoveredDiag is (DownRight + DownLeft + TopRight + TopLeft)
.



/**
* Maximizing level of minimax algorithm
*
* @param +Board: Current game state
* @param +ListMoves: List of valid moves
* @param +Depth: Current depth
* @param +Difficulty: Depth of minimax algorithm
* @param +Player: Current player
* @param +Eant: Previous best game state
* @param +Alpha: Alpha acts like max in MiniMax
* @param -Eres: Maximizing move
* @param -Vres: Score of maximizing move
* @param +Beta: Beta cut-off
*/

maxValue(_, [], _, _, _, E, V, E, V, _).
maxValue(Board, [OCol-ORow-NCol-NRow|OEs], Depth, Difficulty, Player, Eant, Alpha, Eres, Vres, Beta):-
	Depth1 is Depth + 1,
	move(Board, OCol, ORow, NCol, NRow, B1),
	switchPlayer(Player, NP),
	minimax(B1, min, NP, Depth1, Difficulty, _, V1),
	((V1 > Alpha, AlphaAux = V1, Eaux = OCol-ORow-NCol-NRow)
	; (AlphaAux = Alpha, Eaux = Eant)),
	((V1 >= Beta, Eres = OCol-ORow-NCol-NRow, Vres = Beta)
	; maxValue(Board, OEs, Depth, Difficulty, Player, Eaux, AlphaAux, Eres, Vres, Beta)).

/**
* Minimizing level of minimax algorithm
*
* @param +Board: Current game state
* @param +ListMoves: List of valid moves
* @param +Depth: Current depth
* @param +Difficulty: Depth of minimax algorithm
* @param +NP: Next player
* @param +Eant: Previous best game state
* @param +Beta: Beta acts like min in MiniMax
* @param -Eres: Minimizing move
* @param -Vres: Score of minimizing move
* @param +Alpha: Alpha cut-off
*/	

minValue(_, [], _, _, _, E, V, E, V, _).
minValue(Board, [OCol-ORow-NCol-NRow|OEs], Depth, Difficulty, NP, Eant, Beta, Eres, Vres, Alpha):-
	Depth1 is Depth + 1,
	move(Board, OCol, ORow, NCol, NRow, B1),
	switchPlayer(NP, Player),
	minimax(B1, max, Player, Depth1, Difficulty, _, V1),
	((V1 < Beta, BetaAux = V1, Eaux = OCol-ORow-NCol-NRow)
	; (BetaAux = Beta, Eaux = Eant)),
	((V1 =< Alpha, Eres = OCol-ORow-NCol-NRow, Vres = Alpha)
	; minValue(Board, OEs, Depth, Difficulty, NP, Eaux, BetaAux, Eres, Vres, Alpha)).

/**
* Minimax algorithm
*
* @param +Board: Current game state
* @param +Level: max or min
* @param +Player: Current player
* @param +Depth: Current depth
* @param +Difficulty: Depth of minimax algorithm
* @param -Move: Best move determined by minimax algorithm
* @param -Value: Score of best move
*/
	
minimax(Board, max, Player, Depth, Difficulty, Move, Value):-
	Depth \= Difficulty,
	getAllValidMoves(Board, Player, ListMoves),
	maxValue(Board, ListMoves, Depth, Difficulty, Player, _, -9999, Move, Value, 9999).
	
minimax(Board, min, NextPlayer, Depth, Difficulty, Move, Value):-
	Depth \= Difficulty,
	getAllValidMoves(Board, NextPlayer, ListMoves),
	minValue(Board, ListMoves, Depth, Difficulty, NextPlayer, _, 9999, Move, Value, -9999).
	
minimax(Board, _, Player, _, _, OCol-ORow-NCol-NRow, Value) :-
	move(Board, OCol, ORow, NCol, NRow, NewBoard),
	getScore(NewBoard, Value).
	%write('value: '),write(Value),nl.