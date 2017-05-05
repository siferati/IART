/**
* This file implements all of Board predicates
* such as printBoard/1 or validatePlay/5
*/

:- ensure_loaded('utils.pl').


/**
* Returns the board state corresponding to the rule name
*
* @param -Board Game board (list of lists)
*/

empty_board([
  [emptyCell, emptyCell, emptyCell, atkarea, atkarea, atkarea, emptyCell, emptyCell, emptyCell],
  [emptyCell, emptyCell, emptyCell, emptyCell, atkarea, emptyCell, emptyCell, emptyCell, emptyCell],
  [emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell],
  [atkarea, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, atkarea],
  [atkarea, atkarea, emptyCell, emptyCell, castle, emptyCell, emptyCell, atkarea, atkarea],
  [atkarea, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, atkarea],
  [emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell],
  [emptyCell, emptyCell, emptyCell, emptyCell, atkarea, emptyCell, emptyCell, emptyCell, emptyCell],
  [emptyCell, emptyCell, emptyCell, atkarea, atkarea, atkarea, emptyCell, emptyCell, emptyCell]
]).

initial_board([
  [emptyCell, emptyCell, emptyCell, attacker, attacker, attacker, emptyCell, emptyCell, emptyCell],
  [emptyCell, emptyCell, emptyCell, emptyCell, attacker, emptyCell, emptyCell, emptyCell, emptyCell],
  [emptyCell, emptyCell, emptyCell, emptyCell, defender, emptyCell, emptyCell, emptyCell, emptyCell],
  [attacker, emptyCell, emptyCell, emptyCell, defender, emptyCell, emptyCell, emptyCell, attacker],
  [attacker, attacker, defender, defender, king, defender, defender, attacker, attacker],
  [attacker, emptyCell, emptyCell, emptyCell, defender, emptyCell, emptyCell, emptyCell, attacker],
  [emptyCell, emptyCell, emptyCell, emptyCell, defender, emptyCell, emptyCell, emptyCell, emptyCell],
  [emptyCell, emptyCell, emptyCell, emptyCell, attacker, emptyCell, emptyCell, emptyCell, emptyCell],
  [emptyCell, emptyCell, emptyCell, attacker, attacker, attacker, emptyCell, emptyCell, emptyCell]
]).


/**
* Translates board symbols into chars, for easier printing
*
* @param +Value Symbol to translate
* @param -Char Symbol's corresponding char
*/
toChar(emptyCell, ' ').
toChar(king, 'K').
toChar(defender, 'D').
toChar(attacker, 'A').
toChar(castle, 'C').
toChar(atkarea, 'X').


/**
* Prints horizontal board separators
*
* @param +Type Separator to print
*/
printSeparator(topmost):-
  write('    _____________________________________________________ ').

printSeparator(bottom):-
  write('   |_____|_____|_____|_____|_____|_____|_____|_____|_____|').

printSeparator(top):-
  write('   |     |     |     |     |     |     |     |     |     |').

printSeparator(inline):-
  write('  |').

printSeparator(linestart):-
  write('|').


/**
* Print column identifiers
*/
printColIds:-
  write('      A     B     C     D     E     F     G     H     I   ').


/**
* Get list of row identifiers to print
* (length MUST be the same as board rows!)
*
* @param -List List of row ids
*/
getRowIds([' 1 ', ' 2 ', ' 3 ', ' 4 ', ' 5 ', ' 6 ', ' 7 ', ' 8 ', ' 9 ']).


/**
* Get list containing information to display to the user
* (length MUST be the same as board rows!)
*/
getInfo(['A - Attacker', 'C - Castle', 'D - Defender', 'K - King', 'X - Attackers\' Area', '', '', '', '']).


/*
* Prints a line of the board
*
* @param +Line List to print
*/

% stop condition
printLine([]).

% main
printLine([H | T]):-
  write('  '),
  toChar(H, C),
  write(C),
  printSeparator(inline),
  printLine(T).


/**
* Prints board to the terminal
*
* @param +Board List of lists to print
* @param +RowIds List of row identifiers to print
* @param +Info List containing information to display to the user
*/

% stop condition
printBoard([], [], []).

% main
printBoard([H | T], [RH | RT], [IH | IT]):-
  printSeparator(top), nl,
  write(RH), printSeparator(linestart), printLine(H),
  write('     '), write(IH), nl,
  printSeparator(bottom), nl,
  printBoard(T, RT, IT).


/**
* Interface for printBoard/3
*
* @param +Board List of lists to print
*/
printBoard(Board):-
  nl,
  printColIds, nl,
  getRowIds(RowIds),
  getInfo(Info),
  printSeparator(topmost), nl,
  printBoard(Board, RowIds, Info),
  nl.


/**
* Removes a piece from the board
* And replace it with corresponding area
*
* @param +Board Board
* @param +Col Column of piece to remove
* @param +Row Row of piece to remove
* @param -NBoard New board after removal
*/
remove(Board, Col, Row, NBoard):-
  empty_board(EBoard),
  find(Col, Row, EBoard, Piece),
  replace(Col, Row, Board, Piece, NBoard).


/**
* Moves a board piece
*
* @param +Board Board
* @param +OCol Original (current) column of piece to move
* @param +ORow Original (current) row of piece to move
* @param +NCol New column for selected piece
* @param +NRow New row for selected piece
* @param -NBoard New board after movement
*/
move(Board, OCol, ORow, NCol, NRow, NBoard):-
  find(OCol, ORow, Board, Piece),
  replace(NCol, NRow, Board, Piece, AuxBoard),
  remove(AuxBoard, OCol, ORow, NBoard).


/**
* Auxiliary rule to clearPath/5
* Checks in what direction the movement is,
* and calculates the iteration step accordingly
*
* @param +ICol Current column being iterated
* @param +IRow Current row being iterated
* @param +FCol Final column
* @param +FRow Final row
* @param -NextCol Next column to iterate
* @param -NextRow Next row to iterate
*/

% left > right
clearPathIterationStep(ICol, FRow, FCol, FRow, NextCol, FRow):-
  ICol < FCol,
  NextCol is ICol + 1.

% right > left
clearPathIterationStep(ICol, FRow, FCol, FRow, NextCol, FRow):-
  ICol > FCol,
  NextCol is ICol - 1.

% top > bottom
clearPathIterationStep(FCol, IRow, FCol, FRow, FCol, NextRow):-
  IRow < FRow,
  NextRow is IRow + 1.

% bottom > top
clearPathIterationStep(FCol, IRow, FCol, FRow, FCol, NextRow):-
  IRow > FRow,
  NextRow is IRow - 1.


/**
* Checks if there's nothing blocking the path
* between two board positions.
*
* Requires clearPathIterationStep/6 to be called beforehand,
* in order to skip the first iteration (the original position)
*
* @param +Board Board
* @param +ICol Current column being iterated
* @param +IRow Current row being iterated
* @param +FCol Final column
* @param +FRow Final row
*/

% stop condition
clearPathIteration(_, FCol, FRow, FCol, FRow):- !.

% main
clearPathIteration(Board, ICol, IRow, FCol, FRow):-
  find(ICol, IRow, Board, IPiece),
  IPiece \= attacker,
  IPiece \= defender,
  IPiece \= king,
  clearPathIterationStep(ICol, IRow, FCol, FRow, NextCol, NextRow),
  clearPathIteration(Board, NextCol, NextRow, FCol, FRow),
  !.


/**
* Interface for clearPathIteration/5, since it requires
* clearPathIterationStep/6 to be called beforehand
*
* @param +Board Board
* @param +OCol Current column being iterated
* @param +ORow Current row being iterated
* @param +NCol Final column
* @param +NRow Final row
*/
clearPath(Board, OCol, ORow, NCol, NRow):-
  clearPathIterationStep(OCol, ORow, NCol, NRow, ICol, IRow),
  clearPathIteration(Board, ICol, IRow, NCol, NRow).


/**
* Each game rule is listed as a single prolog rule
*
* This implementation is negation based (Status = no),
* meaning it checks if a play breaks a rule,
* not if it follows it. Example:
*
* 0. Can only move attackers, defenders or the king
* 1. Status = yes ← Piece = (attackers OR defenders OR king)
* 2. Status = NOT(yes) ← Piece = NOT(attackers OR defenders OR king)
* 3. Status = no ← Piece = (NOT(attackers) AND NOT(defenders) AND NOT(king))
*
* @param +Board Board
* @param +OCol Original (current) column of piece to move
* @param +ORow Original (current) row of piece to move
* @param +NCol New column for selected piece
* @param +NRow New row for selected piece
* @param +Piece Piece to move
* @param +NPiece New destination of piece
* @param -Status Exit status. yes - play follows rule; no - play breaks rule
*/

% can only move attackers, defenders or the king TODO split this in two, one for each player
gamerule(_, _, _, _, _, Piece, _, no):-
  Piece \= attacker,
  Piece \= defender,
  Piece \= king.

% a piece can only be placed in an empty cell or atk area
gamerule(_, _, _, _, _, _, NPiece, no):-
  NPiece \= emptyCell,
  NPiece \= atkarea.

% a piece can only move in a straight line
gamerule(_, OCol, ORow, NCol, NRow, _, _, no):-
  OCol \= NCol,
  ORow \= NRow.

% a piece can't pass through other pieces
gamerule(Board, OCol, ORow, NCol, NRow, _, _, no):-
  \+clearPath(Board, OCol, ORow, NCol, NRow).


/**
* Checks if a play is valid
*
* This doesn't actually check if a play follows every rule,
* it checks if it breaks any
*
* @param +Board Board
* @param +OCol Original (current) column of piece to move
* @param +ORow Original (current) row of piece to move
* @param +NCol New column for selected piece
* @param +NRow New row for selected piece
*/
validatePlay(Board, OCol, ORow, NCol, NRow):-
  find(OCol, ORow, Board, Piece),
  find(NCol, NRow, Board, NPiece),
  findall(Status, gamerule(Board, OCol, ORow, NCol, NRow, Piece, NPiece, Status), List),
  \+member(no, List).
