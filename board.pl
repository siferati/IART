/**
* This file implements all of Board predicates
*/

:- ensure_loaded('utils.pl').


% empty_board(-Board)
empty_board([
  [emptyCell, emptyCell, emptyCell, atkarea, atkarea, atkarea, emptyCell, emptyCell, emptyCell],
  [emptyCell, emptyCell, emptyCell, emptyCell, atkarea, emptyCell, emptyCell, emptyCell, emptyCell],
  [emptyCell, emptyCell, emptyCell, emptyCell, defarea, emptyCell, emptyCell, emptyCell, emptyCell],
  [atkarea, emptyCell, emptyCell, emptyCell, defarea, emptyCell, emptyCell, emptyCell, atkarea],
  [atkarea, atkarea, defarea, defarea, castle, defarea, defarea, atkarea, atkarea],
  [atkarea, emptyCell, emptyCell, emptyCell, defarea, emptyCell, emptyCell, emptyCell, atkarea],
  [emptyCell, emptyCell, emptyCell, emptyCell, defarea, emptyCell, emptyCell, emptyCell, emptyCell],
  [emptyCell, emptyCell, emptyCell, emptyCell, atkarea, emptyCell, emptyCell, emptyCell, emptyCell],
  [emptyCell, emptyCell, emptyCell, atkarea, atkarea, atkarea, emptyCell, emptyCell, emptyCell]
]).

% initial_board(-Board)
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


% toChar(+Value, -Char)
toChar(emptyCell, ' ').
toChar(king, 'K').
toChar(defender, 'D').
toChar(attacker, 'A').
toChar(castle, 'C').
toChar(defarea, 'O').
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
getInfo(['A - Attacker', 'D - Defender', 'K - King', 'O - Defenders Area', 'X - Attackers Area', 'C - Castle', '', '', '']).


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
  printColIds, nl,
  getRowIds(RowIds),
  getInfo(Info),
  printSeparator(topmost), nl,
  printBoard(Board, RowIds, Info).


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
