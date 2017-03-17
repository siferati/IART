/**
* This file implements all of Board predicates
*/

% empty_board(-Board)
empty_board([
  [emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell],
  [emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell],
  [emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell],
  [emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell],
  [emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell],
  [emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell],
  [emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell],
  [emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell],
  [emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell]
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
*
* @param -List List of row ids
*/
getRowIds([' 1 ', ' 2 ', ' 3 ', ' 4 ', ' 5 ', ' 6 ', ' 7 ', ' 8 ', ' 9 ']).


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
*/

% stop condition
printBoard([], []).

% main
printBoard([H | T], [RH | RT]):-
  printSeparator(top), nl,
  write(RH), printSeparator(linestart), printLine(H), nl,
  printSeparator(bottom), nl,
  printBoard(T, RT).


/**
* Interface for printBoard/2
*
* @param +Board List of lists to print
*/
printBoard(Board):-
  printColIds, nl,
  getRowIds(RowIds),
  printSeparator(topmost), nl,
  printBoard(Board, RowIds).
