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

% toChar(+value, -char)
toChar(emptyCell, ' ').
toChar(king, 'K').
toChar(defender, 'D').
toChar(attacker, 'A').

/**
* Prints horizontal separators of board
*
* @param +Type Separator to print
*/
printSeparator(top):-
  write(' _____________________________________________________ ').

printSeparator(bottom):-
  write('|_____|_____|_____|_____|_____|_____|_____|_____|_____|').

printSeparator(space):-
  write('|     |     |     |     |     |     |     |     |     |').

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
  write('  |'),
  printLine(T).


/**
* Prints board to the terminal
*
* @param +Board List of lists to print
*/

% interface
printBoard(Board):-
  printSeparator(top), nl,
  printBoardAux(Board).

% stop condition
printBoardAux([]).

% main
printBoardAux([H | T]):-
  printSeparator(space), nl,
  write('|'), printLine(H), nl,
  printSeparator(bottom), nl,
  printBoardAux(T).
