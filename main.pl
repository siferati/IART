/**
* Main entry for the program.
*/

:- ensure_loaded('board.pl').

main:-
  initial_board(Board),
  printBoard(Board).






test:-
  initial_board(Board),
  move(Board, 4, 4, 6, 6, NBoard),
  printBoard(NBoard).
