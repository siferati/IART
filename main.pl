/**
* Main entry for the program.
*/

:- ensure_loaded('board.pl').
:- ensure_loaded('utils.pl').

main:-
  initial_board(Board),
  printBoard(Board).
