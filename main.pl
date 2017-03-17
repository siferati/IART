/**
* Main entry for the program.
*/

:- ensure_loaded('board.pl').

main:-
  initial_board(Board),
  printBoard(Board).






test:-
  initial_board(Board),
  move(Board, 4, 4, 6, 6, Board1),
  move(Board1, 5, 4, 5, 8, Board2),
  move(Board2, 1, 4, 0, 0, Board3),
  printBoard(Board3).
