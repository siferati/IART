:- ensure_loaded('board.pl').

main:-
  initial_board(Board),
  printBoard(Board).
