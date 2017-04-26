/**
* This file implements the core functions of the program, such as the game loop
*/

:- ensure_loaded('board.pl').
:- ensure_loaded('interface.pl').


/**
* Main entry for the program
*/
main:-
  startmenu,
  initial_board(Board),
  printBoard(Board).




/**
* TESTING
*/

test_move:-
  initial_board(Board),
  move(Board, 4, 4, 6, 6, Board1),
  move(Board1, 5, 4, 5, 8, Board2),
  move(Board2, 1, 4, 0, 0, Board3),
  printBoard(Board3).
