/**
* This file implements the core functions of the program, such as the game loop
*/

:- ensure_loaded('board.pl').
:- ensure_loaded('interface.pl').


/**
* Game Loop - Where the magic happens
*
* @param +Board Current state of the game board
*/

% main
gameloop(Board):-

  % process input
  askPos(Col, Row, NewCol, NewRow),

  % update
  move(Board, Col, Row, NewCol, NewRow, NewBoard),

  % render
  printBoard(NewBoard),

  % repeat
  gameloop(NewBoard).

% when gameloop fails (ie user pressed exit button)
gameloop(_).


/**
* Main entry for the program
*/

% main
main:-
  startmenu,
  initial_board(Board),
  printBoard(Board),
  gameloop(Board),
  main.

% when main fails (ie user pressed exit button)
main.
