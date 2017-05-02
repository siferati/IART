/**
* This file implements the core functions of the program, such as the game loop
*/

:- ensure_loaded('board.pl').
:- ensure_loaded('interface.pl').


/**
* Decides how to act based on the exit status of askPos/5
*
* @param +Status Exit status of askPos/5
*/

% everything good, continue
askPosHandler(good).

% return to startmenu
askPosHandler(exit):- fail. % gameloop fails > main fails > enters 2nd clause for main



/**
* Game Loop - Where the magic happens
*
* @param +Board Current state of the game board
*/

% main
gameloop(Board):-

  % process input
  askPos(Col, Row, NewCol, NewRow, Status),
  askPosHandler(Status),

  % update
  move(Board, Col, Row, NewCol, NewRow, NewBoard),

  % render
  printBoard(NewBoard),

  % repeat
  gameloop(NewBoard).

% when gameloop fails (ie user pressed exit button)
gameloop(_).


/**
* Decides how to act based on the exit status of startmenu/1
*
* @param +Status Exit status of startmenu/1
*/

% start the game
startmenuHandler(good).

% exit the program
startmenuHandler(exit):- fail. % main fails > enters 2nd clause for main


/**
* Main entry for the program
*/

% main
main:-
  startmenu(Status),
  startmenuHandler(Status),
  initial_board(Board),
  printBoard(Board),
  gameloop(Board),
  main.

% when main fails (ie user pressed exit button)
main.
