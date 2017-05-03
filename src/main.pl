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

  % repeat until user inputs a valid play
  repeat,

    % process input
    askPos(Col, Row, NewCol, NewRow, Status),

    % if user made a play
    (Status \= exit
      ->  (
            % if play is valid
            (validatePlay(Board, Col, Row, NewCol, NewRow)
              ->  (
                    % update
                    move(Board, Col, Row, NewCol, NewRow, NewBoard),

                    % render
                    printBoard(NewBoard),

                    % repeat
                    gameloop(NewBoard)
                  )
              ;   (
                    write('\nPlay is invalid, please make a valid play...\n\n'),
                    fail % go back to repeat
                  )
            )
          )
      ; true % finish gameloop
    ),
  !.


/**
* Main entry for the program
*/

% main
main:-
  startmenu(Status),
  % if user started the game
  (Status \= exit
    ->  (
          initial_board(Board),
          printBoard(Board),
          gameloop(Board),
          main
        )
    ; true % close program
  ).
