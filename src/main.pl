/** TODO remove magic numbers from all src files, such as board width and height
* This file implements the core functions of the program, such as the game loop
*/

:- ensure_loaded('board.pl').
:- ensure_loaded('interface.pl').
:- ensure_loaded('player.pl').


/**
* Game Loop - Where the magic happens
*
* @param +Board Current state of the game board
* @param +Player Current player's turn
*/
gameloop(Board, Player):-

  % repeat until user inputs a valid play
  repeat,

    % process input
    askPos(Player, Col, Row, NewCol, NewRow, Status),

    % if user made a play
    (Status \= exit
      ->  (
            % if play is valid
            (validatePlay(Board, Player, Col, Row, NewCol, NewRow)
              ->  (
                    move(Board, Col, Row, NewCol, NewRow, NewBoard),

                    % update
                    update(NewBoard, NewNewBoard),

                    % TODO check gameover and printf accordingly

                    % render
                    printBoard(NewNewBoard),

                    % repeat
                    switchPlayer(Player, NextPlayer),
                    gameloop(NewNewBoard, NextPlayer)
                  )
              ;   (
                    dsp_invalidPlay,
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
main:-
  startmenu(Status),
  % if user started the game
  (Status \= exit
    ->  (
          initial_board(Board),
          firstPlayer(Player),
          printBoard(Board),
          gameloop(Board, Player),
          main
        )
    ; true % close program
  ).
