/**
* This file implements the core functions of the program, such as the game loop
*/

/*
TODO remove magic numbers from all src files, such as board width and height
TODO review and remove unecessary cuts
*/

:- ensure_loaded('board.pl').
:- ensure_loaded('interface.pl').
:- ensure_loaded('player.pl').


/**
* Game Loop - Where the magic happens
*
* @param +Board Current state of the game board
* @param +Player Current player's turn
* @param +Gamestate Current state of the game
*/
gameloop(Board, Player, Gamestate):-

  % repeat until user inputs a valid play
  repeat,

    % TODO interface gamestate message (interface waits enter press and status = exit)
    write('\nGame state: '), write(Gamestate), nl,

    (Gamestate \= checkmate ->
      % process input
      askPos(Player, Col, Row, NewCol, NewRow, Status)
    ;
      Status = exit
    ),

    % if user made a play
    (Status \= exit
      ->  (
            % if play is valid
            (validatePlay(Board, Player, Col, Row, NewCol, NewRow)
              ->  (
                    move(Board, Col, Row, NewCol, NewRow, NewBoard),

                    % update
                    update(NewBoard, NewNewBoard, Gamestate, NewGamestate),

                    % render
                    printBoard(NewNewBoard),

                    % repeat
                    switchPlayer(Player, NextPlayer),
                    gameloop(NewNewBoard, NextPlayer, NewGamestate)
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
          gameloop(Board, Player, normal),
          main
        )
    ; true % close program
  ).
