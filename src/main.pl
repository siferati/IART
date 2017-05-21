/**
* This file implements the core functions of the program, such as the game loop
*/

:- ensure_loaded('board.pl').
:- ensure_loaded('bot.pl').
:- ensure_loaded('interface.pl').
:- ensure_loaded('player.pl').

/**
* Switches turn
*
* @param +Type Type of game (hvh, hvb)
* @param +Bot Yes if bot is current player. No if current player is human
* @param -NextBot Yes if bot is next player. No if next player is human
*/
switchTurn(hvh, no, no).
switchTurn(hvb, no, yes).
switchTurn(hvb, yes, no).
switchTurn(bvb, yes, yes).


/**
* Game Loop - Where the magic happens
*
* @param +Type Type of game (hvh, hvb)
* @param +Bot Yes if bot is current player. No if current player is human
* @param +Board Current state of the game board
* @param +Player Current player's turn
* @param +Gamestate Current state of the game
* @param +Log Log of plays
*/
gameloop(Type, Bot, Board, Player, Difficulty, Gamestate, Log):-

  % repeat until user inputs a valid play
  repeat,

    (\+gameover(Gamestate) ->
      (Bot = no ->
        % process input
        askPos(Player, Col, Row, NewCol, NewRow, Status)
      ;
        botTurn(Player),
        thinkMove(Board, Player, Difficulty, Col-Row-NewCol-NewRow),
        Status = good
      )
    ;
      switchPlayer(Player, Winner),
      gameovermenu(Winner),
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
                    update(NewBoard, Player, NewNewBoard, NewGamestate),
                    updateLog(Log, Board, NewGamestate, Player, Col, Row, NewCol, NewRow, NewLog),

                    % render
                    (NewGamestate = captured ->
                      % print NOT updated board, in case important pieces were captured
                      printBoard(NewBoard, NewLog)
                    ;
                      printBoard(NewNewBoard, NewLog)
                    ),

                    % repeat
                    switchTurn(Type, Bot, NextBot),
                    switchPlayer(Player, NextPlayer),
                    gameloop(Type, NextBot, NewNewBoard, NextPlayer, Difficulty, NewGamestate, NewLog)
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
  startmenu(Type, Difficulty, Player),
  % if user started the game
  (Type \= exit
    ->  (
		initial_board(Board),
        getLog(Log),
        printBoard(Board, Log),
		(Type == bvb ->
		  firstPlayer(Player),
		  gameloop(Type, yes, Board, Player, Difficulty, normal, Log)
		; Type == hvb ->
		  gameloop(Type, no, Board, Player, Difficulty, normal, Log)
		; firstPlayer(Player),
		  gameloop(Type, no, Board, Player, Difficulty, normal, Log)
		),
        main
        )
    ; true % close program
  ).
