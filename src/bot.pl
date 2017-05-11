/**
* This file implements everything related to the bots
*/

:- use_module(library(random)).

:- ensure_loaded('board.pl').


/**
* Makes a random move
*
* @param +Board Board
* @param +Player Current player's turn
* @param +OCol Original (current) column of piece to move
* @param +ORow Original (current) row of piece to move
* @param +NCol New column for selected piece
* @param +NRow New row for selected piece
*/
randomBot(Board, Player, OCol, ORow, NCol, NRow):-
  findall(
    OOCol-OORow-NNCol-NNRow,
    validatePlay(Board, Player, OOCol, OORow, NNCol, NNRow),
    Plays
  ),
  length(Plays, NPlays),
  random(0, NPlays, ChosenPlay),
  find(ChosenPlay, Plays, OCol-ORow-NCol-NRow).
