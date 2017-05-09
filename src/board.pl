/**
* This file implements all of Board predicates
* such as printBoard/1 or validatePlay/6
*/

:- use_module(library(lists)).

:- ensure_loaded('utils.pl').
:- ensure_loaded('player.pl').


/**
* Returns the board state corresponding to the rule name
*
* @param -Board Game board (list of lists)
*/

empty_board([
  [emptyCell, emptyCell, emptyCell, atkarea, atkarea, atkarea, emptyCell, emptyCell, emptyCell],
  [emptyCell, emptyCell, emptyCell, emptyCell, atkarea, emptyCell, emptyCell, emptyCell, emptyCell],
  [emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell],
  [atkarea, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, atkarea],
  [atkarea, atkarea, emptyCell, emptyCell, castle, emptyCell, emptyCell, atkarea, atkarea],
  [atkarea, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, atkarea],
  [emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell, emptyCell],
  [emptyCell, emptyCell, emptyCell, emptyCell, atkarea, emptyCell, emptyCell, emptyCell, emptyCell],
  [emptyCell, emptyCell, emptyCell, atkarea, atkarea, atkarea, emptyCell, emptyCell, emptyCell]
]).

initial_board([
  [emptyCell, emptyCell, emptyCell, attacker, attacker, attacker, emptyCell, emptyCell, emptyCell],
  [emptyCell, emptyCell, emptyCell, emptyCell, attacker, emptyCell, emptyCell, emptyCell, emptyCell],
  [emptyCell, emptyCell, emptyCell, emptyCell, defender, emptyCell, emptyCell, emptyCell, emptyCell],
  [attacker, emptyCell, emptyCell, emptyCell, defender, emptyCell, emptyCell, emptyCell, attacker],
  [attacker, attacker, defender, defender, king, defender, defender, attacker, attacker],
  [attacker, emptyCell, emptyCell, emptyCell, defender, emptyCell, emptyCell, emptyCell, attacker],
  [emptyCell, emptyCell, emptyCell, emptyCell, defender, emptyCell, emptyCell, emptyCell, emptyCell],
  [emptyCell, emptyCell, emptyCell, emptyCell, attacker, emptyCell, emptyCell, emptyCell, emptyCell],
  [emptyCell, emptyCell, emptyCell, attacker, attacker, attacker, emptyCell, emptyCell, emptyCell]
]).


/**
* Translates board symbols into chars, for easier printing
*
* @param +Value Symbol to translate
* @param -Char Symbol's corresponding char
*/
toChar(emptyCell, ' ').
toChar(king, 'K').
toChar(defender, 'D').
toChar(attacker, 'A').
toChar(castle, 'C').
toChar(atkarea, 'X').


/**
* Checks if two pieces are enemies
*
* @param +PieceA First piece
* @param +PieceB Second piece
*/
enemy(attacker, defender).
enemy(attacker, king).
enemy(defender, attacker).
enemy(king, attacker).
enemy(king, castle). % surprisingly, the castle can help capture the king


/**
* Prints horizontal board separators
*
* @param +Type Separator to print
*/
printSeparator(topmost):-
  write('    _____________________________________________________ ').

printSeparator(bottom):-
  write('   |_____|_____|_____|_____|_____|_____|_____|_____|_____|').

printSeparator(top):-
  write('   |     |     |     |     |     |     |     |     |     |').

printSeparator(inline):-
  write('  |').

printSeparator(linestart):-
  write('|').


/**
* Print column identifiers
*/
printColIds:-
  write('      A     B     C     D     E     F     G     H     I   ').


/**
* Get list of row identifiers to print
* (length MUST be the same as board rows!)
*
* @param -List List of row ids
*/
getRowIds([' 1 ', ' 2 ', ' 3 ', ' 4 ', ' 5 ', ' 6 ', ' 7 ', ' 8 ', ' 9 ']).


/**
* Get list containing information to display to the user
* (length MUST be the same as board rows!)
*/
getInfo(['A - Attacker', 'C - Castle', 'D - Defender', 'K - King', 'X - Attackers\' Area', '', '', '', '']).


/*
* Prints a line of the board
*
* @param +Line List to print
*/

% stop condition
printLine([]).

% main
printLine([H | T]):-
  write('  '),
  toChar(H, C),
  write(C),
  printSeparator(inline),
  printLine(T).


/**
* Prints board to the terminal
*
* @param +Board List of lists to print
* @param +RowIds List of row identifiers to print
* @param +Info List containing information to display to the user
*/

% stop condition
printBoard([], [], []).

% main
printBoard([H | T], [RH | RT], [IH | IT]):-
  printSeparator(top), nl,
  write(RH), printSeparator(linestart), printLine(H),
  write('     '), write(IH), nl,
  printSeparator(bottom), nl,
  printBoard(T, RT, IT).


/**
* Interface for printBoard/3
*
* @param +Board List of lists to print
*/
printBoard(Board):-
  nl,
  printColIds, nl,
  getRowIds(RowIds),
  getInfo(Info),
  printSeparator(topmost), nl,
  printBoard(Board, RowIds, Info),
  nl.


/**
* Removes a piece from the board
* And replaces it with corresponding area
*
* @param +Board Board
* @param +Col Column of piece to remove
* @param +Row Row of piece to remove
* @param -NBoard New board after removal
*/
remove(Board, Col, Row, NBoard):-
  empty_board(EBoard),
  find(Col, Row, EBoard, Piece),
  replace(Col, Row, Board, Piece, NBoard).


/**
* Moves a board piece
*
* @param +Board Board
* @param +OCol Original (current) column of piece to move
* @param +ORow Original (current) row of piece to move
* @param +NCol New column for selected piece
* @param +NRow New row for selected piece
* @param -NBoard New board after movement
*/
move(Board, OCol, ORow, NCol, NRow, NBoard):-
  find(OCol, ORow, Board, Piece),
  replace(NCol, NRow, Board, Piece, AuxBoard),
  remove(AuxBoard, OCol, ORow, NBoard).


/**
* Auxiliary rule to clearPath/5
* Checks in what direction the movement is,
* and calculates the iteration step accordingly
*
* @param +ICol Current column being iterated
* @param +IRow Current row being iterated
* @param +FCol Final column
* @param +FRow Final row
* @param -NextCol Next column to iterate
* @param -NextRow Next row to iterate
*/

% left > right
clearPathIterationStep(ICol, FRow, FCol, FRow, NextCol, FRow):-
  ICol < FCol,
  NextCol is ICol + 1.

% right > left
clearPathIterationStep(ICol, FRow, FCol, FRow, NextCol, FRow):-
  ICol > FCol,
  NextCol is ICol - 1.

% top > bottom
clearPathIterationStep(FCol, IRow, FCol, FRow, FCol, NextRow):-
  IRow < FRow,
  NextRow is IRow + 1.

% bottom > top
clearPathIterationStep(FCol, IRow, FCol, FRow, FCol, NextRow):-
  IRow > FRow,
  NextRow is IRow - 1.


/**
* Checks if there's nothing blocking the path
* between two board positions.
*
* Requires clearPathIterationStep/6 to be called beforehand,
* in order to skip the first iteration (the original position)
*
* @param +Board Board
* @param +OPiece Initial position
* @param +ICol Current column being iterated
* @param +IRow Current row being iterated
* @param +FCol Final column
* @param +FRow Final row
*/

% stop condition
clearPathIteration(Board, OPiece, FCol, FRow, FCol, FRow):- !,
  find(FCol, FRow, Board, FPiece),
  FPiece \= attacker,
  FPiece \= defender,
  (OPiece = king ->
    FPiece \= atkarea
  ;
    FPiece \= king
  ).

% main
clearPathIteration(Board, OPiece, ICol, IRow, FCol, FRow):-
  find(ICol, IRow, Board, IPiece),
  IPiece \= attacker,
  IPiece \= defender,
  (OPiece = king ->
    IPiece \= atkarea
  ;
    IPiece \= king
  ),
  clearPathIterationStep(ICol, IRow, FCol, FRow, NextCol, NextRow),
  clearPathIteration(Board, OPiece, NextCol, NextRow, FCol, FRow),
  !.


/**
* Interface for clearPathIteration/5, since it requires
* clearPathIterationStep/6 to be called beforehand
*
* @param +Board Board
* @param +OCol Current column being iterated
* @param +ORow Current row being iterated
* @param +NCol Final column
* @param +NRow Final row
*/
clearPath(Board, OCol, ORow, NCol, NRow):-
  clearPathIterationStep(OCol, ORow, NCol, NRow, ICol, IRow),
  find(OCol, ORow, Board, OPiece),
  clearPathIteration(Board, OPiece, ICol, IRow, NCol, NRow).


/**
* Each game rule is listed as a single prolog rule
*
* This implementation is negation based (Status = no),
* meaning it checks if a play breaks a rule,
* not if it follows it. Example:
*
* 0. Can only move attackers, defenders or the king
* 1. Status = yes ← Piece = (attackers OR defenders OR king)
* 2. Status = NOT(yes) ← Piece = NOT(attackers OR defenders OR king)
* 3. Status = no ← Piece = (NOT(attackers) AND NOT(defenders) AND NOT(king))
*
* @param +Board Board
* @param +Player Current player's turn
* @param +OCol Original (current) column of piece to move
* @param +ORow Original (current) row of piece to move
* @param +NCol New column for selected piece
* @param +NRow New row for selected piece
* @param +Piece Piece to move
* @param +NPiece New destination of piece
* @param -Status Exit status. yes - play follows rule; no - play breaks rule
*/

% a player can only move his own pieces (attackers, defenders or king)
gamerule(_, Player, _, _, _, _, Piece, _, no):-
  \+ ownPiece(Player, Piece).

% a piece can only be placed in an empty cell or atk area
gamerule(_, _, _, _, _, _, _, NPiece, no):-
  NPiece \= emptyCell,
  NPiece \= atkarea.

% a piece can only move in a straight line
gamerule(_, _, OCol, ORow, NCol, NRow, _, _, no):-
  OCol \= NCol,
  ORow \= NRow.

% a piece can't pass through other pieces
gamerule(Board, _, OCol, ORow, NCol, NRow, _, _, no):-
  \+ clearPath(Board, OCol, ORow, NCol, NRow).


/** TODO try using if/3 instead of status + findall
* Checks if a play is valid
*
* This doesn't actually check if a play follows every rule,
* it checks if it breaks any
*
* @param +Board Board
* @param +Player Current player's turn
* @param +OCol Original (current) column of piece to move
* @param +ORow Original (current) row of piece to move
* @param +NCol New column for selected piece
* @param +NRow New row for selected piece
*/
validatePlay(Board, Player, OCol, ORow, NCol, NRow):-
  find(OCol, ORow, Board, Piece),
  find(NCol, NRow, Board, NPiece),
  findall(Status, gamerule(Board, Player, OCol, ORow, NCol, NRow, Piece, NPiece, Status), List),
  \+member(no, List).


/**
* Checks if a piece is captured
*
* @param +Board Game board
* @param +Piece Piece to analyze
* @param +Col Piece's column
* @param +Row Piece's row
*/

% king
captured(Board, king, Col, Row):- !,
  LCol is Col - 1,
  RCol is Col + 1,
  TRow is Row - 1,
  DRow is Row + 1,
  % don't call find/4 if either LCol, RCol, TRow or DRow is out of bounds
  ( \+ (LCol < 0 | RCol > 8 | TRow < 0 | DRow > 8) ->
    % right
    find(LCol, Row, Board, LPiece),
    enemy(Piece, LPiece),
    % left
    find(RCol, Row, Board, RPiece),
    enemy(Piece, RPiece),
    % top
    find(Col, TRow, Board, TPiece),
    enemy(Piece, TPiece),
    % down
    find(Col, DRow, Board, DPiece),
    enemy(Piece, DPiece)
  ).

% defenders and attackers
captured(Board, Piece, Col, Row):-
  % horizontal
  (
    LCol is Col - 1,
    RCol is Col + 1,
    % don't call find/4 if either LCol or RCol is out of bounds
    ( \+ (LCol < 0 | RCol > 8) ->
      find(LCol, Row, Board, LPiece),
      enemy(Piece, LPiece),
      find(RCol, Row, Board, RPiece),
      enemy(Piece, RPiece)
    )
  % vertical
  |
    TRow is Row - 1,
    DRow is Row + 1,
    % don't call find/4 if either TRow or DRow is out of bounds
    ( \+ (TRow < 0 | DRow > 8) ->
      find(Col, TRow, Board, TPiece),
      enemy(Piece, TPiece),
      find(Col, DRow, Board, DPiece),
      enemy(Piece, DPiece)
    )
  ).


/**
* Iterates through Board and finds every captured piece
*
* delete/3 MUST be called after this rule, to remove
* all delete elements from the returned list
*
* @param +Board Game board
* @param +Col Column of piece being iterated
* @param +Row Row of piece being iterated
* @param -Captured List of all the captured pieces
*/

% stop condition
getCaptured(_, 0, 9, []).

% next row
getCaptured(Board, 9, Row, Captured):-
  NextRow is Row + 1,
  getCaptured(Board, 0, NextRow, Captured).

% main
getCaptured(Board, Col, Row, [Append | Captured]):-
  find(Col, Row, Board, Piece),
  (
    captured(Board, Piece, Col, Row),
    Append = Piece-Col-Row %  diff from Append is Col - Row
  |
    % delete/3 should be called to remove all of these elements
    Append = delete
  ),
  NextCol is Col + 1,
  getCaptured(Board, NextCol, Row, Captured).


/**
* Interface for getCaptured/4
*
* @param +Board Game board
* @param -Captured List of all the captured pieces
*/
getCaptured(Board, Captured):-
  getCaptured(Board, 0, 0, Temp),
  delete(Temp, delete, Captured).


/**
* Removes every captured piece from Board
*
* @param +Board Game board
* @param +Captured List of all the captured pieces
* @param -NewBoard Board with all captured pieces removed
*/

% stop condition
removeCaptured(Board, [], Board).

% main
removeCaptured(Board, [Piece-Col-Row | T], NewBoard):-
  ( % king can never be removed, even when captured
    Piece = king
  |
    remove(Board, Col, Row, NBoard)
  ),
  removeCaptured(NBoard, T, NewBoard).


/**
* Checks the current game state
*
* Possible states:
*   normal - nothing special to note
*   check - the king has one path of escape
*   checkmate - the king has two or more paths of escape
*   captured - the king was captured
*
* @param +Board Current board
* @param +PrevState Previous game state
* @param -State Current state of the game
*/

% check
gamestate(Board, check, State):-
  gamestate(Board, normal, TempState),
  ( % if check two turns in a row, turns into checkmate and player wins
    TempState = check,
    State = checkmate
  |
    State = TempState
  ).

% normal
gamestate(Board, normal, State):-

  findPos(Col, Row, Board, king),

  ( % check if king was captured
    captured(Board, king, Col, Row),
    State = captured

  | % check if the king has any escapes

    ( % right
      clearPath(Board, Col, Row, 8, Row),
      Right = 1
    |
      Right = 0
    ),
    ( % left
      clearPath(Board, Col, Row, 0, Row),
      Left = 1
    |
      Left = 0
    ),
    ( % top
      clearPath(Board, Col, Row, Col, 0),
      Top = 1
    |
      Top = 0
    ),
    ( % down
      clearPath(Board, Col, Row, Col, 8),
      Down = 1
    |
      Down = 0
    ),
    Escapes is Right + Left + Top + Down,
    (
      Escapes = 0,
      State = normal
    |
      Escapes = 1,
      State = check
    |
      Escapes > 1,
      State = checkmate
    )
  ).


/**
* Updates the board, removing captured pieces
*
* @param +Board Current board
* @param -NewBoard Updated board
* @param +CurrentState Current gamestate
* @param -UpdatedState State of updated game
*/
update(Board, NewBoard, CurrentState, UpdatedState):-
  getCaptured(Board, Captured),
  removeCaptured(Board, Captured, NewBoard),
  gamestate(Board, CurrentState, UpdatedState).
