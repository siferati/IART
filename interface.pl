/**
* This file implements the user interface
*/

/**
* When 'e' is pressed, the game exits
* 101 is the ASCII Code for 'e'
*/
exitCode(101).


/** TODO redo these interfaces using skip_line\0 and read_line\1
* Reads input (char), ignoring trailing \n
*
* @param -Input Char read
*/
readChar(Input):-
  get_char(Input),
  get_char(_).


/**
* Reads input (ascii code), ignoring trailing \n
*
* @param -Input Code read
*/
readCode(Input):-
  get_code(Input),
  get_code(_).


/**
* Reads input (int), NOT ignoring trailing \n
*
* @param -Input Int read
*/
get_int(Input):-
  get_code(Code),
  Input is Code - 48. % input = code - '0'


/**
* Reads input (int), ignoring trailing \n
*
* @param -Input Int read
*/
readInt(Input):-
  get_int(Input),
  get_code(_).


/** TODO bad input handling (look at skip_line\0 and read_line\1)
* Reads input (piece position - col and row), ignoring trailing \n
* Fails if the user pressed the exit button
*
* @param -Col Piece's column
* @param -Row Piece's row
* @param -Status Exit status
*/

% main
readPos(Col, Row, good):-
  get_code(Code),
  \+ exitCode(Code),
  Col is Code - 65, % col = code - 'A'
  readInt(Int),
  Row is Int - 1,
  !.

% called when exit button is pressed
readPos(_, _, exit):- !.


/**
* Decides how to act based on the exit status of readPos/3
*
* @param +Status Exit status of readPos/3
*/

% everything is good, continue
readPosHandler(good):- !.

% exit button pressed, go back to main menu
readPosHandler(exit):-
  get_code(_), % clear trailing \n from buffer
  !, fail. % handler fails > askPos fails > gameloop fails > enters 2nd gameloop rule > main


/**
* Asks the user to move a piece
*
* @param -Col Selected piece's column
* @param -Row Selected piece's row
* @param -NewCol Destination column of selected piece
* @param -NewRow Destination row of selected piece
*/
askPos(Col, Row, NewCol, NewRow):-
  write('Choose a piece to move. (e.g. A1)\nPress e to exit.\n'),
  readPos(Col, Row, Status1),
  readPosHandler(Status1),
  write('\nWhere do you want to place the piece? (e.g. B2)\nPress e to exit.\n'),
  readPos(NewCol, NewRow, Status2),
  readPosHandler(Status2).


/**
* Decides how to act based on the startmenu selected option
*
* @param +Input ASCII Code of pressed key
*/

% exit button pressed, close the program
startmenuHandler(Input):-
  exitCode(Input),
  write('\nExited program. Goodbye.\n\n'),
  !, fail. % handler fails > menu fails > enters 2nd main rule > exit

% random key pressed, continue
startmenuHandler(_).


/**
* First menu that appears when the program is started
*/
startmenu:-
  nl,
  write(' ******************** TABLUT ********************'), nl,
  write(' *                                              *'), nl,
  write(' *  Press e to exit, or any other key to start  *'), nl,
  write(' *                                              *'), nl,
  write(' ************************************************'), nl,
  nl,
  get_code(Input),
  skip_line, % TODO temporary. remove once io interfaces have been rebuilt
  startmenuHandler(Input).
