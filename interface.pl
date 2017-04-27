/**
* This file implements the user interface
*/

/**
* When 'e' is pressed, the game exits.
* 101 is the ASCII Code for 'e'.
*/
exitCode(101).


exit(exit).

/**
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


/** TODO bad input handling
* Reads input (piece position - col and row), ignoring trailing \n.
* Fails if the user pressed the exit button.
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
  Row is Int - 1.

% called when exit button is pressed
readPos(_, _, exit).


/**
* Decides how to act based on the exit status of readPos/3
*
* @param +Status Exit status of readPos/3
*/

% everything is good, move forward
statusHandler(good).

% exit button pressed, go back to main menu
statusHandler(exit):-
  get_code(_), % clear trailing \n from buffer
  main.


/**
* Asks the user to move a piece
*
* @param -Col Selected piece's column
* @param -Row Selected piece's row
* @param -NewCol Destination column of selected piece
* @param -NewRow Destination row of selected piece
*/
askPos(Col, Row, NewCol, NewRow):-
  write('Choose a piece to move (e.g. A1)'), nl,
  readPos(Col, Row, Status1),
  statusHandler(Status1),
  write('Where do you want to place the piece? (e.g. B2)'), nl,
  readPos(NewCol, NewRow, Status2),
  statusHandler(Status2).


/**
* First menu that appears when the program is started
*/
startmenu:-
  nl,
  write(' ******************** TABLUT ********************'), nl,
  write(' *                                              *'), nl,
  write(' *            Press any key to start            *'), nl,
  write(' *                                              *'), nl,
  write(' ************************************************'), nl,
  nl,
  get_code(_).
