/**
* This file implements the user interface
* and the necessary I/O operations
*/

/**
* When 'e' is pressed, the game exits
* 101 is the ASCII Code for 'e'
*/
exitCode(101).


/**
* Converts an ASCII code to it's corresponding int
*
* @param -Int Int
* @þaram +Code Code
*/
int_code(Int, Code):-
  Int is Code - 48. % input = code - '0'


/**
* Reads an int from input stream
*
* @param -Input Int read
*/
get_int(Input):-
  get_code(Code),
  int_code(Input, Code).


/**
* Prints a list of ASCII codes to the terminal
*
* @param +List List of codes to print
*/
putCode([]).
putCode([H | T]):- put_code(H), putCode(T).


/**
* Reads a line from the input stream
*
* @param -L Line read
* @param +N Length of line to read. Fails if line read is of different length
* @param -N Returns the length of line read
*/
readLine(L, N):-
  read_line(L),
  length(L, N).


/**
* Analyses the line read and returns the corresponding status
*
* @param +Line Input read
* @param +Length Length of the input line
* @param -Col Piece's column
* @param -Row Piece's row
* @param -Status Exit status
*/

% main
readPosParser([ColCode , RowCode], 2, Col, Row, good):-
  Col is ColCode - 65, % col = code - 'A'
  Col >= 0, Col =< 8,
  int_code(TempRow, RowCode),
  Row is TempRow - 1,
  Row >= 0, Row =< 8,
  !.

% exit button pressed, return to startmenu
readPosParser([Code], 1, _, _, exit):-
  exitCode(Code),
  !.

% unexpected input
readPosParser(_, _, _, _, error):- !.


/**
* Decides how to act based on the exit status of readPosParser/5
*
* @param +Status Exit status of readPosParser/5
*/

% everything is good, continue
readPosParserHandler(good).

% exit button pressed, go back to main menu
readPosParserHandler(exit).

% unexpected input
readPosParserHandler(error):-
  write('\nWrong input, please try again...\n'),
  fail.


/**
* Reads input (piece position - col and row)
*
* @param -Col Piece's column
* @param -Row Piece's row
* @param -Status Exit status
*/
readPos(Col, Row, Status):-
  repeat,
    readLine(Line, Length),
    readPosParser(Line, Length, Col, Row, Status),
    readPosParserHandler(Status),
  !.


/**
* Asks the user to move a piece
*
* @param -Col Selected piece's column
* @param -Row Selected piece's row
* @param -NewCol Destination column of selected piece
* @param -NewRow Destination row of selected piece
* @param -Status Exit status
*/
askPos(Col, Row, NewCol, NewRow, Status):-
  write('Choose a piece to move. (e.g. A1)\nType \'e\' to return to the start menu.\n'),
  readPos(Col, Row, Status1),
  (Status1 = good
    -> (
          write('\nWhere do you want to place the piece? (e.g. B2)\nType \'e\' to return to the start menu.\n'),
          readPos(NewCol, NewRow, Status)
        )
    ; Status = exit
  ).


/**
* Analyses the line read and returns the corresponding status
*
* @param +Line Input read
* @param +Length Length of input line
* @param -Status Exit status
*/

% exit button pressed, close the program
startmenuParser([Code], 1, exit):-
  exitCode(Code),
  !.

% ENTER pressed, start the game
startmenuParser([], 0, good):- !.

% unexpected input
startmenuParser(_, _, error):- !.


/**
* Decides how to act based on the exit status of startmenuParser/3
*
* @param +Status Exit status of startmenuParser/3
*/

% start the game
startmenuParserHandler(good).

% exit the program
startmenuParserHandler(exit):-
  write('\nExited program. Goodbye.\n\n').

% unexpected input
startmenuParserHandler(error):-
  write('\nWrong input, please try again...\n'),
  fail.


/**
* First menu that appears when the program is started
*
* @param +Status Exit status
*/
startmenu(Status):-
  nl,
  write(' ******************** TABLUT ********************'), nl,
  write(' *                                              *'), nl,
  write(' *  Type \'e\' to exit, or press ENTER to start!  *'), nl,
  write(' *                                              *'), nl,
  write(' ************************************************'), nl,
  nl,
  repeat,
    readLine(Line, Length),
    startmenuParser(Line, Length, Status),
    startmenuParserHandler(Status),
  !.
