/**
* This file implements the user interface
* and the necessary I/O operations
*/

:- ensure_loaded('player.pl').


/**
* Text to display to the user
*
* Put under prolog rules to avoid code replication
* and for easier access and modification
*/

dsp_invalidInput:- write('\nInvalid input, please try again...\n').

dsp_invalidPlay:- write('\nInvalid play, please try again...\n').

dsp_pressEnter:- write('\nPress ENTER to continue...\n').

% @param +Player Current player's turn
dsp_playerTurn(Player):-
  toString(Player, Name),
  write('\nPlayer\'s turn: '),
  write(Name), nl.

% @param +Player Current player's turn
dsp_botTurn(Player):-
  dsp_playerTurn(Player),
  write('\nIt\'s the Bot\'s turn!'),
  dsp_pressEnter.

dsp_goodbye:- write('\nExited program. Goodbye.\n\n').

% @param +Player Current player's turn
dsp_choosePiece(Player):-
  dsp_playerTurn(Player),
  write('\nChoose a piece to move. (e.g. a4)\nType \'e\' to return to the start menu.\n').

dsp_placePiece:- write('\nWhere do you want to place the piece? (e.g. b4)\nType \'e\' to return to the start menu.\n').

dsp_check:- write('\nCHECK: The king has one path of escape!\n').

dsp_startmenu:-
  nl,
  write(' ******************** TABLUT ********************'), nl,
  write(' *                                              *'), nl,
  write(' *          Choose an option to start!          *'), nl,
  write(' *                                              *'), nl,
  write(' *          1. Human vs Human                   *'), nl,
  write(' *          2. Human vs Bot                     *'), nl,
  write(' *          3. Bot vs Bot                       *'), nl,
  write(' *          4. Exit                             *'), nl,
  write(' *                                              *'), nl,
  write(' ************************************************'), nl,
  nl.
  
  
dsp_difficulty:-
  nl,
  write(' ******************* SETTINGS *******************'), nl,
  write(' *                                              *'), nl,
  write(' *          Choose an difficulty to play.       *'), nl,
  write(' *                                              *'), nl,
  write(' *          1. Easy                             *'), nl,
  write(' *          2. Medium                           *'), nl,
  write(' *          3. Difficult                        *'), nl,
  write(' *                                              *'), nl,
  write(' ************************************************'), nl,
  nl.
  
dsp_getplayer:-
  nl,
  write(' ******************* SETTINGS *******************'), nl,
  write(' *                                              *'), nl,
  write(' *          Choose your player.                 *'), nl,
  write(' *                                              *'), nl,
  write(' *          1. Attackers                        *'), nl,
  write(' *          2. Defenders                        *'), nl,
  write(' *                                              *'), nl,
  write(' ************************************************'), nl,
  nl.

% @param +Winner Player that won the game
dsp_gameover(Winner):-
  toString(Winner, Name),
  nl,
  write(' ******************* GAMEOVER *******************'), nl,
  write(' *                                              *'), nl,
  write(' *             '), write(Name), write(' won             *'), nl,
  write(' *                                              *'), nl,
  write(' ************************************************'), nl,
  nl,
  dsp_pressEnter.


/**
* When 'e' is pressed, the game exits
* 69 is the ASCII Code for 'E'
* 101 is the ASCII Code for 'e'
*
*/
exitCode(69).
exitCode(101).


/**
* Converts an ASCII code to it's corresponding int
*
* @param -Int Int
* @þaram +Code Code
*/
int_code(Int, Code):-
  Int is Code - "0".


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
readPosParser([ColCode, RowCode], 2, Col, Row, good):-
  (Col is ColCode - "A" | Col is ColCode - "a"),
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
  dsp_invalidInput,
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
* @param +Player Current player's turn
* @param -Col Selected piece's column
* @param -Row Selected piece's row
* @param -NewCol Destination column of selected piece
* @param -NewRow Destination row of selected piece
* @param -Status Exit status
*/
askPos(Player, Col, Row, NewCol, NewRow, Status):-
  dsp_choosePiece(Player),
  readPos(Col, Row, Status1),
  (Status1 = good
    -> (
          dsp_placePiece,
          readPos(NewCol, NewRow, Status)
        )
    ; Status = exit
  ).


% 1. Human vs Human
getDifficultyParser([Code], 1, 0):-
  int_code(Int, Code),
  Int = 1,
  !.

% 2. Human vs Bot
getDifficultyParser([Code], 1, 1):-
  int_code(Int, Code),
  Int = 2,
  !.

% 3. Bot vs Bot
getDifficultyParser([Code], 1, 2):-
  int_code(Int, Code),
  Int = 3,
  !.

% unexpected input
getDifficultyParser(_, _, error):- !.

% 1. atkplayer
getHumanPlayerParser([Code], 1, atkplayer):-
  int_code(Int, Code),
  Int = 1,
  !.

% 2. defplayer
getHumanPlayerParser([Code], 1, defplayer):-
  int_code(Int, Code),
  Int = 2,
  !.

% unexpected input
getHumanPlayerParser(_, _, error):- !.  

/**
* Analyses the line read and returns the corresponding status
*
* @param +Line Input read
* @param +Length Length of input line
* @param -Status Exit status
*/

% 1. Human vs Human
startmenuParser([Code], 1, hvh):-
  int_code(Int, Code),
  Int = 1,
  !.

% 2. Human vs Bot
startmenuParser([Code], 1, hvb):-
  int_code(Int, Code),
  Int = 2,
  !.

% 3. Bot vs Bot
startmenuParser([Code], 1, bvb):-
  int_code(Int, Code),
  Int = 3,
  !.
  
% 4. Exit
startmenuParser([Code], 1, exit):-
  int_code(Int, Code),
  Int = 4,
  !.

% unexpected input
startmenuParser(_, _, error):- !.


/**
* Decides how to act based on the exit status of startmenuParser/3
*
* @param +Status Exit status of startmenuParser/3
*/

% exit the program
startmenuParserHandler(exit):- !, dsp_goodbye.

% unexpected input
startmenuParserHandler(error):-
  !,
  dsp_invalidInput,
  fail.

% start the game
startmenuParserHandler(_).


/**
* First menu that appears when the program is started
*
* @param +Status Exit status
*/
startmenu(Status, Difficulty, Player):-
  dsp_startmenu,
  repeat,
    readLine(Line, Length),
    startmenuParser(Line, Length, Status),
	( Status == hvb ->
		getDifficulty(Difficulty),
		getHumanPlayer(Player)
	; Status == bvb ->
		getDifficulty(Difficulty)
	; true
	), startmenuParserHandler(Status),
  !.

getDifficulty(Difficulty) :-
	dsp_difficulty,
	repeat,
		readLine(Line, Length),
		getDifficultyParser(Line, Length, Difficulty),
		startmenuParserHandler(Difficulty),
	   !.
	   
getHumanPlayer(Player) :-
	dsp_getplayer,
	repeat,
		readLine(Line, Length),
		getHumanPlayerParser(Line, Length, Player),
		startmenuParserHandler(Player),
	   !.		

/**
* Waits until the user presses ENTER
*
* @param +Line Input read
* @param +Length Length of input line
*/
waitEnter([], 0):- !.
waitEnter(_, _):- dsp_pressEnter, fail.


/**
* Shows gameover screen
*
* @param +Winner Player that won the game
*/
gameovermenu(Winner):-
  dsp_gameover(Winner),
  repeat,
    readLine(Line, Length),
    waitEnter(Line, Length),
  !.


/**
* Tells the user it's the bot's turn to play
*
* @param +Player Current player's turn
*/
botTurn(Player):-
  dsp_botTurn(Player),
  repeat,
    readLine(Line, Length),
    waitEnter(Line, Length),
  !.
