/**
* This file implements the user interface
*/

/**
* Read input (char), ignoring trailing \n
*
* @param -Input Char read
*/
getChar(Input):-
  get_char(Input),
  get_char(_).


/**
* Read input (ascii code), ignoring trailing \n
*
* @param -Input Code read
*/
getCode(Input):-
  get_code(Input),
  get_code(_).


/**
* First menu that appears when the program is started
*/
startmenu:-
  write('*********************TABLUT*********************'), nl,
  write('*                                              *'), nl,
  write('*            Press any key to start!           *'), nl,
  write('*                                              *'), nl,
  write('************************************************'), nl,
  get_code(_).
