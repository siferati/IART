/** TODO clean docs and delete findPos, since find now works with constraints
* This file implements multiple utilities used through out this program
*/

:- use_module(library(clpfd)).

/**
* Finds an element in a list (same as nth0)
*
* @param +X Index of element to find
* @param +List List that has the element to be found
* @param -Elem Returns element found
*/
find(0, [H|_], H).
find(X, [_|T], Elem):- X #> 0, NewX #= X - 1, find(NewX, T, Elem).


/**
* Finds an element in a list of lists (2D array)
*
* @param +X Index of element to be found (array[Y][X])
* @param +Y Index of element to be found (array[Y][X])
* @param +List List that has the element to be found
* @param -Elem Element found
*/
find(X, 0, [H|_], Elem):- find(X, H, Elem).
find(X, Y, [_|T], Elem):- Y #> 0, NewY #= Y - 1, find(X, NewY, T, Elem).


/**
* Replaces an element in a list
*
* @param +X Index of element to be replaced
* @param +InList List that has the element to be replaced
* @param +Elem Element to insert (replacement)
* @param -OutList Returned list
*/
replace(0, [_|T], Elem, [Elem|T]).
replace(X, [H|T], Elem, [H|R]):- X > 0, NewX is X - 1, replace(NewX, T, Elem, R).


/**
* Replaces an element in a list of lists (2D array)
*
* @param +X Index of element to be replaced (array[Y][X])
* @param +Y Index of element to be replaced (array[Y][X])
* @param +InList List that has the element to be replaced
* @param +Elem Element to insert (replacement)
* @param -OutList Returned list (2D array)
*/
replace(X, 0, [H|T], Elem, [NewH|T]):- replace(X, H, Elem, NewH).
replace(X, Y, [H|T], Elem, [H|R]):- Y > 0, NewY is Y - 1, replace(X, NewY, T, Elem, R).


/**
* Returns the position of an element in a list
*
* @param -X Index of element
* @param +List List that has the element to be found
* @param +Elem Element to find
*/
findPos(0, [H|_], H).
findPos(X, [_|T], Elem):- findPos(Aux, T, Elem), X is Aux + 1.


/**
* Returns the position of an element in a list of lists (2D array)
*
* @param -X Index of element
* @param -Y Index of element
* @param +List List that has the element to be found
* @param +Elem Element to find
*/
findPos(X, 0, [H|_], Elem):- findPos(X, H, Elem).
findPos(X, Y, [_|T], Elem):- findPos(X, Aux, T, Elem), Y is Aux + 1.


/**
* Concatenates two atoms (e.g. 'I am ' + 21 = 'I am 21')
*
* @param +X First atom to concatenate
* @param +Y Second atom to concatenate
* @param -S Concatenation of X and Y
*/
concat(X, Y, S):-
  name(X, Xs),
  name(Y, Ys),
  append(Xs, Ys, Ss),
  name(S, Ss),
  write(S).


/**
* Concatenates all atoms in the given list
*
* @param +List Atoms to concatenate
* @param -Ss List of all (ascii) codes concatenated
*/
concatAux([], []).
concatAux([H | T], Ss):-
  name(H, Hs),
  append(Hs, Aux, Ss),
  concatAux(T, Aux).


/**
* Concatenates all atoms in the given list
*
* @param +List Atoms to concatenate
* @param -S Concatenation of all atoms
*/
concat(List, S):-
  concatAux(List, Ss),
  name(S, Ss).
