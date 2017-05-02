/**
* This file implements multiple utilities used through out this program
*/

/**
* Finds an element in a list (same as nth0)
*
* @param +X Index of element to find
* @param +List List that has the element to be found
* @param -Elem Returns element found
*/
find(0, [H|_], H).
find(X, [_|T], Elem):- X > 0, NewX is X - 1, find(NewX, T, Elem).


/**
* Finds an element in a list of lists (2D array)
*
* @param +X Index of element to be found (array[Y][X])
* @param +Y Index of element to be found (array[Y][X])
* @param +List List that has the element to be found
* @param -Elem Element found
*/
find(X, 0, [H|_], Elem):- find(X, H, Elem).
find(X, Y, [_|T], Elem):- Y > 0, NewY is Y - 1, find(X, NewY, T, Elem).


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
