/**
* This file implements all of Player predicates
*/

/**
* Returns the player to start the game.
*
* @param -Player Player to start the game
*/
firstPlayer(defplayer).


/**
* Switches turn
*
* @param +CurrentPlayer Current player's turn
* @param -NextPlayer Next player's turn
*/
switchPlayer(atkplayer, defplayer).
switchPlayer(defplayer, atkplayer).


/**
* Checks if the piece the player wants to move belongs to him
*
* @param +Player Current player
* @param +Piece Piece player wants to move
*/
ownPiece(atkplayer, attacker).
ownPiece(defplayer, defender).
ownPiece(defplayer, king).


/**
* Returns the name of each player
*
* @param +Player Current player's turn
* @param -String Name of the player
*/
toString(atkplayer, 'Attacking Player').
toString(defplayer, 'Defending Player').
