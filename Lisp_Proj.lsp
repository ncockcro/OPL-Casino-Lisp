;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;    * Name:  Nicholas Cockcroft                                 ;
;    * Project:  Project #2, Lisp Project        			     ;
;    * Class:  Organization of Programming Languages CMPS 366-01 ;
;    * Date:  October 22, 2018                        		     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;






(print "Welcome to Casino!")

; How to create a let function
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;
;  (defun name1 ()
;  (Let* ((x 5) (y 4)) 
;
;  (cond ((eq 5 x) (print '(x is 5)))
;	  (t (print '(nothing to see here))))))
;
; (name1)
;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	

; How to debug a particular function
;(trace coinToss)
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: CoinToss
; Purpose: To prompt the user for a coin they want to pick to see who plays first 
; Parameters: None
; Return Value: 1 for heads or 2 for tails 
; Local Variables: 
;            headsOrTails, holds either 1 or 2, depending on what the user typed in
; Algorithm: 
;	1) Get input from the user for if they want heads or tails
;  	2) If it is 1, return that
;	3) If it is 2, return that
; 	4) If it is neither of those, recursively call the function til the put in a proper input
; Assistance Received: none 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun CoinToss ()
	(print "Enter '1' for heads or '2' for tails: ")
	(Let* (( headsOrTails ( read ) ))

	(cond ( ( eq headsOrTails '1	) 
				headsOrTails )
		  ( ( eq headsOrTails '2) 
				HeadsOrTails )
		  (t 
		  ( print headsOrTails )) ) ) )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: CheckCoinToss
; Purpose: To check if the coin the user entered in matches the random coin generated
; Parameters:
;	coin, the coin that was randomly generated
;	userGuess, the coin that the user picked
; Return Value: True for if the user was correct, otherwise false
; Local Variables: None
; Algorithm: 
;   1) If the user's guess was correct, output a correct message and return True
;   2) Otherwise, output the incorrect message and return False
; Assistance Received: none 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun CheckCoinToss (coin userGuess)

	(cond ( ( eq coin userGuess ) 
		  ( print "Yes, that was correct! You go first." ) "True" )
		  ( t 
		  ( print "No, that was incorrect! The computer goes first." ) "False" ) ) )
	
	  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: FirstPlayer
; Purpose: To do the coin toss and see who will be the first player
; Parameters: None
; Return Value: "Human" if the human guessed correct, or "Computer" for otherwise
; Local Variables: 
;   player, holds "True" or "False" depending if the user guessed correctly
; Algorithm: 
;	1) If player equals "True", then return "Human"
;   2) Otherwise, return "Computer"
; Assistance Received: none 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun FirstPlayer()
	(Let* (( player ( CheckCoinToss ( random 2 ) ( CoinToss )) ))

	(cond ( ( equal player "True" ) "Human" )
			( t '"Computer" ) ) ) )
	
(print (FirstPlayer))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: LoadDeck
; Purpose: Loads a deck of 52 unique cards
; Parameters: None
; Return Value: A list of lists containing a deck of cards
; Local Variables: None
; Algorithm: 
;	1) Return the list of lists which is the 52 cards
; Assistance Received: none 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun LoadDeck ()
'( (CA) (DA) (HA) (SA) (C2) (D2) (H2) (S2) (C3) (D3) (H3) (S3) (C4) (D4) (H4) (S4) (C5) (D5) (H5) (S5) (C6) (D6) (H6) (S6) (C7) (D7) (H7) (S8) 
(C8) (D8) (H8) (S8) (C9) (D9) (H9) (S9) (CX) (DX) (HX) (SX) (CJ) (DJ) (HJ) (SJ) (CQ) (DQ) (HQ) (SQ) (CK) (DK) (HK) (SK)) )

(print (LoadDeck))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: ActualDeck
; Purpose: Takes in the deck of 52 cards, not finished but might be responsible for randomly shuffling the deck
; Parameters: None
; Return Value: Returns the deck that was passed in
; Local Variables: 
;   deck, holds the deck that was passed into it
; Algorithm: 
; 	1) Store the passed in deck into a local variable
;   2) Return the deck
; Assistance Received: none 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ActualDeck ( passedDeck )
	(Let* (( deck passedDeck ))
			deck ) )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: GetFourCards
; Purpose: To get the first four cards from a deck
; Parameters: 
; Return Value: The first four cards from the deck
; Local Variables: None
; Algorithm: 
; 	1) If count is 0, stop because we got enough cards from the deck and return the four cards list
;   2) Otherwise, append the first card from the deck to the fourCards parameter, decrement count, and send the rest
;	of the deck as a recursive call until four cards have been taken
; Assistance Received: none 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun GetFourCards(deckOfCards count fourCards)

	(cond (( eq count 0 ) 
			fourCards )
		  ( t ( GetFourCards 
		  ( rest deckOfCards ) ( - count 1 ) ( append fourCards ( first deckOfCards )) )) ) )
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: SubtractCardsFromDeck
; Purpose: To remove any number of cards from a deck
; Parameters:
;	deckOfCards, a deck of cards that is passed in
;	count, a count for how many cards are to be removed
; Return Value: The new deck of cards after removing cards
; Local Variables: None
; Algorithm: 
;	1) If count is 0, return the deck of cards
;  	2) Otherwise, recursively call the function with decrementing the count
; Assistance Received: none 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
(defun SubtractCardsFromDeck ( deckOfCards count )

	(cond (( eq count 0 )
			deckOfCards )
		  ( t 
		  ( SubtractCardsFromDeck ( rest deckOfCards ) ( - count 1 ) )) ) )
		
	  

;(print(GetFourCards (LoadDeck) 4 () ))

(print (SubtractCardsFromDeck (LoadDeck) 4))


; ******** Human *********


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: HumanGetMoveChoice
; Purpose: Get the option for if the player want to build, capture, or trail 
; Parameters: None
; Return Value: the move the player picked
; Local Variables: 
; 	playerMove, holds the move the user entered
; Algorithm: 
;	1) If the move is b, return that move
;  	2) If the move is c, return that move
;	3) If the move is t, return that move
; 	4) If it is neither of those, recursively call the function til the user puts in the corrent move
; Assistance Received: none 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun HumanGetMoveChoice()

	(print "Enter 'b' for build, 'c' for capture, or 't' for trail: ")

	(Let* (( playerMove ( read ) ))

	(cond (( eq playerMove 'b ) 
			playerMove)
		  (( eq playerMove 'c )
			playerMove)
	  (( eq playerMove 't )
		playerMove)
	  (t 
	  (HumanGetMoveChoice)) ) ) )
	  
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: CheckTrail
; Purpose: To check if the card a user entered is actually in the player's hand
; Parameters:
;	playerCard, the card the player wants to trail with
; 	playerHand, the list of cards the player has
; Return Value: "True" if the player can trail with that card, "False" otherwise
; Local Variables: None
; Algorithm: 
;	1) If a card from the players equals the card they want to trail with, return "True"
;  	2) If the playerHand equals an empty list, then we cycled through all of the cards and they didnt have it, so return "False"
;	3) If neither of those if statements were true, recursively call the function and reduce the players hand
; Assistance Received: none 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun CheckTrail (playerCard playerHand)


	(cond (( eq playerCard ( first playerHand )) 
			"True" )
		  (( eq playerHand () )
			"False")
		  (( CheckTrail pCard ( rest playerHand ) )) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: MakeTrail
; Purpose: If the player can trail, this will return a list with the player move ("t") and the card they want to trail with
; Parameters:
;	hand, the player's hand
;	table, the cards on the table
; Return Value: a list with the player move ("t") and the card they want to trail with, or "false" if they couldn't trail
; Local Variables: 
; 	trailCard, holds the card the player wants to trail with
; Algorithm: 
;	1) If check trail resolved to "True", then it would return the trail card
;  	2) Otherwise, it would return "False"
; Assistance Received: none 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun MakeTrail (hand table)

	( print "Enter a card you want to trail with" )

	( Let* (( trailCard ( read ) )) 

	(cond (( equal ( CheckTrail trailCard hand ) "True" )
			trailCard )
			( t 
			"False" ) ) ) )
	  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: HumanMakeMove
; Purpose: To call the proper function based on the move the player wants to make
; Parameters:
;	humanHand, holds the hand of the player
;	passedTable, holds the current table on the board
; Return Value: A list with the player's choice and the card(s) they are using for the move
; Local Variables: 
;            playerMove, holds the move the player wants to make
;	hand, holds the hand of the player
;	table, holds the current table
; Algorithm: 
;	1) If the move is "b", print out that builds aren't implemented
;  	2) If the move is "c", those are not implemented yet
;	3) If the move is "t", then we will call the MakeTrail function which will get a trail card that the player
;	wants to use and will then return the player's move choice and the trail card as a list
;	4) If it is neither of those options, recursively call the function til they enter the correct thing
; Assistance Received: none 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun HumanMakeMove ( humanHand passedTable )

	(Let* (( playerMove ( HumanGetMoveChoice ))
	  ( hand humanHand )
	  ( table passedTable ))
	  
	(cond (( eq playerMove 'b ) 
		  ( print "Builds not implemented" ) ( HumanMakeMove hand table ))
		  (( eq playerMove 'c) 
		  (MakeCapture hand table))
		  (( eq playerMove 't ) 
				( cond (( equal (MakeTrail hand table ) 
						"False" ) ( HumanMakeMove hand table ))
						( t ( list playerMove ( MakeTrail hand table ) )) ) )) ) )


(print (HumanMakeMove '(CA) '(CA)))

; Computer Moves
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: PlayRound
; Purpose: To serve as the main game loop for playing a round
; Parameters:
;	passedDeck, the deck of cards
;	passedHumanHand, the cards for the human hand
;	passedComputerHand, the cards for the computer hand
;	passedTable, the cards on the table
;	passedFirstPlayer, the first player, whoever won the coin toss
;	passedNextPlayer, the next player who is supposed to go
;	passedRoundCycle, how many times the function has been recursively called
; Return Value: None as of right now since it is not finished
; Local Variables: 
;   deck, holds the current deck of the round
;	humanHand, holds the current hand of the human
;	computerHand, holds the current hand of the computer
;	table, holds the cards on the table
;	firstPlayer, holds the player who won the coin toss
;	nextPlayer, holds the next player who is supposed to player
;	roundCycle, holds the current recursive call of the round
; Algorithm: 
;	1) 
;  	2)
;	3)
; 	4)
; Assistance Received: none 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun PlayRound(passedDeck passedHumanHand passedComputerHand passedTable passedFirstPlayer passedNextPlayer passedRoundCycle)

	(Let* ((deck passedDeck)
			(humanHand passedHumanHand)
			(computerHand passedComputerHand)
			(table passedTable)
			(firstPlayer passedFirstPlayer)
			(nextPlayer passedNextPlayer)
			(roundCycle passedRoundCycle)
	
	; Check if the human goes first or the computer goes first and then it will call a function
	; for that player to make a move
	(cond ((eq nextPlayer "Human") (HumanMakeMove) (nextPlayer "Computer"))
		  ((eq nextPlayer "Computer") (computerMakeMove) (nextPlayer "Human")))
	
	; Once the first player makes a move, not its time for the second player	
	(cond ((eq nextPlayer "Human") (humanMakeMove) (nextPlayer "Computer"))
		   (eq nextPlayer "Computer") (computerMakeMove) (nextPlayer "Human"))
		   
	(PlayRound)
	
	
; This starts the round of the game
; Passing in: the deck,   humanHand, computerHand, table, firstPlayer, next player, and the round cycle
(PlayRound (ActualDeck (loadDeck)) () () () (FirstPlayer) () 1)
		  



