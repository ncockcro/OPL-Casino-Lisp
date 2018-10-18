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
'( CA DA HA SA C2 D2 H2 S2 C3 D3 H3 S3 C4 D4 H4 S4 C5 D5 H5 S5 C6 D6 H6 S6 C7 D7 H7 S8 
C8 D8 H8 S8 C9 D9 H9 S9 CX DX HX SX CJ DJ HJ SJ CQ DQ HQ SQ CK DK HK SK) )


(defun Randomize (randList randNum)

		(cond ((eq (list-length randList) 52 ) randList)
			  ((equal (CheckIfContains randList randNum) "True") (Randomize randList (random 52)))
			  ((< (list-length randList) 52 ) (Randomize (append (list randNum) randList) (random 52) ))))
			  
(defun CheckIfContains (randList randNum)


	(cond ((eq randList () ) "False")
		  ((eq (first randList) randNum ) "True" )
		  (t (CheckIfContains (rest randList) randNum))))
		  
(defun ShuffleCards (randList cardList shuffledList)

	(cond ((eq randList () ) ShuffledList )
		  (t (ShuffleCards (rest randList) cardList (append (list (nth (first randList) cardList)) shuffledList)))))
		  
		  
(print (ShuffleCards (Randomize () (random 52)) (LoadDeck) () ) )


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
		(ShuffleCards (Randomize () (random 52)) passedDeck () ) )


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
		  ( rest deckOfCards ) ( - count 1 ) ( append fourCards (list ( first deckOfCards ) )) )) ) )
	
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

	(cond (( eq playerCard  (first playerHand )  )
			"True" )
		  (( eq playerHand () )
			"False")
		  (( CheckTrail playerCard ( rest playerHand ) )) ) )

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
			(print "You can not trail with that card." ) "False" ) ) ) )
			
			
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: CheckCaptureTable
; Purpose: To check if there are cards on the table that the player can capture
; Parameters:
;	passedCaptureCard, holds the capture card the player picked
;	passedTable, holds the current table on the board
; Return Value: A list with the player's choice and the card(s) they are using for the move
; Local Variables: 
;   captureCard, holds the capture card the player picked
;	stringFirstCaptureCard, holds the string version of the capture card
;	table, holds the current table
;	stringFirstTableCard, holds the string version of the first card in the table
; Algorithm: 
;	1) If the table is equal to the empty set, then we've cycled through all the table cards and didnt find the capture card so return false
;  	2) If we found a card with the same value as the capture card, then return true
;	3) If neither of the above conditions were true, then keep cycling through the table cards
; Assistance Received: none 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;			
(defun CheckCaptureTable (passedCaptureCard passedTable)

	(Let* ((captureCard passedCaptureCard)
		   (stringFirstCaptureCard (string passedCaptureCard))
		   (table passedTable)
		   (stringFirstTableCard (string (first passedTable))))
		   
		   
	(cond ((eq table () ) "False" )
		  ((eq (char stringFirstCaptureCard 1) (char stringFirstTableCard 1)) "True")
		  (t (CheckCaptureTable captureCard (rest table) ) ) ) ) )
		  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: CheckCaptureHand
; Purpose: To check if the card the player wants to capture with is actually in the player's hand
; Parameters:
;	passedCaptureCard, holds the card the player wants to capture with
;	passedHand, holds the hand of the player
; Return Value: True if the player has the card, false otherwise
; Local Variables: 
;   captureCard, holds the card the player wants to capture with
;	hand, holds the hand of the player
; Algorithm: 
;	1) If the capture card is in the hand, return true
;	2) Otherwise, return false
; Assistance Received: none 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		  
(defun CheckCaptureHand (passedCaptureCard passedHand)

	(Let* ((captureCard passedCaptureCard)
		   (hand passedHand))
		   
	(cond ((eq hand () ) "False")
		  ((eq captureCard (first hand) ) "True")
		  (t (CheckCaptureHand captureCard (rest hand) ) ) ) ) )


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: CheckCapture
; Purpose: To validate if the card the player wants to capture with is one they can capture with
; Parameters:
;	captureCard, holds the card the player wants to capture with
;	hand, holds the hand of the human
;	table, holds the table for the game
; Return Value: Either true if the card can be captured with, or false otherwise
; Local Variables: None
; Algorithm: 
;	1) If the capture card is ok with the table and ok with the player's hand than it returns true
;  	2) Otherwise, returns false
; Assistance Received: none 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		  
(defun CheckCapture ( captureCard hand table )

	(cond (( and (equal (CheckCaptureTable captureCard table) "True")
			(equal (CheckCaptureHand captureCard hand) "True" ) "True" ) )
			
		  (t "False" ) ) )
			
			
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: MakeCapture
; Purpose: To check if the card the player wants to capture with is able to or not
; Parameters:
;	hand, holds the hand of the player
;	table, holds the current table on the board
; Return Value: The capture card the player wants to capture with 
; Local Variables: None
; Algorithm: 
;	1) If the card the player picked is able to be captured with, then it will send the capture card
;	2) Otherwise, return false since it was an invalid card
; Assistance Received: none 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;			
(defun MakeCapture (hand table)

	( print "Enter a card you want to capture with:" )
	
	( Let* (( captureCard ( read ) ))
	
	(cond (( equal ( CheckCapture captureCard hand table ) "True" ) captureCard )
		   ( t (print "You can not capture with that card." ) "False" ) ) ) )
			
			

	  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: HumanMakeMove
; Purpose: To call the proper function based on the move the player wants to make
; Parameters:
;	humanHand, holds the hand of the player
;	passedTable, holds the current table on the board
; Return Value: A list with the player's choice and the card(s) they are using for the move
; Local Variables: 
;   playerMove, holds the move the player wants to make
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
(defun HumanMakeMove ( humanHand passedTable passedMoveCard passedPlayerMove)

	(Let* (( playerMove passedPlayerMove)
	  ( hand humanHand )
	  ( table passedTable )
	  ( moveCard passedMoveCard ))
	  
	  
	(cond (( eq playerMove nil )(HumanMakeMove hand table moveCard (HumanGetMoveChoice) ) ) )
	  
	  
	(cond (( eq playerMove 'b ) 
		  ( print "Builds not implemented" ) (MakeBuild) ( HumanMakeMove hand table () ))
		  (( eq playerMove 'c) 
			(cond ((eq moveCard nil ) (HumanMakeMove hand table (MakeCapture hand table) playerMove ) )
				  (( equal (CheckCapture moveCard hand table) "True" ) ( list playerMove moveCard ) )
				  (( equal (CheckCapture moveCard hand table) "False" ) (HumanMakeMove hand table nil nil ) ) ) )
		  (( eq playerMove 't ) 
				( cond  (( eq moveCard nil ) (HumanMakeMove hand table (MakeTrail hand table) playerMove ) )
						(( equal (CheckTrail moveCard hand) "True" ) ( list playerMove  moveCard ) )
						(( equal (CheckTrail moveCard hand) "False" ) (HumanMakeMove hand table () nil ) )
						( t (HumanMakeMove hand table (MakeTrail hand table) playerMove ) ) ) )) ) )
						
						
						
						
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: CardValue
; Purpose: Get the card value of a card (such as given a card "CX", it will return 10)
; Parameters:
;	card, holds the card that was passed in
; Return Value: The number value of a card
; Local Variables: 
;   stringCard, holds the same card but in string form
; Algorithm: 
;	1) Check which number the card is equal to and return the number value
; Assistance Received: none 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun CardValue ( card )


	(Let* (( stringCard (string card) ) )
	
	
	(cond (( eq (char stringCard 1 ) '#\A) '1 )
		  (( eq (char stringCard 1 ) '#\2) '2 )
		  (( eq (char stringCard 1 ) '#\3) '3 ) 
		  (( eq (char stringCard 1 ) '#\4) '4 ) 
		  (( eq (char stringCard 1 ) '#\5) '5 )
		  (( eq (char stringCard 1 ) '#\6) '6 ) 
		  (( eq (char stringCard 1 ) '#\7) '7 )
		  (( eq (char stringCard 1 ) '#\8) '8 )
		  (( eq (char stringCard 1 ) '#\9) '9 )
		  (( eq (char stringCard 1 ) '#\X) '10)
		  (( eq (char stringCard 1 ) '#\J) '11)
		  (( eq (char stringCard 1 ) '#\Q) '12)
		  (( eq (char stringCard 1 ) '#\K) '13) ) ) )
	
						
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: ComputerMakeTrail
; Purpose: To have the computer trail with their lowest card
; Parameters:
;	passedHand, holds the hand of the computer
;	passedTable, holds the current table on the board
;	passedTrailCard, holds the current card the computer will trail with
; Return Value: A card for the computer to trail with
; Local Variables: 
;   hand, holds the computer's current hand
;	table, holds the current table
;	trailCard, holds the card the computer will trail with
; Algorithm: 
;	1) If the hand is equal to the empty set, then we've gone through all of the computer's hand cards and can return
;	2) If the computer's trail card is greater than the current card in the hand, make that current card the new trail card
;	3) If neither of the above statements were true, reduce the hand size and check for the rest of the cards if one is lower
; Assistance Received: none 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
(defun ComputerMakeTrail ( passedHand passedTable passedTrailCard )

(print passedHand)
(print "Were in computer make trail")


	(Let* (( hand passedHand )
		   ( table passedTable )
		   ( trailCard passedTrailCard ) )
		   
	(cond ((eq hand () ) trailCard )
		  (( < (CardValue (first hand) )  (CardValue trailCard) ) ( ComputerMakeTrail (rest hand) table (first hand) ) )
		  ( t (ComputerMakeTrail (rest hand) table trailCard ) ) ) ) )
		   
		   
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: ComputerGetMoveChoice
; Purpose: To find the best move choice for the computer to use
; Parameters: None
; Return Value: The move that the player should make
; Local Variables: None
; Algorithm: 
;	1) Just return "t" for trailing right now, I don't believe I will have time to implement the other moves
; Assistance Received: none 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ComputerGetMoveChoice () 

't )	
						
						
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: ComputerMakeMove
; Purpose: To remove a card the player trailed with from their hand
; Parameters:
;	passedHand, the current hand of a player
;	passedTrailCard, the trail card to be added to the table
;	passedNewHand, comes in as an empty list and will be used as the new hand for the player
; Return Value: The new table list with the trail card added
; Local Variables:
;	hand, holds the player's current hand
;	trailCard, holds the card the player used to trail with
;	newHand, holds the new hand for the player
; Algorithm: 
;	1) If the current hand is equal to the empty list, return the new hand
;	2) If the current card in hand equals to the trail card, dont add it to the new hand
;	3) If neither of the other statements were true, decrement the hand and add the current card to the new hand
; Assistance Received: none 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;					
(defun ComputerMakeMove ( passedComputerHand passedTable )

	(Let* (( playerMove ( ComputerGetMoveChoice ) )
		   ( computerHand passedComputerHand )
		   ( table passedTable) )
		   
	(cond (( eq playerMove 'b )
		  ( print "Computer is not making a build" ) )
		  (( eq playerMove 'c )
	      ( print "The computer is capturing" ) )
		  (( eq playerMove 't )
			(cond (t (list playerMove ( ComputerMakeTrail computerHand table (string (first computerHand) ) ) ) ) ) ) ) ) )
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: RemoveCaptureCardsFromTable
; Purpose: To remove any captured cards from the table
; Parameters:
;	passedCaptureCard, holds the card the player captured with
;	passedTable, holds the current table on the board
;	passedNewTable, holds the new table list without the previously captured cards
; Return Value: A new list without the captured cards in it
; Local Variables: 
;   captureCard, holds the card the player captured with
;	stringCaptureCard, holds the string version of the capture card
;	table, holds the current table
;	stringFirstTableCard, holds the string version of the first card in the table list
;	newTable, a list containing the new table without the captured cards
; Algorithm: 
;	1) If the table is equal to the empty list, then return the new table list
;  	2) If the value of the capture card equals the value of the current table card were looking at, then we dont add any cards to the table
;	3) If neither of the previous statements were true, then we keep cycling through the table list
; Assistance Received: none 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
(defun RemoveCaptureCardsFromTable ( passedCaptureCard passedTable passedNewTable)

(print passedTable)
(print passedNewTable)


	(Let* ((captureCard passedCaptureCard)
		   (stringCaptureCard (string passedCaptureCard))
		   (table passedTable)
		   (stringFirstTableCard (string (first passedTable) ) )
		   (newTable passedNewTable))
		   
	(cond ((eq table () ) newTable )
		  ((eq (list-length table) 1) (RemoveCaptureCardsFromTable captureCard (rest table) (append table newTable)))
		  ((eq (char stringCaptureCard 1) (char stringFirstTableCard 1) ) (RemoveCaptureCardsFromTable captureCard (rest table) newTable ) )
		  (t (RemoveCaptureCardsFromTable captureCard (rest table) (append (list (first table)) newTable) ) )) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: AddCaptureCardsToPile
; Purpose: To add cards from a capture move and return a new list which is the pile for a player
; Parameters:
;	passedCaptureCard, holds the card that was used for the capture
;	passedTable, holds the current table on the board
;	passedCapturePile, holds the new pile with the captured cards
; Return Value: The new list which has all of the new captured cards added to the pile
; Local Variables: 
;   captureCard, holds the card the player captured with
;	stringCaptureCard, holds the hand of the player
;	table, holds the current table
;	stringFirstTableCard, holds the string version of the first card in the table
;	capturePile, holds the new pile with the captured cards
; Algorithm: 
;	1) If the table is equal to the empty list, add the capture card to the capture pile and return that
;  	2) If the value of the capture card equals the value of the table card, then append the table card to the list for the pile
;	3) If neither of those statements were true, then reduce the table size and keep checking
; Assistance Received: none 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		  
(defun AddCaptureCardsToPile (passedCaptureCard passedTable passedCapturePile)


	(Let* ((captureCard passedCaptureCard)
		   (stringCaptureCard (string passedCaptureCard))
		   (table passedTable)
	       (stringFirstTableCard (string (first passedTable) ) )
		   (capturePile passedCapturePile))
		  
		   
	(cond ((eq table () ) (append (list captureCard) capturePile))
		  ((eq (char stringCaptureCard 1) (char stringFirstTableCard 1) ) (AddCaptureCardsToPile captureCard (rest table) (append (list (first table)) capturePile) ) )
		  (t (AddCaptureCardsToPile captureCard (rest table) capturePile) ) ) ) )
		   
			

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: RemoveTrailFromHand
; Purpose: To remove a card the player trailed with from their hand
; Parameters:
;	passedHand, the current hand of a player
;	passedTrailCard, the trail card to be added to the table
;	passedNewHand, comes in as an empty list and will be used as the new hand for the player
; Return Value: The new table list with the trail card added
; Local Variables:
;	hand, holds the player's current hand
;	trailCard, holds the card the player used to trail with
;	newHand, holds the new hand for the player
; Algorithm: 
;	1) If the current hand is equal to the empty list, return the new hand
;	2) If the current card in hand equals to the trail card, dont add it to the new hand
;	3) If neither of the other statements were true, decrement the hand and add the current card to the new hand
; Assistance Received: none 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun RemoveTrailFromHand (passedHand passedTrailCard passedNewHand)

		(Let* ((hand passedHand)
			   (trailCard passedTrailCard)
			   (newHand passedNewHand))
			   
		
			   
		; If the hand is empty, then we have cycled through everything and can return the new hand
		(cond ((eq hand () ) newHand )
		
			  ; If the current card were looking at is the new card, don't add it to the new hand list
			  ((eq (first hand) trailCard) (RemoveTrailFromHand (rest hand) trailCard newHand) )
			 
			  ; If neither of the either statements were true, add the current card to the new hand and decrement the old hand
			  (t (RemoveTrailFromHand (rest hand) trailCard (append passedNewHand (list (first hand) ) ) ) ) ) ) )
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: AddTrailToTable
; Purpose: To add a trail card to the table in a round
; Parameters:
;	passedTable, the current table in a round
;	passedTrailCard, the trail card to be added to the table
; Return Value: The new table list with the trail card added
; Local Variables: None
; Algorithm: 
;	1) Return the new appended list with the trail card added
; Assistance Received: none 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
(defun AddTrailToTable (passedTable passedTrailCard)

	( append passedTable passedTrailCard ) )	   
	


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: PlayRound
; Purpose: To serve as the main game loop for playing a round
; Parameters:
;	passedDeck, the deck of cards
;	passedHumanHand, the cards for the human hand
;	passedComputerHand, the cards for the computer hand
;	passedTable, the cards on the table
;	passedNextPlayer, the next player who is supposed to go
;	passedFirstGame, either "True" or "False", an indicated for if we need to deal cards to the table or not
; Return Value: None as of right now since it is not finished
; Local Variables: 
;   deck, holds the current deck of the round
;	humanHand, holds the current hand of the human
;	computerHand, holds the current hand of the computer
;	table, holds the cards on the table
;	nextPlayer, holds the next player who is supposed to player
;	firstGame, holds either "True" or "False" indicating if we need to deal cards to the table or not
; Algorithm: 
;	1) 
;  	2)
;	3)
; 	4)
; Assistance Received: none 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun PlayRound ( passedDeck passedHumanHand passedComputerHand passedTable passedHumanPile passedComputerPile passedNextPlayer passedRoundCycle passedPlayerMove&Card passedFirstGame )

	( Let* (( deck passedDeck )
			( humanHand passedHumanHand )
			( computerHand passedComputerHand )
			( table passedTable )
			( humanPile passedHumanPile )
			( computerPile passedComputerPile )
			( nextPlayer passedNextPlayer )
			( roundCycle passedRoundCycle ) 
			( playerMove&Card passedPlayerMove&Card )
			( firstGame passedFirstGame ) )
		
			
			
			
	; If this is a brand new game, we need to deal cards to the player
	( cond (( eq roundCycle 1)
		   ( PlayRound deck (GetFourCards deck 4 ()) computerHand table humanPile computerPile nextPlayer (+ roundCycle 1) () firstGame )) )
		   
	; Then remove the first four cards from the deck
	( cond (( eq roundCycle 2)
		   ( PlayRound ( SubtractCardsFromDeck deck 4) humanHand computerHand table humanPile computerPile nextPlayer (+ roundCycle 1) () firstGame )))
		 
	; Then deal four cards to the computer
	( cond ((eq roundCycle 3)
		   ( PlayRound deck humanHand (GetFourCards deck 4 ()) table humanPile computerPile nextPlayer (+ roundCycle 1) () firstGame )))
		
	; Then remove the first four cards from the deck
	( cond ((eq roundCycle 4)
		   ( PlayRound ( SubtractCardsFromDeck deck 4) humanHand computerHand table humanPile computerPile nextPlayer (+ roundCycle 1) () firstGame )))
	
	; Then deal four cards to the table
	( cond ((eq roundCycle 5)
		(cond ((equal firstGame "True")
		   ( PlayRound deck humanHand computerHand (GetFourCards deck 4 ()) humanPile computerPile nextPlayer (+ roundCycle 1) () firstGame ))) ) )
		   
	; If it is not the first game, then we want to skip adding cards to the table and just increment the round counter
	( cond ((eq roundCycle 5)
		(cond ((equal firstGame "False")
			( PlayRound deck humanHand computerHand table humanPile computerPile nextPlayer (+ roundCycle 1) () firstGame ) ) ) ) )
		
	; Then finally remove the first four cards from the table
	( cond ((eq roundCycle 6)
		(cond ((equal firstGame "True" )
		   ( PlayRound ( SubtractCardsFromDeck deck 4) humanHand computerHand table humanPile computerPile nextPlayer (+ roundCycle 1) () "False" ))) ) )
		   
	; This condition exists if it is not the first time playing in a round and we dont want to deal cards to the table
	( cond ((eq roundCycle 6)
		(cond ((equal firstGame "False" )
			( PlayRound deck humanHand computerHand table humanPile computerPile nextPlayer (+ roundCycle 1) () firstGame) ) ) ) )
			
			
	( cond (( eq roundCycle 7 ) ( print "Table:" ) ( print table ) (print "Hand:") (print humanHand) ( print "Human pile:" ) ( print humanPile )
								(print "Computer Hand:" ) (print computerHand ) ( print "Computer piile:" ) ( print computerPile ) )  )
	
	; If the next player is the human, then they will go...
	( cond (( eq roundCycle 7 ) 
			(cond (( equal nextPlayer "Human" ) (print "It's your turn") ( PlayRound deck humanHand computerHand table humanPile computerPile "Computer" 
												(+ roundCycle 1) (print (HumanMakeMove humanHand table nil (HumanGetMoveChoice) )) firstGame ) ) ) ) )
	
	;Otherwise, then the computer will go...	
	( cond (( eq roundCycle 7 ) 
		(cond (( equal nextPlayer "Computer" ) (print "It's the computer's turn") ( PlayRound deck humanHand computerHand table humanPile computerPile 
												"Human" (+ roundCycle 1) (ComputerMakeMove computerHand table ) firstGame ) ) ) ) )
		
	; Now, if the move the player chose is a trail, we need to add the card to the table...
	( cond (( eq roundCycle 8 )
			(cond (( eq (first playerMove&Card) 't ) ( PlayRound deck humanHand computerHand (AddTrailToTable table (rest playerMove&Card) ) humanPile computerPile nextPlayer
												 (+ roundCycle 1) playerMove&Card firstGame ) ) ) ) )
												 
	; Now, if the move the player chose is a trail, we need to add the card to the table...
	( cond (( eq roundCycle 8 )
			(cond (( eq (first playerMove&Card) 'c ) ( PlayRound deck (RemoveTrailFromHand humanHand (first (rest playerMove&Card) ) () ) computerHand table
													humanPile computerPile nextPlayer (+ roundCycle 1) playerMove&Card firstGame ) ) ) ) )
		

	; If the next player is "Computer", then we know the human just made the last move and must change their hand depending on what they
	; did. In this case, this is checking if the player trailed, and will remove the trail card from their hand
	( cond (( eq roundCycle 9 )
			(cond (( equal nextPlayer "Computer" )
					(cond (( eq (first playerMove&Card) 'T )
												( PlayRound deck (RemoveTrailFromHand humanHand (first (rest playerMove&Card) ) () )
												computerHand table humanPile computerPile nextPlayer (+ roundCycle 1)
												playerMove&Card firstGame ) ) ) ) ) ) )
												
												
	; If the next player is "Computer", then we know the human just made the last move and must change their hand depending on what they
	; did. In this case, this is checking if the player captured and adjusting the cards 
	( cond (( eq roundCycle 9 )
			(cond (( equal nextPlayer "Computer" )
					(cond (( eq (first playerMove&Card) 'C )
												( PlayRound deck humanHand
												computerHand (RemoveCaptureCardsFromTable (first (rest playerMove&Card) ) table () )  (AddCaptureCardsToPile (first (rest playerMove&Card ) ) table humanPile) computerPile nextPlayer (+ roundCycle 1)
												playerMove&Card firstGame ) ) ) ) ) ) )
												
	; If the next player is "Human", then we know the computer just made the last move and must change their hand depending on what they
	; did. In this case, this is checking if the player trailed, and will remove the trail card from their hand
	( cond (( eq roundCycle 9 )
			(cond (( equal nextPlayer "Human" )
					(cond (( eq (first playerMove&Card) 'T ) 
												( PlayRound deck humanHand (RemoveTrailFromHand computerHand (first (rest playerMove&Card) ) () )
												table humanPile computerPile nextPlayer (+ roundCycle 1)
												playerMove&Card firstGame ) ) ) ) ) ) )
												
												
	; After a player has made a move and the player's hand and table is properly adjusted, we need to check if the player's hands
	; are empty and if we need to deal move cards
	( cond (( eq roundCycle 10 )
			(cond ((eq humanHand ())
				(cond ((eq computerHand ()) 
					(cond (( eq deck ()) (list humanPile computerPile ) )
					( t (PlayRound deck humanHand computerHand table humanPile computerPile nextPlayer 1 playerMove&Card firstGame ) ) ) ) ) ) ) ) )

	( cond (( eq roundCycle 10 ) (print "And then we go back here") (PlayRound deck humanHand computerHand table humanPile computerPile nextPlayer 7 playerMove&Card firstGame) ) ) ) )
	
	;( cond (( eq roundCycle 12 ) (print "round 11")  (list humanPile computerPile ) ) ) ) )
	
	
	
	
				 	   
	;( PlayRound deck humanHand computerHand table nextPlayer (+ roundCycle 1) () playerMove playerCard ) ) )
	


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: TenDiamonds
; Purpose: To determine if a player has the ten of diamonds that way they can get 2 points
; Parameters:
;	passedPile, the pile for a player
;	passedPlayerScore, the current player's score
; Return Value: the score of the player either with the added 2 points or not
; Local Variables: 
; 	pile, holds the player's pile
;	playerScore, a current player's score
; Algorithm: 
;	1) If the player's pile is equal to the empty list, then return the player's score
;  	2) If the first element in the player's pile is equal to the ten of diamonds, call TenDiamonds again but increment the score by 2
;	3) If neither of the previous situations were true, recursively call the function again with reducing the pile by one
; Assistance Received: none 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun TenDiamonds ( passedPile passedPlayerScore )

	(Let* (( pile passedPile )
		  ( playerScore passedPlayerScore ))
	
	; If the current card were looking at is the empty set, then return the score
	(cond ((eq pile () ) playerScore )
	
		  ; If the card were looking at is the ten of diamonds, then call the function again and increment the score by 2
		  (( eq (first pile) 'DX ) (TenDiamonds (rest pile) ( + playerScore 2) ) )
		  
		  ; If it was neither of those things, then recursively call the function with the rest of the player pile
		  ( t (TenDiamonds ( rest pile ) playerScore ) ) ) ) )
		 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: TwoSpades
; Purpose: To check if a player has the two of spades and increment their account
; Parameters: 
;	passedPile, a player's pile
;	passedPlayerScore, a player's score
; Return Value: An updated score of the player with either plus 1 or plus nothing
; Local Variables: 
;	pile, holds the player's pile
;	playerScore, holds the player's current score
; Algorithm: 
;	1) If the player's pile is an empty list, return the updated player score
;  	2) If the first element in the player's pile is the two of spades, increment the player's score by 1
;	3) If neither of those evaluated to true, recursively call TwoSpades and deduct a card from the player's pile
; Assistance Received: none 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		 
( defun TwoSpades ( passedPile passedPlayerScore )

	(Let* (( pile passedPile )
		  ( playerScore passedPlayerScore ))
		  
	(cond  (( eq pile () ) playerScore )
	
		   (( eq (first pile ) 'S2 ) (TwoSpades ( rest pile ) (+ playerScore 1) ) )
		   (( t (TwoSpades ( rest pile ) playerScore ) ) ) ) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: CountAces
; Purpose: To count the aces from a player's pile and increment a player's score
; Parameters:
;	passedPile, a player's pile
;	passedPlayerScore, a player's score
; Return Value: An updated version of the player's score incremented by how many aces they had
; Local Variables: 
;	pile, holds the player's pile
;	playerScore, holds the player's score
; Algorithm: 
;	1) If the pile is equal to the empty set, return the player's score
;  	2) If the first card in the player's pile is equal to ace of club, recursively call and increment the score
;	3) If the first card in the player's pile is equal to ace of diamonds, recursively call and increment the score
; 	4) If the first card in the player's pile is equal to ace of hearts, recursively call and increment the score
;	5) If the first card in the player's pile is equal to ace of spades, recursively call and increment the score
;	6) If none of those options were true, recursively call CountAces and decrement a card from the player's pile
; Assistance Received: none 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		   
( defun CountAces ( passedPile passedPlayerScore )

	(Let* (( pile passedPile )
		  ( playerScore passedPlayerScore ))
		  
	(cond (( eq pile () ) playerScore )
		  (( eq (first pile ) 'CA ) ( CountAces ( rest pile ) ( + playerScore 1 ) ) )
		  (( eq (first pile ) 'DA ) ( CountAces ( rest pile ) ( + playerScore 1 ) ) )
		  (( eq (first pile ) 'HA ) ( CountAces ( rest pile ) ( + playerScore 1 ) ) )
		  (( eq (first pile ) 'SA ) ( CountAces ( rest pile ) ( + playerScore 1 ) ) )
		  (t (CountAces ( rest pile ) playerScore ) ) ) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: CalculateScore
; Purpose: To calculate a player's score and check who has the ten of diamonds, 2 of spades, and how many aces
; Parameters:
;	passedPile, a player's pile
;	passedPlayerScore, a player's score
;	passedScoreCounter, a count for keeping track of what to check next
; Return Value: A player's score after checking the different things that can get them points
; Local Variables: 
;	playerPile, holds the player's pile
;	playerScore, holds the player's score
;	scoreCounter, holds the counter for what to do next
; Algorithm: 
;	1) If score counter is equal to 1, check if the player has the ten of diamonds
;  	2) If score counter is equal to 2, check if the player has the two of spades
;	3) If score counter is equal to 3, check for how many aces a player has
; 	4) If score counter is equal to 4, return with the new score counter for that player
; Assistance Received: none 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	  
(defun CalculateScore ( passedPlayerPile passed playerScore passedScoreCounter)

	(Let* (( playerPile passedPlayerPile )
		  ( playerScore passedPlayerScore )
		  ( scoreCounter passedScoreCounter ))
	
	; Look for the 10 of diamonds
	(cond (( eq scoreCounter 1 ) ( CalculateScore playerPile ( TenDiamonds playerPile 0 ) ( + scoreCounter 1 ) ) )
		  (( eq scoreCounter 2 ) (CalculateScore playerPile ( TwoSpades playerPile 0 ) ( + scoreCounter 1 ) ) )
		  (( eq scoreCounter 3 ) (CalculateScore playerPile ( CountAces playerPile ) ( + scoreCounter 1 ) ) )
		  (( eq scoreCounter 4 ) playerScore ) ) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: CheckWhoHasMoreSpades
; Purpose: To figure out which player has more spades and give them a point
; Parameters:
;	passedPlayerPiles, both of the player's piles
;	passedHumanNumSpades, a player's number of spades
;	passedComputerNumSpades, a computer's number of spades
;	passedCounter, a count for knowing where I am at in checking the number of spades
; Return Value: Returns "Human" if they had more spades, "Computer" if they had more, or "Neither" if they were even
; Local Variables: 
;	playerPiles, holds the player's piles
;	humanNumSpades, holds the number of spades the human has
;	computerNumSpades, holds the number of spades the computer has
;	counter, just a counter for keeping track of what to do next
; Algorithm: 
;	1) WORK IN PROGRESS
; Assistance Received: none 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun CheckWhoHasMoreSpades ( passedPlayerPiles passedHumanNumSpades passedComputerNumSpades passedCounter)

	(Let* (( playerPiles passedPlayerPiles )
		  ( humanNumSpades passedHumanNumSpades )
		  ( computerNumSpades passedComputerNumSpades )
		  ( counter passedCounter ) ) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: CountCards
; Purpose: To count the number of cards a player has
; Parameters:
;	passedPlayerPile, a player's pile
;	passedCardCount, the number of cards for the player
; Return Value: The number of cards a player has
; Local Variables: 
;	playerPile, holds the player's pile
;	cardCount, holds the count of a cards a player has
; Algorithm: 
;	1) If the players pile is the empty list, then return the count of the cards
;  	2) Otherwise, recursively call CountCards and decrement the playerPile and increment the card count by 1
; Assistance Received: none 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun CountCards ( passedPlayerPile passedCardCount )

	(Let* (( playerPile passedPlayerPile )
		  ( cardCount passedCardCount ))
	
	(cond ((eq playerPile ()) cardCount )
		  (t (CountCards ( rest playerPile ) ( + cardCount 1 ) ) ) ) ) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: CheckWhoHasMoreCards
; Purpose: To check which player has more cards
; Parameters:
;	passedPlayerPiles, the player's pile
;	passedHumanNumCards, the number of cards for the human player
;	passedComputerNumCards, the number of cards for the computer player
;	passedCounter, a counter to keep track of where we are at in the function
; Return Value: The player with the most cards, either "Human", "Computer", or "Neither"
; Local Variables: 
;	playerPiles, holds the player's pile
;	humanNumCards, holds the count of a humans cards
;	computerNumCards, holds the count of a computers cards
;	counter, holds the counter of where the function is at
; Algorithm: 
;	1) If the counter is equal to 1, get the number of cards the human has
;  	2) If the counter is equal to 2, get the number of cards the computer has
;	3) Then check if the human has more cards that the computer, the computer has more cards
;	than the human, or if they have the same number of cards, and return that
; Assistance Received: none 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun CheckWhoHasMoreCards (passedPlayerPiles passedHumanNumCards passedComputerNumCards passedCounter)

	(Let* (( playerPiles passedPlayerPiles )
		  ( humanNumCards passedHumanNumCards )
		  ( computerNumCards passedComputerNumCards )
		  ( counter passedCounter ))
		  
	(cond ((eq counter 1) (CheckWhoHasMoreCards playerPiles ( CountCards ( first playerPiles ) 0 ) computerNumCards ( + counter 1 ) ) )
		  (( eq counter 2 ) ( CheckWhoHasMoreCards playerPiles humanNumCards ( CountCards ( rest playerPiles ) 0 ) ( + counter 1 ) ) )
		  (( < humanNumCards computerNumCards ) "Human" )
		  (( > humanNumCards computerNumCards ) "Computer" )
		  (( eq humanNumCards computerNumCards ) "Neither" ) ) ) )
		  
		  
		  
		  

		  


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: PlayTournament
; Purpose: To play through a tournament of casino
; Parameters:
;	passedHumanScore, the score for the human player
;	passedComputerScore, the score for the computer player
;	passedroundCounter, the counter for how many rounds there has been
;	passedPile, contains two lists of the players piles, the first is the human, the second is the computer
;	passedTallyScore, kind of like a boolean value, if the scores need to be tallied, this will be "True", otherwise "False"
;	passedTallyScoreCounter, counter for knowing which function to call for calculating the player's points
; Return Value: None
; Local Variables: 
;   humanScore, holds the human's score for the tournament
;	computerScore, holds the computer's score for the tournament
;	roundCounter, holds the current round they are on
;	pile, holds the piles for each of the players in a lists with two sub lists, the first is the human's, the second is the computer's
; 	tallyScore, holds either "True" or "False" if the scores need to be tallied
;	tallyScoreCounter, keeps track of which function to call to keep track of points
; Algorithm: 
;	1) 
;  	2)
;	3)
; 	4)
; Assistance Received: none 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun PlayTournament ( passedHumanScore passedComputerScore passedRoundCounter passedPile passedTallyScore passedTallyScoreCounter passedMoreCardPlayer passedMoreSpadePlayer )

	(Let* (( humanScore passedHumanScore )
	   ( computerScore passedComputerScore )
	   ( roundCounter passedRoundCounter )
	   ( pile passedPile )
	   ( tallyScore passedTallyScore )
	   (tallyScoreCounter passedTallyScoreCounter)
	   ( moreCardPlayer passedMoreCardPlayer )
	   ( moreSpadePlayer passedMoreSpadePlayer ))
	   
	   
	   
	
	; If the human and computer scores are the same and the human's score is greater than 21, then we have a tie!
	(cond (( eq tallyScore "True")
			(cond (( eq tallyScoreCounter 1) (PlayTournament humanScore computerScore roundCounter pile tallyScore ( + tallyScoreCounter 1) 
				(CheckWhoHasMoreCards pile 0 0 1 ) "Neither" ) ) ) ) ) 
				
				
			
		  ; After getting the player with the most cards, we now need to check if that player is the human and add 3 points to their score
	(cond (( eq tallyScore "True" )
			(cond (( eq moreCardPlayer "Human" ) (PlayTournament ( + humanScore 3 ) computerScore roundCounter pile tallyScore tallyScoreCounter "Neither" "Neither") ) ) ) )
			
		  ; After getting the player with the most cards, now we must check if the computer had the most cards and add 3 points of necessary
	(cond (( eq tallyScore "True" )
			(cond (( eq moreCardPlayer "Computer" ) (PlayTournament humanScore ( + computerScore 3 ) roundCounter pile tallyScore tallyScoreCounter "Neither" "Neither" ) ) ) ) )
				
		  ; Now we need to get the player who has the most spades
	(cond (( eq tallyScore "True" )
			(cond (( eq tallyScoreCounter 2 ) ( PlayTournament humanScore computerScore roundCounter pile tallyScore 
				( + tallyScoreCounter 1 ) "Neither" (CheckWhoHasMoreSpades pile 0 0 1 ) ) ) ) ) )
				
		  ; If the human had more spades, give them a point...
	(cond (( eq tallyScore "True" )
			(cond (( eq moreSpadePlayer "Human" ) (PlayTournament ( + humanScore 1 ) computerScore roundCounter pile tallyScore tallyScoreCounter "Neither" "Neither" ) ) ) ) )
				
		  ; If the computer had more spades, give them a point
	(cond (( eq tallyScore "True" )
			(cond (( eq moreSpadePlayer "Computer" ) (PlayTournament humanScore ( + computerScore 1 ) roundCounter pile tallyScore tallyScoreCounter
				"Neither" "Neither") ) ) ) )
			
		  ; Now we need to calculate the rest of the ways to earn points for the human...
	(cond (( eq tallyScore "True" )
			(cond (( eq tallyScoreCounter 3 ) ( PlayTournament (+ humanScore (CalculateScore (first pile) 0 1)) computerScore roundCounter pile tallyScore
				(+ tallyScoreCounter 1) "Neither" "Neither") ) ) ) )
				
		  ; Now we need to calculate the rest of the ways to earn points for the computer...
	(cond (( eq tallyScore "True" )
			(cond (( eq tallyScoreCounter 4 ) (PlayTournament humanScore (+ computerScore (CalculateScore (rest pile) 0 1)) roundCounter pile "False"
				(+ tallyScoreCounter 1) "Neither" "Neither") ) ) ) )
				
	
		  ; Check if it was a tie between the players
	(cond ((eq passedHumanScore computerScore )
			(cond (( >= passedHumanScore 21 ) (print "It's a tie!") ( return nil ) ) ) )
			
				
		  ; If the human score is greater or equal to 21 and the computer's score is less than 21, then the human won!
		  (( >= humanScore 21 ) 
			(cond (( < computerScore 21 ) (print "You Won!") ( return nil ) ) ) )
	
		  ; If the computer score is greater or equal to 21 and the human's score is less than 21, then the computer won!
		  (( >= computerScore 21) 
			(cond (( < humanScore 21) (print "The computer won.") ( return nil ) ) ) ) )
				
		  ; If no one won the game yet, play a round, increment the score, and get the player's piles from the round
		  (PlayTournament humanScore computerScore (+ roundCounter 1) (PlayRound (ActualDeck (loadDeck)) () () () () () (FirstPlayer) 1 () "True" ) "True"  1 "Neither" "Neither" ) ) )
		  
		
	

;(print (open "C:\\Users\\ncockcro\\AppData\\Roaming\\Microsoft\\Windows\\test.txt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: OpeningMenu
; Purpose: To serve as the opening menu when a player first boots up the game
; Parameters: None
; Return Value: Depeninding on the option the user picks, it will call the appropriate function
; Local Variables:  None
; Algorithm: 
;	1) If the option is 1, load a game of Casino
;  	2) If the option is 2, play a new game of Casino
;	3) If the option is 3, exit the program
; 	4) If the option was neither of those, call the function again and ask the user
; Assistance Received: none 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun OpeningMenu () 

(print "Enter '1' to load a game")
(print "Enter '2' to play a new game")
(print "Enter '3' to exit")

	(Let* ((option (read)))

	(cond ((eq option 1) option)
		  ((eq option 2) (PlayTournament 0 0 1 (PlayRound (ActualDeck (loadDeck)) "True" () () () () (FirstPlayer) 1 () "True" ) "True" 1 "Neither" "Neither"))
		  ((eq option 3) (return))
		  (t (print "Incorrect menu option") (OpeningMenu)))))
	


		  
		  
(print (OpeningMenu))

	

	
		  



