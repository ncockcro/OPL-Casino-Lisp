;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;    * Name:  Nicholas Cockcroft                                 ;
;    * Project:  Project #2, Lisp Project        			     ;
;    * Class:  Organization of Programming Languages CMPS 366-01 ;
;    * Date:  October 23, 2018                        		     ;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print "Welcome to Casino!")
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: CoinToss
; Purpose: To prompt the user for a coin they want to pick to see who plays first 
; Parameters: None
; Return Value: 1 for heads or 2 for tails 
; Local Variables: 
;   headsOrTails, holds either 1 or 2, depending on what the user typed in
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

	; If the user typed 1, that is a valid option
	(cond ( ( eq headsOrTails '1	) 
				headsOrTails )
				
		  ; If the user typed 2, that is also a valid option
		  ( ( eq headsOrTails '2) 
				HeadsOrTails )
				
		  ; Otherwise, throw an error and recursively call this function
		  (t 
		  (print "Invalid choice. Pick either '1' for heads or '2' for tails.") (CoinToss) ) ) ) )


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

	; If the user's guess and the random coin is equal, then they guessed correct and we will return true
	(cond ( ( eq coin userGuess ) 
		  ( print "Yes, that was correct! You go first." ) "True" )
		  
		  ; Otherwise, the user was incorrect and we return false
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
'( CA DA HA SA C2 D2 H2 S2 C3 D3 H3 S3 C4 D4 H4 S4 C5 D5 H5 S5 C6 D6 H6 S6 C7 D7 H7 S7 
C8 D8 H8 S8 C9 D9 H9 S9 CX DX HX SX CJ DJ HJ SJ CQ DQ HQ SQ CK DK HK SK) )


(defun Randomize (randList randNum)

		; If the length of the random numbers list is 52, then return the random list because it is complete with 52 cards
		(cond ((eq (list-length randList) 52 ) randList)
		
			  ; If (CheckIfContains) is true, then that means the random number was already picked and we will recursively call (Randomize)
			  ;	again until we get a different random number
			  ((equal (CheckIfContains randList randNum) "True") (Randomize randList (random 52)))
			  
			  ; If neither of the other options was true, then we just simply need to add the current random number
			  ; to the list of random numbers and keeo generating more until we hit 52 random numbers
			  ((< (list-length randList) 52 ) (Randomize (append (list randNum) randList) (random 52) ))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: CheckIfContains
; Purpose: Checks if a list of numbers already contains a certain number
; Parameters:
;	randList, a list of random numbers
;	randNum, a number to see if it is in the list of random numbers
; Return Value: "True" if the number is in the list, "False" otherwise
; Local Variables: None
; Algorithm: 
;	1) If the list is empty then that means we didn't find it and return "False"
;  	2) If the number is equal to the first element in the list, then return "True"
;	3) If neither of the other statements were true, then keep cycling through the list of random numbers
; Assistance Received: none 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;			  
(defun CheckIfContains (randList randNum)

	; If the list is empty, then everything has been cycled through and we didnt find the card, return "False"
	(cond ((eq randList () ) "False")
	
		  ; If the number is equal to the first number in the list, return true because we found the number
		  ((eq (first randList) randNum ) "True" )
		  
		  ; Otherwise, keep cycling through the list of random numbers
		  (t (CheckIfContains (rest randList) randNum))))
		  
		  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: ShuffleCards
; Purpose: Take the random list previously generated and use it to shuffle the list of cards that were in order
; Parameters:
;	randList, the list of random numbers
;	cardList, the list of cards in order
;	shuffledList, the new list that will be genersted with cards in the random order
; Return Value: The new list of cards in random order
; Local Variables: None
; Algorithm: 
;	1) If the random list is empty, return the new shuffled list of cards
;  	2) Otherwise, recursively call this function again with decrementing the list of random numbers, passed the list of cards again,
;	and adding the card at the random number index to the new list of shuffled cards
; Assistance Received: none 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ShuffleCards (randList cardList shuffledList)

	(cond ((eq randList () ) ShuffledList )
		  (t (ShuffleCards (rest randList) cardList (append (list (nth (first randList) cardList)) shuffledList)))))
		 


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
(defun CheckTrail (playerCard playerHand table)

	(cond ((equal (CheckOtherCardsForCapture playerHand table "False") "True") "False")
		  (( eq playerCard  (first playerHand )  )
			"True" )
		  (( eq playerHand () )
			"False")
		  (( CheckTrail playerCard ( rest playerHand ) table )) ) )
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: CheckCurrentCard
; Purpose: To check if a card has the same value as any of the cards on the table
; Parameters:
;	card, the current card we are comparing with
;	hand, the player's hand
; Return Value: "True" or "False if the player can capture
; Local Variables: None
; Algorithm: 
;	1) If the table is empty, return "False"
;  	2) If the value of the card is equal to the current card in the table were looking at, return "True"
;	3) Otherwise, recursively call this function with the rest of the cards from the tab;e and check the rest
; Assistance Received: none 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
(defun CheckCurrentCard (card table)

	(cond ((eq table () ) "False")
		  ((eq (char (string card) 1) (char (string(first table)) 1) ) "True")
		  (t (CheckCurrentCard card (rest table)))) )
		  
		  

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: CheckOtherCardsForCapture
; Purpose: To check if any cards in the player's hand can be used to capture a card on the table
; Parameters:
;	hand, the player's hand
;	table, the cards on the table
;	canCapture, true or false value for if the player can capture
; Return Value: "True" or "False if the player can capture
; Local Variables: 
; 	trailCard, holds the card the player wants to trail with
; Algorithm: 
;	1) If the player's hand is empty, then we've cycled through everything and the player cant capture
;  	2) If the paramet canCapture was true then return true
;	3) Otherwise, recursively call this function with the rest of the cards from the hand and check the rest
; Assistance Received: none 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		  
(defun CheckOtherCardsForCapture (hand table canCapture)

	(cond ((equal canCapture "True") "True")
	
		   ((eq hand () ) "False" )
	
			
		  (t (CheckOtherCardsForCapture (rest hand) table (CheckCurrentCard (first hand) table)))))
			
	

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
	
	; If the card the player wants to trail with is able to be used for a capture, then output that they can not trail with it
	(cond (( equal ( CheckOtherCardsForCapture hand table "False" ) "True" ) (print "You can not trail with that card because there is a card on the table with thesame value") "False" ) )

	; If the player is able to trail with the card they selected, then return the trail card
	(cond (( equal ( CheckTrail trailCard hand table) "True" )
			trailCard )
			
			; Otherwise, just print that they cant trail with that card
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
	  
	  
	; If the player hasn't picked a move, ask them what move they want to make
	(cond (( eq playerMove nil )(HumanMakeMove hand table moveCard (HumanGetMoveChoice) ) ) )
	  
	; If the player wants to build, recursively call and ask for a different move
	(cond (( eq playerMove 'b ) 
		  ( print "Builds not implemented" ) ( HumanMakeMove hand table nil (HumanGetMoveChoice) ))
		  (( eq playerMove 'c) 
			(cond ((eq moveCard nil ) (HumanMakeMove hand table (MakeCapture hand table) playerMove ) )
				  (( equal (CheckCapture moveCard hand table) "True" ) ( list playerMove moveCard ) )
				  (( equal (CheckCapture moveCard hand table) "False" ) (HumanMakeMove hand table nil (HumanGetMoveChoice) ) ) ) )
		  (( eq playerMove 't ) 
				( cond  (( eq moveCard nil ) (HumanMakeMove hand table (MakeTrail hand table) playerMove ) )
						(( equal (CheckTrail moveCard hand table) "True" ) ( list playerMove  moveCard ) )
						(( equal (CheckTrail moveCard hand table) "False" ) (HumanMakeMove hand table nil (HumanGetMoveChoice) ) )
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


	(Let* (( hand passedHand )
		   ( table passedTable )
		   ( trailCard passedTrailCard ) )
		   
	; If the hand is empty, somtimes the particular card can be a string so if it is, then I am converting it to a regular
	; atom so that it can be properly worked with
	(cond ((eq hand () ) (print "Computer is trailing with:") (print trailCard) (cond ((eq (stringp trailCard) 't) (intern trailCard )) 
								(t trailCard) )
	)
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



	(Let* ((captureCard passedCaptureCard)
		   (stringCaptureCard (string passedCaptureCard))
		   (table passedTable)
		   (stringFirstTableCard (string (first passedTable) ) )
		   (newTable passedNewTable))
		   
	(cond ((eq table () ) newTable )
		  ((eq (char stringCaptureCard 1) (char stringFirstTableCard 1) ) (RemoveCaptureCardsFromTable captureCard (rest table) newTable ) )
		  ((eq (list-length table) 1) (RemoveCaptureCardsFromTable captureCard (rest table) (append table newTable)))
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
; Function Name: SaveGame
; Purpose: To write the contents of the game to a text file
; Parameters:
;	passedRound, the current round
;	passedComputerScore, the computer's score
;	passedComputerHand, the computer's hand
;	passedComputerPile, the computer's pile
;	passedHumanScore, the human's pile
;	passedHumanHand, the human's hand
;	passedHumanPile, the human's pile
;	passedTable, the table of the board
;	passedLastCapture, the player that captured last
;	passedDeck, the current deck
;	passedNextPlayer, the next player who is supposed to play
; Return Value: None, it ends the game
; Local Variables: None
; Algorithm: 
;	1) Open the output file
;	2) Write all of the game's content to the text file
;	3) Close the file and exit the program
; Assistance Received: none 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun SaveGame (passedRound passedComputerScore passedComputerHand passedComputerPile passedHumanScore passedHumanHand passedHumanPile passedTable
					passedLastCapture passedDeck passedNextPlayer)

	(Let* ((outputFile (open "Word.txt" 
							:direction :output )))
							
	(write-line "(" outputFile)
	(write-line "      ; Round:" outputFile)
	(format outputFile "      ")
	(write-line (write-to-string passedRound) outputFile)
	(write-line "   " outputFile)
	
	(write-line "      ; Computer Score:" outputFile)
	(format outputFile "      ")
	(write-line (write-to-string passedComputerScore) outputFile)
	(write-line "   " outputFile)
	
	(write-line "      ; Computer Hand:" outputFile)
	(format outputFile "      ")
	(write-line (write-to-string passedComputerHand) outputFile)
	(write-line "   " outputFile)
	
	(write-line "      ; Computer Pile:" outputFile)
	(format outputFile "      ")
	(write-line (write-to-string passedComputerPile) outputFile)
	(write-line "   " outputFile)
	
	(write-line "      ; Human Score:" outputFile)
	(format outputFile "      ")
	(write-line (write-to-string passedHumanScore) outputFile)
	(write-line "   " outputFile)
	
	(write-line "      ; Human Hand:" outputFile)
	(format outputFile "      ")
	(write-line (write-to-string passedHumanHand) outputFile)
	(write-line "   " outputFile)
	
	(write-line "      ; Human Pile:" outputFile)
	(format outputFile "      ")
	(write-line (write-to-string passedHumanPile) outputFile)
	(write-line "   " outputFile)
	
	(write-line "      ; Table:" outputFile)
	(format outputFile "      ")
	(write-line (write-to-string passedTable) outputFile)
	(write-line "   " outputFile)
	
	(write-line "      ; Last Capturer:" outputFile)
	(format outputFile "      ")
	(write-line (write-to-string passedLastCapture) outputFile)
	(write-line "   " outputFile)
	
	(write-line "      ; Deck:" outputFile)
	(format outputFile "      ")
	(write-line (write-to-string passedDeck) outputFile)
	(write-line "   " outputFile)
	
	(write-line "      ; Next Player:" outputFile)
	(format outputFile "      ")
	(write-line (write-to-string passedNextPlayer) outputFile)
	(write-line "   " outputFile)
	
	(write-line ")" outputFile)
	
	(close outputFile)
	(return nil)) )

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: MoveMainMenu
; Purpose: To be displayed each time before each user makes a move
; Parameters:
;	passedTable, the current table in a round
;	passedTrailCard, the trail card to be added to the tablepassedRound, the current round
;	passedComputerScore, the computer's score
;	passedComputerHand, the computer's hand
;	passedComputerPile, the computer's pile
;	passedHumanScore, the human's pile
;	passedHumanHand, the human's hand
;	passedHumanPile, the human's pile
;	passedTable, the table of the board
;	passedLastCapture, the player that captured last
;	passedDeck, the current deck
;	passedNextPlayer, the next player who is supposed to play
; Return Value: The new table list with the trail card added
; Local Variables: None
; Algorithm: 
;	1) If the user types 1, save the game in its current statements
;	2) If they press 2, dont do anything, the move will be handled by the next function after MoveMainMenu
;	3) If they type 3, end the program
; Assistance Received: none 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun MoveMainMenu (passedRound passedComputerScore passedComputerHand passedComputerPile passedHumanScore passedHumanHand passedHumanPile passedTable
					passedLastCapture passedDeck passedNextPlayer)

	(print "Enter '1' to save the game")
	(print "Enter '2' to make a move")
	(print "Enter '3' to exit")

	(Let* ((move (read)))
	
	(cond ((eq move '1) (SaveGame passedRound passedComputerScore passedComputerHand passedComputerPile passedHumanScore passedHumanHand passedHumanPile passedTable
					passedLastCapture passedDeck passedNextPlayer))
		  ((eq move '2) )
		  ((eq move '3) (return nil))
		  (t (MoveMainMenu passedRound passedComputerScore passedComputerHand passedComputerPile passedHumanScore passedHumanHand passedHumanPile passedTable
					passedLastCapture passedDeck passedNextPlayer)))))
		  
		  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: AddTableCardsToPile
; Purpose: At the end of a round, we need to add the table cards to the player who captured last
; Parameters:
;	table, the current table for the round
;	pile, a player's pile of cards
; Return Value: The new pile of cards with all the cards from the table added to it
; Local Variables: None
; Algorithm: 
;	1) If the table is empty, return the pile
;  	2) Otherwise, decrement the table and add the first card from the table to the pile
; Assistance Received: none 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;		  
(defun AddTableCardsToPile (table pile)

	(cond ((eq table ()) pile)
		  (t (AddTableCardsToPile (rest table) (append (list (first table)) pile)))))
		  
	


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: PlayRound
; Purpose: To serve as the main game loop for playing a round
; Parameters:
;	passedDeck, the deck of cards
;	passedHumanHand, the cards for the human hand
;	passedComputerHand, the cards for the computer hand
;	passedTable, the cards on the table
;	passedNextPlayer, the next player who is supposed to go
;	passedRoundCycle, a counter for knowing what to do next in the round
;	passedPlayerMove&Card, holds the player's move and the card they used for the move
;	passedFirstGame, either "True" or "False", an indicated for if we need to deal cards to the table or not
;	passedLastCapturePlayer, the player that captured last
;	passedCurrentRound, the current round of the tournament, stored for saving feature
;	passedHumanScore, the score for the human, stored for saving feature
;	passedComputerScore, the score for the computer, stored for saving feature
; Return Value: A list containing, the player who captured last, the human's pile, and the computer's pile, in that order
; Local Variables: 
;   deck, holds the current deck of the round
;	humanHand, holds the current hand of the human
;	computerHand, holds the current hand of the computer
;	table, holds the cards on the table
;	nextPlayer, holds the next player who is supposed to player
;	roundCycle, a counter for knowing which step to do next in the round
;	playerMove&Card, holds the player's move choice and card they used, a lit
;	firstGame, holds either "True" or "False" indicating if we need to deal cards to the table or not
;	lastCapturePlayer, holds the player that captured last
;	currentRound, holds the current round of the tournament
;	humanScore, holds the human's score
;	computerScore, holds the computer's score
; Algorithm: 
;	1) First, we need to deal 4 cards to the human
;  	2) Then, Remove those four cards from the deck
;	3) Then, deal 4 cards to the computer
; 	4) Then, remove those four cards from the deck
;	5) Then, if it is a new round, we need to deal 4 cards to the table
;	6) And also need to remove those 4 cards from the deck
;	7) Then, we output the table, hands, piles, and deck, and propt the next player to make a move
;	8) After they make a move, the next two steps are for adjusting the table, player's hand, and piles depending on the move they made
;	9) Then we check if both of the player's hand are empty and if the deck is empty. If just the player's hands are empty but not the
;	deck, we deal more cards. If everything is empty, we end the round. If none of the things are empty, we go back to step 7 for the next
;	player to go
; Assistance Received: none 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun PlayRound ( passedDeck passedHumanHand passedComputerHand passedTable passedHumanPile passedComputerPile passedNextPlayer
				   passedRoundCycle passedPlayerMove&Card passedFirstGame passedLastCapturePlayer passedCurrentRound passedHumanScore passedComputerScore)

	( Let* (( deck passedDeck )
			( humanHand passedHumanHand )
			( computerHand passedComputerHand )
			( table passedTable )
			( humanPile passedHumanPile )
			( computerPile passedComputerPile )
			( nextPlayer passedNextPlayer )
			( roundCycle passedRoundCycle ) 
			( playerMove&Card passedPlayerMove&Card )
			( firstGame passedFirstGame )
			( lastCapturePlayer passedLastCapturePlayer )
			( currentRound passedCurrentRound )
			( humanScore passedHumanScore )
			( computerScore passedComputerScore ) )
			
			
	; If this is a brand new game, we need to deal cards to the player
	( cond (( eq roundCycle 1)
		   ( PlayRound deck (GetFourCards deck 4 ()) computerHand table humanPile computerPile nextPlayer 
		   (+ roundCycle 1) () firstGame lastCapturePlayer currentRound humanScore computerScore )) )
		   
	; Then remove the first four cards from the deck
	( cond (( eq roundCycle 2)
		   ( PlayRound ( SubtractCardsFromDeck deck 4) humanHand computerHand table humanPile computerPile nextPlayer 
		   (+ roundCycle 1) () firstGame lastCapturePlayer currentRound humanScore computerScore )))
		 
	; Then deal four cards to the computer
	( cond ((eq roundCycle 3)
		   ( PlayRound deck humanHand (GetFourCards deck 4 ()) table humanPile computerPile nextPlayer 
		   (+ roundCycle 1) () firstGame lastCapturePlayer currentRound humanScore computerScore )))
		
	; Then remove the first four cards from the deck
	( cond ((eq roundCycle 4)
		   ( PlayRound ( SubtractCardsFromDeck deck 4) humanHand computerHand table humanPile computerPile nextPlayer 
		   (+ roundCycle 1) () firstGame lastCapturePlayer currentRound humanScore computerScore )))
	
	; Then deal four cards to the table
	( cond ((eq roundCycle 5)
		(cond ((equal firstGame "True")
		   ( PlayRound deck humanHand computerHand (GetFourCards deck 4 ()) humanPile computerPile nextPlayer 
		   (+ roundCycle 1) () firstGame lastCapturePlayer currentRound humanScore computerScore ))) ) )
		   
	; If it is not the first game, then we want to skip adding cards to the table and just increment the round counter
	( cond ((eq roundCycle 5)
		(cond ((equal firstGame "False")
			( PlayRound deck humanHand computerHand table humanPile computerPile nextPlayer 
			(+ roundCycle 1) () firstGame lastCapturePlayer currentRound humanScore computerScore ) ) ) ) )
		
	; Then finally remove the first four cards from the table
	( cond ((eq roundCycle 6)
		(cond ((equal firstGame "True" )
		   ( PlayRound ( SubtractCardsFromDeck deck 4) humanHand computerHand table humanPile computerPile nextPlayer 
		   (+ roundCycle 1) () "False" lastCapturePlayer currentRound humanScore computerScore ))) ) )
		   
	; This condition exists if it is not the first time playing in a round and we dont want to deal cards to the table
	( cond ((eq roundCycle 6)
		(cond ((equal firstGame "False" )
			( PlayRound deck humanHand computerHand table humanPile computerPile nextPlayer 
			(+ roundCycle 1) () firstGame lastCapturePlayer currentRound humanScore computerScore ) ) ) ) )
			
			
	( cond (( eq roundCycle 7 ) ( print "Table:" ) ( print table ) (print "Hand:") (print humanHand) ( print "Human pile:" ) ( print humanPile )
								(print "Computer Hand:" ) (print computerHand ) ( print "Computer pile:" ) ( print computerPile ) (print "Deck: ") (print deck))  )
	
	; If the next player is the human, then they will go...
	( cond (( eq roundCycle 7 )
			(cond (( equal nextPlayer "Human" ) (print "It's your turn") (MoveMainMenu currentRound computerScore computerHand
												computerPile humanScore humanHand humanPile table lastCapturePlayer deck nextPlayer) 
												( PlayRound deck humanHand computerHand table humanPile computerPile "Computer" 
												(+ roundCycle 1) (HumanMakeMove humanHand table nil (HumanGetMoveChoice) ) firstGame
												lastCapturePlayer currentRound humanScore computerScore ) ) )
												
			(cond (( eq nextPlayer 'human ) (print "It's your turn") (MoveMainMenu currentRound computerScore computerHand
												computerPile humanScore humanHand humanPile table lastCapturePlayer deck nextPlayer)
												( PlayRound deck humanHand computerHand table humanPile computerPile "Computer" 
												(+ roundCycle 1) (HumanMakeMove humanHand table nil (HumanGetMoveChoice) ) firstGame
												lastCapturePlayer currentRound humanScore computerScore ) ) ) ) )
	
	;Otherwise, then the computer will go...	
	( cond (( eq roundCycle 7 )
		(cond (( equal nextPlayer "Computer" ) (print "It's the computer's turn") (MoveMainMenu currentRound computerScore computerHand
												computerPile humanScore humanHand humanPile table lastCapturePlayer deck nextPlayer)
												( PlayRound deck humanHand computerHand table humanPile computerPile 
												"Human" (+ roundCycle 1) (ComputerMakeMove computerHand table ) firstGame
												lastCapturePlayer currentRound humanScore computerScore) ) )
												
		(cond (( eq nextPlayer 'Computer ) (print "It's the computer's turn") (MoveMainMenu currentRound computerScore computerHand
												computerPile humanScore humanHand humanPile table lastCapturePlayer deck nextPlayer) 
												( PlayRound deck humanHand computerHand table humanPile computerPile 
												"Human" (+ roundCycle 1) (ComputerMakeMove computerHand table ) firstGame 
												lastCapturePlayer currentRound humanScore computerScore) ) ) ) )
		
	; Now, if the move the player chose is a trail, we need to add the card to the table...
	( cond (( eq roundCycle 8 )
			(cond (( eq (first playerMove&Card) 't ) ( PlayRound deck humanHand computerHand (AddTrailToTable table (rest playerMove&Card) ) humanPile computerPile nextPlayer
												 (+ roundCycle 1) playerMove&Card firstGame 
												 lastCapturePlayer currentRound humanScore computerScore ) ) ) ) )
												 
	; Now, if the move the player chose is a trail, we need to add the card to the table...
	( cond (( eq roundCycle 8 )
			(cond (( eq (first playerMove&Card) 'c ) ( PlayRound deck (RemoveTrailFromHand humanHand (first (rest playerMove&Card) ) () ) computerHand table
													humanPile computerPile nextPlayer (+ roundCycle 1) playerMove&Card firstGame 
													lastCapturePlayer currentRound humanScore computerScore ) ) ) ) )
		

	; If the next player is "Computer", then we know the human just made the last move and must change their hand depending on what they
	; did. In this case, this is checking if the player trailed, and will remove the trail card from their hand
	( cond (( eq roundCycle 9 )
			(cond (( equal nextPlayer "Computer" )
					(cond (( eq (first playerMove&Card) 'T )
												( PlayRound deck (RemoveTrailFromHand humanHand (first (rest playerMove&Card) ) () )
												computerHand table humanPile computerPile nextPlayer (+ roundCycle 1)
												playerMove&Card firstGame lastCapturePlayer currentRound humanScore computerScore ) ) ) ) ) ) )
												
												
	; If the next player is "Computer", then we know the human just made the last move and must change their hand depending on what they
	; did. In this case, this is checking if the player captured and adjusting the cards 
	( cond (( eq roundCycle 9 )
			(cond (( equal nextPlayer "Computer" )
					(cond (( eq (first playerMove&Card) 'C )
												( PlayRound deck humanHand
												computerHand (RemoveCaptureCardsFromTable (first (rest playerMove&Card) ) table () )  (AddCaptureCardsToPile (first (rest playerMove&Card ) ) table humanPile) computerPile nextPlayer (+ roundCycle 1)
												playerMove&Card firstGame "Human" currentRound humanScore computerScore) ) ) ) ) ) )
												
	; If the next player is "Human", then we know the computer just made the last move and must change their hand depending on what they
	; did. In this case, this is checking if the player trailed, and will remove the trail card from their hand
	( cond (( eq roundCycle 9 )
			(cond (( equal nextPlayer "Human" )
					(cond (( eq (first playerMove&Card) 'T ) 
												( PlayRound deck humanHand (RemoveTrailFromHand computerHand (first (rest playerMove&Card) ) () )
												table humanPile computerPile nextPlayer (+ roundCycle 1)
												playerMove&Card firstGame lastCapturePlayer currentRound humanScore computerScore ) ) ) ) ) ) )
												
												
	; After a player has made a move and the player's hand and table is properly adjusted, we need to check if the player's hands
	; are empty and if we need to deal move cards
	( cond (( eq roundCycle 10 )
			(cond ((eq humanHand ()) (print "Human hand empty")
				(cond ((eq computerHand ())  (print "Computer hand empty")
					(cond (( eq deck ()) (print "Deck is empty") (PlayRound deck humanHand computerHand table humanPile computerPile 
																	nextPlayer (+ roundCycle 1) playerMove&Card "True" lastCapturePlayer currentRound humanScore computerScore ) )
					( t (PlayRound deck humanHand computerHand table humanPile computerPile 
						nextPlayer 1 playerMove&Card firstGame lastCapturePlayer currentRound humanScore computerScore ) ) ) ) ) ) ) ) )

	; If both of the player's hands were not empty, then the next player can make a move
	( cond (( eq roundCycle 10 )
			(cond ((equal firstGame "False" ) (PlayRound deck humanHand computerHand table humanPile computerPile 
																	nextPlayer 7 playerMove&Card firstGame lastCapturePlayer currentRound humanScore computerScore ) ) ) ) )

	; If it is the end of the round, then we need to add the table cards to the player who captured last															
	( cond (( eq roundCycle 11 )
			(cond ((equal lastCapturePlayer "Human") (PlayRound deck humanHand computerHand table (AddTableCardsToPile table humanPile) computerPile nextPlayer (+ roundCycle 1) playerMove&Card firstGame lastCapturePlayer currentRound humanScore computerScore) )
				  ((equal lastCapturePlayer "Computer") (PlayRound deck humanHand computerHand table humanPile (AddTableCardsToPile table computerPile) nextPlayer (+ roundCycle 1) playerMove&Card firstGame lastCapturePlayer currentRound humanScore computerScore) ) ) ) )
	
	; After the cards from the table have been added, return the list of the lastCapturePlayer, human pile, and computer pile to the tournament class
	( cond (( eq roundCycle 12 ) (print "-----------------")(print "Human pile: ") (print humanPile) (print "ComputerPile: ") 
								 (print computerPile) (print "-----------------") (list lastCapturePlayer humanPile computerPile ) ) ) ) )
			 	   
	


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
		  (( eq (first pile) 'DX ) (print "Had the ten of diamonds, plus two points") (TenDiamonds (rest pile) ( + playerScore 2) ) )
		  
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
	
		   (( eq (first pile ) 'S2 ) (print "Had 2 of spades, plus a point") (TwoSpades ( rest pile ) (+ playerScore 1) ) )
		   ( t (TwoSpades ( rest pile ) playerScore ) ) ) ) )

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
		  (( eq (first pile ) 'CA ) (print "Had Ace of clubs, plus a point.") ( CountAces ( rest pile ) ( + playerScore 1 ) ) )
		  (( eq (first pile ) 'DA ) (print "Had Ace of diamonds, plus a point.") ( CountAces ( rest pile ) ( + playerScore 1 ) ) )
		  (( eq (first pile ) 'HA ) (print "Had Ace of hearts, plus a point.") ( CountAces ( rest pile ) ( + playerScore 1 ) ) )
		  (( eq (first pile ) 'SA ) (print "Had Ace of spades, plus a point.") ( CountAces ( rest pile ) ( + playerScore 1 ) ) )
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
(defun CalculateScore ( passedPlayerPile passedPlayerScore passedScoreCounter)

	(Let* (( playerPile passedPlayerPile )
		  ( playerScore passedPlayerScore )
		  ( scoreCounter passedScoreCounter ))
	
	; Look for the 10 of diamonds
	(cond (( eq scoreCounter 1 ) ( CalculateScore playerPile ( TenDiamonds playerPile playerScore ) ( + scoreCounter 1 ) ) )
		  (( eq scoreCounter 2 ) (CalculateScore playerPile ( TwoSpades playerPile playerScore ) ( + scoreCounter 1 ) ) )
		  (( eq scoreCounter 3 ) (CalculateScore playerPile ( CountAces playerPile playerScore ) ( + scoreCounter 1 ) ) )
		  (( eq scoreCounter 4 ) playerScore ) ) ) )
		  
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: CountSpades
; Purpose: To count the number of spades in a player's pile
; Parameters:
;	passedPlayerPile, a player's pile of cards
;	passedSpadeCount, a counter for the number of spades
; Return Value: Returns the number of spades in a player's pile
; Local Variables: 
;	playerPile, holds the player's pile
;	spadeCount, the count of the number of spades
; Algorithm: 
;	1) If the players pile is empty, return the count of spades
;	2) If the current card is a spade, increment the count for spades
;	3) Else, cycle through the rest of the cards but dont increase the count of spades
; Assistance Received: none 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun CountSpades ( passedPlayerPile passedSpadeCount )

	(Let* (( playerPile passedPlayerPile )
		  ( spadeCount passedSpadeCount )
		  ( stringFirstSpadeCard (string (first playerPile) ) ))
	
	; Once we've cycled through all of the cards, return the count of spades
	(cond ((eq playerPile ()) spadeCount )
		  ; If the card is a spade, increment the count...
		  ((eq (char stringFirstSpadeCard 0) '#\S) (CountSpades (rest playerPile) (+ spadeCount 1) ) )
		  ; Else, keep cycling through the rest of the cards
		  (t (CountSpades ( rest playerPile ) spadeCount ) ) ) ) )
		  

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
;	1) Get the number of spades the human has
;	2) Get the number of spades the computer has
;	3) If the human has more spades than the computer, return "Human"
;	4) If the computer hase more spades than the human, return "Computer"
;	5) If neither of them have more spades, return "Neither"
; Assistance Received: none 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun CheckWhoHasMoreSpades ( passedPlayerPiles passedHumanNumSpades passedComputerNumSpades passedCounter)

	(Let* (( playerPiles passedPlayerPiles )
		  ( humanNumSpades passedHumanNumSpades )
		  ( computerNumSpades passedComputerNumSpades )
		  ( counter passedCounter ) )
		  
		  

	; First get the number of spades for the human player...
	(cond ((eq counter 1) (CheckWhoHasMoreSpades playerPiles ( CountSpades (first (rest playerPiles )) 0 ) computerNumSpades ( + counter 1 ) ) )
	
		  ; Then get the number of spades the computer has...
		  (( eq counter 2 ) ( CheckWhoHasMoreSpades playerPiles humanNumSpades ( CountSpades (first (rest ( rest playerPiles ) ) ) 0 ) ( + counter 1 ) ) )
		  
		  
		  ;	If the human has more cards than the computer, return "Human"
		  (( > humanNumSpades computerNumSpades ) (print "Number of spades each player had:")
		  (print "------------------")
		  (print "The human had:")
		  (print humanNumSpades)
		  (print "The computer had:")
		  (print computerNumSpades)
		  (print "------------------")"Human" )
		  
		  ;	If the computer has more cards then the human, return "Computer"
		  (( < humanNumSpades computerNumSpades )(print "Number of spades each player had:")
		  (print "------------------")
		  (print "The human had:")
		  (print humanNumSpades)
		  (print "The computer had:")
		  (print computerNumSpades)
		  (print "------------------") "Computer" )
		  
		  ;	If neither of them have more spades, return "Neither"
		  (( eq humanNumSpades computerNumSpades )(print "Number of spades each player had:")
		  (print "------------------")
		  (print "The human had:")
		  (print humanNumSpades)
		  (print "The computer had:")
		  (print computerNumSpades)
		  (print "------------------") "Neither" ) )
				) )

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

		  
	
	; First, we need to get the count of the human player's pile...
	(cond ((eq counter 1) (CheckWhoHasMoreCards playerPiles ( CountCards ( first (rest playerPiles)  ) 0 ) computerNumCards ( + counter 1 ) ) )
	
		  ; Then we need to get the count of the computer's pile...
		  (( eq counter 2 ) ( CheckWhoHasMoreCards playerPiles humanNumCards ( CountCards (first (rest ( rest playerPiles ))) 0 ) ( + counter 1 ) ) )
		  
		  
		  ; If the human has more cards than the computer, return "Human"
		  (( > humanNumCards computerNumCards ) (print "------------------")
		  (print "Number of cards each player had:")
		  (print "The human had:")
		  (print humanNumCards)
		  (print "The computer had:")
		  (print computerNumCards)
		  (print "------------------") "Human" )
		  
		  ; If the computer has more cards than the human, return "Computer"
		  (( < humanNumCards computerNumCards ) (print "------------------")
		  (print "Number of cards each player had:")
		  (print "The human had:")
		  (print humanNumCards)
		  (print "The computer had:")
		  (print computerNumCards)
		  (print "------------------") "Computer" )
		  
		  ; If neither of them have more cards, return "Neither"
		  (( eq humanNumCards computerNumCards ) (print "------------------")
		  (print "Number of cards each player had:")
		  (print "The human had:")
		  (print humanNumCards)
		  (print "The computer had:")
		  (print computerNumCards)
		  (print "------------------") (print "Neither of the player's had more cards, no points awarded.") "Neither" ) ) ) )
		  
		  
		  
		  

		  


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
;	passedMoreCardPlayer, represents the player who has more cards
;	passedMoreSpadePlayer, represents the player who has more spades
; Return Value: None
; Local Variables: 
;   humanScore, holds the human's score for the tournament
;	computerScore, holds the computer's score for the tournament
;	roundCounter, holds the current round they are on
;	pile, holds the piles for each of the players in a lists with two sub lists, the first is the human's, the second is the computer's
; 	tallyScore, holds either "True" or "False" if the scores need to be tallied
;	tallyScoreCounter, keeps track of which function to call to keep track of points
;	moreCardPlayer, the player with more cards
;	moreSpadePlayer, the player with more spades
; Algorithm: 
;	1) If tallyScore is "True" then that means we need to calculate the score and start by getting the player who has more cards
;  	2) Then we check if either it was the human or computer that had more cards and output whichever player had more
;	3) Then we check which of the player's has more spades and output which ever one had more
; 	4) Then we calculate all of the other points that the player might have made (i.e. have ace cards)
;	5) Then we outout the final scores and check to see if any of the players won
;	6) If no one won, then play another round and repeat the process
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
	   
	   
	   
	   
	
	; Checking to see which of the player's have more cards
	(cond (( equal tallyScore "True")
			(cond (( eq tallyScoreCounter 1) (print "The player's score before calculating was:") (print humanScore) (print "The computer's score before calculating was:") (print computerScore) 
				(PlayTournament humanScore computerScore roundCounter pile tallyScore ( + tallyScoreCounter 1) 
				(CheckWhoHasMoreCards pile 0 0 1 ) "Neither" ) ) ) ) ) 
				
				
			
	; After getting the player with the most cards, we now need to check if that player is the human and add 3 points to their score
	(cond (( equal tallyScore "True" )
			(cond (( equal moreCardPlayer "Human" ) (print "The human had more cards, plus three points.")
			(PlayTournament ( + humanScore 3 ) computerScore roundCounter pile tallyScore tallyScoreCounter "Neither" moreSpadePlayer) ) ) ) )
			
	; After getting the player with the most cards, now we must check if the computer had the most cards and add 3 points of necessary
	(cond (( equal tallyScore "True" )
			(cond (( equal moreCardPlayer "Computer" ) (print "The computer had more cards, plus three points.")
			(PlayTournament humanScore ( + computerScore 3 ) roundCounter pile tallyScore tallyScoreCounter "Neither" moreSpadePlayer ) ) ) ) )
				
	; Now we need to get the player who has the most spades
	(cond (( equal tallyScore "True" )
			(cond (( eq tallyScoreCounter 2 ) ( PlayTournament humanScore computerScore roundCounter pile tallyScore 
				( + tallyScoreCounter 1 ) moreCardPlayer (CheckWhoHasMoreSpades pile 0 0 1 ) ) ) ) ) )
				
	; If the human had more spades, give them a point...
	(cond (( equal tallyScore "True" )
			(cond (( equal moreSpadePlayer "Human" ) (print "The human had more spades, plus a point.")
			(PlayTournament ( + humanScore 1 ) computerScore roundCounter pile tallyScore tallyScoreCounter MoreCardPlayer "Neither" ) ) ) ) )
				
	; If the computer had more spades, give them a point
	(cond (( equal tallyScore "True" )
			(cond (( equal moreSpadePlayer "Computer" ) (print "The computer had more spades, plus a point.")
			(PlayTournament humanScore ( + computerScore 1 ) roundCounter pile tallyScore tallyScoreCounter
				moreCardPlayer "Neither") ) ) ) )
			
	; Now we need to calculate the rest of the ways to earn points for the human...
	(cond (( equal tallyScore "True" )
			(cond (( eq tallyScoreCounter 3 ) (print "-----------------------") (print "The human also made points by:") 
			( PlayTournament (+ humanScore (CalculateScore (first (rest pile)) 0 1)) computerScore roundCounter pile tallyScore
				(+ tallyScoreCounter 1) moreCardPlayer moreSpadePlayer) ) ) ) )
				
	; Now we need to calculate the rest of the ways to earn points for the computer...
	(cond (( equal tallyScore "True" )
			(cond (( eq tallyScoreCounter 4 ) (print "-----------------------") (print "The computer also made points by:") 
			(PlayTournament humanScore (+ computerScore (CalculateScore (first (rest (rest pile))) 0 1)) roundCounter pile tallyScore
				(+ tallyScoreCounter 1) moreCardPlayer moreSpadePlayer) ) ) ) )
	

	; Here we are outputting the player's scores after calculating everything for that round
	(cond (( equal tallyScore "True" )
			(cond ((eq tallyScoreCounter 5 ) (print "Human Score After Calculating: ") (print humanScore) (print "Computer Score After Calculating: ") (print computerScore)
			(PlayTournament humanScore (+ computerScore (CalculateScore (rest pile) 0 1)) roundCounter pile "False"
				(+ tallyScoreCounter 1) moreCardPlayer moreSpadePlayer) ) ) ) )
				
	
		  ; Check if it was a tie between the players
	(cond ((equal passedHumanScore computerScore )
			(cond (( >= passedHumanScore 21 ) (print "It's a tie!") ( return nil ) ) ) )
			
				
		  ; If the human score is greater or equal to 21 and the computer's score is less than 21, then the human won!
		  (( >= humanScore 21 ) 
			(cond (( < computerScore 21 ) (print "You Won!") ( return nil ) ) ) )
	
		  ; If the computer score is greater or equal to 21 and the human's score is less than 21, then the computer won!
		  (( >= computerScore 21) 
			(cond (( < humanScore 21) (print "The computer won.") ( return nil ) ) ) ) )
			
			(return nil)
				
		  ; If no one won the game yet, play a round, increment the score, and get the player's piles from the round
		  (print (first pile))
		  (PlayTournament humanScore computerScore (+ roundCounter 1) (PlayRound (ActualDeck (loadDeck)) () () () () () (first (first pile)) 1 () "True" nil roundCounter humanScore computerScore ) "True"  1 "Neither" "Neither" ) ) )
		  
		  
		
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: AskForSave
; Purpose: To ask the user for a save file to load from and return the file stream
; Parameters: None
; Return Value: The contents of a save file
; Local Variables:  None
; Algorithm: 
;	1) Propmt the user for a file they want to open
;  	2) Return the filestream
; Assistance Received: none 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun AskForSave ()

	(print "Enter a file to load from: ")

	(Let* ((fileName (read) ) )
			(cond (t (open fileName)) )))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: ReadSave
; Purpose: To serve as getting the contents of the file
; Parameters: None
; Return Value: The list which contains all of the sub lists of the load file
; Local Variables:  None
; Algorithm: 
;	1) Return the list which contains all of the content for the game
; Assistance Received: none 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun ReadSave (passedFile)

	(cond
		  (t (read passedFile))))
	
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Function Name: LoadGame
; Purpose: To load the game with the list of content from the save file
; Parameters:
;	save, the list containing all of the necessary fields to load the game
; Return Value: Nothing, when this function returns, the game is over
; Local Variables:  None
; Algorithm: 
;	1) Play the tournament with the information from the save file
; Assistance Received: none 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;	
(defun LoadGame (save)

(print (length save))

	; If the save has a build in it, I can atleast load in the save but thats it 
	(cond ((eq (length save) 12) (PlayTournament (nth 4 save) (nth 1 save) (nth 0 save) (PlayRound (nth 10 save) (nth 5 save) (nth 2 save) (nth 7 save) (nth 6 save) (nth 3 save) (nth 11 save) 7 () "False" (nth 9 save) (nth 0 save) (nth 4 save) (nth 1 save) ) "True" 1 "Neither" "Neither" ) )
	
		  ; Otherwise, if there are no builds, then we can atleast load it in aswell
		  ((eq (length save) 11) (PlayTournament (nth 4 save) (nth 1 save) (nth 0 save) (PlayRound (nth 9 save) (nth 5 save) (nth 2 save) (nth 7 save) (nth 6 save) (nth 3 save) (nth 10 save) 7 () "False" (nth 8 save) (nth 0 save) (nth 4 save) (nth 1 save) ) "True" 1 "Neither" "Neither") )
	(t (print "Invalid save.") (return nil) ) ) )
	
	


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

	(cond ((eq option 1) (LoadGame (ReadSave (AskForSave) )) )
		  ((eq option 2) (PlayTournament 0 0 1 (PlayRound (ActualDeck (loadDeck)) "True" () () () () (FirstPlayer) 1 () "True" "Nil" 1 0 0 ) "True" 1 "Neither" "Neither"))
		  ((eq option 3) (return))
		  (t (print "Incorrect menu option") (OpeningMenu)))))
	


		  
; Function call which starts the whole program		  
(OpeningMenu)

; Used for validating that all of the functions for calculating the scores work
;(PlayTournament 0 0 1 '( ("Computer") (DJ DA C3 C5 CA HA SA) (SX SQ SK D6 H8 S2 DX)) "True" 1 "Neither" "Neither")

