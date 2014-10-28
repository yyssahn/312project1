
testBoard = ["WWW-WW-------BB-BBB"]
type Pawn = (Char,Int,Int)
crusher_r4j8 ::[String]->Char->Int->Int->[[String]]
crusher_r4j8 state team numMove numN = reverse(state_search_r4j8 [state] team numMove numN)


state_search_r4j8 ::[[String]] ->Char->Int->Int->[[String]]
state_search_r4j8 path team numMove numN  = path


get_Char :: [String]->Int->Int->Char
get_Char state x y
	|	null state = 'A'
	|	y == 0 = get_Char_helper (head state) x
	|	otherwise = get_Char (tail state) x (y-1)

get_Char_helper::[Char]->Int->Char
get_Char_helper row x = head (mynthtail x row)
	
mynthtail:: Int->[a]->[a]
mynthtail n list1
	| null list1 = list1
	| n == 0 = list1
	| otherwise = mynthtail (n-1) (tail list1)


get_pawn_char :: Pawn->Char
get_pawn_char (a,b,c)= a
get_pawn_x :: Pawn->Int
get_pawn_x (a,b,c)= b
get_pawn_y :: Pawn->Int
get_pawn_y (a,b,c) = c