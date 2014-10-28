
testBoard = ["WWW-WW-------BB-BBB"]
testBoardState =["WWW","-WW-", "-----","-BB-","BBB"]
type Pawn = (Char,Int,Int)
crusher_r4j8 ::[String]->Char->Int->Int->[[String]]
crusher_r4j8 state team numMove numN = reverse(state_search_r4j8 [state] team numMove numN)


state_search_r4j8 ::[[String]] ->Char->Int->Int->[[String]]
state_search_r4j8 path team numMove numN  = path

make_board :: [Char]->Int->Int->[String]
make_board input n helper
	|	null input = []
	|	helper == n = make_board_helper input n (helper -2)
	|	otherwise = (first_n_items input (n + helper)):make_board (mynthtail (n+helper) input) n (helper+1)


first_n_items ::[Char]->Int->[Char]
first_n_items [] n = []
first_n_items (x:xs) 0 = []
first_n_items (x:xs) n = x : (first_n_items xs (n-1))
 
make_PawnList:: [String]->Int->[Pawn]
make_PawnList state yhelper
	| null state = []
	| otherwise = (make_PawnList_helper (head state) 0 yhelper) ++ (make_PawnList (tail state) (yhelper +1))


make_PawnList_helper :: [Char]->Int->Int->[Pawn]
make_PawnList_helper str helper y
	|	null str = []
	|	head str /= '-'  = ((head str),helper,y) : make_PawnList_helper (tail str) (helper+1) y  	
	| 	otherwise = make_PawnList_helper (tail str) (helper + 1) y

make_board_helper ::[Char]->Int->Int->[String]
make_board_helper input n helper
	|	helper == 0 = (first_n_items input n ):[]
	|	otherwise =  (first_n_items input (n + helper)): make_board_helper (mynthtail (n+helper) input)  n (helper-1)

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