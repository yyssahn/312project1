
testBoard = ["WWW-WW-------BB-BBB"]
testBoardState =["WWW","-WW-", "-----","-BB-","BBB"]

type Pawn = (Char,Int,Int)

testPawn = ('W',0,0)
testPawnList = [('W',0,0),('W',0,1),('W',0,2),('W',1,1),('W',1,2),('B',3,1),('B',3,2),('B',4,0),('4',4,1),('B',4,2)]

testBoardState2 = ["-W---","--W---","---W---","--------","---------"]

topleft::Int
topleft = 0
topright::Int
topright = 1 
left::Int
left = 2
right::Int
right = 3
bottomleft::Int
bottomleft = 4
bottomright::Int
bottomright = 5



crusher_r4j8 ::[String]->Char->Int->Int->[String]
crusher_r4j8 state team numMove numN = reverse(state_search_r4j8 state team numMove numN)


state_search_r4j8 ::[String] ->Char->Int->Int->[String]
state_search_r4j8 path team numMove numN  = path

get_pawn_count::[String]->Char->Int
get_pawn_count state pawn
	|	null state = 0
	|	otherwise = (get_pawn_count_helper (head state) pawn) + get_pawn_count (tail state) pawn

get_pawn_count_helper::[Char]->Char->Int
get_pawn_count_helper state pawn
	|	null state = 0
	|	(head state) ==pawn = 1 + get_pawn_count_helper (tail state) pawn
	|	otherwise = get_pawn_count_helper (tail state) pawn

make_board :: [Char]->Int->Int->[String]
make_board input n helper
	|	null input = []
	|	helper == n = make_board_helper input n (helper -2)
	|	otherwise = (first_n_items input (n + helper)):make_board (mynthtail (n+helper) input) n (helper+1)

is_game_over :: [String]->Int->Bool
is_game_over state n
	|	get_pawn_count state 'W' < n || get_pawn_count state 'B' < 3 = True
	|	otherwise = False


first_n_items ::[Char]->Int->[Char]
first_n_items [] n = []
first_n_items (x:xs) 0 = []
first_n_items (x:xs) n = x : (first_n_items xs (n-1))
 
make_PawnList:: [String]->Int->[Pawn]
make_PawnList state yhelper
	| null state = []
	| otherwise = (make_PawnList_helper (head state) 0 yhelper) ++ (make_PawnList (tail state) (yhelper +1))

make_into_single_string :: [String] -> String
make_into_single_string input
	| 	null input = []
	| 	otherwise = (head input) ++ (make_into_single_string (tail input))

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
	|	null state = ' '
	|	y == 0 && (length (head state)) > x = get_Char_helper (head state) x
	|	y == 0 && (length (head state)) <= x = ' '
	|	otherwise = get_Char (tail state) x (y-1)

get_Char_helper::[Char]->Int->Char

get_Char_helper row x = head (mynthtail x row)
	
mynthtail:: Int->[a]->[a]
mynthtail n list1
	| null list1 = list1
	| n == 0 = list1
	| otherwise = mynthtail (n-1) (tail list1)


empty :: [String]->Int->Int->Bool
empty state x y
	| null state = False
	|  x<0 || y < 0	= False
	|	(get_Char state x y) == '-' =True
	|	otherwise = False


--------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------

possible_jump::[String]->Pawn->Int->Int->Bool
possible_jump state pawn size dir	
	|	get_pawn_y pawn < size-2 = possible_jump_up state pawn dir
	|	otherwise = False

--	|	get_pawn_y pawn ==(size-2) = possible_jump_centerup state pawn dir
--	|	get_pawn_y pawn ==(size-1) = possible_jump_center state pawn dir
--	|	get_pawn_y pawn ==(size) = possible_jump_centerdown state pawn dir
--	|	get_pawn_y pawn >(size) = possible_jump_down state pawn dir

--------------------------------------------------------------------------------------------------------------------------------------

possible_jump_up ::[String]->Pawn->Int->Bool
possible_jump_up state pawn dir
	|	dir == topleft  && (get_Char state (get_pawn_x pawn -1) (get_pawn_y pawn -1)) == (get_pawn_char pawn) = can_jump state (get_pawn_char pawn)(get_pawn_x pawn -2) (get_pawn_y pawn -2)
--	|	dir == topright = empty state (get_pawn_x pawn) (get_pawn_y pawn - 1)
--	|	dir == left = empty state (get_pawn_x pawn - 1) (get_pawn_y pawn)
--	|	dir == right = empty state (get_pawn_x pawn + 1) (get_pawn_y pawn )
--	|	dir == bottomleft = empty state (get_pawn_x pawn) (get_pawn_y pawn + 1)
--	|	dir == bottomright = empty state (get_pawn_x pawn + 1) (get_pawn_y pawn + 1)
	|	otherwise = False


can_jump ::[String]->Char->Int->Int->Bool
can_jump state pawn x y
	|	(get_Char state x y) == pawn || (get_Char state x y) == ' ' = False 
	|	otherwise = True
--------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------

	
possible_move ::[String]->Pawn->Int->Int->Bool
possible_move state pawn size dir
	|	get_pawn_y pawn < (size-1) = possible_move_up state pawn dir
	|	get_pawn_y pawn == (size-1) = possible_move_center state pawn dir
	|	get_pawn_y pawn >= size = possible_move_down state pawn dir

possible_move_up ::[String]->Pawn->Int->Bool
possible_move_up state pawn dir
	|	dir == topleft  = empty state (get_pawn_x pawn - 1) (get_pawn_y pawn - 1)
	|	dir == topright = empty state (get_pawn_x pawn) (get_pawn_y pawn - 1)
	|	dir == left = empty state (get_pawn_x pawn - 1) (get_pawn_y pawn)
	|	dir == right = empty state (get_pawn_x pawn + 1) (get_pawn_y pawn )
	|	dir == bottomleft = empty state (get_pawn_x pawn) (get_pawn_y pawn + 1)
	|	dir == bottomright = empty state (get_pawn_x pawn + 1) (get_pawn_y pawn + 1)


possible_move_center::[String]->Pawn->Int->Bool
possible_move_center state pawn dir
	|	dir == topleft  = empty state (get_pawn_x pawn - 1) (get_pawn_y pawn - 1)
	|	dir == topright = empty state (get_pawn_x pawn) (get_pawn_y pawn - 1)
	|	dir == left = empty state (get_pawn_x pawn - 1) (get_pawn_y pawn)
	|	dir == right = empty state (get_pawn_x pawn + 1) (get_pawn_y pawn )
	|	dir == bottomleft = empty state (get_pawn_x pawn - 1) (get_pawn_y pawn + 1)
	|	dir == bottomright = empty state (get_pawn_x pawn) (get_pawn_y pawn + 1)


possible_move_down ::[String]->Pawn->Int->Bool
possible_move_down state pawn dir
	|	dir == topleft  = empty state (get_pawn_x pawn) (get_pawn_y pawn - 1)
	|	dir == topright = empty state (get_pawn_x pawn+1) (get_pawn_y pawn - 1)
	|	dir == left = empty state (get_pawn_x pawn - 1) (get_pawn_y pawn)
	|	dir == right = empty state (get_pawn_x pawn + 1) (get_pawn_y pawn )
	|	dir == bottomleft = empty state (get_pawn_x pawn - 1) (get_pawn_y pawn + 1)
	|	dir == bottomright = empty state (get_pawn_x pawn) (get_pawn_y pawn + 1)
	
	
--
-- Pawn getters
--
get_pawn_char :: Pawn->Char
get_pawn_char (a,b,c)= a
get_pawn_x :: Pawn->Int
get_pawn_x (a,b,c)= b
get_pawn_y :: Pawn->Int
get_pawn_y (a,b,c) = c