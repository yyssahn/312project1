
testBoard = ["WWW-WW-------BB-BBB"]
testBoardState =["WWW","-WW-", "-----","-BB-","BBB"]

testBoardState1 =["WWW","--W-", "-w---","-BB-","BBB"]
simpleBoardState = ["WW","---","BB"]
type Pawn = (Char,Int,Int)

testPawn = ('W',0,0)
testPawnList = [('W',0,0),('W',1,0),('W',2,0),('W',1,1),('W',2,1),('B',1,3),('B',2,3),('B',0,4),('4',1,4),('B',2,4)]

testBoardState2 = ["-W---","--W---","---W---","--------","---------"]

type PathScore= ([[String]],Int)

minnum::Int
minnum = 0
maxnum::Int
maxnum = 1

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




-------------------------------------------------------------------------------
generate_path :: [String]->Char->Int->Int->[[[String]]]->[[[String]]]
generate_path state turn turnnum n path
	|	turnnum ==0 = path
	|	(length path)== 0  = generate_path state (nextturn turn) (turnnum-1) n (make_patharray_helper [state] (generate_possible_state state turn (make_PawnList state 0) n))
	|	otherwise = generate_path state (nextturn turn) (turnnum -1) n (make_patharray path turn n)


make_patharray:: [[[String]]]->Char->Int->[[[String]]]
make_patharray paths turn n 
	|	null paths = []
	|	otherwise = (make_patharray_helper (head paths) (generate_possible_state (last(head paths))  turn (make_PawnList (last (head paths)) 0) n ) ) ++ make_patharray (tail paths) turn n

make_patharray_helper :: [[String]]->[[String]]->[[[String]]]
make_patharray_helper path states
	|	null states = []
	|	elem (head states) path = make_patharray_helper path (tail states)
	|	otherwise = (reverse ( (head states):(reverse path))) : make_patharray_helper path (tail states) 
-------------------------------------------------------------------------------


make_pathscore :: [[[String]]]->Char->Int->[PathScore]
make_pathscore paths turn n 
	|	null paths = []
	|	otherwise = (make_pathscore_helper (head paths) turn n) : make_pathscore (tail paths) turn n 

make_pathscore_helper :: [[String]]->Char->Int->PathScore
make_pathscore_helper path turn n = (path , get_score (last path) turn n)

crusher_r4j8 ::[String]->Char->Int->Int->[String]
crusher_r4j8 state team numMove numN = reverse(state_search_r4j8 state team numMove numN)


init_pathscore :: [PathScore] ->[PathScore]
init_pathscore [] = []
init_pathscore (p:ps) = (init ( get_ps_path p ),get_ps_score p): init_pathscore (ps)

trim_pathscore ::[PathScore]->Int->[PathScore]->[PathScore]
trim_pathscore ps minmax track
	| null ps = track
	| null track = trim_pathscore (tail ps) minmax [(head ps)]
	| (get_ps_path(head ps)) /= (get_ps_path (head track)) = trim_pathscore (tail ps) minmax (head ps : track)
	| otherwise =  trim_pathscore (tail ps) minmax ((trim_pathscore_helper (head ps) (head track) minmax) :tail track)

trim_pathscore_helper::PathScore->PathScore->Int->PathScore
trim_pathscore_helper a b minmax
	|	minmax== maxnum && (get_ps_score a) > (get_ps_score b) = a
	|	minmax==maxnum && (get_ps_score a) <= (get_ps_score b) = b
 	|	minmax== minnum && (get_ps_score a) > (get_ps_score b) = b
	|	otherwise  = a
	


nextturn :: Char->Char
nextturn input
	| input == 'W' ='B'
	| otherwise = 'W'

state_search_r4j8 ::[String] ->Char->Int->Int->[String]
state_search_r4j8 path team numMove numN  = path

get_score :: [String]->Char->Int->Int
get_score state pawn n
	|	pawn == 'W' && (get_pawn_count state 'B' < n) = 1000
	|	pawn == 'W' = (get_pawn_count state 'W') - (get_pawn_count state 'B')
	|	pawn == 'B' && (get_pawn_count state 'W' < n) = 1000
	|	otherwise = (get_pawn_count state 'B') - (get_pawn_count state 'W')


generate_possible_state :: [String]->Char->[Pawn]->Int->[[String]]
generate_possible_state state turn pawns n
	|	null pawns= []
	|	get_pawn_char (head pawns) /= turn = generate_possible_state state turn (tail pawns) n
	|	otherwise = (generate_possible_state_helper state (head pawns) n)++ generate_possible_state state turn (tail pawns) n

generate_possible_state_helper ::[String] ->Pawn->Int->[[String]]
generate_possible_state_helper state pawn n =
	(if (possible_move state pawn n topleft) then (slide_pawn state pawn topleft n):[] else []) ++ 
	(if (possible_move state pawn n topright) then (slide_pawn state pawn topright n):[] else []) ++ 
	(if (possible_move state pawn n left) then (slide_pawn state pawn left n):[] else []) ++ 
	(if (possible_move state pawn n right) then (slide_pawn state pawn right n):[] else []) ++ 
	(if (possible_move state pawn n bottomleft) then (slide_pawn state pawn bottomleft n):[] else []) ++ 
	(if (possible_move state pawn n bottomright) then (slide_pawn state pawn bottomright n):[] else []) ++ 	
	(if (possible_jump state pawn n topleft) then (jump_pawn state pawn topleft n):[] else []) ++ 
	(if (possible_jump state pawn n topright) then (jump_pawn state pawn topright n):[] else []) ++ 
	(if (possible_jump state pawn n left) then (jump_pawn state pawn left n):[] else []) ++ 
	(if (possible_jump state pawn n right) then (jump_pawn state pawn right n):[] else []) ++ 
	(if (possible_jump state pawn n bottomleft) then (jump_pawn state pawn bottomleft n):[] else []) ++ 
	(if (possible_jump state pawn n bottomright) then (jump_pawn state pawn bottomright n):[] else [])

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
	|	x < 0 = ' '
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
	|	get_pawn_y pawn < (size-2) = possible_jump_up state pawn dir
	|	get_pawn_y pawn ==(size-2) = possible_jump_centerup state pawn dir
	|	get_pawn_y pawn ==(size-1) = possible_jump_center state pawn dir
	|	get_pawn_y pawn ==(size) = possible_jump_centerdown state pawn dir
	|	get_pawn_y pawn >(size) = possible_jump_down state pawn dir
	|	otherwise = False
--------------------------------------------------------------------------------------------------------------------------------------

possible_jump_up ::[String]->Pawn->Int->Bool
possible_jump_up state pawn dir
	|	dir == topleft  && (get_Char state (get_pawn_x pawn -1) (get_pawn_y pawn -1)) == (get_pawn_char pawn) = can_jump state (get_pawn_char pawn)(get_pawn_x pawn -2) (get_pawn_y pawn -2)
	|	dir == topright && (get_Char state (get_pawn_x pawn) (get_pawn_y pawn -1)) == (get_pawn_char pawn)= can_jump state (get_pawn_char pawn) (get_pawn_x pawn) (get_pawn_y pawn - 2)
	|	dir == left && (get_Char state (get_pawn_x pawn - 1)(get_pawn_y pawn)) == (get_pawn_char pawn) = can_jump state (get_pawn_char pawn)(get_pawn_x pawn - 2) (get_pawn_y pawn)
	|	dir == right && (get_Char state (get_pawn_x pawn + 1)(get_pawn_y pawn)) == (get_pawn_char pawn) = can_jump state (get_pawn_char pawn)(get_pawn_x pawn + 2) (get_pawn_y pawn)
	|	dir == bottomleft && (get_Char state (get_pawn_x pawn )(get_pawn_y pawn+1)) == (get_pawn_char pawn) = can_jump state (get_pawn_char pawn)(get_pawn_x pawn) (get_pawn_y pawn+2)
	|	dir == bottomright && (get_Char state (get_pawn_x pawn +1)(get_pawn_y pawn+1)) == (get_pawn_char pawn) = can_jump state (get_pawn_char pawn)(get_pawn_x pawn + 2) (get_pawn_y pawn+2)
	|	otherwise = False


possible_jump_centerup ::[String]->Pawn->Int->Bool
possible_jump_centerup state pawn dir
	|	dir == topleft  && (get_Char state (get_pawn_x pawn -1) (get_pawn_y pawn -1)) == (get_pawn_char pawn) = can_jump state (get_pawn_char pawn)(get_pawn_x pawn -2) (get_pawn_y pawn -2)
	|	dir == topright && (get_Char state (get_pawn_x pawn) (get_pawn_y pawn -1)) == (get_pawn_char pawn)= can_jump state (get_pawn_char pawn) (get_pawn_x pawn) (get_pawn_y pawn - 2)
	|	dir == left && (get_Char state (get_pawn_x pawn - 1)(get_pawn_y pawn)) == (get_pawn_char pawn) = can_jump state (get_pawn_char pawn)(get_pawn_x pawn - 2) (get_pawn_y pawn)
	|	dir == right && (get_Char state (get_pawn_x pawn + 1)(get_pawn_y pawn)) == (get_pawn_char pawn) = can_jump state (get_pawn_char pawn)(get_pawn_x pawn + 2) (get_pawn_y pawn)
	|	dir == bottomleft && (get_Char state (get_pawn_x pawn )(get_pawn_y pawn+1)) == (get_pawn_char pawn) = can_jump state (get_pawn_char pawn)(get_pawn_x pawn - 1) (get_pawn_y pawn+2)
	|	dir == bottomright && (get_Char state (get_pawn_x pawn +1)(get_pawn_y pawn+1)) == (get_pawn_char pawn) = can_jump state (get_pawn_char pawn)(get_pawn_x pawn + 1) (get_pawn_y pawn+2)
	|	otherwise = False


possible_jump_center ::[String]->Pawn->Int->Bool
possible_jump_center state pawn dir
	|	dir == topleft  && (get_Char state (get_pawn_x pawn -1) (get_pawn_y pawn -1)) == (get_pawn_char pawn) = can_jump state (get_pawn_char pawn)(get_pawn_x pawn -2) (get_pawn_y pawn -2)
	|	dir == topright && (get_Char state (get_pawn_x pawn) (get_pawn_y pawn -1)) == (get_pawn_char pawn)= can_jump state (get_pawn_char pawn) (get_pawn_x pawn) (get_pawn_y pawn - 2)
	|	dir == left && (get_Char state (get_pawn_x pawn - 1)(get_pawn_y pawn)) == (get_pawn_char pawn) = can_jump state (get_pawn_char pawn)(get_pawn_x pawn - 2) (get_pawn_y pawn)
	|	dir == right && (get_Char state (get_pawn_x pawn + 1)(get_pawn_y pawn)) == (get_pawn_char pawn) = can_jump state (get_pawn_char pawn)(get_pawn_x pawn + 2) (get_pawn_y pawn)
	|	dir == bottomleft && (get_Char state (get_pawn_x pawn - 1)(get_pawn_y pawn+1)) == (get_pawn_char pawn) = can_jump state (get_pawn_char pawn)(get_pawn_x pawn - 2) (get_pawn_y pawn+2)
	|	dir == bottomright && (get_Char state (get_pawn_x pawn)(get_pawn_y pawn+1)) == (get_pawn_char pawn) = can_jump state (get_pawn_char pawn)(get_pawn_x pawn) (get_pawn_y pawn+2)
	|	otherwise = False



possible_jump_centerdown ::[String]->Pawn->Int->Bool
possible_jump_centerdown state pawn dir
	|	dir == topleft  && (get_Char state (get_pawn_x pawn) (get_pawn_y pawn -1)) == (get_pawn_char pawn) = can_jump state (get_pawn_char pawn)(get_pawn_x pawn -1) (get_pawn_y pawn -2)
	|	dir == topright && (get_Char state (get_pawn_x pawn+1) (get_pawn_y pawn -1)) == (get_pawn_char pawn)= can_jump state (get_pawn_char pawn) (get_pawn_x pawn+1) (get_pawn_y pawn - 2)
	|	dir == left && (get_Char state (get_pawn_x pawn - 1)(get_pawn_y pawn)) == (get_pawn_char pawn) = can_jump state (get_pawn_char pawn)(get_pawn_x pawn - 2) (get_pawn_y pawn)
	|	dir == right && (get_Char state (get_pawn_x pawn + 1)(get_pawn_y pawn)) == (get_pawn_char pawn) = can_jump state (get_pawn_char pawn)(get_pawn_x pawn + 2) (get_pawn_y pawn)
	|	dir == bottomleft && (get_Char state (get_pawn_x pawn - 1)(get_pawn_y pawn+1)) == (get_pawn_char pawn) = can_jump state (get_pawn_char pawn)(get_pawn_x pawn - 2) (get_pawn_y pawn+2)
	|	dir == bottomright && (get_Char state (get_pawn_x pawn)(get_pawn_y pawn+1)) == (get_pawn_char pawn) = can_jump state (get_pawn_char pawn)(get_pawn_x pawn) (get_pawn_y pawn+2)
	|	otherwise = False

possible_jump_down ::[String]->Pawn->Int->Bool
possible_jump_down state pawn dir
	|	dir == topleft  && (get_Char state (get_pawn_x pawn) (get_pawn_y pawn -1)) == (get_pawn_char pawn) = can_jump state (get_pawn_char pawn)(get_pawn_x pawn) (get_pawn_y pawn -2)
	|	dir == topright && (get_Char state (get_pawn_x pawn+1) (get_pawn_y pawn -1)) == (get_pawn_char pawn)= can_jump state (get_pawn_char pawn) (get_pawn_x pawn+2) (get_pawn_y pawn - 2)
	|	dir == left && (get_Char state (get_pawn_x pawn - 1)(get_pawn_y pawn)) == (get_pawn_char pawn) = can_jump state (get_pawn_char pawn)(get_pawn_x pawn - 2) (get_pawn_y pawn)
	|	dir == right && (get_Char state (get_pawn_x pawn + 1)(get_pawn_y pawn)) == (get_pawn_char pawn) = can_jump state (get_pawn_char pawn)(get_pawn_x pawn + 2) (get_pawn_y pawn)
	|	dir == bottomleft && (get_Char state (get_pawn_x pawn - 1)(get_pawn_y pawn+1)) == (get_pawn_char pawn) = can_jump state (get_pawn_char pawn)(get_pawn_x pawn - 2) (get_pawn_y pawn+2)
	|	dir == bottomright && (get_Char state (get_pawn_x pawn)(get_pawn_y pawn+1)) == (get_pawn_char pawn) = can_jump state (get_pawn_char pawn)(get_pawn_x pawn) (get_pawn_y pawn+2)
	|	otherwise = False


can_jump ::[String]->Char->Int->Int->Bool
can_jump state pawn x y
	|	x<0||y<0 =False
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
	


------
----
-----


--jumping pawn 
jump_pawn :: [String]->Pawn->Int->Int->[String]
jump_pawn state pawn dir n
	| 	get_pawn_y (pawn) < (n - 2 )  = jump_pawn_up state pawn dir
	|	get_pawn_y (pawn) == (n-2) = jump_pawn_centerup state pawn dir
	|	get_pawn_y (pawn) == (n-1) = jump_pawn_center state pawn dir
	|	get_pawn_y (pawn) == n = jump_pawn_centerdown state pawn dir
	|	otherwise = jump_pawn_down state pawn dir

jump_pawn_up :: [String]->Pawn->Int->[String]
jump_pawn_up state pawn dir
	|	dir == topleft = move_pawn state pawn (get_pawn_x pawn - 2) (get_pawn_y pawn - 2)
	|	dir == topright	=move_pawn state pawn (get_pawn_x pawn) (get_pawn_y pawn - 2)
	|	dir == left	= move_pawn state pawn (get_pawn_x pawn - 2) (get_pawn_y pawn)
	|	dir == right = move_pawn state pawn (get_pawn_x pawn+ 2) (get_pawn_y pawn)
	|	dir == bottomleft = move_pawn state pawn (get_pawn_x pawn) (get_pawn_y pawn + 2)
	|	otherwise = move_pawn state pawn (get_pawn_x pawn +2) (get_pawn_y pawn +2)


jump_pawn_centerup :: [String]->Pawn->Int->[String]
jump_pawn_centerup state pawn dir
	|	dir == topleft = move_pawn state pawn (get_pawn_x pawn - 2) (get_pawn_y pawn - 2)
	|	dir == topright	=move_pawn state pawn (get_pawn_x pawn) (get_pawn_y pawn - 2)
	|	dir == left	= move_pawn state pawn (get_pawn_x pawn - 2) (get_pawn_y pawn)
	|	dir == right = move_pawn state pawn (get_pawn_x pawn+ 2) (get_pawn_y pawn)
	|	dir == bottomleft = move_pawn state pawn (get_pawn_x pawn-1) (get_pawn_y pawn + 2)
	|	otherwise = move_pawn state pawn (get_pawn_x pawn +1) (get_pawn_y pawn +2)


jump_pawn_center :: [String]->Pawn->Int->[String]
jump_pawn_center state pawn dir
	|	dir == topleft = move_pawn state pawn (get_pawn_x pawn - 2) (get_pawn_y pawn - 2)
	|	dir == topright	=move_pawn state pawn (get_pawn_x pawn) (get_pawn_y pawn - 2)
	|	dir == left	= move_pawn state pawn (get_pawn_x pawn - 2) (get_pawn_y pawn)
	|	dir == right = move_pawn state pawn (get_pawn_x pawn+ 2) (get_pawn_y pawn)
	|	dir == bottomleft = move_pawn state pawn (get_pawn_x pawn -2) (get_pawn_y pawn + 2)
	|	otherwise = move_pawn state pawn (get_pawn_x pawn) (get_pawn_y pawn +2)


jump_pawn_centerdown :: [String]->Pawn->Int->[String]
jump_pawn_centerdown state pawn dir
	|	dir == topleft = move_pawn state pawn (get_pawn_x pawn - 1) (get_pawn_y pawn - 2)
	|	dir == topright	=move_pawn state pawn (get_pawn_x pawn + 1) (get_pawn_y pawn - 2)
	|	dir == left	= move_pawn state pawn (get_pawn_x pawn -  2) (get_pawn_y pawn)
	|	dir == right = move_pawn state pawn (get_pawn_x pawn+ 2) (get_pawn_y pawn)
	|	dir == bottomleft = move_pawn state pawn (get_pawn_x pawn - 2) (get_pawn_y pawn + 2)
	|	otherwise = move_pawn state pawn (get_pawn_x pawn) (get_pawn_y pawn +2)


jump_pawn_down :: [String]->Pawn->Int->[String]
jump_pawn_down state pawn dir
	|	dir == topleft = move_pawn state pawn (get_pawn_x pawn) (get_pawn_y pawn - 2)
	|	dir == topright	=move_pawn state pawn (get_pawn_x pawn+2) (get_pawn_y pawn - 2)
	|	dir == left	= move_pawn state pawn (get_pawn_x pawn - 2) (get_pawn_y pawn)
	|	dir == right = move_pawn state pawn (get_pawn_x pawn+ 2) (get_pawn_y pawn)
	|	dir == bottomleft = move_pawn state pawn (get_pawn_x pawn-2) (get_pawn_y pawn + 2)
	|	otherwise = move_pawn state pawn (get_pawn_x pawn) (get_pawn_y pawn +2)


--moving pawn one space
slide_pawn :: [String]->Pawn->Int->Int->[String]
slide_pawn state pawn dir n 
	| 	get_pawn_y (pawn) < (n - 1 )  = slide_pawn_up state pawn dir
	|	get_pawn_y (pawn) == (n-1) = slide_pawn_center state pawn dir
	|	otherwise = slide_pawn_down state pawn dir


slide_pawn_up:: [String]->Pawn->Int->[String]
slide_pawn_up state pawn dir 
	|	dir == topleft = move_pawn state pawn (get_pawn_x pawn - 1) (get_pawn_y pawn - 1)
	|	dir == topright	=move_pawn state pawn (get_pawn_x pawn) (get_pawn_y pawn - 1)
	|	dir == left	= move_pawn state pawn (get_pawn_x pawn - 1) (get_pawn_y pawn)
	|	dir == right = move_pawn state pawn (get_pawn_x pawn+ 1) (get_pawn_y pawn)
	|	dir == bottomleft = move_pawn state pawn (get_pawn_x pawn) (get_pawn_y pawn + 1)
	|	otherwise = move_pawn state pawn (get_pawn_x pawn +1) (get_pawn_y pawn +1)



slide_pawn_center:: [String]->Pawn->Int->[String]
slide_pawn_center  state pawn dir
	|	dir == topleft  = move_pawn state pawn (get_pawn_x pawn - 1) (get_pawn_y pawn - 1)
	|	dir == topright = move_pawn state pawn  (get_pawn_x pawn) (get_pawn_y pawn - 1)
	|	dir == left = move_pawn state pawn (get_pawn_x pawn - 1) (get_pawn_y pawn)
	|	dir == right = move_pawn state pawn  (get_pawn_x pawn + 1) (get_pawn_y pawn )
	|	dir == bottomleft = move_pawn state pawn  (get_pawn_x pawn - 1) (get_pawn_y pawn + 1)
	|	dir == bottomright = move_pawn state pawn (get_pawn_x pawn) (get_pawn_y pawn + 1)


slide_pawn_down:: [String]->Pawn->Int->[String]
slide_pawn_down state pawn dir
	|	dir == topleft  = move_pawn state pawn (get_pawn_x pawn) (get_pawn_y pawn - 1)
	|	dir == topright = move_pawn state pawn (get_pawn_x pawn+1) (get_pawn_y pawn - 1)
	|	dir == left = move_pawn state pawn (get_pawn_x pawn - 1) (get_pawn_y pawn)
	|	dir == right = move_pawn state pawn (get_pawn_x pawn + 1) (get_pawn_y pawn )
	|	dir == bottomleft = move_pawn state pawn (get_pawn_x pawn - 1) (get_pawn_y pawn + 1)
	|	dir == bottomright = move_pawn state pawn (get_pawn_x pawn) (get_pawn_y pawn + 1)

move_pawn :: [String]->Pawn->Int->Int->[String]
move_pawn state pawn x y = move_pos (empty_pos state (get_pawn_x pawn) (get_pawn_y pawn)) (get_pawn_char pawn) x y
	
empty_pos :: [String]->Int->Int->[String]
empty_pos [] x y = []
empty_pos (x:xs) xpos 0 = (empty_pos_helper x xpos): xs
empty_pos (x:xs) xpos ypos = x:empty_pos xs xpos (ypos-1)

empty_pos_helper :: [Char]->Int->[Char]
empty_pos_helper [] x = []
empty_pos_helper (a:as) 0 = '-':as
empty_pos_helper (a:as) x = a : empty_pos_helper as (x-1)



move_pos::[String]->Char->Int->Int->[String]
move_pos [] pawn x y = []
move_pos (a:as) pawn x 0 = (move_pos_helper a pawn x): as
move_pos (a:as) pawn x y = a : move_pos as pawn x (y-1)

move_pos_helper::[Char]->Char->Int->[Char]
move_pos_helper [] pawn x = []
move_pos_helper (a:as) pawn 0 = pawn : as
move_pos_helper (a:as) pawn x = a : move_pos_helper as pawn (x-1)

	
--
-- Pawn getters
--
get_pawn_char :: Pawn->Char
get_pawn_char (a,b,c)= a
get_pawn_x :: Pawn->Int
get_pawn_x (a,b,c)= b
get_pawn_y :: Pawn->Int
get_pawn_y (a,b,c) = c

--
-- Pawn getters
--
get_ps_path :: PathScore->[[String]]
get_ps_path (a,b)= a
get_ps_score :: PathScore->Int
get_ps_score (a,b)= b