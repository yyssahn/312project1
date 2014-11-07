

--
-- Sample Boards generated for testing purpose
--

testBoard = ["WWW-WW-------BB-BBB"]
testBoardState =["WWW","-WW-", "-----","-BB-","BBB"]
testBoardState1 =["WWW","--W-", "-w---","-BB-","BBB"]
simpleBoardState = ["W-","-W-","BB"]
testBoardState2 =["WW-","-WW-", "--W--","-BB-","BBB"]
testBoardState3 =["WW--WW---W---BBB---"]

--
-- Custom Type declarations, Pawn represents single piece in a game and PathScore is used to evaluate Path based on scoring function
--

type Pawn = (Char,Int,Int)
type PathScore= ([[String]],Int)

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
-- PathScore getters
--
get_ps_path :: PathScore->[[String]]
get_ps_path (a,b)= a
get_ps_score :: PathScore->Int
get_ps_score (a,b)= b

--
-- Constants that is used for the porject. These are created to make coding easier
--

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


--
--
--
crusher_r4j8 ::[String]->Char->Int->Int->[String]
crusher_r4j8 state turn turnnum n = make_into_stringarray_r4j8 (crusher_r4j8_helper(make_board_r4j8 (head state) n 0) turn turnnum n [])

crusher_r4j8_helper :: [String]->Char->Int->Int->[[String]]->[[String]]
crusher_r4j8_helper state turn turnnum n path
	|	null path = crusher_r4j8_helper (get_best_next_r4j8 (backtrack_ps_r4j8 (make_pathscore_r4j8 (generate_path_r4j8 state turn turnnum n []) turn n )   )  [] ) (nextturn turn) (turnnum-1) n ([state]++      [(get_best_next_r4j8 (backtrack_ps_r4j8 (make_pathscore_r4j8 (generate_path_r4j8 state turn turnnum n []) turn n )   )  [] )]  )
	|	turnnum == 0 = path
	|	is_game_over_r4j8 (last path) n = path
	|	otherwise = crusher_r4j8_helper (last path) (nextturn turn)(turnnum -1) n (path ++ [(get_best_next_r4j8 (backtrack_ps_r4j8 (make_pathscore_r4j8 (generate_path_r4j8 (last path) turn turnnum n []) turn n )   )  [] )] )


-------------------------------------------------------------------------------
generate_path_r4j8 :: [String]->Char->Int->Int->[[[String]]]->[[[String]]]
generate_path_r4j8 state turn turnnum n path
	|	turnnum ==0 = path
	|	(length path)== 0  = generate_path_r4j8 state (nextturn turn) (turnnum-1) n (make_patharray_r4j8_helper [state] (generate_possible_state_r4j8 state turn (make_PawnList_r4j8 state 0) n) n)
	|	otherwise = generate_path_r4j8 state (nextturn turn) (turnnum -1) n (make_patharray_r4j8 path turn n)

make_patharray_r4j8:: [[[String]]]->Char->Int->[[[String]]]
make_patharray_r4j8 paths turn n 
	|	null paths = []
	|	otherwise = (make_patharray_r4j8_helper (head paths) (generate_possible_state_r4j8 (last(head paths))  turn (make_PawnList_r4j8 (last (head paths)) 0) n ) n) ++ make_patharray_r4j8 (tail paths) turn n

make_patharray_r4j8_helper :: [[String]]->[[String]]->Int->[[[String]]]
make_patharray_r4j8_helper path states n
	|	null states = []
	|	is_game_over_r4j8 (last path) n = [path]
	|	elem (head states) path = make_patharray_r4j8_helper path (tail states) n 
	|	otherwise = (reverse ( (head states):(reverse path))) : make_patharray_r4j8_helper path (tail states) n 
-------------------------------------------------------------------------------


make_pathscore_r4j8 :: [[[String]]]->Char->Int->[PathScore]
make_pathscore_r4j8 paths turn n 
	|	null paths = []
	|	otherwise = (make_pathscore_r4j8_helper (head paths) turn n) : make_pathscore_r4j8 (tail paths) turn n 

make_pathscore_r4j8_helper :: [[String]]->Char->Int->PathScore
make_pathscore_r4j8_helper path turn n = (path , get_score_r4j8 (last path) turn n)


backtrack_ps_r4j8 ::[PathScore] -> [PathScore]
backtrack_ps_r4j8 ps
	|	length(get_ps_path (head ps)) == 1 = ps
	|	length(get_ps_path (head ps)) == 2 = ps
	|	odd (length(get_ps_path (head ps))) = backtrack_ps_r4j8 (trim_pathscore_r4j8 (init_pathscore_r4j8 ps) minnum [])
	|	otherwise = backtrack_ps_r4j8 (trim_pathscore_r4j8 (init_pathscore_r4j8 ps) maxnum []) 

init_pathscore_r4j8 :: [PathScore] ->[PathScore]
init_pathscore_r4j8 ps
	|	(length(get_ps_path (head ps))) >= (length(get_ps_path (head (tail ps)))) = init_pathscore_r4j8_helper ps (length(get_ps_path (head ps)))
	|	otherwise = init_pathscore_r4j8_helper ps (length(get_ps_path (head (tail ps))))
	
init_pathscore_r4j8_helper ::[PathScore]->Int->[PathScore]
init_pathscore_r4j8_helper ps leng
	| null ps = [] 
	|	(length(get_ps_path (head ps))) < leng = (head ps) : init_pathscore_r4j8_helper (tail ps) leng 
	|	otherwise =(init ( get_ps_path (head ps)),get_ps_score (head ps)): init_pathscore_r4j8_helper (tail ps) leng
trim_pathscore_r4j8 ::[PathScore]->Int->[PathScore]->[PathScore]
trim_pathscore_r4j8 ps minmax track
	| null ps = track
	| null track = trim_pathscore_r4j8 (tail ps) minmax [(head ps)]
	| (get_ps_path(head ps)) /= (get_ps_path (head track)) = trim_pathscore_r4j8 (tail ps) minmax (head ps : track)
	| otherwise =  trim_pathscore_r4j8 (tail ps) minmax ((trim_pathscore_r4j8_helper (head ps) (head track) minmax) :tail track)

trim_pathscore_r4j8_helper::PathScore->PathScore->Int->PathScore
trim_pathscore_r4j8_helper a b minmax
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

get_score_r4j82 :: [String]->Char->Int->Int
get_score_r4j82 state pawn n
	|	pawn == 'W' && (get_pawn_count_r4j8 state 'B' < n) = 1000
	|	pawn == 'W' = (get_pawn_count_r4j8 state 'W') - (get_pawn_count_r4j8 state 'B')
	|	pawn == 'B' && (get_pawn_count_r4j8 state 'W' < n) = 1000
	|	otherwise = (get_pawn_count_r4j8 state 'B') - (get_pawn_count_r4j8 state 'W')




get_score_r4j8 :: [String]->Char->Int-> Int
get_score_r4j8 state pawn  n = (length (get_score_r4j8_helper state pawn (make_PawnList_r4j8 state 0) n) ) + get_score_r4j82 state pawn n


get_score_r4j8_helper:: [String]->Char->[Pawn]->Int->[[String]]
get_score_r4j8_helper state turn pawns n
	|	null pawns= []
	|	get_pawn_char (head pawns) /= turn = get_score_r4j8_helper state turn (tail pawns) n
	|	otherwise = (get_score_r4j8_helper_01 state (head pawns) n)++ get_score_r4j8_helper state turn (tail pawns) n

get_score_r4j8_helper_01 ::[String] ->Pawn->Int->[[String]]
get_score_r4j8_helper_01 state pawn n =
	(if (possible_jump_r4j8 state pawn n topleft) then (jump_pawn_r4j8 state pawn topleft n):[] else []) ++ 
	(if (possible_jump_r4j8 state pawn n topright) then (jump_pawn_r4j8 state pawn topright n):[] else []) ++ 
	(if (possible_jump_r4j8 state pawn n left) then (jump_pawn_r4j8 state pawn left n):[] else []) ++ 
	(if (possible_jump_r4j8 state pawn n right) then (jump_pawn_r4j8 state pawn right n):[] else []) ++ 
	(if (possible_jump_r4j8 state pawn n bottomleft) then (jump_pawn_r4j8 state pawn bottomleft n):[] else []) ++ 
	(if (possible_jump_r4j8 state pawn n bottomright) then (jump_pawn_r4j8 state pawn bottomright n):[] else [])


get_best_next_r4j8::[PathScore]->[PathScore]->[String]
get_best_next_r4j8 [] (p:ps) = get_best_next_r4j8_helper p
get_best_next_r4j8 (x:xs) [] = get_best_next_r4j8 xs [x]
get_best_next_r4j8 (x:xs) (p:ps) = if (get_ps_score x) > (get_ps_score p) then get_best_next_r4j8 xs [x] else get_best_next_r4j8 xs [p]

get_best_next_r4j8_helper::PathScore->[String]
get_best_next_r4j8_helper ps = last (get_ps_path ps)


generate_possible_state_r4j8 :: [String]->Char->[Pawn]->Int->[[String]]
generate_possible_state_r4j8 state turn pawns n
	|	null pawns= []
	|	get_pawn_char (head pawns) /= turn = generate_possible_state_r4j8 state turn (tail pawns) n
	|	otherwise = (generate_possible_state_r4j8_helper state (head pawns) n)++ generate_possible_state_r4j8 state turn (tail pawns) n



generate_possible_state_r4j8_helper ::[String] ->Pawn->Int->[[String]]
generate_possible_state_r4j8_helper state pawn n =
	(if (possible_move_r4j8 state pawn n topleft) then (slide_pawn_r4j8 state pawn topleft n):[] else []) ++ 
	(if (possible_move_r4j8 state pawn n topright) then (slide_pawn_r4j8 state pawn topright n):[] else []) ++ 
	(if (possible_move_r4j8 state pawn n left) then (slide_pawn_r4j8 state pawn left n):[] else []) ++ 
	(if (possible_move_r4j8 state pawn n right) then (slide_pawn_r4j8 state pawn right n):[] else []) ++ 
	(if (possible_move_r4j8 state pawn n bottomleft) then (slide_pawn_r4j8 state pawn bottomleft n):[] else []) ++ 
	(if (possible_move_r4j8 state pawn n bottomright) then (slide_pawn_r4j8 state pawn bottomright n):[] else []) ++ 	
	(if (possible_jump_r4j8 state pawn n topleft) then (jump_pawn_r4j8 state pawn topleft n):[] else []) ++ 
	(if (possible_jump_r4j8 state pawn n topright) then (jump_pawn_r4j8 state pawn topright n):[] else []) ++ 
	(if (possible_jump_r4j8 state pawn n left) then (jump_pawn_r4j8 state pawn left n):[] else []) ++ 
	(if (possible_jump_r4j8 state pawn n right) then (jump_pawn_r4j8 state pawn right n):[] else []) ++ 
	(if (possible_jump_r4j8 state pawn n bottomleft) then (jump_pawn_r4j8 state pawn bottomleft n):[] else []) ++ 
	(if (possible_jump_r4j8 state pawn n bottomright) then (jump_pawn_r4j8 state pawn bottomright n):[] else [])



get_pawn_count_r4j8::[String]->Char->Int
get_pawn_count_r4j8 state pawn
	|	null state = 0
	|	otherwise = (get_pawn_count_r4j8_helper (head state) pawn) + get_pawn_count_r4j8 (tail state) pawn

get_pawn_count_r4j8_helper::[Char]->Char->Int
get_pawn_count_r4j8_helper state pawn
	|	null state = 0
	|	(head state) ==pawn = 1 + get_pawn_count_r4j8_helper (tail state) pawn
	|	otherwise = get_pawn_count_r4j8_helper (tail state) pawn


is_game_over_r4j8 :: [String]->Int->Bool
is_game_over_r4j8 state n
	|	get_pawn_count_r4j8 state 'W' < n || get_pawn_count_r4j8 state 'B' < 3 = True
	|	otherwise = False


first_n_items_r4j8 ::[Char]->Int->[Char]
first_n_items_r4j8 [] n = []
first_n_items_r4j8 (x:xs) 0 = []
first_n_items_r4j8 (x:xs) n = x : (first_n_items_r4j8 xs (n-1))
 
----------------------------------------------------------------------------------------------------------
---
--- used to turn basic form of user input to the board that we can manipulate
---
make_board_r4j8 :: [Char]->Int->Int->[String]
make_board_r4j8 input n helper
	|	null input = []
	|	helper == n = make_board_r4j8_helper input n (helper -2)
	|	otherwise = (first_n_items_r4j8 input (n + helper)):make_board_r4j8 (mynthtail_r4j8 (n+helper) input) n (helper+1)





-----------------------------------------------------------------------------------------------------------
----
---- used to convert Path of states into [String] to be printed
----
make_into_stringarray_r4j8 :: [[String]]->[String]
make_into_stringarray_r4j8 [] = []
make_into_stringarray_r4j8 (x:xs) = (make_into_single_string_r4j8 x) : make_into_stringarray_r4j8 xs

make_into_single_string_r4j8 :: [String] -> String
make_into_single_string_r4j8 input
	| 	null input = []
	| 	otherwise = (head input) ++ (make_into_single_string_r4j8 (tail input))




make_PawnList_r4j8:: [String]->Int->[Pawn]
make_PawnList_r4j8 state yhelper
	| null state = []
	| otherwise = (make_PawnList_r4j8_helper (head state) 0 yhelper) ++ (make_PawnList_r4j8 (tail state) (yhelper +1))


make_PawnList_r4j8_helper :: [Char]->Int->Int->[Pawn]
make_PawnList_r4j8_helper str helper y
	|	null str = []
	|	head str /= '-'  = ((head str),helper,y) : make_PawnList_r4j8_helper (tail str) (helper+1) y  	
	| 	otherwise = make_PawnList_r4j8_helper (tail str) (helper + 1) y

make_board_r4j8_helper ::[Char]->Int->Int->[String]
make_board_r4j8_helper input n helper
	|	helper == 0 = (first_n_items_r4j8 input n ):[]
	|	otherwise =  (first_n_items_r4j8 input (n + helper)): make_board_r4j8_helper (mynthtail_r4j8 (n+helper) input)  n (helper-1)

get_Char_r4j8 :: [String]->Int->Int->Char
get_Char_r4j8 state x y
	|	null state = ' '
	|	x < 0 = ' '
	|	y == 0 && (length (head state)) > x = get_Char_r4j8_helper (head state) x
	|	y == 0 && (length (head state)) <= x = ' '
	|	otherwise = get_Char_r4j8 (tail state) x (y-1)

get_Char_r4j8_helper::[Char]->Int->Char

get_Char_r4j8_helper row x = head (mynthtail_r4j8 x row)
	
mynthtail_r4j8:: Int->[a]->[a]
mynthtail_r4j8 n list1
	| null list1 = list1
	| n == 0 = list1
	| otherwise = mynthtail_r4j8 (n-1) (tail list1)


empty_r4j8 :: [String]->Int->Int->Bool
empty_r4j8 state x y
	| null state = False
	|  x<0 || y < 0	= False
	|	(get_Char_r4j8 state x y) == '-' =True
	|	otherwise = False


--------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------

possible_jump_r4j8::[String]->Pawn->Int->Int->Bool
possible_jump_r4j8 state pawn size dir	
	|	get_pawn_y pawn < (size-2) = possible_jump_r4j8_up state pawn dir
	|	get_pawn_y pawn ==(size-2) = possible_jump_r4j8_centerup state pawn dir
	|	get_pawn_y pawn ==(size-1) = possible_jump_r4j8_center state pawn dir
	|	get_pawn_y pawn ==(size) = possible_jump_r4j8_centerdown state pawn dir
	|	get_pawn_y pawn >(size) = possible_jump_r4j8_down state pawn dir
	|	otherwise = False
--------------------------------------------------------------------------------------------------------------------------------------

possible_jump_r4j8_up ::[String]->Pawn->Int->Bool
possible_jump_r4j8_up state pawn dir
	|	dir == topleft  && (get_Char_r4j8 state (get_pawn_x pawn -1) (get_pawn_y pawn -1)) == (get_pawn_char pawn) = can_jump_r4j8 state (get_pawn_char pawn)(get_pawn_x pawn -2) (get_pawn_y pawn -2)
	|	dir == topright && (get_Char_r4j8 state (get_pawn_x pawn) (get_pawn_y pawn -1)) == (get_pawn_char pawn)= can_jump_r4j8 state (get_pawn_char pawn) (get_pawn_x pawn) (get_pawn_y pawn - 2)
	|	dir == left && (get_Char_r4j8 state (get_pawn_x pawn - 1)(get_pawn_y pawn)) == (get_pawn_char pawn) = can_jump_r4j8 state (get_pawn_char pawn)(get_pawn_x pawn - 2) (get_pawn_y pawn)
	|	dir == right && (get_Char_r4j8 state (get_pawn_x pawn + 1)(get_pawn_y pawn)) == (get_pawn_char pawn) = can_jump_r4j8 state (get_pawn_char pawn)(get_pawn_x pawn + 2) (get_pawn_y pawn)
	|	dir == bottomleft && (get_Char_r4j8 state (get_pawn_x pawn )(get_pawn_y pawn+1)) == (get_pawn_char pawn) = can_jump_r4j8 state (get_pawn_char pawn)(get_pawn_x pawn) (get_pawn_y pawn+2)
	|	dir == bottomright && (get_Char_r4j8 state (get_pawn_x pawn +1)(get_pawn_y pawn+1)) == (get_pawn_char pawn) = can_jump_r4j8 state (get_pawn_char pawn)(get_pawn_x pawn + 2) (get_pawn_y pawn+2)
	|	otherwise = False


possible_jump_r4j8_centerup ::[String]->Pawn->Int->Bool
possible_jump_r4j8_centerup state pawn dir
	|	dir == topleft  && (get_Char_r4j8 state (get_pawn_x pawn -1) (get_pawn_y pawn -1)) == (get_pawn_char pawn) = can_jump_r4j8 state (get_pawn_char pawn)(get_pawn_x pawn -2) (get_pawn_y pawn -2)
	|	dir == topright && (get_Char_r4j8 state (get_pawn_x pawn) (get_pawn_y pawn -1)) == (get_pawn_char pawn)= can_jump_r4j8 state (get_pawn_char pawn) (get_pawn_x pawn) (get_pawn_y pawn - 2)
	|	dir == left && (get_Char_r4j8 state (get_pawn_x pawn - 1)(get_pawn_y pawn)) == (get_pawn_char pawn) = can_jump_r4j8 state (get_pawn_char pawn)(get_pawn_x pawn - 2) (get_pawn_y pawn)
	|	dir == right && (get_Char_r4j8 state (get_pawn_x pawn + 1)(get_pawn_y pawn)) == (get_pawn_char pawn) = can_jump_r4j8 state (get_pawn_char pawn)(get_pawn_x pawn + 2) (get_pawn_y pawn)
	|	dir == bottomleft && (get_Char_r4j8 state (get_pawn_x pawn )(get_pawn_y pawn+1)) == (get_pawn_char pawn) = can_jump_r4j8 state (get_pawn_char pawn)(get_pawn_x pawn - 1) (get_pawn_y pawn+2)
	|	dir == bottomright && (get_Char_r4j8 state (get_pawn_x pawn +1)(get_pawn_y pawn+1)) == (get_pawn_char pawn) = can_jump_r4j8 state (get_pawn_char pawn)(get_pawn_x pawn + 1) (get_pawn_y pawn+2)
	|	otherwise = False


possible_jump_r4j8_center ::[String]->Pawn->Int->Bool
possible_jump_r4j8_center state pawn dir
	|	dir == topleft  && (get_Char_r4j8 state (get_pawn_x pawn -1) (get_pawn_y pawn -1)) == (get_pawn_char pawn) = can_jump_r4j8 state (get_pawn_char pawn)(get_pawn_x pawn -2) (get_pawn_y pawn -2)
	|	dir == topright && (get_Char_r4j8 state (get_pawn_x pawn) (get_pawn_y pawn -1)) == (get_pawn_char pawn)= can_jump_r4j8 state (get_pawn_char pawn) (get_pawn_x pawn) (get_pawn_y pawn - 2)
	|	dir == left && (get_Char_r4j8 state (get_pawn_x pawn - 1)(get_pawn_y pawn)) == (get_pawn_char pawn) = can_jump_r4j8 state (get_pawn_char pawn)(get_pawn_x pawn - 2) (get_pawn_y pawn)
	|	dir == right && (get_Char_r4j8 state (get_pawn_x pawn + 1)(get_pawn_y pawn)) == (get_pawn_char pawn) = can_jump_r4j8 state (get_pawn_char pawn)(get_pawn_x pawn + 2) (get_pawn_y pawn)
	|	dir == bottomleft && (get_Char_r4j8 state (get_pawn_x pawn - 1)(get_pawn_y pawn+1)) == (get_pawn_char pawn) = can_jump_r4j8 state (get_pawn_char pawn)(get_pawn_x pawn - 2) (get_pawn_y pawn+2)
	|	dir == bottomright && (get_Char_r4j8 state (get_pawn_x pawn)(get_pawn_y pawn+1)) == (get_pawn_char pawn) = can_jump_r4j8 state (get_pawn_char pawn)(get_pawn_x pawn) (get_pawn_y pawn+2)
	|	otherwise = False



possible_jump_r4j8_centerdown ::[String]->Pawn->Int->Bool
possible_jump_r4j8_centerdown state pawn dir
	|	dir == topleft  && (get_Char_r4j8 state (get_pawn_x pawn) (get_pawn_y pawn -1)) == (get_pawn_char pawn) = can_jump_r4j8 state (get_pawn_char pawn)(get_pawn_x pawn -1) (get_pawn_y pawn -2)
	|	dir == topright && (get_Char_r4j8 state (get_pawn_x pawn+1) (get_pawn_y pawn -1)) == (get_pawn_char pawn)= can_jump_r4j8 state (get_pawn_char pawn) (get_pawn_x pawn+1) (get_pawn_y pawn - 2)
	|	dir == left && (get_Char_r4j8 state (get_pawn_x pawn - 1)(get_pawn_y pawn)) == (get_pawn_char pawn) = can_jump_r4j8 state (get_pawn_char pawn)(get_pawn_x pawn - 2) (get_pawn_y pawn)
	|	dir == right && (get_Char_r4j8 state (get_pawn_x pawn + 1)(get_pawn_y pawn)) == (get_pawn_char pawn) = can_jump_r4j8 state (get_pawn_char pawn)(get_pawn_x pawn + 2) (get_pawn_y pawn)
	|	dir == bottomleft && (get_Char_r4j8 state (get_pawn_x pawn - 1)(get_pawn_y pawn+1)) == (get_pawn_char pawn) = can_jump_r4j8 state (get_pawn_char pawn)(get_pawn_x pawn - 2) (get_pawn_y pawn+2)
	|	dir == bottomright && (get_Char_r4j8 state (get_pawn_x pawn)(get_pawn_y pawn+1)) == (get_pawn_char pawn) = can_jump_r4j8 state (get_pawn_char pawn)(get_pawn_x pawn) (get_pawn_y pawn+2)
	|	otherwise = False

possible_jump_r4j8_down ::[String]->Pawn->Int->Bool
possible_jump_r4j8_down state pawn dir
	|	dir == topleft  && (get_Char_r4j8 state (get_pawn_x pawn) (get_pawn_y pawn -1)) == (get_pawn_char pawn) = can_jump_r4j8 state (get_pawn_char pawn)(get_pawn_x pawn) (get_pawn_y pawn -2)
	|	dir == topright && (get_Char_r4j8 state (get_pawn_x pawn+1) (get_pawn_y pawn -1)) == (get_pawn_char pawn)= can_jump_r4j8 state (get_pawn_char pawn) (get_pawn_x pawn+2) (get_pawn_y pawn - 2)
	|	dir == left && (get_Char_r4j8 state (get_pawn_x pawn - 1)(get_pawn_y pawn)) == (get_pawn_char pawn) = can_jump_r4j8 state (get_pawn_char pawn)(get_pawn_x pawn - 2) (get_pawn_y pawn)
	|	dir == right && (get_Char_r4j8 state (get_pawn_x pawn + 1)(get_pawn_y pawn)) == (get_pawn_char pawn) = can_jump_r4j8 state (get_pawn_char pawn)(get_pawn_x pawn + 2) (get_pawn_y pawn)
	|	dir == bottomleft && (get_Char_r4j8 state (get_pawn_x pawn - 1)(get_pawn_y pawn+1)) == (get_pawn_char pawn) = can_jump_r4j8 state (get_pawn_char pawn)(get_pawn_x pawn - 2) (get_pawn_y pawn+2)
	|	dir == bottomright && (get_Char_r4j8 state (get_pawn_x pawn)(get_pawn_y pawn+1)) == (get_pawn_char pawn) = can_jump_r4j8 state (get_pawn_char pawn)(get_pawn_x pawn) (get_pawn_y pawn+2)
	|	otherwise = False


can_jump_r4j8 ::[String]->Char->Int->Int->Bool
can_jump_r4j8 state pawn x y
	|	x<0||y<0 =False
	|	(get_Char_r4j8 state x y) == pawn || (get_Char_r4j8 state x y) == ' ' = False 
	|	otherwise = True
--------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------
--------------------------------------------------------------------------------------------------------------------------------------




	
possible_move_r4j8 ::[String]->Pawn->Int->Int->Bool
possible_move_r4j8 state pawn size dir
	|	get_pawn_y pawn < (size-1) = possible_move_r4j8_up state pawn dir
	|	get_pawn_y pawn == (size-1) = possible_move_r4j8_center state pawn dir
	|	get_pawn_y pawn >= size = possible_move_r4j8_down state pawn dir

possible_move_r4j8_up ::[String]->Pawn->Int->Bool
possible_move_r4j8_up state pawn dir
	|	dir == topleft  = empty_r4j8 state (get_pawn_x pawn - 1) (get_pawn_y pawn - 1)
	|	dir == topright = empty_r4j8 state (get_pawn_x pawn) (get_pawn_y pawn - 1)
	|	dir == left = empty_r4j8 state (get_pawn_x pawn - 1) (get_pawn_y pawn)
	|	dir == right = empty_r4j8 state (get_pawn_x pawn + 1) (get_pawn_y pawn )
	|	dir == bottomleft = empty_r4j8 state (get_pawn_x pawn) (get_pawn_y pawn + 1)
	|	dir == bottomright = empty_r4j8 state (get_pawn_x pawn + 1) (get_pawn_y pawn + 1)


possible_move_r4j8_center::[String]->Pawn->Int->Bool
possible_move_r4j8_center state pawn dir
	|	dir == topleft  = empty_r4j8 state (get_pawn_x pawn - 1) (get_pawn_y pawn - 1)
	|	dir == topright = empty_r4j8 state (get_pawn_x pawn) (get_pawn_y pawn - 1)
	|	dir == left = empty_r4j8 state (get_pawn_x pawn - 1) (get_pawn_y pawn)
	|	dir == right = empty_r4j8 state (get_pawn_x pawn + 1) (get_pawn_y pawn )
	|	dir == bottomleft = empty_r4j8 state (get_pawn_x pawn - 1) (get_pawn_y pawn + 1)
	|	dir == bottomright = empty_r4j8 state (get_pawn_x pawn) (get_pawn_y pawn + 1)


possible_move_r4j8_down ::[String]->Pawn->Int->Bool
possible_move_r4j8_down state pawn dir
	|	dir == topleft  = empty_r4j8 state (get_pawn_x pawn) (get_pawn_y pawn - 1)
	|	dir == topright = empty_r4j8 state (get_pawn_x pawn+1) (get_pawn_y pawn - 1)
	|	dir == left = empty_r4j8 state (get_pawn_x pawn - 1) (get_pawn_y pawn)
	|	dir == right = empty_r4j8 state (get_pawn_x pawn + 1) (get_pawn_y pawn )
	|	dir == bottomleft = empty_r4j8 state (get_pawn_x pawn - 1) (get_pawn_y pawn + 1)
	|	dir == bottomright = empty_r4j8 state (get_pawn_x pawn) (get_pawn_y pawn + 1)
	


------
----
-----


--jumping pawn 
jump_pawn_r4j8 :: [String]->Pawn->Int->Int->[String]
jump_pawn_r4j8 state pawn dir n
	| 	get_pawn_y (pawn) < (n - 2 )  = jump_pawn_r4j8_up state pawn dir
	|	get_pawn_y (pawn) == (n-2) = jump_pawn_r4j8_centerup state pawn dir
	|	get_pawn_y (pawn) == (n-1) = jump_pawn_r4j8_center state pawn dir
	|	get_pawn_y (pawn) == n = jump_pawn_r4j8_centerdown state pawn dir
	|	otherwise = jump_pawn_r4j8_down state pawn dir

jump_pawn_r4j8_up :: [String]->Pawn->Int->[String]
jump_pawn_r4j8_up state pawn dir
	|	dir == topleft = move_pawn_r4j8 state pawn (get_pawn_x pawn - 2) (get_pawn_y pawn - 2)
	|	dir == topright	=move_pawn_r4j8 state pawn (get_pawn_x pawn) (get_pawn_y pawn - 2)
	|	dir == left	= move_pawn_r4j8 state pawn (get_pawn_x pawn - 2) (get_pawn_y pawn)
	|	dir == right = move_pawn_r4j8 state pawn (get_pawn_x pawn+ 2) (get_pawn_y pawn)
	|	dir == bottomleft = move_pawn_r4j8 state pawn (get_pawn_x pawn) (get_pawn_y pawn + 2)
	|	otherwise = move_pawn_r4j8 state pawn (get_pawn_x pawn +2) (get_pawn_y pawn +2)


jump_pawn_r4j8_centerup :: [String]->Pawn->Int->[String]
jump_pawn_r4j8_centerup state pawn dir
	|	dir == topleft = move_pawn_r4j8 state pawn (get_pawn_x pawn - 2) (get_pawn_y pawn - 2)
	|	dir == topright	=move_pawn_r4j8 state pawn (get_pawn_x pawn) (get_pawn_y pawn - 2)
	|	dir == left	= move_pawn_r4j8 state pawn (get_pawn_x pawn - 2) (get_pawn_y pawn)
	|	dir == right = move_pawn_r4j8 state pawn (get_pawn_x pawn+ 2) (get_pawn_y pawn)
	|	dir == bottomleft = move_pawn_r4j8 state pawn (get_pawn_x pawn-1) (get_pawn_y pawn + 2)
	|	otherwise = move_pawn_r4j8 state pawn (get_pawn_x pawn +1) (get_pawn_y pawn +2)


jump_pawn_r4j8_center :: [String]->Pawn->Int->[String]
jump_pawn_r4j8_center state pawn dir
	|	dir == topleft = move_pawn_r4j8 state pawn (get_pawn_x pawn - 2) (get_pawn_y pawn - 2)
	|	dir == topright	=move_pawn_r4j8 state pawn (get_pawn_x pawn) (get_pawn_y pawn - 2)
	|	dir == left	= move_pawn_r4j8 state pawn (get_pawn_x pawn - 2) (get_pawn_y pawn)
	|	dir == right = move_pawn_r4j8 state pawn (get_pawn_x pawn+ 2) (get_pawn_y pawn)
	|	dir == bottomleft = move_pawn_r4j8 state pawn (get_pawn_x pawn -2) (get_pawn_y pawn + 2)
	|	otherwise = move_pawn_r4j8 state pawn (get_pawn_x pawn) (get_pawn_y pawn +2)


jump_pawn_r4j8_centerdown :: [String]->Pawn->Int->[String]
jump_pawn_r4j8_centerdown state pawn dir
	|	dir == topleft = move_pawn_r4j8 state pawn (get_pawn_x pawn - 1) (get_pawn_y pawn - 2)
	|	dir == topright	=move_pawn_r4j8 state pawn (get_pawn_x pawn + 1) (get_pawn_y pawn - 2)
	|	dir == left	= move_pawn_r4j8 state pawn (get_pawn_x pawn -  2) (get_pawn_y pawn)
	|	dir == right = move_pawn_r4j8 state pawn (get_pawn_x pawn+ 2) (get_pawn_y pawn)
	|	dir == bottomleft = move_pawn_r4j8 state pawn (get_pawn_x pawn - 2) (get_pawn_y pawn + 2)
	|	otherwise = move_pawn_r4j8 state pawn (get_pawn_x pawn) (get_pawn_y pawn +2)


jump_pawn_r4j8_down :: [String]->Pawn->Int->[String]
jump_pawn_r4j8_down state pawn dir
	|	dir == topleft = move_pawn_r4j8 state pawn (get_pawn_x pawn) (get_pawn_y pawn - 2)
	|	dir == topright	=move_pawn_r4j8 state pawn (get_pawn_x pawn+2) (get_pawn_y pawn - 2)
	|	dir == left	= move_pawn_r4j8 state pawn (get_pawn_x pawn - 2) (get_pawn_y pawn)
	|	dir == right = move_pawn_r4j8 state pawn (get_pawn_x pawn+ 2) (get_pawn_y pawn)
	|	dir == bottomleft = move_pawn_r4j8 state pawn (get_pawn_x pawn-2) (get_pawn_y pawn + 2)
	|	otherwise = move_pawn_r4j8 state pawn (get_pawn_x pawn) (get_pawn_y pawn +2)


--moving pawn one space
slide_pawn_r4j8 :: [String]->Pawn->Int->Int->[String]
slide_pawn_r4j8 state pawn dir n 
	| 	get_pawn_y (pawn) < (n - 1 )  = slide_pawn_r4j8_up state pawn dir
	|	get_pawn_y (pawn) == (n-1) = slide_pawn_r4j8_center state pawn dir
	|	otherwise = slide_pawn_r4j8_down state pawn dir


slide_pawn_r4j8_up:: [String]->Pawn->Int->[String]
slide_pawn_r4j8_up state pawn dir 
	|	dir == topleft = move_pawn_r4j8 state pawn (get_pawn_x pawn - 1) (get_pawn_y pawn - 1)
	|	dir == topright	=move_pawn_r4j8 state pawn (get_pawn_x pawn) (get_pawn_y pawn - 1)
	|	dir == left	= move_pawn_r4j8 state pawn (get_pawn_x pawn - 1) (get_pawn_y pawn)
	|	dir == right = move_pawn_r4j8 state pawn (get_pawn_x pawn+ 1) (get_pawn_y pawn)
	|	dir == bottomleft = move_pawn_r4j8 state pawn (get_pawn_x pawn) (get_pawn_y pawn + 1)
	|	otherwise = move_pawn_r4j8 state pawn (get_pawn_x pawn +1) (get_pawn_y pawn +1)



slide_pawn_r4j8_center:: [String]->Pawn->Int->[String]
slide_pawn_r4j8_center  state pawn dir
	|	dir == topleft  = move_pawn_r4j8 state pawn (get_pawn_x pawn - 1) (get_pawn_y pawn - 1)
	|	dir == topright = move_pawn_r4j8 state pawn  (get_pawn_x pawn) (get_pawn_y pawn - 1)
	|	dir == left = move_pawn_r4j8 state pawn (get_pawn_x pawn - 1) (get_pawn_y pawn)
	|	dir == right = move_pawn_r4j8 state pawn  (get_pawn_x pawn + 1) (get_pawn_y pawn )
	|	dir == bottomleft = move_pawn_r4j8 state pawn  (get_pawn_x pawn - 1) (get_pawn_y pawn + 1)
	|	dir == bottomright = move_pawn_r4j8 state pawn (get_pawn_x pawn) (get_pawn_y pawn + 1)


slide_pawn_r4j8_down:: [String]->Pawn->Int->[String]
slide_pawn_r4j8_down state pawn dir
	|	dir == topleft  = move_pawn_r4j8 state pawn (get_pawn_x pawn) (get_pawn_y pawn - 1)
	|	dir == topright = move_pawn_r4j8 state pawn (get_pawn_x pawn+1) (get_pawn_y pawn - 1)
	|	dir == left = move_pawn_r4j8 state pawn (get_pawn_x pawn - 1) (get_pawn_y pawn)
	|	dir == right = move_pawn_r4j8 state pawn (get_pawn_x pawn + 1) (get_pawn_y pawn )
	|	dir == bottomleft = move_pawn_r4j8 state pawn (get_pawn_x pawn - 1) (get_pawn_y pawn + 1)
	|	dir == bottomright = move_pawn_r4j8 state pawn (get_pawn_x pawn) (get_pawn_y pawn + 1)

move_pawn_r4j8 :: [String]->Pawn->Int->Int->[String]
move_pawn_r4j8 state pawn x y = move_pos_r4j8 (empty_r4j8_pos state (get_pawn_x pawn) (get_pawn_y pawn)) (get_pawn_char pawn) x y
	
empty_r4j8_pos :: [String]->Int->Int->[String]
empty_r4j8_pos [] x y = []
empty_r4j8_pos (x:xs) xpos 0 = (empty_r4j8_pos_helper x xpos): xs
empty_r4j8_pos (x:xs) xpos ypos = x:empty_r4j8_pos xs xpos (ypos-1)

empty_r4j8_pos_helper :: [Char]->Int->[Char]
empty_r4j8_pos_helper [] x = []
empty_r4j8_pos_helper (a:as) 0 = '-':as
empty_r4j8_pos_helper (a:as) x = a : empty_r4j8_pos_helper as (x-1)



move_pos_r4j8::[String]->Char->Int->Int->[String]
move_pos_r4j8 [] pawn x y = []
move_pos_r4j8 (a:as) pawn x 0 = (move_pos_r4j8_helper a pawn x): as
move_pos_r4j8 (a:as) pawn x y = a : move_pos_r4j8 as pawn x (y-1)

move_pos_r4j8_helper::[Char]->Char->Int->[Char]
move_pos_r4j8_helper [] pawn x = []
move_pos_r4j8_helper (a:as) pawn 0 = pawn : as
move_pos_r4j8_helper (a:as) pawn x = a : move_pos_r4j8_helper as pawn (x-1)

	