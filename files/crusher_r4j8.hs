
testBoard = ["WWW-WW-------BB-BBB"]

crusher_r4j8 ::[String]->Char->Int->Int->[[String]]
crusher_r4j8 state team numMove numN = reverse(state_search_r4j8 [state] team numMove numN)


state_search_r4j8 ::[[String]] ->Char->Int->Int->[[String]]
state_search_r4j8 path team numMove numN  = path

--minimax_r4j8 ::[String]->Char->Int->Int	
