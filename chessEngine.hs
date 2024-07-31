import Data.List (sortBy)
import Data.List (elemIndex)
import Data.Maybe
import Data.Ord (comparing)

type Location = (Char, Integer)
data Player = White | Black deriving (Show, Eq)
data Piece = P Location | N Location | K Location | Q Location | R Location
						| B Location deriving (Show, Eq)
type Board = (Player, [Piece], [Piece])

charToString :: Char -> String
charToString c = [c]

setBoard :: Board
setBoard 	= (White, [R ('h',1),N ('g',1),B ('f',1),K ('e',1),
				Q ('d',1),B ('c',1),N ('b',1),R ('a',1),
				P ('h',2),P ('g',2),P ('f',2),P ('e',2),
				P ('d',2),P ('c',2),P ('b',2),P ('a',2)] ,
				[R ('h',8),N ('g',8),B ('f',8),K ('e',8),
				Q ('d',8),B ('c',8),N ('b',8),R ('a',8),
				P ('h',7),P ('g',7),P ('f',7),P ('e',7),
				P ('d',7),P ('c',7),P ('b',7),P ('a',7)])
	

allPosList = [ (8,"a"),(8,"b"),(8,"c"),(8,"d"),(8,"e"),(8,"f"),(8,"g"),(8,"h"),
			   (7,"a"),(7,"b"),(7,"c"),(7,"d"),(7,"e"),(7,"f"),(7,"g"),(7,"h"),
			   (6,"a"),(6,"b"),(6,"c"),(6,"d"),(6,"e"),(6,"f"),(6,"g"),(6,"h"),
			   (5,"a"),(5,"b"),(5,"c"),(5,"d"),(5,"e"),(5,"f"),(5,"g"),(5,"h"),
			   (4,"a"),(4,"b"),(4,"c"),(4,"d"),(4,"e"),(4,"f"),(4,"g"),(4,"h"),
			   (3,"a"),(3,"b"),(3,"c"),(3,"d"),(3,"e"),(3,"f"),(3,"g"),(3,"h"),
			   (2,"a"),(2,"b"),(2,"c"),(2,"d"),(2,"e"),(2,"f"),(2,"g"),(2,"h"),
			   (1,"a"),(1,"b"),(1,"c"),(1,"d"),(1,"e"),(1,"f"),(1,"g"),(1,"h")]

visualizeBoard (p,w,b) = putStrLn ("   a     b     c     d     e     f     g     h \n" ++ toString (loop (p,w,b) allPosList) 0 8 ++ "\n \n Turn: " ++ getColor (p,w,b))

toString [] i j = ""
toString (h:t) i j | i==0 = (show j) ++ h ++ (toString t (i+1) j)
				   | i/=8 = h ++ (toString t (i+1) j)
				   | otherwise = "\n" ++ (toString (h:t) 0 (j-1))

loop (p,w,b) [] = []		   
loop (p,w,b) allPosList = (searchPos (p,w,b) (head allPosList)) ++ (loop (p,w,b) (tail allPosList))
	
	
	
searchPos (p,[],[]) (n,c) = ["|    |"]

searchPos (p,(hw:tw),[]) (n,c)  | charToString (charGetter hw) == c && (intGetter hw) == n = [ ("| " ++ (pieceGetter hw) ++ "W |") ]
								| otherwise = searchPos (p,(tw),[]) (n,c)
	
searchPos (p,[],(hb:tb)) (n,c) | charToString (charGetter hb) == c && (intGetter hb) == n = [ ("| " ++ (pieceGetter hb) ++ "B |") ]
							   | otherwise = searchPos (p,[],(tb)) (n,c)

searchPos (p,(hw:tw),(hb:tb)) (n,c) | charToString (charGetter hw) == c && (intGetter hw) == n = [ ("| " ++ (pieceGetter hw) ++ "W |") ]
									| charToString (charGetter hb) == c && (intGetter hb) == n = [ ("| " ++ (pieceGetter hb) ++ "B |") ]
									| otherwise = searchPos (p,(tw),(tb)) (n,c)




charGetter (R (c,n)) = c
charGetter (B (c,n)) = c
charGetter (K (c,n)) = c
charGetter (P (c,n)) = c
charGetter (Q (c,n)) = c
charGetter (N (c,n)) = c

intGetter (R (c,n)) = n
intGetter (B (c,n)) = n
intGetter (K (c,n)) = n
intGetter (P (c,n)) = n
intGetter (Q (c,n)) = n
intGetter (N (c,n)) = n

pieceGetter (R _) = "R"
pieceGetter (B _) = "B"
pieceGetter (K _) = "K"
pieceGetter (P _) = "P"
pieceGetter (Q _) = "Q"
pieceGetter (N _) = "N"

locationGetter (R (c,n)) = (c,n)
locationGetter (B (c,n)) = (c,n)
locationGetter (K (c,n)) = (c,n)
locationGetter (P (c,n)) = (c,n)
locationGetter (Q (c,n)) = (c,n)
locationGetter (N (c,n)) = (c,n)	


suggestMove (R (c,n)) (p,(hw:tw),(hb:tb)) = suggestMoveRook (R (c,n)) (p,(hw:tw),(hb:tb)) [(c,1), (c,2), (c,3), (c,4), (c,5), (c,6), (c,7), (c,8), ('a',n), ('b',n), ('c',n), ('d',n), ('e',n), ('f',n), ('g',n), ('h',n)] 
suggestMove (K (c,n)) (p,(hw:tw),(hb:tb)) = suggestMoveKing (K (c,n)) (p,(hw:tw),(hb:tb)) [(c,n+1),(c,n-1),((addChar c 1),n), ((addChar c (-1)),n),((addChar c 1),n+1),((addChar c (-1)),n+1),((addChar c 1),n-1),((addChar c (-1)),n-1)]
suggestMove  (B (c,n)) (p,(hw:tw),(hb:tb)) = suggestMoveBishop (B (c,n)) (p,(hw:tw),(hb:tb)) [(addChar c 1,n+1),(addChar c 2,n+2),(addChar c 3,n+3),(addChar c 4,n+4),(addChar c 5,n+5),(addChar c 6,n+6),(addChar c 7,n+7),
																								(addChar c (-1),n+1),(addChar c (-2),n+2),(addChar c (-3),n+3),(addChar c (-4),n+4),(addChar c (-5),n+5),(addChar c (-6),n+6),(addChar c (-7),n+7),
																								(addChar c (-1),n-1),(addChar c (-2),n-2),(addChar c (-3),n-3),(addChar c (-4),n-4),(addChar c (-5),n-5),(addChar c (-6),n-6),(addChar c (-7),n-7),
																								(addChar c 1,n-1),(addChar c 2,n-2),(addChar c 3,n-3),(addChar c 4,n-4),(addChar c 5,n-5),(addChar c 6,n-6),(addChar c 7,n-7)]
suggestMove  (Q (c,n)) (p,(hw:tw),(hb:tb)) = (suggestMove (B (c,n)) (p,(hw:tw),(hb:tb))) ++ (suggestMove (R (c,n)) (p,(hw:tw),(hb:tb)))
suggestMove  (P (c,n)) (p,(hw:tw),(hb:tb)) = suggestMovePawn (P (c,n)) (p,(hw:tw),(hb:tb)) [(c,n+1),(addChar c 1,n+1),(addChar c (-1),n+1),(c,n+2)]
suggestMove  (N (c,n)) (p,(hw:tw),(hb:tb)) = suggestMoveKnight (N (c,n)) (p,(hw:tw),(hb:tb)) [(addChar c 1,n+2),(addChar c (-1),n+2),(addChar c 1,n-2),(addChar c (-1),n-2),(addChar c (-2),n+1),(addChar c (-2),n-1),(addChar c 2,n+1),(addChar c 2,n-1)]

suggestMovePawn (P (c,n)) (p,(hw:tw),(hb:tb)) [] = []
suggestMovePawn (P (c,n)) (p,(hw:tw),(hb:tb)) l   | isLegal (P (c,n)) (p,(hw:tw),(hb:tb)) (head l) = [head l] ++ (suggestMovePawn (P (c,n)) (p,(hw:tw),(hb:tb)) (tail l))
												  | otherwise = (suggestMovePawn (P (c,n)) (p,(hw:tw),(hb:tb)) (tail l))

suggestMoveRook (R (c,n)) (p,(hw:tw),(hb:tb)) [] = []
suggestMoveRook (R (c,n)) (p,(hw:tw),(hb:tb)) l | isLegal (R (c,n)) (p,(hw:tw),(hb:tb)) (head l) = [head l] ++ (suggestMoveRook (R (c,n)) (p,(hw:tw),(hb:tb)) (tail l))
												| otherwise = (suggestMoveRook (R (c,n)) (p,(hw:tw),(hb:tb)) (tail l))




suggestMoveBishop (B (c,n)) (p,(hw:tw),(hb:tb)) [] = []
suggestMoveBishop (B (c,n)) (p,(hw:tw),(hb:tb)) l | isLegal (B (c,n)) (p,(hw:tw),(hb:tb)) (head l) = [head l] ++ (suggestMoveBishop (B (c,n)) (p,(hw:tw),(hb:tb)) (tail l))
												  | otherwise = (suggestMoveBishop (B (c,n)) (p,(hw:tw),(hb:tb)) (tail l))

suggestMoveKing (K (c,n)) (p,(hw:tw),(hb:tb)) [] = []
suggestMoveKing (K (c,n)) (p,(hw:tw),(hb:tb)) l | (isLegal (K (c,n)) (p,(hw:tw),(hb:tb)) (head l)) = [head l] ++ (suggestMoveKing (K (c,n)) (p,(hw:tw),(hb:tb)) (tail l))
												| otherwise = (suggestMoveKing (K (c,n)) (p,(hw:tw),(hb:tb)) (tail l))




suggestMoveKnight (N (c,n)) (p,(hw:tw),(hb:tb)) [] = []
suggestMoveKnight (N (c,n)) (p,(hw:tw),(hb:tb)) l  | isLegal (N (c,n)) (p,(hw:tw),(hb:tb)) (head l) = [head l] ++ (suggestMoveKnight (N (c,n)) (p,(hw:tw),(hb:tb)) (tail l))
												   | otherwise = (suggestMoveKnight (N (c,n)) (p,(hw:tw),(hb:tb)) (tail l))



addChar c i = addCharHelper c i ['a','b','c','d','e','f','g','h']
addCharHelper c i l | (fromJust $ elemIndex c l) + i > ((length l)-1) ||(fromJust $ elemIndex c l) + i < 0  = 'z'
					| otherwise = l!!((fromJust $ elemIndex c l) + i)

compareChar c1 c2 = compareCharHelper c1 c2 ['a','b','c','d','e','f','g','h']
compareCharHelper c1 c2 l = (fromJust $ elemIndex c1 l) < (fromJust $ elemIndex c2 l)

			
isLegal:: Piece -> Board -> Location -> Bool
isLegal p (pi, w, b) (i,j) | not (j >=1 && j <= 8 && (inBetween 'a' 'h' i)) = False
						   | findColor (pi, w, b) (locationGetter p) == findColor (pi, w, b) (i,j) = False
						   | pieceGetter p == "R" = rookMoves (locationGetter p) (i,j) (pi, w, b)
                           | pieceGetter p == "B" = bishopMoves (locationGetter p) (i,j) (pi, w, b)
						   | pieceGetter p == "K" = kingMoves (locationGetter p) (i,j) (pi, w, b)
						   | pieceGetter p == "P" = pawnMoves (locationGetter p) (i,j) (pi, w, b)
						   | pieceGetter p == "Q" = queenMoves (locationGetter p) (i,j) (pi, w, b)
						   | pieceGetter p == "N" = knightMoves (locationGetter p) (i,j) (pi, w, b)

	
knightMoves (i,j) (ri,rj) (pi, w, b)  | rj == j+2 && ri == (addChar i 1) = True
                                      | rj == j+2 && ri == (addChar i (-1)) = True
						              | rj == (j-2) && ri == (addChar i 1) = True
						              | rj == (j-2) && ri == (addChar i (-1)) = True
									  | rj == j+1 && ri == (addChar i 2) = True
									  | rj == j+1 && ri == (addChar i (-2)) = True
									  | rj == j-1 && ri == (addChar i 2) = True
									  | rj == j-1 && ri == (addChar i (-2)) = True
						              | otherwise = False
							  							  
pawnMoves (i,j) (ri,rj) (pi, w, b)  | findColor (pi, w, b) (i,j) == "White" = movePawnWhite (i,j) (ri,rj) (pi, w, b)
									| findColor (pi, w, b) (i,j) == "Black" = movePawnBlack (i,j) (ri,rj) (pi, w, b)
									
movePawnWhite (i,j) (ri,rj) (pi, w, b) | i == ri && rj == (j+1) && isOccupied (i,j+1) (pi, w, b) = False 
									   | i == ri && rj == (j+1) = True
						               | ri == (nextChar i) &&  rj == j+1 && isOccupied ((nextChar i), (j+1)) (pi, w, b) = True
						               | ri == (prevChar i) &&  rj == j+1 && isOccupied ((prevChar i), (j-1)) (pi, w, b) = True
									   | j==2 && rj == j+2 && not (isOccupied (i,j+2) (pi, w, b)) && not (isOccupied (i,j+1) (pi, w, b)) = True
                                       | otherwise = False
									   
movePawnBlack (i,j) (ri,rj) (pi, w, b) | i == ri && rj == (j-1) && isOccupied (i,j+1) (pi, w, b) = False 
									   | i == ri && rj == (j-1) = True
						               | ri == (nextChar i) &&  rj == j-1 && isOccupied ((nextChar i), (j-1)) (pi, w, b) = True
						               | ri == (prevChar i) &&  rj == j-1 && isOccupied ((prevChar i), (j-1)) (pi, w, b) = True
									   | j==7 && rj == j-2 && not (isOccupied (i,j-2) (pi, w, b)) && not (isOccupied (i,j-1) (pi, w, b)) = True
                                       | otherwise = False

rookMoves (i,j) (ri,rj) (pi, w, b)  | i == ri && rj>j = sameColumnRookUp (i,j) (ri,rj) (pi, w, b)
									| i == ri && rj<j = sameColumnRookDown (i,j) (ri,rj) (pi, w, b)
                                    | j == rj && compareChar i ri = sameRowRookRight (i,j) (ri,rj) (pi, w, b)
									| j == rj && compareChar ri i = sameRowRookLeft (i,j) (ri,rj) (pi, w, b)
						            | otherwise = False
									
sameColumnRookUp (i,j) (ri,rj) (pi, w, b)  | (j >=1 && j <= 8 && (inBetween 'a' 'h' i)) /= True = False
										   | rj == (j+1) = True
										   | isOccupied (i,j+1) (pi, w, b) = False
										   | otherwise = sameColumnRookUp (i,j+1) (ri,rj) (pi, w, b)

sameColumnRookDown (i,j) (ri,rj) (pi, w, b) | (j >=1 && j <= 8 && (inBetween 'a' 'h' i)) /= True = False
											| rj == (j-1) = True
										    | (isOccupied (i,(j-1)) (pi, w, b)) = False
										    | otherwise = sameColumnRookDown (i,j-1) (ri,rj) (pi, w, b) 
											
sameRowRookRight (i,j) (ri,rj) (pi, w, b) | (j >=1 && j <= 8 && (inBetween 'a' 'h' i)) /= True = False
										  | ri == addChar i 1 = True
										  | isOccupied (addChar i 1,j) (pi, w, b) = False
										  | otherwise = sameRowRookRight (addChar i 1,j) (ri,rj) (pi, w, b)
										  
sameRowRookLeft (i,j) (ri,rj) (pi, w, b)  | (j >=1 && j <= 8 && (inBetween 'a' 'h' i)) /= True = False
										  | ri == addChar i (-1) = True
										  | isOccupied (addChar i (-1),j) (pi, w, b) = False
										  | otherwise = sameRowRookLeft (addChar i (-1),j) (ri,rj) (pi, w, b)
			
bishopMoves (i,j) (ri,rj) (pi, w, b) |rj > j &&  compareChar i ri = diagonalUpRight (i,j) (ri,rj) (pi, w, b)
                                     |rj > j &&  compareChar ri i = diagonalUpLeft (i,j) (ri,rj) (pi, w, b)
									 |rj < j &&  compareChar i ri = diagonalDownRight (i,j) (ri,rj) (pi, w, b)
									 |rj < j &&  compareChar ri i = diagonalDownLeft (i,j) (ri,rj) (pi, w, b)
                                     |otherwise = False
							
queenMoves  (i,j) (ri,rj) (pi, w, b) |(rookMoves (i,j) (ri,rj) (pi, w, b)) || (bishopMoves (i,j) (ri,rj) (pi, w, b)) = True
						             |otherwise = False

kingMoves (i,j) (ri,rj) (pi, w, b) |ri == (nextChar i) && rj == j = True
						           |ri == (prevChar i) && rj == j  = True
					               |ri == (nextChar i) && rj == (j+1) = True
						           |ri == (prevChar i) && rj == (j+1)  = True
						           |ri == (nextChar i) && rj == (j-1)  = True
						           |ri == (prevChar i) && rj == (j-1)  = True
						           |(ri == i && rj == (j+1)) = True
						           |(ri == i && rj == (j-1))  = True
						           |otherwise = False

nextChar 'a' = 'b'
nextChar 'b' = 'c'
nextChar 'c' = 'd'
nextChar 'd' = 'e'
nextChar 'e' = 'f'
nextChar 'f' = 'g'
nextChar 'g' = 'h'
nextChar 'h' = 'z'


prevChar 'h' = 'g'
prevChar 'g' = 'f'
prevChar 'f' = 'e'
prevChar 'e' = 'd'
prevChar 'd' = 'c'
prevChar 'c' = 'b'
prevChar 'b' = 'a'
prevChar 'a' = 'z'

outOfBoundsNext 'h' = True
outOfBoundsprev 'a' = True  

inBetween 'h' 'h' x = if ( x == 'h') then True else False
inBetween c1 c2 x = if (c1 == x) then True else inBetween (nextChar c1) c2 x


diagonalUpRight (i,j) (ri,rj) (pi, w, b)  |isOccupied ((nextChar i),(j+1)) (pi, w, b) && (nextChar i) == ri && (j+1) == rj = True
										  |isOccupied ((nextChar i),(j+1)) (pi, w, b) =False
										  |(ri == (nextChar i)) && (rj == (j+1)) = True
					                      |diagonalUpRight ((nextChar i),(j+1)) (ri,rj) (pi, w, b) = True
										  |otherwise = False
							  
diagonalUpLeft (i,j) (ri,rj) (pi, w, b)  |isOccupied ((prevChar i),(j+1)) (pi, w, b) && (prevChar i) /= ri && (j+1) /= rj = False
                                         |(ri == (prevChar i)) && (rj == (j+1)) = True
					                     |diagonalUpLeft ((prevChar i),(j+1)) (ri,rj) (pi, w, b) = True
					                     |otherwise = False

diagonalDownRight (i,j) (ri,rj) (pi, w, b)  |isOccupied ((nextChar i),(j-1)) (pi, w, b) && (nextChar i) /= ri && (j-1) /= rj = False
											|(ri == (nextChar i)) && (rj == (j-1)) = True
					                        |diagonalDownRight ((nextChar i),(j-1)) (ri,rj) (pi, w, b) = True
					                        |otherwise = False
								 
diagonalDownLeft (i,j) (ri,rj) (pi, w, b) 
										   |isOccupied ((prevChar i),(j-1)) (pi, w, b) && (prevChar i) /= ri && (j-1) /= rj = False
										   |(ri == (prevChar i)) && (rj == (j-1)) = True
					                       |diagonalDownLeft ((prevChar i),(j-1)) (ri,rj) (pi, w, b) = True
					                       |otherwise = False
							  
isOccupied (i,j) (pi, [], []) = False	
isOccupied (i,j) (pi, [], b) |isEqual (locationGetter(head b)) (i,j) = True
							 |otherwise = isOccupied (i,j)  (pi,[], (tail b))	
							   
isOccupied (i,j) (pi, w, []) |isEqual (locationGetter(head w)) (i,j) = True 
							 |otherwise = isOccupied (i,j)  (pi,(tail w), [])
			  
isOccupied (i,j) (pi, w, b)  |isEqual (locationGetter(head w)) (i,j) = True
                             |isEqual (locationGetter(head b)) (i,j) = True
							 |otherwise = isOccupied (i,j)  (pi,(tail w), (tail b))
							 


isEqual (i,j) (ir,jr) |i == ir && j == jr = True
                      |otherwise = False
					  

findColor (p,[],[]) (ri,rj) = ""
findColor (p,[],(hb:tb)) (ri,rj)  | ri == (charGetter hb) && rj == (intGetter hb) = "Black"
						  | otherwise = findColor (p,[],(tb)) (ri,rj)
						  
findColor (p,(hw:tw),[]) (ri,rj) | ri == (charGetter hw) && rj == (intGetter hw) = "White"
								 | otherwise = findColor (p,(tw),[]) (ri,rj)
									  
findColor (p,(hw:tw),(hb:tb)) (ri,rj) | ri == (charGetter hw) && rj == (intGetter hw) = "White"
									  | ri == (charGetter hb) && rj == (intGetter hb) = "Black"
									  | otherwise = findColor (p,(tw),(tb)) (ri,rj)
									  
getColor (White,w,b) = "White"
getColor (Black,w,b) = "Black"

move piece loc (p,(hw:tw),(hb:tb)) | findColor (p,(hw:tw),(hb:tb)) (locationGetter piece) /= getColor (p,(hw:tw),(hb:tb)) = error "It is not your turn"
								   | not (isLegal piece (p,(hw:tw),(hb:tb)) loc) = error "illegal move"
								   | (isOccupied loc (p,(hw:tw),(hb:tb))) && findColor (p,(hw:tw),(hb:tb)) (locationGetter piece) == "Black" = (White,removeWhite loc (p,(hw:tw),(hb:tb)),moveBlack piece loc (p,(hw:tw),(hb:tb)))
								   | (isOccupied loc (p,(hw:tw),(hb:tb))) && findColor (p,(hw:tw),(hb:tb)) (locationGetter piece) == "White" = (Black,moveWhite piece loc (p,(hw:tw),(hb:tb)),removeBlack loc (p,(hw:tw),(hb:tb)))
								   | getColor (p,(hw:tw),(hb:tb)) == "Black" = (White,(hw:tw),moveBlack piece loc (p,(hw:tw),(hb:tb)))
								   | getColor (p,(hw:tw),(hb:tb)) == "White" = (Black,moveWhite piece loc (p,(hw:tw),(hb:tb)),(hb:tb))
								   


removeWhite (c,n) (p,(hw:tw),(hb:tb)) | charGetter hw == c && intGetter hw == n = tw
									  | otherwise = [hw] ++ removeWhite (c,n) (p,(tw),(hb:tb))

removeBlack (c,n) (p,(hw:tw),(hb:tb)) | charGetter hb == c && intGetter hb == n = tb
									  | otherwise = [hb] ++ removeBlack (c,n) (p,(hw:tw),(tb))
											
moveBlack (R (char,int)) (c,n) (p,(hw:tw),(hb:tb)) = removeBlack  (char,int) (p,(hw:tw),(hb:tb)) ++ [R (c,n)]
moveBlack (B (char,int)) (c,n) (p,(hw:tw),(hb:tb)) = removeBlack  (char,int) (p,(hw:tw),(hb:tb)) ++ [B (c,n)]
moveBlack (K (char,int)) (c,n) (p,(hw:tw),(hb:tb)) = removeBlack  (char,int) (p,(hw:tw),(hb:tb)) ++ [K (c,n)]
moveBlack (P (char,int)) (c,n) (p,(hw:tw),(hb:tb)) = removeBlack  (char,int) (p,(hw:tw),(hb:tb)) ++ [P (c,n)]
moveBlack (Q (char,int)) (c,n) (p,(hw:tw),(hb:tb)) = removeBlack  (char,int) (p,(hw:tw),(hb:tb)) ++ [Q (c,n)]
moveBlack (N (char,int)) (c,n) (p,(hw:tw),(hb:tb)) = removeBlack  (char,int) (p,(hw:tw),(hb:tb)) ++ [N (c,n)]


moveWhite (R (char,int)) (c,n) (p,(hw:tw),(hb:tb)) = removeWhite  (char,int) (p,(hw:tw),(hb:tb)) ++ [R (c,n)]
moveWhite (B (char,int)) (c,n) (p,(hw:tw),(hb:tb)) = removeWhite  (char,int) (p,(hw:tw),(hb:tb)) ++ [B (c,n)]
moveWhite (K (char,int)) (c,n) (p,(hw:tw),(hb:tb)) = removeWhite  (char,int) (p,(hw:tw),(hb:tb)) ++ [K (c,n)]
moveWhite (P (char,int)) (c,n) (p,(hw:tw),(hb:tb)) = removeWhite  (char,int) (p,(hw:tw),(hb:tb)) ++ [P (c,n)]
moveWhite (Q (char,int)) (c,n) (p,(hw:tw),(hb:tb)) = removeWhite  (char,int) (p,(hw:tw),(hb:tb)) ++ [Q (c,n)]
moveWhite (N (char,int)) (c,n) (p,(hw:tw),(hb:tb)) = removeWhite  (char,int) (p,(hw:tw),(hb:tb)) ++ [N (c,n)]
