
import Control.Monad

import Data.Array.Unboxed
import Data.List
import Data.Maybe
import Data.Tree

import System.Environment

data Direction = R | D | Init

type Index = (Int,Int)
type Board = UArray Index Int
newtype ValidBoard = ValidBoard { getBoard :: Board }
	deriving (Eq,Show)

leaves :: Tree a -> [a]
leaves t = map rootLabel $ leaves' [] t where
	leaves' :: [Tree a] -> Tree a -> [Tree a]
	leaves' rs node@(Node _ []) = node:rs
	leaves' rs (Node _ ts) = concatMap (leaves' rs) ts

emptyBoard :: Int -> Board
emptyBoard n = array ((0,0),(n-1,n))
               [ ((i,j),-1) | j <- [0..n], i <- [0..n-1] ]

ixFromDirAndIx :: Direction -> Index -> Index
ixFromDirAndIx R (i,j) = (i,j+1)
ixFromDirAndIx D (i,j) = (i+1,j)
ixFromDirAndIx Init ix = ix

place :: Int -> Direction -> Index -> Board -> Board
place i dir ix board =
	let secondIx = ixFromDirAndIx dir ix in
	board // [(ix,i),(secondIx,i)]

extractFromList :: (Eq a) => a -> [a] -> Maybe (a,[a])
extractFromList x ys =
	case partition (x ==) ys of
		( z:_ , zs ) -> Just (z,zs)
		(  [] ,  _ ) -> Nothing

iterateTree :: (Int,Board,Direction,[Index])
            -> (Maybe Board,[(Int,Board,Direction,[Index])])
iterateTree (_,_,_,[]) = error "Board must have minimum size 1."
iterateTree (i,board,Init,ixs) =
	(Just board, [(i,board,R,ixs),(i,board,D,ixs)])
iterateTree (i,board,dir,ix1:ix2:[]) =
	if ixFromDirAndIx dir ix1 == ix2
		then (Just $ place i dir ix1 board, [])
		else (Nothing, [])
iterateTree (i,board,dir,ix1:ix2:ixs) =
	let secondIx = ixFromDirAndIx dir ix1
	    newBoard = place i dir ix1 board
	    newI     = i+1 in 
	if secondIx == ix2
		then (Just newBoard, [(newI,newBoard,R,ixs),(newI,newBoard,D,ixs)])
		else
			case extractFromList secondIx ixs of
				Just (_,zs) ->
					(Just newBoard,
						[(newI,newBoard,R,ix2:zs),(newI,newBoard,D,ix2:zs)])
				Nothing     -> (Nothing, [])

allBoardPaths :: Int -> Tree (Maybe Board)
allBoardPaths n =
	let startBoard = emptyBoard n in
	unfoldTree iterateTree (0, startBoard, Init, indices startBoard)

allBoards :: Int -> [ValidBoard]
allBoards n = map ValidBoard $ catMaybes . leaves $ allBoardPaths n



boardGetN :: ValidBoard -> Int
boardGetN = snd . snd . bounds . getBoard

adjustIx :: ValidBoard -> ValidBoard
adjustIx board = adjustIx' 0 (indices . getBoard $ board) board where
	adjustIx' :: Int -> [Index] -> ValidBoard -> ValidBoard
	adjustIx' _ [] theBoard = theBoard
	adjustIx' p (ix@(i,j):ixs) theBoard
		| newIxHorz `elem` ixs
			&& (currentVal == (getBoard theBoard ! newIxHorz))
		  = adjustIx' (p+1)
		  	(snd . fromJust $ extractFromList newIxHorz ixs)
		    $ ValidBoard (getBoard theBoard // [(ix,p),(newIxHorz,p)])
		| newIxVert `elem` ixs
		  && (currentVal == (getBoard theBoard ! newIxVert))
		  = adjustIx' (p+1)
		  	(snd . fromJust $ extractFromList newIxVert ixs)
		    $ ValidBoard (getBoard theBoard // [(ix,p),(newIxVert,p)])
		| otherwise = error "adjustIx was applied to an invalid board"
		where currentVal = getBoard theBoard ! ix
		      newIxHorz = (i,j+1)
		      newIxVert = (i+1,j)


transformBoard :: (Int -> Index -> Index) -> ValidBoard -> ValidBoard
transformBoard f board =
  adjustIx . ValidBoard $
  	ixmap (bounds . getBoard $ board)
  		(f (boardGetN board)) $ getBoard board

flipHorz :: ValidBoard -> ValidBoard
flipHorz = transformBoard (\n (i,j) -> (i,n-j))

flipVert :: ValidBoard -> ValidBoard 
flipVert = transformBoard (\n (i,j) -> (n-1-i,j))

rotate :: ValidBoard -> ValidBoard 
rotate = transformBoard (\n (i,j) -> (n-1-i,n-j))


isEquivalent :: ValidBoard -> ValidBoard -> Bool
isEquivalent b1 b2 =
	or $ map ((== b1) . ($ b2)) [flipHorz,flipVert,rotate]

equivalenceClasses :: (a -> a -> Bool) -> [a] -> [[a]]
equivalenceClasses predicate vals = eqClasses' vals [] where
	eqClasses'     [] rs = rs
	eqClasses' (x:xs) rs =
		let (existingClass,notClass) = partition (predicate x . head) rs in
	  if null existingClass
		  then eqClasses' xs $ [x]:rs
		  else eqClasses' xs $ (x:(head existingClass)):notClass

allBoardEqClasses :: Int -> [[ValidBoard]]
allBoardEqClasses n = equivalenceClasses isEquivalent $ allBoards n


getBoardNumLen :: ValidBoard -> Int
getBoardNumLen board =
	let n = boardGetN board in
  length . show $ n*(n-1) `div` 2

alignChars :: Int -> String -> String
alignChars n s | len < n   = replicate (n-len) ' ' ++ s
               | otherwise = s
               where len = length s

printAlignedInt :: Int -> Int -> String
printAlignedInt minLen i = alignChars minLen $ show i

putStrArrayLn :: ValidBoard -> IO ()
putStrArrayLn board =
	let (_,(maxX,maxY)) = bounds . getBoard $ board
	    xRange = [0..maxX]
	    yRange = [0..maxY]
	    minLen = getBoardNumLen board in do
	forM_ xRange (\i -> do
		forM_ yRange (\j -> putStr $ 
			(printAlignedInt minLen $ getBoard board ! (i,j)) ++ " ")
		putStrLn "")
	putStrLn ""

--injectSeparator :: String -> (a -> IO ()) -> a -> IO ()
--injectSeparator separator f x = f x >> putStr separator

main :: IO ()
main = do
	[nString] <- getArgs
	let n = read nString
	let eqClasses = allBoardEqClasses n
	--print $ length eqClasses
	--putStrLn ""
	mapM_ (putStrArrayLn . head) eqClasses