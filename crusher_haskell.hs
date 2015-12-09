{---------------------------------------
    Geneva Qitong Sun   48927123 d4m8
    Martin Tsang        41611112 d7z7
    Chuong Long Yeh     33135112 v6h8
----------------------------------------}

-- Piece is a data representation of possible pieces on a board
-- where D is an empty spot on the board
--         W is a piece of the White player
--         B is a piece of the Black player
data Piece = D | W | B deriving (Eq, Show)

-- Point is a tuple of 2 elements
-- representing a point on a grid system
-- where the first element represents the x coordinate
--       the second element represents the y coordinate
type Point = (Int, Int)

-- Tile is a tuple of 2 elements
-- representing what a point is occupied by
-- where the first element represents a piece
--       the second element represents a point
type Tile  = (Piece, Point)

-- Board is a list of Pieces, thus it is an internal representation
-- of the provided string representation of the board, it maintains
-- the same order as the string representation of the board
type Board = [Piece]

-- Grid is a list of Points, thus it is an internal representation
-- of the hexagonal grid system translated into a coordinate
-- system to easily maintain and make moves on the board
type Grid = [Point]

-- State is a list of Tile, thus it is an internal representation precisely
-- for the purposes of zipping the board and the grid together in order
-- to keep easier track of the effects on the pieces of making moves on grid
type State = [Tile]

-- Next is a data representation for storing and passing around information within
-- the tree generating function, allowing it to correctly generate new children
--
-- Next consists of 4 elements
-- where usedDepth is an integer reprsenting the current depth level
--         newBoard is the next board to add to the tree
--          seenBoards is the updated history to avoid possible future trouble boards
--          cplayer is the current player for whom the board was generated for
data Next a = Next {usedDepth :: Int, newBoard :: a, seenBoards :: [a], cplayer :: Piece}

-- Tree is a data representation for the search tree, it is an extention of
-- the rose tree widely used for implementing such unequally branched search trees
--
-- Tree consists of 3 elements
-- where depth is an integer representing the depth level of the node
--          board is the game state at that node
--          nextBoards are the child nodes of the current node
data Tree a = Node {depth :: Int, board :: a, nextBoards :: [Tree a]} deriving (Show)

-- BoardTree is the internal representation of the search tree of the given board
-- that is to be generatated for correctly implementing the minimax algorithm.
type BoardTree = Tree Board

-- Slide is a tuple of 2 elements
-- an internal representation of a slide
-- where the first element represents the point to move from
--          the second element represents the adjacent point to move to
type Slide = (Point,Point)

-- Jump is a tuple of 3 elements
-- an internal representation of a leap
-- where the first element represents the point to move from
--          the second element represents the adjacent point to move over
--         the third element represents the point to move to
type Jump = (Point,Point,Point)

-- Move is a tuple of 2 elements
-- an internal representation of a move
-- where the first element represents the point to move from
--          the second element represents the point to move to
--
-- Note: in essence it is the same as a slide however the idea
--         is that a jump can be reduced to a move as in effect
--         nothing happens the point moved over in a jump
type Move = (Point,Point)

-- Some test results to see what functions are producing
run = crusher ["-WW--W---W---BW-BBB","-WW-WW---W---BB-BBB","WWW-WW-------BB-BBB"] 'B' 2 3
grid0 = generateGrid 3 2 4 []
slides0 = generateSlides grid0 3
jumps0 = generateLeaps grid0 3
board0 = strToBoard "WWW-WW-------BB-BBB"
boards0 = [strToBoard "WWW-W--------BB-BBB", strToBoard "WWW--W-------BB-BBB", strToBoard "WWW-W--------BB-BBB"]
newBoards0 = generateNewStates board0 [] grid0 slides0 jumps0 W
tree0 = generateTree board0 [] grid0 slides0 jumps0 W 2 3
heuristic0 = boardEvaluator W [] 3


-- crusher
--
-- This function consumes a list of boards, a player, the depth of
-- search tree, the size of the provide boards, and produces the
-- next best board possible for the provided player, and accordingly
-- makes the move and returns new board consed onto the list of boards
--
-- Arguments:
-- -- (current:old): current represents the most recent board, old is
--                   the history of all boards already seen in game
-- -- p: 'W' or 'B' representing the player the program is
-- -- d: an Integer indicating depth of search tree
-- -- n: an Integer representing the dimensions of the board
--
-- Returns: a list of String with the new current board consed onto the front
--

crusher :: [String] -> Char -> Int -> Int -> [String]
crusher (current:old) p d n
    -- if depth = 0, return the current board and the history
    | d == 0 = (current:old)
    -- if the game is over, return the current board and the history
    | gameOver (strToBoard current) (map strToBoard old) n = (current:old)
    -- otherwise return a new board with the next best move
    -- include the current board in the history
        -- use minimax to calculate the next best move
        -- use generateTree to generate all possible tree of slides and jumps
        -- use generateSlides to generate all possible slides
        -- use generateLeaps to generate all possible leaps
        -- heuristic determines the 'goodness' of a move using boardEvaluator
    | otherwise = boardToStr (minimax boardTree n heuristic):(current:old)
    where
        history = [strToBoard x | x <- old]
        grid = generateGrid n (n - 1) (n + 1) []
        player = if p == 'W' then W else B
        boardTree = generateTree (strToBoard current) history grid (generateSlides grid n) (generateLeaps grid n) player d n


-- gameOver
--
-- This function consumes a board, a list of boards, and the dimension
-- of board and determines whether the given board is in a state where
-- the game has ended by checking if the board is present in the provided
-- list of boards or either the W or B pieces are less than dimension of board
--
-- Arguments:
-- -- board: a Board representing the most recent board
-- -- history: a list of Boards of representing all boards already seen
-- -- n: an Integer representing the dimensions of the board
--
-- Returns: True if the board is in a state where the game has ended, otherwise False
--

-- game is over when 1) board is already in history, or 2) number of pieces is less than n
gameOver :: Board -> [Board] -> Int -> Bool
gameOver board history n = (board `elem` history) || not (enoughPieces board n)

-- enoughPieces determines whether the board contains >=n black and white pieces
enoughPieces :: Board -> Int -> Bool
enoughPieces board n = countPieces board W >= n && countPieces board B >= n

-- count the number of given piece in the given board
countPieces :: Board -> Piece -> Int
countPieces board player = length (filter (== player) board)

--
-- strToBoard
--
-- This function consumes a list of characters which can be either 'W' or 'B'
-- or '-' and converts them to a list of pieces, i.e W or B or D respectively
--
-- Arguments:
-- -- s: the String to convert into piece-wise representation
--
-- Note: This function would convert "WWW-WW-------BB-BBB" to
--          [W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B]
--
-- Returns: the Board corresponding to the string
--

strToBoard :: String  -> Board
strToBoard s = map (\ x -> check x) s
    where
        check 'W' = W
        check 'B' = B
        check '-' = D

--
-- boardToStr
--
-- This function consumes a board which is a list of either W or B  or D and
-- converts them to a list of characters, i.e 'W' or 'B' or 'D' respectively
--
-- Arguments:
-- -- b: the Board to convert into char-wise representation
--
-- Note: This function would convert [W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B]
--          to "WWW-WW-------BB-BBB"
--
-- Returns: the String corresponding to the board
--

boardToStr :: Board -> String
boardToStr b = map (\ x -> check x) b
    where
        check W = 'W'
        check B = 'B'
        check D = '-'

--
-- generateGrid
--
-- This function consumes three integers (described below) specifying how to
-- properly generate the grid and also a list as an accumulator; to generate a
-- regular hexagon of side length n, pass n (n- 1) (2 * (n - 1)) and []
--
-- Arguments:
-- -- n1: one more than max x-coordinate in the row, initialized always to n
-- -- n2: the number of rows away from the middle row of the grid
-- -- n3: the current y-coordinate i.e the current row number
-- -- acc: an accumulator that keeps track of accumulating rows of grid
--           initialized to []
--
-- Note: This function on being passed 3 2 4 [] would produce
--         [(0,0),(1,0),(2,0)
--          (0,1),(1,1),(2,1),(3,1)
--          (0,2),(1,2),(2,2),(3,2),(4,2)
--          (0,3),(1,3),(2,3),(3,3)
--          (0,4),(1,4),(2,4)]
--
-- Returns: the corresponding Grid i.e the acc when n3 == -1
--

generateGrid :: Int -> Int -> Int -> Grid -> Grid
generateGrid n1 n2 n3 acc
    | n3 == -1        = acc
    | otherwise     = generateGrid nn1 (n2 - 1) (n3 - 1) (row ++ acc)
        where
            row = map (\ x -> (x,n3)) [0 .. (n1 - 1)]
            nn1 = if n2 > 0 then n1 + 1 else n1 - 1

--
-- generateSlides
--
-- This function consumes a grid and the size of the grid, accordingly
-- generates a list of all possible slides from any point on the grid to
-- any adjacent point on the grid
--
-- Arguments:
-- -- b: the Grid to generate slides for
-- -- n: an Integer representing the dimensions of the grid
--
-- Note: This function is only called at the initial setup of the game,
--          it is a part of the internal representation of the game, this
--         list of all possible slides is only generated once; and when
--          generating next moves, the program decides which slides out of
--         all these possible slides could a player actually make
--
-- Returns: the list of all Slides possible on the given grid
-- Slide = (Point (slide-from), Point (slide-to))
-- Grid = [Point]
-- Point = (x , y)

-- Generate a list of Slides with each Point, and concat them
generateSlides :: Grid -> Int -> [Slide]
generateSlides b n = concat [generateSlides_helper x b n | x <- b] -- TODO

-- Generate all possible slide points per point
generateSlides_helper ::  Point -> Grid-> Int-> [Slide]
generateSlides_helper points b n =
        if(snd(points) < n - 1 )
            then
                let
                    --TL
                    both_minus_one  = if((fst(points)-1,    snd(points)-1)  `elem` b ) then [(points,(fst(points)-1,    snd(points)-1))]    else []
                    --TR
                    y_minus_one     = if((fst(points),      snd(points)-1)  `elem` b ) then [(points,(fst(points),      snd(points)-1))]    else []
                    --R
                    x_plus_one      = if((fst(points)+1,    snd(points))    `elem` b ) then [(points,(fst(points)+1,    snd(points)))]      else []
                    --BR
                    both_plus_one   = if((fst(points)+1,    snd(points)+1)  `elem` b ) then [(points,(fst(points)+1,    snd(points)+1))]    else []
                    --BL
                    y_plus_one      = if((fst(points),      snd(points)+1)  `elem` b ) then [(points,(fst(points),      snd(points)+1))]    else []
                    --L
                    x_minus_one     = if((fst(points)-1,    snd(points))    `elem` b ) then [(points,(fst(points)-1,    snd(points)))]      else []
                in
                    x_plus_one ++ y_plus_one ++ both_plus_one ++ x_minus_one ++ y_minus_one ++ both_minus_one
            else
                let
                    --TL
                    y_minus_one     = if((fst(points),      snd(points)-1)  `elem` b ) then [(points,(fst(points),      snd(points)-1))]    else []
                    --TR
                    plus_minus_one  = if((fst(points)+1,    snd(points)-1)  `elem` b ) then [(points,(fst(points)+1,    snd(points)-1))]    else []
                    --R
                    x_plus_one      = if((fst(points)+1,    snd(points))    `elem` b ) then [(points,(fst(points)+1,    snd(points)))]      else []
                    --BR
                    y_plus_one      = if((fst(points),      snd(points)+1)  `elem` b ) then [(points,(fst(points),      snd(points)+1))]    else []
                    --BL
                    minus_plus_one  = if((fst(points)-1,    snd(points)+1)  `elem` b ) then [(points,(fst(points)-1,    snd(points)+1))]    else []
                    --L
                    x_minus_one     = if((fst(points)-1,    snd(points))    `elem` b ) then [(points,(fst(points)-1,    snd(points)))]      else []
                in
                    y_minus_one ++ plus_minus_one ++ x_plus_one ++ y_plus_one ++ minus_plus_one ++ x_minus_one


--
-- generateLeaps
--
-- This function consumes a grid and the size of the grid, accordingly
-- generates a list of all possible leaps from any point on the grid over
-- any adjacent point on the grid to any point next to the adjacent point
-- such that it is movement in the same direction
--
-- Arguments:
-- -- b: the Grid to generate leaps for
-- -- n: an Integer representing the dimensions of the grid
--
-- Note: This function is only called at the initial setup of the game,
--          it is a part of the internal representation of the game, this
--         list of all possible leaps is only generated once; and when
--          generating next moves, the program decides which leaps out of
--         all these possible leaps could a player actually make
--
-- Returns: the list of all Jumps possible on the given grid
--
-- Grid = [Point]
-- Point = (x , y)
--  Jump = (Point,Point,Point)
generateLeaps :: Grid -> Int -> [Jump]
generateLeaps b n = concat [generateLeaps_helper x b n | x <- b]

-- Generate all possible leaps per point
generateLeaps_helper ::  Point -> Grid-> Int-> [Jump]
generateLeaps_helper points b n =
            -- y <= 0
            if(snd(points) <= n-3 ) then
                    let
                    --TL
                    both_minus_one = if( (fst(points)-2,snd(points)-2) `elem` b ) then [(points,(fst(points)-1, snd(points)-1),(fst(points)-2, snd(points)-2))] else []
                    --TR
                    y_minus_one = if( (fst(points),snd(points)-2) `elem` b ) then [(points,(fst(points), snd(points)-1),(fst(points), snd(points)-2))] else []
                    --R
                    x_plus_one = if( (fst(points)+2,snd(points)) `elem` b ) then [(points,(fst(points)+1, snd(points)),(fst(points)+2, snd(points)))] else []
                    --BR
                    both_plus_one = if( (fst(points)+2,snd(points)+2) `elem` b ) then [(points,(fst(points)+1, snd(points)+1),(fst(points)+2, snd(points)+2))] else []
                    --BL
                    y_plus_one = if( (fst(points),snd(points)+2) `elem` b ) then [(points,(fst(points), snd(points)+1),(fst(points), snd(points)+2))] else []
                    --L
                    x_minus_one = if( (fst(points)-2,snd(points)) `elem` b ) then [(points,(fst(points)-1, snd(points)),(fst(points)-2, snd(points)))] else []
                    in
                    x_plus_one++y_plus_one++both_plus_one++x_minus_one++y_minus_one++both_minus_one

            else
            -- y = 1
            if (snd(points) == n-2) then
                    let
                    --TL
                    both_minus_one = if( (fst(points)-2,snd(points)-2) `elem` b ) then [(points,(fst(points)-1, snd(points)-1),(fst(points)-2, snd(points)-2))] else []
                    --TR
                    y_minus_one = if( (fst(points),snd(points)-2) `elem` b ) then [(points,(fst(points), snd(points)-1),(fst(points), snd(points)-2))] else []
                    --R
                    x_plus_one = if( (fst(points)+2,snd(points)) `elem` b ) then [(points,(fst(points)+1, snd(points)),(fst(points)+2, snd(points)))] else []
                    --BR
                    both_plus_one = if( (fst(points)+1,snd(points)+2) `elem` b ) then [(points,(fst(points)+1, snd(points)+1),(fst(points)+1, snd(points)+2))] else []
                    --BL
                    y_plus_one = if( (fst(points)-1,snd(points)+2) `elem` b ) then [(points,(fst(points), snd(points)+1),(fst(points)-1, snd(points)+2))] else []
                    --L
                    x_minus_one = if( (fst(points)-2,snd(points)) `elem` b ) then [(points,(fst(points)-1, snd(points)),(fst(points)-2, snd(points)))] else []
                    in
                    x_plus_one++y_plus_one++both_plus_one++x_minus_one++y_minus_one++both_minus_one

            else
            -- y = 2
            if (snd(points) == n-1) then
                    let
                    --TL
                    both_minus_one = if( (fst(points)-2,snd(points)-2) `elem` b ) then [(points,(fst(points)-1, snd(points)-1),(fst(points)-2, snd(points)-2))] else []
                    --TR
                    y_minus_one = if( (fst(points),snd(points)-2) `elem` b ) then [(points,(fst(points), snd(points)-1),(fst(points), snd(points)-2))] else []
                    --R
                    x_plus_one = if( (fst(points)+2,snd(points)) `elem` b ) then [(points,(fst(points)+1, snd(points)),(fst(points)+2, snd(points)))] else []
                    --BR
                    both_plus_one = if( (fst(points),snd(points)+2) `elem` b ) then [(points,(fst(points), snd(points)+1),(fst(points), snd(points)+2))] else []
                    --BL
                    y_plus_one = if( (fst(points)-2,snd(points)+2) `elem` b ) then [(points,(fst(points)-1, snd(points)+1),(fst(points)-2, snd(points)+2))] else []
                    --L
                    x_minus_one = if( (fst(points)-2,snd(points)) `elem` b ) then [(points,(fst(points)-1, snd(points)),(fst(points)-2, snd(points)))] else []
                    in
                    x_plus_one++y_plus_one++both_plus_one++x_minus_one++y_minus_one++both_minus_one

            else
            -- y = 3
            if (snd(points) == n) then
                    let
                    --TL
                    both_minus_one = if( (fst(points)-1,snd(points)-2) `elem` b ) then [(points,(fst(points), snd(points)-1),(fst(points)-1, snd(points)-2))] else []
                    --TR
                    y_minus_one = if( (fst(points)+1,snd(points)-2) `elem` b ) then [(points,(fst(points)+1, snd(points)-1),(fst(points)+1, snd(points)-2))] else []
                    --R
                    x_plus_one = if( (fst(points)+2,snd(points)) `elem` b ) then [(points,(fst(points)+1, snd(points)),(fst(points)+2, snd(points)))] else []
                    --BR
                    y_plus_one = if( (fst(points),snd(points)+2) `elem` b ) then [(points,(fst(points), snd(points)+1),(fst(points), snd(points)+2))] else []
                    --BL
                    minus_plus_one = if( (fst(points)-2,snd(points)+2) `elem` b ) then [(points,(fst(points)-1, snd(points)+1),(fst(points)-2, snd(points)+2))] else []
                    --L
                    x_minus_one = if( (fst(points)-2,snd(points)) `elem` b ) then [(points,(fst(points)-1, snd(points)),(fst(points)-2, snd(points)))] else []
                    in
                    x_plus_one++y_plus_one++x_minus_one++y_minus_one++both_minus_one++minus_plus_one

            else
            -- y >= 4
            --if (snd(points) >= n+1) then
                    let
                    --TL
                    y_minus_one = if( (fst(points),snd(points)-2) `elem` b ) then [(points,(fst(points), snd(points)-1),(fst(points), snd(points)-2))] else []
                    --TR
                    plus_minus_one = if( (fst(points)+2,snd(points)-2) `elem` b ) then [(points,(fst(points)+1, snd(points)-1),(fst(points)+2, snd(points)-2))] else []
                    --R
                    x_plus_one = if( (fst(points)+2,snd(points)) `elem` b ) then [(points,(fst(points)+1, snd(points)),(fst(points)+2, snd(points)))] else []
                    --BR
                    y_plus_one = if( (fst(points),snd(points)+2) `elem` b ) then [(points,(fst(points), snd(points)+1),(fst(points), snd(points)+2))] else []
                    --BL
                    minus_plus_one = if( (fst(points)-2,snd(points)+2) `elem` b ) then [(points,(fst(points)-1, snd(points)+1),(fst(points)-2, snd(points)+2))] else []
                    --L
                    x_minus_one = if( (fst(points)-2,snd(points)) `elem` b ) then [(points,(fst(points)-1, snd(points)),(fst(points)-2, snd(points)))] else []
                    in
                    y_minus_one++plus_minus_one++x_plus_one++y_plus_one++minus_plus_one++x_minus_one




--
-- stateSearch
--
-- This function consumes the arguments described below, based on the internal
-- representation of the game, if there is no point in playing the game as the
-- current board is in a state where the game has ended then just return the
-- board, else generate a search tree till the specified depth and apply
-- minimax to it by using the appropriately generated heuristic
--
-- Arguments:
-- -- board: a Board representing the most recent board
-- -- history: a list of Boards of representing all boards already seen
-- -- grid: the Grid representing the coordinate-grid the game being played
-- -- slides: the list of all Slides possible for the given grid
-- -- jumps: the list of all Jumps possible for the given grid
-- -- player: W or B representing the player the program is
-- -- depth: an Integer indicating depth of search tree
-- -- num: an Integer representing the dimensions of the board
--
-- Returns: the current board if game is over,
--          otherwise produces the next best board
--

--stateSearch :: Board -> [Board] -> Grid -> [Slide] -> [Jump] -> Piece -> Int -> Int -> Board
--stateSearch board history grid slides jumps player depth num = -- To Be Completed

--
-- generateTree
--
-- This function consumes the arguments described below, and builds a search
-- tree till specified depth from scratch by using the current board and
-- generating all the next states recursively; however it doesn't generate
-- children of those states which are in a state where the game has ended.
--
-- Arguments:
-- -- board: a Board representing the most recent board
-- -- history: a list of Boards of representing all boards already seen
-- -- grid: the Grid representing the coordinate-grid the game being played
-- -- slides: the list of all Slides possible for the given grid
-- -- jumps: the list of all Jumps possible for the given grid
-- -- player: W or B representing the player the program is
-- -- depth: an Integer indicating depth of search tree
-- -- n: an Integer representing the dimensions of the board
--
-- Returns: the corresponding BoardTree generated till specified depth
--

generateTree :: Board -> [Board] -> Grid -> [Slide] -> [Jump] -> Piece -> Int -> Int -> BoardTree
generateTree board history grid slides jumps player depth n
                | (gameOver board history n) = Node {depth = depth, board = board, nextBoards = []}
                | depth == 0 = Node {depth = depth, board = board, nextBoards = []}
                | otherwise = Node {depth = depth,
                                    board = board,
                                    nextBoards = [(generateTree x history grid slides jumps (getEnemy player) (depth-1) n)| x <-(generateNewStates board history grid slides jumps player)]}

--
-- generateNewStates
--
-- This function consumes the arguments described below, it first generates a
-- list of valid moves, applies those moves to the current board to generate
-- a list of next boards, and then checks whether or not that move would
-- have been possible by filtering out those boards already seen before
--
-- Arguments:
-- -- board: a Board representing the most recent board
-- -- board = [Piece] = = D | W | B deriving (Eq, Show)
-- -- [W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B]

-- -- history: a list of Boards of representing all boards already seen
-- -- [[W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B],[W,W,W,D,W,W,D,D,D,D,D,D,D,B,B,D,B,B,B]]

-- -- grid: the Grid representing the coordinate-grid the game being played
-- -- Grid = [Point] = (Int,Int)
-- --       [(0,0),(1,0),(2,0)
--          (0,1),(1,1),(2,1),(3,1)
--          (0,2),(1,2),(2,2),(3,2),(4,2)
--          (0,3),(1,3),(2,3),(3,3)
--          (0,4),(1,4),(2,4)]

-- -- slides: the list of all Slides possible for the given grid
-- --  Slide = (Point,Point)

-- -- jumps: the list of all Jumps possible for the given grid
-- -- Jump = (Point,Point,Point)

-- -- player: W or B representing the player the program is
--
-- Returns: the list of next boards
--

generateNewStates :: Board -> [Board] -> Grid -> [Slide] -> [Jump] -> Piece -> [Board]
generateNewStates board history grid slides jumps player =
                  let fullList = concat [(generateNewStates_helper_jump board grid point jumps player) ++
                                      (generateNewStates_helper_slide board grid point slides player) |
                                      point <- grid , player == (getPiece board grid point)] in
                  [x | x <- fullList , not(x `elem` history)]

-- Argument
-- Point = (0,1)

-- Returns list of boards - possible combination of moves for one point.
generateNewStates_helper_slide :: Board -> Grid ->  Point -> [Slide] -> Piece -> [Board]
generateNewStates_helper_slide board grid point loS player=
              let listOfMoves = [x | x <- loS , fst(x) == point]
                  filterMoves = [y | y <- listOfMoves , (getPiece board grid (snd y)) == D]
                  in
                  [updateBoardSlide board grid point player z  | z <- filterMoves]


-- Returns list of boards - possible combination of Jumps for one point.
generateNewStates_helper_jump :: Board -> Grid -> Point -> [Jump] -> Piece -> [Board]
generateNewStates_helper_jump board grid point loJ player =
              let listOfJumps = [x | x <- loJ, frst(x) == point]
                  enemy = getEnemy player
                  filterJumps = [y | y <- listOfJumps,
                            (((getPiece board grid (thrd y)) == D ) || ((getPiece board grid (thrd y)) == enemy )) &&
                            (getPiece board grid (scnd y) == player)]
                  in
                  [updateBoardJump board grid point player z | z <- filterJumps]

-- Getting Values from the tripple tuple
frst :: (a, b, c) -> a
frst (x, _, _) = x
scnd :: (a, b, c) -> b
scnd (_,x, _) = x
thrd :: (a, b, c) -> c
thrd (_, _, x) = x

-- Get Enemy Player
getEnemy :: Piece -> Piece
getEnemy n = if(n == W) then B else W

--Get's the Piece of the given coordinates
-- Arguments:
-- Board - Current Board
-- Grid - Coordinate system
-- Point - Given Point/Coordinate
getPiece :: Board -> Grid -> Point -> Piece
getPiece board grid p
               | p == (head grid) = (head board)
               | otherwise = getPiece (tail board) (tail grid) p

updateBoardSlide :: Board -> Grid -> Point -> Piece -> Slide -> Board
updateBoardSlide board grid point player slide = addPiece (deletePiece board [] grid point) [] grid player (snd slide)

updateBoardJump :: Board -> Grid -> Point -> Piece -> Jump -> Board
updateBoardJump board grid point player jump = addPiece (deletePiece board [] grid point) [] grid player (thrd jump)

deletePiece :: Board -> Board -> Grid -> Point -> Board
deletePiece board acc grid point
                | point == (head grid) = acc ++ [D] ++ (tail board)
                | otherwise = deletePiece (tail board) (acc ++ [head board]) (tail grid) point

addPiece :: Board -> Board -> Grid -> Piece -> Point -> Board
addPiece board acc grid player point
                | point == (head grid) = acc ++ [player] ++ (tail board)
                | otherwise = addPiece (tail board) (acc ++ [head board]) (tail grid) player point

--
-- moveGenerator
--
-- This function consumes a state, a list of possible jumps,
-- a list of possible slides and a player from whose perspective
-- to generate moves, to check which of these jumps and slides
-- the player could actually make, and produces a list of valid moves
--
-- Arguments:
-- -- state: a State representing the most recent state
-- -- slides: the list of all Slides possible for the given grid
-- -- jumps: the list of all Jumps possible for the given grid
-- -- player: W or B representing the player the program is
--
-- Note: This is the only instance where the program makes use of the
--         type State, for our purposes it is zipping the board and the
--         grid together for making it easier to make moves.
--
-- Note:
-- -- oP is opponentsPieces
-- -- pP is playersPieces
-- -- vS is validSlides
-- -- vJ is validJumps
--
-- Returns: the list of all valid moves that the player could make
--
{-- moveGenerator is implemented within the generateNewStates function --}


--
-- boardEvaluator
--
-- This function consumes a board and performs a static board evaluation, by
-- taking into account whose perspective the program is playing from, the list
-- of boards already seen, the size of the board, and whether or not it is the
-- program's turn or not; to generate quantitative measures of the board, and
-- accordingly produce a goodness value of the given board
--
-- Arguments:
-- -- player: W or B representing the player the program is
-- -- history: a list of Boards of representing all boards already seen
-- -- n: an Integer representing the dimensions of the board
-- -- board: a Board representing the most recent board
-- -- myTurn: a Boolean indicating whether it is the program's turn or the opponents.
--
-- Returns: the goodness value of the provided board
--

boardEvaluator :: Piece -> [Board] -> Int -> Board -> Bool -> Int
boardEvaluator player history n board myTurn 
 | gameOver board history n && myTurn = -10
 | gameOver board history n && not myTurn = 10
 | otherwise = countPieces board player - countPieces board (getEnemy player)

--
-- minimax
--
-- This function implements the minimax algorithm, it consumes a search tree,
-- and an appropriate heuristic to apply to the tree, by applying minimax it
-- produces the next best board that the program should make a move to
--
-- Arguments:
-- -- (Node _ b children): a BoardTree to apply minimax algorithm on
-- -- heuristic: a paritally evaluated boardEvaluator representing the
--                 appropriate heuristic to apply based on the size of the board,
--                 who the program is playing as, and all the boards already seen
--
-- Returns: the next best board
--

minimax :: BoardTree -> Int -> (Board -> Bool -> Int -> Int) -> Board
minimax (Node n b children) m heuristic =
      let list = if((n `mod` 2 ) == 0 ) then
                        [((board x), (minimax' x m heuristic True))| x <- children]
                        else
                        [((board x), (minimax' x m heuristic False))| x <- children]
                in
                let value = maximum [(snd x) | x<- list]
                in
                head [(fst x) | x <- list, (snd x) == value]
--
-- minimax'
--
-- This function is a helper to the actual minimax function, it consumes
-- a search tree, an appropriate heuristic to apply to the leaf nodes of
-- the tree, and based on whether it would have been the maximizing
-- player's turn, it accordingly propogates the values upwards until
-- it reaches the top to the base node, and produces that value.
--
-- Arguments:
-- -- (Node _ b []): a BoardTree
-- -- (Node _ b children): a BoardTree
-- -- heuristic: a paritally evaluated boardEvaluator representing the
--                 appropriate heuristic to apply based on the size of the board,
--                 who the program is playing as, and all the boards already seen
-- -- maxPlayer: a Boolean indicating whether the function should be maximizing
--                  or miniziming the goodness values of its children
--
-- Returns: the minimax value at the top of the tree
--
--data Tree a = Node {depth :: Int, board :: a, nextBoards :: [Tree a]} deriving (Show)
--data Boardtree  = Tree board
minimax' :: BoardTree -> Int -> (Board -> Bool -> Int -> Int) -> Bool -> Int
minimax' boardTree n heuristic maxPlayer =
                let list =  [if (null (nextBoards x)) then (heuristic (board x) maxPlayer n) else (minimax' x n heuristic (not maxPlayer))  | x <- (nextBoards boardTree) ]
                in
                if(maxPlayer) then (maximum list) else (minimum list)

heuristic :: Board -> Bool -> Int -> Int
heuristic board myTurn n =
            let piece = if(myTurn)then W else B
            in
            boardEvaluator piece [] n board myTurn