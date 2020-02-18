import Control.Monad.RWS.Lazy
import Data.Function (on)
import Data.List (intersperse, sort)
import System.IO (withFile, IOMode(WriteMode), hPutStrLn)
import System.Process (system)

-- first Int is its number
--  Add has two parents
--  Rand none
data Node = Add Int Node Node | Rand Int
  deriving (Show, Eq)

-- number first int
number :: Node -> Int
number (Add a _ _) = a
number (Rand a)    = a

firstNode :: Node -> Node
firstNode (Rand _)    = undefined
firstNode (Add _ n _) = n

secondNode :: Node -> Node
secondNode (Rand _)    = undefined
secondNode (Add _ _ n) = n

-- derived Ord doesn't sort by first Int
instance Ord Node where
  compare = compare `on` number

isAdd :: Node -> Bool
isAdd (Add _ _ _) = True
isAdd _           = False

isRand :: Node -> Bool
isRand (Rand _) = True
isRand _        = False

-- Random Sharing of Zero (RSZ) is given by short program
-- RWS is Reader/Writer/State and can be run with
-- runRWS :: RWS r w s a -> r -> s -> (a, s, w)
type RSZ k s a = RWS k k s a

type NodeFinder a = RSZ [Node] (Int, Int, [(Node, Node)]) a

-- runs program and calculate commands followed by outputs
--process :: RSZ [Node] (Int, Int, [Int]) [Node] -> ((Int, Int), [Node], [Int])
process :: NodeFinder [Node] -> ((Int, Int), [Node], [Int])
process rsz = let (a, (s, s', ds), k) = runRWS rsz k (0, s, [])
                  cs = map (replace ds).sort $ filter isAdd k
                  os = map number a
                  replace [] n     = n
                  replace (d:ds) n = if fst d == n
                                       then snd d
                                       else replace ds n
              in ((s, length os), cs, os)

pretty :: ((Int, Int), [Node], [Int]) -> [[Char]]
pretty ((r, o), rs, os) = firstLine:secondLine:otherLines
  where firstLine = join " " $ map show [o-1, r]
        secondLine = join " " $ map show $ sort os
        otherLines = map showContent rs
        showContent (Add _ a b) = join " " $ map (show.number) [a, b]
        join x = concat . intersperse x

-- reads number from state, uses it for random and increases state
rand :: NodeFinder Node
rand = do
  (r, a, ds) <- get -- state
  put (r+1, a, ds)  -- state
  ns <- ask         -- reader (reads from writer)
  let n' = Rand r
  tell [n']         -- writer (writes to reader)
  return n'

-- add two nodes and return result
add :: Node -> Node -> NodeFinder Node
add x y = do
  (r, a, ds) <- get -- state
  put (r, a+1, ds)  -- state
  ns <- ask
  let n' = Add a x y
  tell [n']
  return n'

-- alternate randoms with sums of neighboring randoms
line' n | n<3 = undefined
line' 3 = do
  x <- rand
  y <- rand
  z <- add x y
  return [y, z, x]
line' n = do
  xs <- line' (n-1)
  x <- rand
  y <- add (head xs) x
  return (x:y:xs)

-- random followed by sums of randoms ending in random
line n = do
  xs <- line' n
  return $ head xs:f xs
    where f [Rand r]    = [Rand r]
          f (Rand r:ns) = f ns
          f (n:ns)      = n:f ns

-- alternating circle of randoms and their sums
circle' n = do
  xs <- line' n
  r <- rand
  x <- add (head xs) r
  y <- add r (last xs)
  return (y:r:x:xs)

-- circle of sum of randoms
circle n = do
  xs <- circle' n
  return $ filter isAdd xs

-- given two nodes, return both after adding fresh random
connect n n' = do
  r <- rand
  x <- add n r
  y <- add r n'
  return (x, y)

-- replace m-th and n-th output by their sums with new random
link m n = \xs -> do
  let (as, bs) = splitAt m xs
  let (cs, ds) = splitAt (n-m) bs
  (x, y) <- (connect `on `head) cs ds
  return $ as ++ (x:tail cs) ++ (y:tail ds)

-- replace m-th and n-th output by their sums with new random
--  BUT 1st component of m-th output first added to new random
link' m n = \xs -> do
  r <- rand
  let (as, bs) = splitAt m xs
  let (cs, ds) = splitAt (n-m) bs
  let c = head cs
  x <- if isAdd c
         then do
           let c' = Add (number c) r $ secondNode c
           (r', a, ds) <- get      -- state
           put (r', a, (c, c'):ds) -- state
           add c' $ firstNode c
         else add c r
  y <- add r (head ds)
  return $ as ++ (x:tail cs) ++ (y:tail ds)

-- replace m-th and n-th output by their sums with new random
--  BUT 2nd component of m-th output first added to new random
link'' m n = \xs -> do
  r <- rand
  let (as, bs) = splitAt m xs
  let (cs, ds) = splitAt (n-m) bs
  let c = head cs
  x <- if isAdd c
         then do
           let c' = Add (number c) r $ firstNode c
           (r', a, ds) <- get      -- state
           put (r', a, (c, c'):ds) -- state
           add c' $ secondNode c
         else add c r
  y <- add r (head ds)
  return $ as ++ (x:tail cs) ++ (y:tail ds)

-- given two lists of nodes connect them pairwise and return results
extend ns ns' = do
  let zs = zip ns ns'
  zs' <- mapM (uncurry connect) zs
  return $ uncurry (++) $ unzip zs'

stack ns ns' = do
  let zs = zip ns ns'
  zs' <- mapM (uncurry add) zs
  return zs'

extend' x y = do
  as <- x
  bs <- y
  extend as bs

stack' x y = do
  as <- x
  bs <- y
  stack as bs

mirror xs = do
  return $ reverse xs

dump fileName prog = withFile fileName WriteMode (\h -> mapM (hPutStrLn h) $ pretty $ process prog)
check fileName prog = withFile fileName WriteMode (\h -> mapM (hPutStrLn h) $ pretty $ process prog) >> system ("./checkMaskRefresh " ++ fileName)
