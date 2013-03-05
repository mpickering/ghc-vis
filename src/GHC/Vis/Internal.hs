{-# LANGUAGE CPP, MagicHash, DeriveDataTypeable, NoMonomorphismRestriction, RankNTypes, RecordWildCards #-}

{- |
   Module      : GHC.Vis.Internal
   Copyright   : (c) Dennis Felsing
   License     : 3-Clause BSD-style
   Maintainer  : dennis@felsin9.de

 -}
module GHC.Vis.Internal (
  parseBoxes,
  parseBoxes2,
  parseBoxesHeap,
  showClosure,
  showClosureFields
  )
  where

#if __GLASGOW_HASKELL__ < 706
import Prelude hiding (catch)
#endif

import GHC.Vis.Types
import GHC.HeapView hiding (pkg, modl, fun, arrWords)

import Control.Monad
import Control.Monad.State hiding (State, fix)

import Data.Word
import Data.Char
import Data.Maybe (catMaybes)
import Data.List hiding (insert)
import qualified Data.IntMap as M

import Text.Printf
import Unsafe.Coerce

import Control.Monad.Trans.Maybe

import System.IO.Unsafe

-- TODO: Remove
instance Eq Box where
  a == b = unsafePerformIO $ areBoxesEqual a b

parseBoxes2 bs = do
  (hg,starts) <- multiBuildHeapGraph 100 bs
  return $ (hg, boundMultipleTimes hg $ map snd starts)

--data Type = Thunk' | Link' | 
--
--data Info = Info { typ :: Type

augment (HeapGraph m) = do
    return bindings
  where
    bindings = boundMultipleTimes $ HeapGraph m

    bindingLetter i = case hgeClosure (iToE i) of
        ThunkClosure {..} -> 't'
        SelectorClosure {..} -> 't'
        APClosure {..} -> 't'
        PAPClosure {..} -> 'f'
        BCOClosure {..} -> 't'
        FunClosure {..} -> 'f'
        _ -> 'x'

    iToE i = m M.! i

  --where go hge = hge{hgeData = hgeData hge}

-- | In the given HeapMap, list all indices that are used more than once. The
-- second parameter adds external references, commonly @[heapGraphRoot]@.
boundMultipleTimes :: HeapGraph a -> [HeapGraphIndex] -> [HeapGraphIndex]
boundMultipleTimes (HeapGraph m) roots = map head $ filter (not.null) $ map tail $ group $ sort $
     roots ++ concatMap (catMaybes . allPtrs . hgeClosure) (M.elems m)

-- | Walk the heap for a list of objects to be visualized and their
--   corresponding names.
parseBoxes :: [NamedBox] -> IO [[VisObject]]
parseBoxes bs = do
  r <- generalParseBoxes evalState bs
  case r of
    Just x -> return x
    _ -> do
      putStrLn "Failure, trying again"
      parseBoxes bs

-- | Walk the heap for a list of objects to be visualized and their
--   corresponding names. Also return the resulting 'HeapMap' and another
--   'HeapMap' that does not contain BCO pointers.
parseBoxesHeap :: [NamedBox] -> IO ([[VisObject]], PState)
parseBoxesHeap bs = do
  r <- generalParseBoxes runState bs
  case r of
    (Just x, y) -> return (x,y)
    _ -> parseBoxesHeap bs

generalParseBoxes ::
     (PrintState (Maybe [[VisObject]]) -> PState -> b)
  -> [NamedBox] -> IO b
generalParseBoxes f bs = do
  h <- walkHeapSimply bs
  h2 <- walkHeapWithBCO bs
  return $ f (g bs) $ PState 1 1 1 h h2
  where g bs' = runMaybeT $ go bs'
        go ((_,b'):b's) = do h <- lift $ gets heapMap'
                             (_,c) <- lookupT b' h
                             r <- parseClosure b' c
                             rs <- go b's
                             return $ simplify r : rs
        go [] = return []

lookupT :: Eq a => a -> [(a,b)] -> MaybeT PrintState b
lookupT b' h = MaybeT $ return $ lookup b' h

-- Pulls together multiple Unnamed objects to one
simplify :: [VisObject] -> [VisObject]
simplify [] = []
simplify [Named a bs] = [Named a $ simplify bs]
simplify [a] = [a]
simplify (Unnamed a : Unnamed b : xs) = simplify $ Unnamed (a ++ b) : xs
simplify (Named a bs : xs) = Named a (simplify bs) : simplify xs
simplify (a:xs) = a : simplify xs

parseClosure :: Box -> Closure -> MaybeT PrintState [VisObject]
parseClosure b c = do
  o <- correctObject b
  case o of
    Link n -> return [Link n] -- Don't build infinite heaps
    _      -> do i <- parseInternal b c
                 return $ insertObjects o i

correctObject :: Box -> MaybeT PrintState VisObject
correctObject box = do
  r <- lift $ countReferences box
  n <- getName box

  case n of
    Just name -> return $ Link name
    Nothing -> if r > 1 then
                 (do setName box
                     mbName <- getName box
                     name <- MaybeT $ return mbName
                     return $ Named name [])
                 else return $ Unnamed ""

insertObjects :: VisObject -> [VisObject] -> [VisObject]
insertObjects _ xs@(Function _ : _) = xs
insertObjects _ xs@(Thunk _ : _) = xs
insertObjects (Link name) _ = [Link name]
insertObjects (Named name _) xs = [Named name xs]
insertObjects (Unnamed _) xs = xs
insertObjects _ _ = error "unexpected arguments"

-- TODO: Remove
walkHeapSimply _ = return []
walkHeapWithBCO _ = return []

-- | Follows 'GHC.HeapView.BCOClosure's, but not the debugging data structures
--   (ByteCodeInstr.BreakInfo) of GHC.
pointersToFollow2 :: Closure -> IO [Box]
pointersToFollow2 (MutArrClosure _ _ _ bPtrs) =
  do cPtrs <- mapM getBoxedClosureData bPtrs
     return $ fix $ zip bPtrs cPtrs
  where fix ((_,ConsClosure _ _ _ _ "ByteCodeInstr" "BreakInfo"):_:_:xs) = fix xs
        fix ((_,ConsClosure _ _ _ _ "ByteCodeInstr" "BreakInfo"):_:xs) = fix xs
        fix ((_,ConsClosure _ _ _ _ "ByteCodeInstr" "BreakInfo"):xs) = fix xs
        fix ((x,_):xs) = x : fix xs
        fix [] = []

pointersToFollow2 x = return $ allPtrs x

-- walkHeap, but with a maximum depth
--walkHeapDepth :: [NamedBox] -> IO HeapMap
--walkHeapDepth bs = foldM topNodes [dummy bs] bs >>= \s -> foldM goStart s bs
--  where topNodes l (b,n) = do -- Adds the top nodes without looking at their pointers
--            c' <- getBoxedClosureData b
--            return $ insert (b, (Just n, c')) l
--        goStart l (b,_) = do -- Ignores that the top nodes are already in the heap map
--          c' <- getBoxedClosureData b
--          p  <- pointersToFollow c'
--          foldM (\l b -> go l b 30) l p
--        go l _ 0 = return l
--        go l b x = case lookup b l of
--          Just _  -> return l
--          Nothing -> do
--            c' <- getBoxedClosureData b
--            p  <- pointersToFollow c'
--            foldM (\l b -> go l b (x-1)) (insert (b, (Nothing, c')) l) p

-- Additional map operations
adjust :: (HeapEntry -> HeapEntry) -> Box -> HeapMap -> Maybe HeapMap
adjust f b h = do i <- findIndex (\(y,_) -> y == b) h
                  let (h1,(_,x):h2) = splitAt i h
                  return $ h1 ++ ((b,f x) : h2)

setName :: Box -> MaybeT PrintState ()
setName b = do PState ti fi bi h h2 <- lift get
               (_,c) <- lookupT b h2
               let (n, ti',fi',bi') = case c of
                     ThunkClosure{} -> ('t' : show ti, ti+1, fi, bi)
                     APClosure{}    -> ('t' : show ti, ti+1, fi, bi)
                     FunClosure{}   -> ('f' : show fi, ti, fi+1, bi)
                     PAPClosure{}   -> ('f' : show fi, ti, fi+1, bi)
                     _              -> ('x' : show bi, ti, fi, bi+1)
                   set (Nothing, closure) = (Just n, closure)
                   set _ = error "unexpected pattern"
               h' <- MaybeT $ return $ adjust set b h
               h2' <- MaybeT $ return $ adjust set b h2
               lift $ put $ PState ti' fi' bi' h' h2'

setVisited :: Box -> MaybeT PrintState ()
setVisited b = do PState ti fi bi h h2 <- lift get
                  let set (Nothing, closure) = (Just "visited", closure)
                      set _ = error "unexpected pattern"
                  h2' <- MaybeT $ return $ adjust set b h2
                  lift $ put $ PState ti fi bi h h2'

getName :: Box -> MaybeT PrintState (Maybe String)
getName b = do h <- lift $ gets heapMap'
               (name,_) <- lookupT b h
               return name

getSetName :: Box -> MaybeT PrintState String
getSetName b = do mn <- getName b
                  case mn of
                    Nothing   -> do setName b
                                    name <- getName b
                                    MaybeT $ return name
                    Just name -> return name

-- How often is a box referenced in the entire heap map
countReferences :: Box -> PrintState Int
countReferences b = do
  h <- gets heapMap'
  return $ sum $ map countR h
 where countR (_,(_,c)) = length $ filter (== b) $ allPtrs c

parseInternal :: Box -> Closure -> MaybeT PrintState [VisObject]
parseInternal _ (ConsClosure _ [] [dataArg] _pkg modl name) =
 return [Unnamed $ case (modl, name) of
    k | k `elem` [ ("GHC.Word", "W#")
                 , ("GHC.Word", "W8#")
                 , ("GHC.Word", "W16#")
                 , ("GHC.Word", "W32#")
                 , ("GHC.Word", "W64#")
                 ] -> name ++ " " ++ show dataArg

    k | k `elem` [ ("GHC.Integer.Type", "S#")
                 , ("GHC.Types", "I#")
                 , ("GHC.Int", "I8#")
                 , ("GHC.Int", "I16#")
                 , ("GHC.Int", "I32#")
                 , ("GHC.Int", "I64#")
                 ] -> name ++ " " ++ show ((fromIntegral :: Word -> Int) dataArg)

    ("GHC.Types", "C#") -> show . chr $ fromIntegral dataArg
    --("GHC.Types", "C#") -> '\'' : (chr $ fromIntegral dataArg) : "'"

    ("GHC.Types", "D#") -> printf "D# %0.5f" (unsafeCoerce dataArg :: Double)
    ("GHC.Types", "F#") -> printf "F# %0.5f" (unsafeCoerce dataArg :: Double)

    _ -> printf "%s %d" (infixFix name) dataArg
  ]

-- Empty ByteStrings point to a nullForeignPtr, evaluating it leads to an
-- Prelude.undefined exception
parseInternal _ (ConsClosure (StgInfoTable 1 3 _ _) _ [_,0,0] _ "Data.ByteString.Internal" "PS")
  = return [Unnamed "ByteString 0 0"]

parseInternal _ (ConsClosure (StgInfoTable 1 3 _ _) [bPtr] [_,start,end] _ "Data.ByteString.Internal" "PS")
  = do cPtr  <- liftM mbParens $ contParse bPtr
       return $ Unnamed (printf "ByteString %d %d " start end) : cPtr

parseInternal _ (ConsClosure (StgInfoTable 2 3 _ _) [bPtr1,bPtr2] [_,start,end] _ "Data.ByteString.Lazy.Internal" "Chunk")
  = do cPtr1 <- liftM mbParens $ contParse bPtr1
       cPtr2 <- liftM mbParens $ contParse bPtr2
       return $ Unnamed (printf "Chunk %d %d " start end) : cPtr1 ++ [Unnamed " "] ++ cPtr2

parseInternal _ (ConsClosure (StgInfoTable 2 0 _ _) [bHead,bTail] [] _ "GHC.Types" ":")
  = do cHead <- liftM mbParens $ contParse bHead
       cTail <- liftM mbParens $ contParse bTail
       return $ cHead ++ [Unnamed ":"] ++ cTail

parseInternal _ (ConsClosure _ bPtrs dArgs _ _ name)
  = do cPtrs <- mapM (liftM mbParens . contParse) bPtrs
       let tPtrs = intercalate [Unnamed " "] cPtrs
       let sPtrs = if null tPtrs then [Unnamed ""] else Unnamed " " : tPtrs
       return $ Unnamed (unwords $ infixFix name : map show dArgs) : sPtrs

parseInternal _ (ArrWordsClosure _ _ arrWords)
  = return $ intercalate [Unnamed ","] (map (\x -> [Unnamed (printf "0x%x" x)]) arrWords)

parseInternal _ (IndClosure _ b)
  = contParse b

parseInternal _ (SelectorClosure _ b)
  = contParse b

parseInternal _ (BlackholeClosure _ b)
  = contParse b

parseInternal _ BlockingQueueClosure{}
  = return [Unnamed "BlockingQueue"]

parseInternal _ (OtherClosure (StgInfoTable _ _ cTipe _) _ _)
  = return [Unnamed $ show cTipe]

parseInternal _ (UnsupportedClosure (StgInfoTable _ _ cTipe _))
  = return [Unnamed $ show cTipe]

-- Reversed order of ptrs
parseInternal b (ThunkClosure _ bPtrs args) = do
  name <- getSetName b
  cPtrs <- mapM contParse $ reverse bPtrs
  let tPtrs = intercalate [Unnamed ","] cPtrs
      sPtrs = if null tPtrs then [Unnamed ""] else Unnamed "(" : tPtrs ++ [Unnamed ")"]
      sArgs = Unnamed $ if null args then "" else show args
  return $ Thunk (infixFix name) : sPtrs ++ [sArgs]

parseInternal b (FunClosure _ bPtrs args) = do
  name <- getSetName b
  cPtrs <- mapM contParse $ reverse bPtrs
  let tPtrs = intercalate [Unnamed ","] cPtrs
      sPtrs = if null tPtrs then [Unnamed ""] else Unnamed "(" : tPtrs ++ [Unnamed ")"]
      sArgs = Unnamed $ if null args then "" else show args
  return $ Function (infixFix name) : sPtrs ++ [sArgs]

-- bPtrs here can currently point to Nothing, because else we might get infinite heaps
parseInternal _ (MutArrClosure _ _ _ bPtrs)
  = do cPtrs <- mutArrContParse bPtrs
       let tPtrs = intercalate [Unnamed ","] cPtrs
       return $ if null tPtrs then [Unnamed ""] else Unnamed "(" : tPtrs ++ [Unnamed ")"]

parseInternal _ (MutVarClosure _ b)
  = do c <- contParse b
       return $ Unnamed "MutVar " : c

parseInternal b (BCOClosure _ _ _ bPtr _ _ _)
  = do cPtrs <- bcoContParse [bPtr]
       let tPtrs = intercalate [Unnamed ","] cPtrs
       r <- lift $ countReferences b
       return $ if null tPtrs then if r > 1 then [Unnamed "BCO"] else [Unnamed ""] else (if r > 1 then Unnamed "BCO(" else Unnamed "(") : tPtrs ++ [Unnamed ")"]
  -- = do case lookup b h of
  --        Nothing -> c <- getBoxedClosureData bPtr
  --        Just (_,c) -> p  <- parseClosure bPtr c
  -- = do vs <- contParse bPtr
  --      let ls = filter isExternal $ filter isLink vs

  --          isLink (Link _) = True
  --          isLink _ = False

  --          isExternal (Link n) = all (notHasName n) vs

  --          notHasName n (Named m _) = n /= m
  --          notHasName n (Function m) = n /= m
  --          notHasName _ _ = True
  --      return vs

parseInternal b (APClosure _ _ _ fun pl) = do
  name <- getSetName b
  fPtr <- contParse fun
  pPtrs <- mapM contParse $ reverse pl
  let tPtrs = intercalate [Unnamed ","] pPtrs
      sPtrs = if null tPtrs then [Unnamed ""] else Unnamed "[" : tPtrs ++ [Unnamed "]"]
  return $ Thunk (infixFix name) : fPtr ++ sPtrs

parseInternal b (PAPClosure _ _ _ fun pl) = do
  name <- getSetName b
  fPtr <- contParse fun
  pPtrs <- mapM contParse $ reverse pl
  let tPtrs = intercalate [Unnamed ","] pPtrs
      sPtrs = if null tPtrs then [Unnamed ""] else Unnamed "[" : tPtrs ++ [Unnamed "]"]
  return $ Function (infixFix name) : fPtr ++ sPtrs

parseInternal b (APStackClosure _ fun pl) = do
  name <- getSetName b
  fPtr <- contParse fun
  pPtrs <- mapM contParse $ reverse pl
  let tPtrs = intercalate [Unnamed ","] pPtrs
      sPtrs = if null tPtrs then [Unnamed ""] else Unnamed "[" : tPtrs ++ [Unnamed "]"]
  return $ Thunk (infixFix name) : fPtr ++ sPtrs

parseInternal _ (MVarClosure _ qHead qTail qValue)
   = do cHead <- liftM mbParens $ contParse qHead
        cTail <- liftM mbParens $ contParse qTail
        cValue <- liftM mbParens $ contParse qValue
        return $ Unnamed "MVar#(" : cHead ++ [Unnamed ","] ++ cTail ++ [Unnamed ","] ++ cValue ++ [Unnamed ")"]

contParse :: Box -> MaybeT PrintState [VisObject]
contParse b = do h <- lift $ gets heapMap
                 (_,c) <- lookupT b h
                 parseClosure b c

-- It turned out that bcoContParse actually does go into an infinite loop, for
-- example for this:
-- foldr' op i [] = i
-- foldr' op i (x:xs) = op x (foldr' op i xs)
-- :view foldr'
-- We fix this by giving visited closures a dummy name, so we recognize when we
-- get into a loop.
bcoContParse :: [Box] -> MaybeT PrintState [[VisObject]]
bcoContParse [] = return []
bcoContParse (b:bs) = gets heapMap >>= \h -> case lookup b h of
  Nothing    -> do let ptf = unsafePerformIO $ getBoxedClosureData b >>= pointersToFollow2
                   r <- lift $ countReferences b
                   n <- getName b
                   case n of
                     Just _ -> bcoContParse bs
                     Nothing -> do
                       when (r > 1) $ setVisited b
                       bcoContParse $ ptf ++ bs
  Just (_,c) -> do p  <- parseClosure b c
                   ps <- bcoContParse bs
                   return $ p : ps

mutArrContParse :: [Box] -> MaybeT PrintState [[VisObject]]
mutArrContParse [] = return []
mutArrContParse (b:bs) = gets heapMap >>= \h -> case lookup b h of
  Nothing -> mutArrContParse bs
  Just (_,c) -> do p  <- parseClosure b c
                   ps <- mutArrContParse bs
                   return $ p : ps

-- TODO: Doesn't work quite right, for example with (1,"fo")
mbParens :: [VisObject] -> [VisObject]
mbParens t = if needsParens
    then Unnamed "(" : t ++ [Unnamed ")"]
    else t
  where needsParens = go (0 :: Int) $ show t
        go 0 (' ':_) = True
        go _ [] = False
        go c (')':ts) = go (c-1) ts
        go c ('(':ts) = go (c+1) ts
        go c ('\'':'(':ts) = go c ts
        go c ('\'':')':ts) = go c ts
        go c (_:ts) = go c ts
--mbParens t | ' ' `objElem` t = Unnamed "(" : t ++ [Unnamed ")"]
--           | otherwise     = t
--  where objElem c = any go
--          where go (Unnamed xs) = c `elem` xs
--                go (Named _ os) = any go os
--                go _ = False

-- | Textual representation of Heap objects, used in the graph visualization.
showClosure :: Closure -> String
showClosure = unwords . showClosureFields

showClosureFields :: GenClosure t -> [String]
showClosureFields (ConsClosure _ _ [dataArg] _ modl name) =
 case (modl, name) of
    k | k `elem` [ ("GHC.Word", "W#")
                 , ("GHC.Word", "W8#")
                 , ("GHC.Word", "W16#")
                 , ("GHC.Word", "W32#")
                 , ("GHC.Word", "W64#")
                 ] -> [name, show dataArg]

    k | k `elem` [ ("GHC.Integer.Type", "S#")
                 , ("GHC.Types", "I#")
                 , ("GHC.Int", "I8#")
                 , ("GHC.Int", "I16#")
                 , ("GHC.Int", "I32#")
                 , ("GHC.Int", "I64#")
                 ] -> [name, show ((fromIntegral :: Word -> Int) dataArg)]

    ("GHC.Types", "C#") -> ["C#", [chr $ fromIntegral dataArg]]

    ("GHC.Types", "D#") -> ["D#", printf "%0.5f" (unsafeCoerce dataArg :: Double)]
    ("GHC.Types", "F#") -> ["F#", printf "%0.5f" (unsafeCoerce dataArg :: Double)]

    -- :m +GHC.Arr
    -- let b = array ((1,1),(3,2)) [((1,1),42),((1,2),23),((2,1),999),((2,2),1000),((3,1),1001),((3,2),1002)]
    -- b
    -- :view b
    _ -> [name, show dataArg]

showClosureFields (ConsClosure (StgInfoTable 1 3 _ 0) _ [_,0,0] _ "Data.ByteString.Internal" "PS")
  = ["ByteString","0","0"]

showClosureFields (ConsClosure (StgInfoTable 1 3 _ 0) [_] [_,start,end] _ "Data.ByteString.Internal" "PS")
  = ["ByteString",printf "%d" start,printf "%d" end]

showClosureFields (ConsClosure (StgInfoTable 2 3 _ 1) [_,_] [_,start,end] _ "Data.ByteString.Lazy.Internal" "Chunk")
  = ["Chunk",printf "%d" start,printf "%d" end]

showClosureFields (ConsClosure _ _ dArgs _ _ name)
  = name : map show dArgs

-- Reversed order of ptrs
showClosureFields ThunkClosure{}
  = ["Thunk"]

showClosureFields SelectorClosure{}
  = ["Selector"]

-- Probably should delete these from Graph
showClosureFields IndClosure{}
  = ["Ind"]

showClosureFields BlackholeClosure{}
  = ["Blackhole"]

showClosureFields APClosure{}
  = ["AP"]

showClosureFields PAPClosure{}
  = ["PAP"]

showClosureFields APStackClosure{}
  = ["APStack"]

showClosureFields BCOClosure{}
  = ["BCO"]

showClosureFields (ArrWordsClosure _ _ arrWords)
  = map (printf "0x%x") arrWords

showClosureFields MutArrClosure{}
  = ["MutArr"]

showClosureFields MutVarClosure{}
  = ["MutVar"]

showClosureFields MVarClosure{}
  = ["MVar#"]

showClosureFields FunClosure{}
  = ["Fun"]

showClosureFields BlockingQueueClosure{}
  = ["BlockingQueue"]

showClosureFields (OtherClosure (StgInfoTable _ _ cTipe _) _ _)
  = [show cTipe]

showClosureFields (UnsupportedClosure (StgInfoTable _ _ cTipe _))
  = [show cTipe]

--showClosure c = "Missing pattern for " ++ show c

-- | Make infix names prefix
infixFix :: String -> String
infixFix xs
  | isInfix xs = '(' : xs ++ ")"
  | otherwise  = xs

-- | Determine whether a name is an infix name, based on
-- http://www.haskell.org/onlinereport/haskell2010/haskellch2.html
isInfix :: String -> Bool
isInfix []              = False
isInfix ('[':_)         = False
isInfix ('(':_)         = False
isInfix (x:_)
  | x `elem` ascSymbols = True
  | isSymbol x          = True
  | isPunctuation x     = True
  | otherwise           = False
  where ascSymbols = "!#$%&*+./<=>?@  \\^|-~:"
