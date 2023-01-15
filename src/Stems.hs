{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NoImplicitPrelude #-}

module Stems where

import qualified Data.List as L
import qualified Data.Map as M
import qualified Data.Maybe
import qualified Data.Text as T
import Protolude
import qualified Prelude

newtype Stem a = Stem {unStem :: [a]} deriving (Eq, Foldable, Show)

instance Semigroup (Stem a) where
  (<>) :: Stem a -> Stem a -> Stem a
  Stem x <> Stem y = Stem (x <> y)

instance Monoid (Stem a) where
  mempty :: Stem a
  mempty = Stem mempty

comp :: Eq a => Stem a -> Stem a -> Maybe (Stem a)
comp (Stem ys) (Stem xs)
  | ys `isPrefixOf` xs = Just $ Stem $ drop (length ys) xs
  | otherwise = Nothing

superStem :: Eq a => Stem a -> Stem a -> Bool
superStem y = isJust . comp y

dist :: Eq a => Stem a -> Stem a -> Maybe Int
dist y = fmap length . comp y

newtype Population a = Population [Stem a] deriving (Foldable, Show)

cleanWords :: Text -> [Text]
cleanWords text = L.nub $ T.toCaseFold <$> filter (not . T.null) (cleanWord <$> T.words text)
  where
    cleanWord = T.dropAround (not . isLetter)

-- | read input from a file
population :: FilePath -> IO (Population Char)
population fp = do
  text <- readFile fp
  return $ Population $ Stem <$> toS <$> cleanWords text

subs :: Eq a => Stem a -> Population a -> Population a
subs x (Population xs) = Population $ filter (superStem x) xs

size :: Population a -> Int
size = length

value :: Eq a => Population a -> Stem a -> Stem a -> Maybe Int
value xs y x = (size (subs x xs) *) <$> dist y x

-- | given a population and one stem in it computes the (at-most) 'n' most valuable
-- super stems
query ::
  Eq a =>
  -- | maximum number of super stems
  Int ->
  -- | stem population
  Population a ->
  -- | base stem
  Stem a ->
  -- | super stems
  [Stem a]
query n p@(Population stems) base =
  take n $
    sortOn (Down . value p base) $
      filter (superStem base) stems

mainNaive :: FilePath -> Int -> IO b
mainNaive fp n = do
  --  [fp, n'] <- getArgs
  p <- population fp
  let q = query n p
  forever $ do
    x <- Prelude.getLine
    traverse (Prelude.putStrLn . unStem) $ q $ Stem x

-- TODO: refine the value function so that query returns something better :-)
-- (in case you do not like it)

-- TODO: define population as a trie usind Data.Map
-- so that values are computed at trie construction time

type ToBeImplemented = Void

data PopulationTrie a = PopulationTrie (Maybe (Stem a)) (M.Map a (PopulationTrie a))
  deriving (Show, Eq)

emptyTrie :: PopulationTrie a
emptyTrie = PopulationTrie Nothing M.empty

-- | primitive operation to update a population trie with a new stem
insert :: Ord a => Stem a -> PopulationTrie a -> PopulationTrie a
insert (Stem []) p = p
insert s@(Stem [_]) (PopulationTrie _ m) = PopulationTrie (Just s) m
insert (Stem (k : ks)) (PopulationTrie x m) =
  let t = Data.Maybe.fromMaybe emptyTrie (M.lookup k m)
      t'' = insert (Stem ks) t
   in PopulationTrie x (M.insert k t'' m)

lookupSubtree :: Ord a => Stem a -> PopulationTrie a -> PopulationTrie a
lookupSubtree (Stem []) p = p
lookupSubtree (Stem (k : ks)) (PopulationTrie _ m) = case M.lookup k m of
  Just t -> lookupSubtree (Stem ks) t
  Nothing -> emptyTrie

trieToList :: PopulationTrie a -> [Stem a]
trieToList (PopulationTrie x m) = case x of
  Nothing -> expand
  Just word -> word : expand
  where
    expand = [Stem (char : word) | (char, trie) <- M.toList m, Stem word <- trieToList trie]

trieSize :: PopulationTrie a -> Int
trieSize (PopulationTrie x m) =
  let n = case x of
        Nothing -> 0
        Just _ -> 1
   in n + sum (trieSize <$> M.elems m)

trieValue :: Ord a => PopulationTrie a -> Stem a -> Stem a -> Maybe Int
trieValue xs y x = (trieSize (lookupSubtree x xs) *) <$> dist y x

populationTrie :: FilePath -> PopulationTrie Char -> IO (PopulationTrie Char)
populationTrie fp p = do
  text <- readFile fp
  return $ foldr insert p $ Stem . toS <$> cleanWords text

-- | select the trie of the given stem and traverse all children to
-- rebuild the 'n' best super-stems
queryTrie ::
  Ord a =>
  -- | maximum number of super stems
  Int ->
  -- | stem population
  PopulationTrie a ->
  -- | base ste
  Stem a ->
  -- | super stems
  [Stem a]
queryTrie n p base =
  take
    n
    ( sortOn (Down . trieValue p base) $
        (base <>) <$> trieToList (lookupSubtree base p)
    )

mainTrie :: FilePath -> Int -> IO b
mainTrie fp n = do
  --  [fp, n'] <- getArgs
  p <- populationTrie fp (PopulationTrie Nothing M.empty)
  let q = queryTrie n p
  forever $ do
    x <- Prelude.getLine
    q' <-
      if "@l:" `isPrefixOf` x
        then do
          let (_, fp') = splitAt 3 x
          p' <- populationTrie fp' p
          return $ queryTrie n p'
        else return q
    traverse (Prelude.putStrLn . unStem) $ q' $ Stem x

-- TODO: prove that is possible/impossible to prune the search based on values only
-- and that is unnecessary/necessary to store some other auxiliary information at
-- each node

-- TODO: optimize the population trie by hardcoding 'n' and tracking some
-- auxiliary information in the nodes to then traverse only the necessary subtries
-- to collect the best super-stems of a node

data Node a = Node {stem :: !(Maybe (Stem a)), bestStems :: ![Stem a]}

data PopulationTrieO a = PopulationTrieO !(Node a) !(M.Map a (PopulationTrieO a))

emptyTrieO :: PopulationTrieO a
emptyTrieO = PopulationTrieO (Node Nothing []) M.empty

trieOToList :: PopulationTrieO a -> [Stem a]
trieOToList (PopulationTrieO (Node x _) m) = case x of
  Nothing -> expand
  Just word -> word : expand
  where
    expand = [Stem (char : word) | (char, trie) <- M.toList m, Stem word <- trieOToList trie]

-- | primitive operation to update a population trie with a new stem
insertO :: Ord a => Stem a -> PopulationTrieO a -> PopulationTrieO a
insertO (Stem []) p = p
insertO s@(Stem [_]) (PopulationTrieO _ m) = PopulationTrieO (Node (Just s) []) m
insertO (Stem (k : ks)) (PopulationTrieO x m) =
  let t = Data.Maybe.fromMaybe emptyTrieO (M.lookup k m)
      t'' = insertO (Stem ks) t
   in PopulationTrieO x (M.insert k t'' m)

trieSizeO :: PopulationTrieO a -> Int
trieSizeO (PopulationTrieO (Node x _) m) =
  let n = case x of
        Nothing -> 0
        Just _ -> 1
   in n + sum (trieSizeO <$> M.elems m)

lookupSubtreeO :: Ord a => Stem a -> PopulationTrieO a -> PopulationTrieO a
lookupSubtreeO (Stem []) p = p
lookupSubtreeO (Stem (k : ks)) (PopulationTrieO _ m) = case M.lookup k m of
  Just t -> lookupSubtreeO (Stem ks) t
  Nothing -> emptyTrieO

trieValueO :: Ord a => PopulationTrieO a -> Stem a -> Stem a -> Maybe Int
trieValueO xs y x = (trieSizeO (lookupSubtreeO x xs) *) <$> dist y x

trackTrie :: Ord a => Int -> PopulationTrieO a -> PopulationTrieO a
trackTrie n t@(PopulationTrieO (Node x _) m) =
  PopulationTrieO (Node x (bestNodes t)) (M.map (trackTrie n) m)
  where
    bestNodes t' = take n $ sortOn (Down . trieValueO t' mempty) $ trieOToList t'

populationTrieO :: Int -> FilePath -> StateT (PopulationTrieO Char) IO ()
populationTrieO n fp = do
  text <- liftIO $ readFile fp
  oldTrie <- get
  put $ trackTrie n $ foldr insertO oldTrie $ Stem . toS <$> cleanWords text

-- | select the trie of the given stem and traverse only necessary children to
-- rebuild the 'n' best super-stems
queryTrieO ::
  Ord a =>
  -- | stem population
  PopulationTrieO a ->
  -- | base ste
  Stem a ->
  -- | super stems
  [Stem a]
queryTrieO p base =
  let PopulationTrieO (Node _ xs) _ = lookupSubtreeO base p
   in (base <>) <$> xs

queryTrieInfixO :: Ord a => PopulationTrieO a -> Stem a -> [Stem a]
queryTrieInfixO p pat = go pat p pat
  where
    go base p' pat' =
      let p''@(PopulationTrieO (Node _ xs) _) = lookupSubtreeO pat' p'
       in ((base <>) <$> xs) <> go base p'' pat

-- TODO: write main so that we can add files to the population interactevely as
-- we perform searches

mainTrieO :: FilePath -> Int -> IO [()]
mainTrieO fp n = do
  p <- execStateT (populationTrieO n fp) emptyTrieO
  let q = queryTrieO p
  go q p
  where
    go q p = do
      x <- Prelude.getLine
      (p', q') <-
        if "@l:" `isPrefixOf` x
          then do
            let (_, fp') = splitAt 3 x
            p' <- execStateT (populationTrieO n fp') p
            return (p', queryTrieO p')
          else return (p, q)
      traverse_ (Prelude.putStrLn . unStem) $ q' $ Stem x
      go q' p'

-- TODO: improve the trie so that it returns also matches for infixes
