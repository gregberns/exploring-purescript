module Parse where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import Data.Show (show)
--import Data.Array ((!!), index, toUnfoldable)
import Data.List (List(..), (:), (!!), index,  
                  toUnfoldable, fromFoldable, foldl, insertBy, insertAt,
                  sortBy)
import Data.Function (on)
import Data.Ord (compare)
import Data.Traversable (sequence)
import Data.Maybe (Maybe(..))
import Data.Tuple

-- toUnfoldable [[1,2,3],[4,5,6],[7,8,9]]
-- table = fromFoldable [
--     [1,2,3],
--     [4,5,6],
--     [7,8,9]
--   ]

table = fromFoldable [
    fromFoldable ([1,2,3]),
    fromFoldable ([4,5,6]),
    fromFoldable ([7,8,9])
  ]

tableB = [1,2,3]

trans :: List Int -> List Int
trans = map (\r -> r + 1)

headers :: List (Tuple Int String)
headers = ((Tuple 0 "Col1") : (Tuple 1 "Col2") : (Tuple 2 "Col3") : Nil)

-- > getCol 1 table
-- (Just [2,5,8])
getTblCol :: forall a. Int -> List (List a) -> Maybe (List a)
getTblCol i tbl = sequence (map (\r -> index r i) tbl)

-- > getTblRow 0 table
-- (Just [1,2,3])
getTblRow :: forall a. Int -> List (List a) -> Maybe (List a)
getTblRow i tbl = index tbl i


--validate that all rows in table have same length
--reorder columns?
--build map from one column to another, then build new table with the new order

--colMap = [Tuple 0 1, Tuple 1 2, Tuple 2 0]
colMap = (Tuple 0 1 : Tuple 1 2: Tuple 2 0 : Nil)

-- > remap colMap table
-- [(Tuple 1 (Just [2,5,8])),(Tuple 2 (Just [3,6,9])),(Tuple 0 (Just [1,4,7]))]
-- remap :: List (Tuple Int Int) -> List (List Int) -> List (Tuple Int (Maybe (List Int)))  -- -> List (List Int)
-- remap colmap tbl =
--   let 
--     --f (Tuple a b) = Tuple (snd t) (getTblCol (snd t) tbl)
--     rem t (Tuple a b) = Tuple a (getTblCol b t)
--     -- order t = undefined
--   in
--      (map (rem tbl) colmap)


-- sortOrder :: List (Tuple Int (Maybe (List Int))) -> List (List Int)
-- sortOrder tbl =
--   let 
    
--     compareTuple (Tuple a b) (Tuple a' b') = compare a a'    
    
--     -- insertBy :: forall a. (a -> a -> Ordering) -> a -> List a -> List a
--     -- ord :: Tuple Int
--     ord :: forall i l. List (Tuple i l) -> (Tuple i l) -> List (Tuple i l)
--     ord acc t = insertBy compareTuple t acc
--   in
    
--     foldl ord Nil tbl

-- reorder :: List (Tuple Int Int) -> List (List Int) -> List (List Int)
-- reorder mapping table =
--   --maybe sort for security
  
--   let
--     --insertAt :: forall a. Int -> a -> Array a -> Maybe (Array a)
--     reorderRow row = insertAt j i
--   in
--     mapMaybe reorderRow table



foreign import addString :: Array (Tuple String String) -> String -> Array String
addS a b = addString a b


foreign import remapIndexes :: forall a b c. Array (Tuple a b) -> Array c -> Array c

--rem = remapIndexes [Tuple 0 1, Tuple 1 2, Tuple 2 0] [4,5,6]

--rem = remapIndexes ( Tuple 0 1 : Tuple 1 2 : Tuple 2 0 : Nil) (4:5:6:Nil)

reorderCols :: List (Tuple Int Int) -> List (List Int) -> List (List Int)
reorderCols cols tbl = 
  let 
    --f :: 
    f col = fromFoldable ( remapIndexes (toUnfoldable cols) (toUnfoldable col) )
  in
    map f tbl

rem = reorderCols colMap table

-- Read first line and get column names
-- Column names are now 

--Translate from one table to another

--Validate data types
--  Need to provide column info in with the actual data from a column

