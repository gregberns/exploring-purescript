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


foreign import remapIndexes :: forall a b c. Array (Tuple a b) -> Array c -> Array c

-- usage :
-- reorderCols (Tuple 0 1 : Tuple 1 2: Tuple 2 0 : Nil) ((1 : 2 : 3 : Nil) : (4 : 5 : 6 : Nil) : (7 : 8 : 9 : Nil) : Nil)
-- output: 
-- ((3 : 1 : 2 : Nil) : (6 : 4 : 5 : Nil) : (9 : 7 : 8 : Nil) : Nil)
reorderCols :: forall a. List (Tuple Int Int) -> List (List a) -> List (List a)
reorderCols cols tbl = 
  let 
    --f :: 
    f col = fromFoldable ( remapIndexes (toUnfoldable cols) (toUnfoldable col) )
  in
    map f tbl

--rem = reorderCols colMap table
rem = reorderCols (Tuple 0 1 : Tuple 1 2: Tuple 2 0 : Nil) ((1 : 2 : 3 : Nil) : (4 : 5 : 6 : Nil) : (7 : 8 : 9 : Nil) : Nil)






-- Read first line and get column names
-- Column names are now 

--Translate from one table to another

--Validate data types
--  Need to provide column info in with the actual data from a column

