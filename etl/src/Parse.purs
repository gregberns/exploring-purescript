module Parse where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import Data.Show (show)
import Data.Array ((!!), index,toUnfoldable)
import Data.List
import Data.Traversable (sequence)
import Data.Maybe
import Data.Tuple

-- toUnfoldable [[1,2,3],[4,5,6],[7,8,9]]
table = [
    [1,2,3],
    [4,5,6],
    [7,8,9]
  ]

tableB = [1,2,3]

trans :: Array Int -> Array Int
trans = map (\r -> r + 1)

headers :: Array (Tuple Int String)
headers = [Tuple 0 "Col1", Tuple 1 "Col2", Tuple 2 "Col3"]

-- > getCol 1 table
-- (Just [2,5,8])
getTblCol :: forall a. Int -> Array (Array a) -> Maybe (Array a)
getTblCol i tbl = sequence (map (\r -> index r i) tbl)

-- > getTblRow 0 table
-- (Just [1,2,3])
getTblRow :: forall a. Int -> Array (Array a) -> Maybe (Array a)
getTblRow i tbl = index tbl i


--validate that all rows in table have same length
--reorder columns?
--build map from one column to another, then build new table with the new order

colMap = [Tuple 0 1, Tuple 1 2, Tuple 2 0]

remap colmap tbl =
  map (\t -> Tuple (snd t) (getTblCol (snd t) tbl)) colmap




-- Read first line and get column names
-- Column names are now 

--Translate from one table to another

--Validate data types
--  Need to provide column info in with the actual data from a column

