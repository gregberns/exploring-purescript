module Parse where

import Prelude
import Effect (Effect)
import Effect.Console (log)

import Data.Show (show)
--import Data.Array ((!!), index, toUnfoldable)
import Data.List (List(..), (:), (!!), index,  
                  toUnfoldable, fromFoldable, foldl, insertBy, insertAt,
                  sortBy, zipWith)
import Data.Function (on)
import Data.Ord (compare)
import Data.Traversable (sequence)
import Data.Maybe (Maybe(..))
import Data.Tuple
import Data.Int (fromString)
import Unsafe.Coerce (unsafeCoerce)
-- import Data.Semiring.Free (V(..))
import Data.Validation.Semiring
import Data.Semiring.Free (Free(..))


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


--Take a look at this to replace this function
--https://pursuit.purescript.org/packages/purescript-arrays/5.1.0/docs/Data.Array.ST.Iterator
foreign import remapIndexes :: forall a b c. Array (Tuple a b) -> Array c -> Array c

-- usage :
-- reorderCols (Tuple 0 1 : Tuple 1 2: Tuple 2 0 : Nil) ((1 : 2 : 3 : Nil) : (4 : 5 : 6 : Nil) : (7 : 8 : 9 : Nil) : Nil)
-- output: 
-- ((3 : 1 : 2 : Nil) : (6 : 4 : 5 : Nil) : (9 : 7 : 8 : Nil) : Nil)
reorderCols :: forall a. List (Tuple Int Int) -> List (List a) -> List (List a)
reorderCols cols tbl = 
  let 
    f col = fromFoldable ( remapIndexes (toUnfoldable cols) (toUnfoldable col) )
  in
    map f tbl

--rem = reorderCols colMap table
rem = reorderCols (Tuple 0 1 : Tuple 1 2: Tuple 2 0 : Nil) ((1 : 2 : 3 : Nil) : (4 : 5 : 6 : Nil) : (7 : 8 : 9 : Nil) : Nil)






-- Lets figure out how to translate....

someRecord = {
  headers: ["a", "b", "c"],
  datum: [ 
    [ 1,2,3 ]
  ]
}

translation = {
  from: ["a", "b", "c"],
  translate: [Tuple "a" "a'", Tuple "b" "b'", Tuple "c" "c'"],
  to: ["b'", "a'", "c'"]
}

validateTranslation = 
  --ensure that all columns match
  unsafeCoerce

createTranslationTable :: Array (Tuple String String) -> Array String -> Array String -> Array (Tuple Int Int)
createTranslationTable a b =
  --do the translation here
  --ensure lengths are the same? names line up

  unsafeCoerce

doTranslation =
  --actually do the translation from one to another
  1 --undefined


--Validate data types

-- 0) Start with an array of strings
-- 1) Grab 'type map' which is the description of what types columns should be

--Data type of a 'Row' Should be an array of Columns
--Data type of a 'Column' should be a record of a 'Format', 'ID', 'Label'(human readable)

data RowFormat = RowFormat (List ColumnFormat)
data ColumnFormat = Column { name :: String, format :: CellFormat }
data CellFormat = StrFmt | IntegerFmt -- | RegexFmt String

data CellType = 
    Str String 
  | Integer Int
  | FormattedString { name:: String, value:: String}

instance showCellType :: Show CellType where
  show (Str a) = a
  show (Integer i) = show i
  show (FormattedString {name, value}) = value

type CellValue = 
    { colName :: String
    , value :: CellType
    }

colFormat = RowFormat 
   ((Column { name: "col1", format: StrFmt }) 
  : (Column { name: "col2", format: IntegerFmt })
  : Nil)

cols = (
    ("Greg": "1": Nil)
  : ("John": "abc": Nil)
  : Nil
  )

--WORKS
-- checkRowsFormat :: RowFormat -> List (List String) -> Maybe (List (List CellValue))
-- checkRowsFormat fmt rows =
--   sequence (map (checkColumnFormat fmt) rows)

-- checkColumnFormat :: RowFormat -> List String -> Maybe (List CellValue)
-- checkColumnFormat (RowFormat fmt) row =
--   sequence (zipWith (\f r -> checkCellFormat f r) fmt row)

-- checkCellFormat :: ColumnFormat -> String -> Maybe CellValue 
-- checkCellFormat (Column {name: name, format: fmt}) value =
--   let
--     val = case fmt of
--             StrFmt -> Just (Str value)
--             IntegerFmt -> map (\i -> Integer i) (fromString value)
--             RegexFmt regx -> Just (Str value)
--   in
--     map (\v -> { colName: name, value: v }) val
--WORKS

--create a 'thing' that I can pass each row through
--the 'validators' will already have been created 
--and aligned with the column order

--validator is like List(a->b)
--validator is      V (Free Error) (List String -> List CellValue)
validateRow :: 
     (V (Free Error) (List String -> List CellValue))
  -> (V (Free Error) (List String))
  -> (V (Free Error) (List CellValue))
validateRow validator row =
  (<*>) validator row

-- rowValidator ::
--      RowFormat
--   -> (V (Free Error) (List String -> List CellValue))
-- rowValidator (RowFormat columnFormats) =
--   return (\row -> columnFormats  )
--   --sequence (zipWith (\f r -> checkCellFormat f r) fmt row)

-- cellFormatter :: ColumnFormat -> (V (Free Error) (String -> CellValue))
-- cellFormatter (Column {name: name, format: fmt}) =
--   pure (\value -> 
--     let
--       val = case fmt of
--               StrFmt -> pure (\str -> Str str)
--               IntegerFmt -> bimap (\i -> Integer i) (fromString value)
--               --RegexFmt regx -> Just (Str value)
--     in
--       map (\v -> { colName: name, value: v }) val
--   )

type Input = String

data ValidateResult a =
  --   UnexpectedEof
  -- | ExpectedEof Input
  -- | UnexpectedChar Char
  -- | UnexpectedString Chars
    UnparsableString String
  | Result Input a
  --deriving Eq

data Validate a = P (Input -> ValidateResult a)

validate :: forall a.
  Validate a
  -> Input
  -> ValidateResult a
validate (P p) =
  p

instance showValidateResult :: (Show a) =>  Show (ValidateResult a) where
  show (UnparsableString s) = "UnparsableString: " <> s
  show (Result i a) = "Result >" <> i <> "< " <> (show a)



-- stringFormatter :: (V (Free Error) (String -> ValidateResult))
stringFormatter :: Validate CellType
stringFormatter =
  P (\str -> Result str (Str str))

--intFormatter :: (V (Free Error) (String -> ValidateResult))
intFormatter :: Validate CellType
intFormatter =
  P (\str -> 
              --can simplify this
    case (map (\i -> Integer i) (fromString str)) of
      Just x  -> Result str x
      Nothing -> UnparsableString str --(pure (ValidateError "Cannot parse as int"))
  )
-- intFormatter =
--   pure (\str -> 
--     maybeToV 
--       (map (\i -> Integer i) (fromString str))
--       (ValidateError "Cannot parse as int: " <> str)
--   )
-- intFormatter =
--   pure (\str -> 
--     case (map (\i -> Integer i) (fromString str)) of
--       Just x  -> x
--       Nothing -> (pure (ValidateError "Cannot parse as int"))
--   )
        -- map (\i -> Integer i) (fromString value)

--Natural transformation from Maybe to V
maybeToV :: forall a. Maybe a -> Error -> (V (Free Error) (a))
maybeToV (Just x) _  = pure x
maybeToV Nothing err = invalid (pure err)

--start by creating a list of validation functions, based on the column type
-- rowValidation :: RowFormat -> (List String -> V (Free Error) Person)
-- rowValidation (RowFormat formats) =

data Error = ValidateError String

-- validate :: 
--   (List String -> V (Free Error) (List CellValue)) 
--   -> List (List String)
--   -> V (Free Error) (List (List CellValue))
-- validate validator rows =
--   foldl (\acc r -> (<*>) acc (validator r)) mempty rows

--then either map over or use an applicative






-- validateString :: 
-- validateString v = 

-- (<$> { first: _, last: _, contact: _} (validateName person.first)) <*>


-- Read first line and get column names
-- Column names are now 

-- Translate from one column layout to another

-- Validate data types
--   Need to provide column info in with the actual data from a column

