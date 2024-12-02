import System
import System.File
import System.File.ReadWrite
import System.File.Handle
import System.File.Mode as FileMode
import Data.List
import Data.String
import Data.Either
import Data.Maybe
import Data.Vect
import Debug.Trace

%default total

-- recursively read n lines from the given file, bails out on FileError
readNLines : (n: Nat) -> File -> IO (Either FileError (Vect n String))
readNLines 0 _ = pure $ Right Nil
readNLines (S n) f = do
  line <- fGetLine f
  recurseWithRes line
  where
    recurseWithRes : Either FileError String -> IO (Either FileError (Vect (S n) String))
    recurseWithRes (Right l) = (mapSnd (l ::)) <$> (readNLines n f)
    recurseWithRes (Left e) = pure (Left FileReadError)

rowsToNumLists : Foldable f => f String -> Maybe (List Nat, List Nat)
rowsToNumLists = map transpTupleList . foldlM (\acc => (map (:: acc)) . (takeTwoWords >=> parseTwoNums) . words) []
  where
    takeTwoWords : List String -> Maybe (String, String)
    takeTwoWords (w :: w' :: Nil) = Just (w, w')
    takeTwoWords _ = Nothing

    transpMaybeTup : (Maybe a, Maybe a) -> Maybe (a, a)
    transpMaybeTup ((Just x), (Just y)) = Just (x, y)
    transpMaybeTup _ = Nothing

    parseTwoNums : (String, String) -> Maybe (Nat, Nat)
    parseTwoNums = transpMaybeTup . bimap parsePositive parsePositive

    transpTupleList : List (a, a) -> (List a, List a)
    transpTupleList [] = ([], [])
    transpTupleList ((x, y) :: xs) =
      let (ls, rs) = transpTupleList xs in (x :: ls, y :: rs)

diffLists : List Nat -> List Nat -> Nat
diffLists (x :: xs) (y :: ys) = (minus x y) + (minus y x) + diffLists xs ys
diffLists _ _ = 0

similarityScore : List Nat -> List Nat -> Nat
similarityScore (x :: xs) ys = x * (count (==x) ys) + (similarityScore xs ys)
similarityScore _ _ = 0

solve1 : Vect n String -> Maybe Nat
solve1 = map (uncurry diffLists . bimap sort sort) . rowsToNumLists

solve2 : Vect n String -> Maybe Nat
solve2 = map (uncurry similarityScore) . rowsToNumLists

main : IO ()
main = do
  args <- getArgs

  let filePath = (fromMaybe "./input.txt" . head' . drop 1) args
  let nLines = (fromMaybe 10 . (head' >=> parsePositive) . drop 2) args

  -- read file, this generates an Either FileError String
  mInputLines <- withFile filePath FileMode.Read pure (readNLines nLines)

  {-
    Try taking the right side, if it succeeds, solve the puzzle and show the result,
    otherwise the file could no be read.
  -}
  printLn . fromMaybe "Error reading File" . map show . (getRight >=> solve1) $ mInputLines
  printLn . fromMaybe "Error reading File" . map show . (getRight >=> solve2) $ mInputLines
