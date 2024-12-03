import System
import System.File.ReadWrite
import System.File.Handle
import System.File.Mode as FileMode
import Data.List
import Data.String
import Data.Either
import Data.Maybe
import Data.Vect

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

rowsToNumLists : {len : Nat} -> Vect len String -> Maybe (Vect len Nat, Vect len Nat)
rowsToNumLists ss = do
  wordTuples <- traverse (bitraverse head' (head' . drop 1) . dup . words) ss
  numTuples <- traverse (bitraverse parsePositive parsePositive) wordTuples
  pure $ ([x | (x, _) <- numTuples], [y | (_, y) <- numTuples])

diffLists : List Nat -> List Nat -> Nat
diffLists xs ys = sum [x + y | x <- xs, y <- ys]

similarityScore : {len : Nat} -> Vect len Nat -> Vect len Nat -> Nat
similarityScore xs ys = sum [x * (count (==x) ys) | x <- xs]

solve1 : {len : Nat} -> Vect len String -> Maybe Nat
solve1 = map (uncurry diffLists . bimap (sort . toList) (sort . toList)) . rowsToNumLists

solve2 : {len : Nat} -> Vect len String -> Maybe Nat
solve2 = map (uncurry similarityScore) . rowsToNumLists

main : IO ()
main = do
  args <- getArgs

  let filePath = (fromMaybe "./input.txt" . head' . drop 1) args
  let nLines = (fromMaybe 10 . (head' >=> parsePositive) . drop 2) args

  mInputLines <- withFile filePath FileMode.Read pure (readNLines nLines)

  case mInputLines of
       (Right input) => do
         printLn . fromMaybe (-1) . solve1 $ input
         printLn . fromMaybe (-1) . solve2 $ input
       (Left _) => printLn "Error reading file"
