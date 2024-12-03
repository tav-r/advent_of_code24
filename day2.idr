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

parseInput : Vect n String -> Maybe (Vect n (List Nat))
parseInput = traverse (traverse parsePositive . words)

safetyPredicate : List Nat -> Bool
safetyPredicate ks = (sorted ks || sorted (reverse ks)) &&
  all (\x => x<=3 && x > 0) [(minus x y) + (minus y x) | (x, y) <- zip ks (drop 1 ks)]

solve1 : Vect n String -> Maybe Nat
solve1 = map (count safetyPredicate) . parseInput

splitToTolerantLists : List Nat -> List (List Nat)
splitToTolerantLists [] = []
splitToTolerantLists (x :: xs) = xs :: ((x ::) <$> splitToTolerantLists xs)

example : splitToTolerantLists [1, 2, 3, 4] = [[2, 3, 4], [1, 3, 4], [1, 2, 4], [1, 2, 3]]
example = Refl

solve2 : Vect n String -> Maybe Nat
solve2 = map (count (any safetyPredicate) . map splitToTolerantLists) . parseInput

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
