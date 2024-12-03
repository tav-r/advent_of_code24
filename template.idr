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

solve1 : Vect n String -> Maybe Nat
solve1 strs = ?a

solve2 : Vect n String -> Maybe Nat
solve2 strs = ?b

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
