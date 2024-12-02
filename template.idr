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

solve : Vect n String -> Nat
solve strs = ?solve_rhs

main : IO ()
main = do
  args <- getArgs

  let filePath = (fromMaybe "./input.txt" . head') args
  let nLines = (fromMaybe 10 . (head' >=> parsePositive) . drop 1) args

  -- read file, this generates an Either FileError String
  mInputLines <- withFile filePath FileMode.Read pure (readNLines nLines)

  {-
    Try taking the right side, if it succeeds, solve the puzzle and show the result,
    otherwise the file could no be read.
  -}
  printLn . fromMaybe "Error reading File" . (map (show . solve) . getRight) $ mInputLines
