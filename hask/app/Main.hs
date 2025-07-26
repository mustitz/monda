module Main where

import Data.Maybe (fromMaybe)
import Control.Monad (void)

import Text.Parsec

import System.Environment


data Severity = Err | Warn deriving (Show, Eq)

data Diagnostic = Diagnostic
  { severity :: Severity
  , message :: String
  , position :: SourcePos
  } deriving (Show)


data Jump = Jump Int SourcePos

data Chunk = Chunk
  { content :: String
  , pos :: SourcePos
  , jumps :: [Jump]
  }

instance Show Jump where
  show (Jump offset pos) = "<" ++ show offset ++ "> " ++ show pos

instance Show Chunk where
  show chunk = show (pos chunk) ++ " " ++ show (content chunk) ++ " @@@ " ++ show (jumps chunk)


data LineExtractorState = LineExtractorState
  { chunks :: [Chunk]
  , diagnostics :: [Diagnostic]
  , hasErrors :: Bool
  } deriving (Show)

type LineExtractor = Parsec String LineExtractorState


addChunk :: Chunk -> LineExtractor ()
addChunk chunk = do
  state <- getState
  putState $ state { chunks = chunks state ++ [chunk] }

addErr :: SourcePos -> String -> LineExtractor ()
addErr pos msg = do
  modifyState $ \state -> state
    { diagnostics = diagnostics state ++ [Diagnostic Err msg pos]
    , hasErrors = True
    }

addWarn :: SourcePos -> String -> LineExtractor ()
addWarn pos msg = do
  modifyState $ \state -> state
    { diagnostics = diagnostics state ++ [Diagnostic Warn msg pos]
    }


escapedNewline :: LineExtractor ()
escapedNewline = do
  void $ char '\\'
  void $ char '\n'
  return ()

sourceChar :: LineExtractor (Char, Maybe SourcePos)
sourceChar = do
  escapes <- many escapedNewline
  jumpPos <- getPosition
  ch <- anyChar
  let maybeJump = if null escapes then Nothing else Just jumpPos
  return (ch, maybeJump)

collectLine :: String -> [Jump] -> LineExtractor (String, [Jump])
collectLine acc jumps = do
  result <- optionMaybe sourceChar
  case result of
    Nothing -> do
      pos <- getPosition
      addWarn pos "Missing final newline"
      return (reverse ('\n':acc), jumps)
    Just (ch, Nothing) -> processChar ch jumps
    Just (ch, Just pos) -> processChar ch $ jumps ++ [Jump (length acc) pos]
  where processChar ch newJumps = do
          let newAcc = ch : acc
          if ch == '\n'
            then return (reverse newAcc, newJumps)
            else collectLine newAcc newJumps

readLine :: LineExtractor ()
readLine = do
  originPos <- getPosition
  (firstChar, maybeJump) <- sourceChar
  let startPos = fromMaybe originPos maybeJump
  if firstChar == '\n'
    then addChunk $ Chunk ['\n'] startPos []
    else do
      (content, jumps) <- collectLine [firstChar] []
      addChunk $ Chunk content startPos jumps

readLines :: LineExtractor LineExtractorState
readLines = do
  void $ manyTill readLine eof
  getState

extractLines :: String -> String -> Either ParseError LineExtractorState
extractLines fn content =
  let initialState = LineExtractorState [] [] False
  in runParser readLines initialState fn content


printDiagnostics :: [Diagnostic] -> IO ()
printDiagnostics diags = do
 putStrLn $ "Found " ++ show (length diags) ++ " diagnostics"
 mapM_ printDiagnostic diags

printDiagnostic :: Diagnostic -> IO ()
printDiagnostic diag = putStrLn $ show (severity diag) ++ ": " ++ message diag ++ " at " ++ show (position diag)

printChunks :: [Chunk] -> IO ()
printChunks chunks = do
  putStrLn $ "Found " ++ show (length chunks) ++ " chunks"
  mapM_ printChunk chunks

printChunk :: Chunk -> IO ()
printChunk chunk = putStrLn $ show chunk

printState :: LineExtractorState -> IO ()
printState state = do
  printDiagnostics $ diagnostics state
  printChunks $ chunks state

parseConent :: String -> String -> IO ()
parseConent fn content = do
  putStrLn $ "Parsing file: " ++ fn
  case extractLines fn content of
     Left err -> print err
     Right state -> printState state

parseFile :: String -> IO ()
parseFile fn = do
  content <- readFile fn
  parseConent fn content

parseFiles :: [String] -> IO ()
parseFiles [] = do pure ()
parseFiles (fn : xs) = do
  parseFile fn
  parseFiles xs


main :: IO ()
main = do
  args <- getArgs
  if length args == 0
    then putStrLn "Usage: program <file> [files]"
    else parseFiles args
