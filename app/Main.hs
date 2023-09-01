module Main where

import qualified Data.Char as Char
import Data.Text (Text)
import qualified Data.Text as Text
import qualified Data.Text.IO as TextIO
import Data.Maybe (isJust, fromMaybe)
import Control.Monad (join)
import Control.Applicative ((<|>))
import Data.List (find, unfoldr, mapAccumL)
import Control.Exception (assert)

main :: IO ()
main = do
  file <- TextIO.readFile "app/Main.hs"
  let results = showResults (readResults file)
  if file == results
    then putStrLn "OK"
    else TextIO.putStrLn results
  
data Lexeme
  = OParen
  | CParen
  | OBracket
  | CBracket
  | OBrace
  | CBrace
  | BTick
  | LArrow
  | RArrow
  | BSlash
  | Assign
  | DColon
  | Comma
  | As
  | Case
  | Data
  | Do
  | Else
  | If
  | Import
  | In
  | Let
  | Module
  | Of
  | Pipe
  | Qualified
  | Then
  | Where
  | Indent Int
  | Symbol Text
  | InfixSymbol Text
  | String Text
  | Int Int
  | Float Float
  | Char Char
  | Path [Text]
  | Comment Text
  deriving (Eq, Show)

data Token = Token
  { lexeme :: Lexeme
  , length :: Int
  , line :: Int  
  , start :: Int
  }
  deriving (Eq, Show)

data Result
  = Result Lexeme Int
  | Stripped Result Int
  deriving (Eq, Show)

type Lexer = Text -> Maybe (Result, Text)

yield :: Lexeme -> Int -> b -> Maybe (Result, b)
yield tok len rest = Just (Result tok len, rest)

matchHead :: (Char -> Bool) -> Text -> Bool
matchHead cond txt = case Text.uncons txt of
  Just (h, t) -> cond h
  Nothing -> False

symbolChar :: Char -> Bool
symbolChar char = Char.isAlphaNum char || char == '_' || char == '\''

infixSymbolChar :: Char -> Bool
infixSymbolChar char = case char of
  '.' -> True
  '=' -> True
  '<' -> True
  '>' -> True
  '|' -> True
  '#' -> True
  '$' -> True
  '@' -> True
  '%' -> True
  '!' -> True
  '+' -> True
  '-' -> True
  '*' -> True
  '/' -> True
  '\\' -> True
  '^' -> True
  '&' -> True
  '?' -> True
  '~' -> True
  ':' -> True
  _ -> False

spaces :: Text -> Maybe (Int, Text)
spaces txt = let
  (s, r) = Text.span (== ' ') txt
  in if Text.null s
    then Nothing
    else Just (Text.length s, r)

delim :: Lexer
delim txt = do
  (h, t) <- Text.uncons txt
  case h of
    '(' -> yield OParen 1 t
    ')' -> yield CParen 1 t
    '[' -> yield OBracket 1 t
    ']' -> yield CBracket 1 t
    '{' -> yield OBrace 1 t
    '}' -> yield CBrace 1 t
    ',' -> yield Comma 1 t
    '`' -> yield BTick 1 t
    _ -> Nothing
  
keyword' :: (Char -> Bool) -> Lexeme -> Text -> Text -> Maybe (Result, Text)
keyword' cond lex kw txt = do
  rest <- Text.stripPrefix kw txt
  if matchHead cond rest
    then Nothing
    else yield lex (Text.length kw) rest

keyword :: Lexeme -> Text -> Text -> Maybe (Result, Text)
keyword = keyword' symbolChar

infixKeyword :: Lexeme -> Text -> Text -> Maybe (Result, Text)
infixKeyword = keyword' infixSymbolChar

keywords :: [Lexer]
keywords = map (uncurry keyword)
  [ (As, "as")
  , (Case, "case")
  , (Data, "data")
  , (Do, "do")
  , (Else, "else")
  , (If, "if")
  , (Import, "import")
  , (In, "in")
  , (Let, "let")
  , (Module, "module")
  , (Of, "of")
  , (Qualified, "qualified")
  , (Then, "then")
  , (Where, "where")
  ]

infixKeywords :: [Lexer]
infixKeywords = map (uncurry infixKeyword)
  [ (Assign, "=")
  , (DColon, "::")
  , (RArrow, "->")
  , (LArrow, "<-")
  ]

symbol :: Lexer
symbol txt = do
  (h, t) <- Text.uncons txt
  if Char.isLetter h || h == '_'
    then let
      (s, rest) = Text.span symbolChar t
      sym = Text.cons h s
      in yield (Symbol sym) (Text.length sym) rest
    else Nothing

infixSymbol :: Lexer
infixSymbol txt = let
  (sym, rest) = Text.span infixSymbolChar txt
  in if Text.null sym
    then Nothing
    else yield (Symbol sym) (Text.length sym) rest

path :: Lexer
path text = let
  parts :: Text -> [(Int, Text)]
  parts txt = case symbol txt of
    Just (Result (Symbol sym) len, rest) -> case Text.uncons rest of
      Just ('.', t) -> if matchHead Char.isLetter t
        then (len + 1, sym) : parts t
        else [(len, sym)]
      _ -> [(len, sym)] 
    Nothing -> []

  pathLen :: [(Int, Text)] -> (Int, [Text])
  pathLen = mapAccumL (\acc (len, sym) -> (acc + len, sym)) 0

  in case pathLen $ parts text of
    (_, []) -> Nothing
    (len, [sym]) -> yield (Symbol sym) len $ Text.drop len text
    (len, syms) -> yield (Path syms) len $ Text.drop len text
  
indent :: Lexer
indent txt = do
  t <- Text.stripPrefix "\n" txt
  let (len, rest) = fromMaybe (0, t) $ spaces t
  yield (Indent len) len rest

char :: Lexer
char txt = let
  escapeChar txt = do
    rest <- Text.stripPrefix "\\" txt
    (h, rest) <- Text.uncons rest
    let char = case h of
          'n' -> '\n'
          't' -> '\t'
          'r' -> '\r'
          '\\' -> '\\'
          '\'' -> '\''
    return (char, 4, rest)
  singleChar txt = do
    (char, rest) <- Text.uncons txt
    return (char, 3, rest)
  in do
    rest <- Text.stripPrefix "'" txt
    (char, num, rest) <- escapeChar rest <|> singleChar rest
    rest <- Text.stripPrefix "'" rest
    return (Result (Char char) num, rest) 

string :: Lexer
string txt = let
  run acc txt = let
    (x, y) = Text.break (=='"') txt
    str = Text.append acc x
    rest = Text.drop 1 y
    escape = odd $ Text.length $ Text.takeWhileEnd (=='\\') str
    in if escape
      then run (Text.snoc str '"') rest
      else yield (String str) (Text.length str + 2) rest
  in Text.stripPrefix "\"" txt >>= run mempty

intText :: Text -> Maybe (Text, Text)
intText txt = let
  span = Text.span Char.isNumber txt
  in if Text.null (fst span)
    then Nothing
    else Just span

int :: Lexer
int txt = do
  (i, rest) <- intText txt
  yield (Int $ read $ Text.unpack i) (Text.length i) rest

float :: Lexer
float txt = do
  (i1, rest) <- intText txt
  rest <- Text.stripPrefix "." rest
  (i2, rest) <- intText rest
  let f = Float $ read $ Text.unpack i1 ++ '.' : Text.unpack i2
  yield f (Text.length i1 + Text.length i2 + 1) rest

comment :: Lexer
comment = undefined

strip :: Lexer -> Lexer
strip lex txt = do
  let stripped n (o, r) = (Stripped o (n + 1), r)
  (h, t) <- Text.uncons txt
  case h of
    ' ' -> case spaces t of
      Just (n, rest) -> stripped n <$> lex rest
      Nothing -> stripped 0 <$> lex t
    _ -> lex txt

lexemes :: [Lexer]
lexemes = [indent, delim, char, string, int, float]
  ++ keywords ++ infixKeywords ++ [path, infixSymbol]

alt :: [Lexer] -> Lexer
alt opts txt = join $ find isJust $ map (\f -> f txt) opts

readResults :: Text -> [Result]
readResults = unfoldr $ strip $ alt lexemes

tokenize :: Text -> [Token]
tokenize = let
  run (line, start) res = case res of
    Result lex@(Indent num) _ -> ((line + 1, num), Token lex num (line + 1) 0)
    Result lex len -> ((line, start + len), Token lex len line start)
    Stripped result len -> run (line, start + len) result
  in snd . mapAccumL run (0, 0) . readResults

showLexeme :: Lexeme -> Text
showLexeme lex = case lex of
  OParen -> "("
  CParen -> ")"
  OBracket -> "["
  CBracket -> "]"
  OBrace -> "{"
  CBrace -> "}"
  LArrow -> "<-"
  RArrow -> "->"
  BSlash -> "\\"
  Assign -> "="
  DColon -> "::"
  Comma -> ","
  BTick -> "`"
  As -> "as"
  Case -> "case"
  Data -> "data"
  Do -> "do"
  Else -> "else"
  If -> "if"
  Import -> "import"
  In -> "in"
  Let -> "let"
  Module -> "module"
  Of -> "of"
  Pipe -> "|"
  Qualified -> "qualified"
  Then -> "then"
  Where -> "where"
  Indent x -> Text.cons '\n' $ Text.replicate x " "
  Symbol x -> x
  InfixSymbol x -> x
  String x -> Text.cons '"' $ Text.snoc x '"'
  Int x -> Text.pack $ show x
  Float x -> Text.pack $ show x
  Char x -> Text.pack $ show x
  Path x -> Text.intercalate "." x
  Comment x -> Text.append "--" x

showResult :: Result -> Text
showResult result = case result of
  Result lex _ -> showLexeme lex
  Stripped res num -> Text.append (Text.replicate num " ") $ showResult res

showResults :: [Result] -> Text
showResults = foldr (Text.append . showResult) mempty
