module Parser
  ( parseLine
  , tokenize
  , Token(TCmd, TChar, TRParent, TLParent, TEscape)
  , ParseResult(ParsedResult)
  , Parser(Parsed, UnParsed)
  , ParseError(PE)
  ) where

import Data.Functor ((<&>))
import Syntax
  ( Align(AlignCenter, AlignLeft, AlignRight)
  , BarCmd(DrawText, SetAlign, SetBG, SetFG)
  )

newtype ParseError =
  PE String

data Token
  = TCmd
  | TChar Char
  | TLParent
  | TRParent
  | TEscape Char
  deriving (Eq)

data ParseResult a =
  ParsedResult a [Token]

data Parser a
  = Parsed (ParseResult a)
  | UnParsed ParseError

instance Show Token where
  show TCmd = "^"
  show TLParent = "("
  show TRParent = ")"
  show (TEscape c) = "\\" ++ show c
  show (TChar c) = show c

instance Functor ParseResult where
  fmap f (ParsedResult a t) = ParsedResult (f a) t

instance Applicative ParseResult where
  pure a = ParsedResult a []
  (ParsedResult f _) <*> p = fmap f p

instance Functor Parser where
  fmap f (Parsed p) = Parsed (fmap f p)
  fmap _ (UnParsed err) = UnParsed err

instance Applicative Parser where
  pure a = Parsed (pure a)
  (UnParsed err) <*> _ = UnParsed err
  (Parsed (ParsedResult f _)) <*> p = fmap f p
  liftA2 f (Parsed (ParsedResult a ts)) (Parsed (ParsedResult b ts')) =
    Parsed (ParsedResult (f a b) ts')
  liftA2 f (UnParsed err) _ = UnParsed err
  liftA2 f _ (UnParsed err) = UnParsed err

instance Monad Parser where
  return = pure
  (Parsed (ParsedResult a ts)) >>= f =
    case f a of
      (Parsed (ParsedResult fa ts')) -> Parsed (ParsedResult fa ts)
      (UnParsed err) -> UnParsed err
  (UnParsed err) >>= _ = UnParsed err

instance MonadFail Parser where
  fail str = UnParsed $ PE str

instance Show a => Show (Parser a) where
  show (Parsed (ParsedResult a ts)) = show a ++ show ts
  show (UnParsed err) = show err

instance Show ParseError where
  show (PE err) = "ParsingError: " ++ show err

tokenize :: String -> [Token]
tokenize "" = []
tokenize ('^':s) = TCmd : tokenize s
tokenize ('(':s) = TLParent : tokenize s
tokenize (')':s) = TRParent : tokenize s
tokenize ('\\':c:s) = TEscape c : tokenize s
tokenize (c:s) = TChar c : tokenize s

liftParse :: (a -> b -> c) -> Parser a -> ([Token] -> Parser b) -> Parser c
liftParse _ (UnParsed err) _ = UnParsed err
liftParse f (Parsed (ParsedResult a ts)) pr =
  liftA2 f (Parsed (ParsedResult a ts)) (pr ts)

parseString :: [Token] -> Parser String
parseString [] = Parsed (ParsedResult "" [])
parseString (TChar c:t) = parseString t <&> (c :)
parseString (TEscape c:t) = parseString t <&> (c :)
parseString (TCmd:ts) = Parsed (ParsedResult "" (TCmd : ts))
parseString (t:ts) = UnParsed (PE ("Unexpected str" ++ show t ++ show ts))

parseCmdName :: [Token] -> Parser String
parseCmdName [] = parseString []
parseCmdName (TChar c:ts) = parseCmdName ts <&> (c :)
parseCmdName (TLParent:ts) = Parsed (ParsedResult "" ts)
parseCmdName (t:ts) = UnParsed (PE ("CmdNa Unexpected " ++ show t))

parseCmdValue :: [Token] -> Parser String
parseCmdValue [] = parseString []
parseCmdValue (TChar c:ts) = parseCmdValue ts <&> (c :)
parseCmdValue (TRParent:ts) = Parsed (ParsedResult "" ts)
parseCmdValue (t:ts) = UnParsed (PE ("CmdV Unexpected " ++ show t))

parseCmd :: [Token] -> Parser BarCmd
parseCmd ts = liftParse (,) (parseCmdName ts) parseCmdValue >>= uncurry toBarCmd
  where
    toBarCmd "fg" value = return $ SetFG value
    toBarCmd "bg" value = return $ SetBG value
    toBarCmd "align" "center" = return $ SetAlign AlignCenter
    toBarCmd "align" "left" = return $ SetAlign AlignLeft
    toBarCmd "align" "right" = return $ SetAlign AlignRight
    toBarCmd "align" v = fail ("Unknow alignment " ++ v)
    toBarCmd name _ = fail ("Unknow command " ++ name)

parseToken :: [Token] -> Parser [BarCmd]
parseToken [] = Parsed (ParsedResult [] [])
parseToken (TCmd:ts) = liftParse (:) (parseCmd ts) parseToken
parseToken (TChar c:ts) =
  liftParse (\str cmd -> DrawText (c : str) : cmd) (parseString ts) parseToken
parseToken (TEscape c:ts) =
  liftParse (\str cmd -> DrawText (c : str) : cmd) (parseString ts) parseToken
parseToken (t:ts) = UnParsed (PE ("Unexpected token" ++ show t))

parseLine :: String -> Parser [BarCmd]
parseLine str = parseToken $ tokenize str
