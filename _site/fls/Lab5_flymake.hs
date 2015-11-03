import Control.Applicative
import Data.Char
import Data.Tuple

newtype Parser result = Parser { runParser :: String ->
                                              [(String, result)] }

succeed :: r -> Parser r
succeed v = Parser $ \stream -> [(stream, v)]



instance Functor Parser where
  fmap f (Parser pattern) = Parser $ (fmap . fmap . fmap) f pattern


instance Applicative Parser where
  pure result = succeed result
  Parser pattern_map <*> Parser pattern
    = Parser $ \s -> [(u, f a) | (t, f) <- pattern_map s, (u, a) <- pattern t]

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s -> case s of
  []   -> []
  a:as
    | p a -> [(as, a)]
    | otherwise -> []

char :: Char -> Parser Char
char = satisfy . (==)

alpha = satisfy isAlpha
digit = satisfy isDigit
space = satisfy isSpace

charList :: String -> Parser Char
charList = satisfy . (flip elem)

string :: String -> Parser String
string [] = pure []
string (c:cs) = (:) <$> char c <*> string cs


instance Alternative Parser where
  empty = Parser $ const []
  Parser pattern1 <|> Parser pattern2 = Parser $ liftA2 (++) pattern1 pattern2

end :: Parser ()
end = Parser $ \stream -> [(stream, ()) | null stream]

just :: Parser r -> Parser r
just pattern = const <$> pattern <*> end

(<.>) :: Parser r1 -> Parser r2 -> Parser r2
parser1 <.> parser2 = fmap (flip const) parser1 <*> parser2

(<?>) :: (r -> Bool) -> Parser r -> Parser r
predicate <?> (Parser parser)
  = Parser $ \s -> [(t, r) | (t, r) <- parser s, predicate r]

number = (fmap (:) digit) <*> (number <|> succeed [])
  
