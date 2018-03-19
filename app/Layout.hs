{-# LANGUAGE FlexibleContexts #-}

module Layout () where

--   -- * Layout combinators
--   IMegaparsec,
--   laidout,
--   indented,
--   align,
--   runIndentParser,
-- ) where

-- import Data.Text.Lazy
-- import Lexer

-- import Text.Megaparsec (ParseError)
-- import Text.Megaparsec.Pos
-- import Text.Megaparsec.Prim hiding (State)

-- import Control.Monad.Identity
-- import Control.Applicative ((<$>))

-- -- Indentation sensitive Megaparsec monad.
-- type IMegaparsec a = Megaparsec Token ParseState a

-- data ParseState = ParseState
--   { indents :: Column
--   } deriving (Show)

-- initParseState :: ParseState
-- initParseState = ParseState 0

-- indentCmp
--   :: (Column -> Column -> Bool)
--   -> IMegaparsec ()
-- indentCmp cmp = do
--   col <- sourceColumn <$> getPosition
--   current <- indents <$> getState
--   guard (col `cmp` current)

-- withIndent :: Monad m =>Column-> Column -> MegaparsecT s ParseState m b -> MegaparsecT s ParseState m b
-- withIndent cur pos m = do
--   modifyState $ \st -> st { indents = pos }
--   res <- m
--   modifyState $ \st -> st { indents = cur }
--   return res

-- laidout :: Megaparsec s ParseState a -> Megaparsec s ParseState a
-- laidout m = do
--   cur <- indents <$> getState
--   pos <- sourceColumn <$> getPosition
--   res <- withIndent cur pos m
--   return res

-- indented :: IMegaparsec ()
-- indented = indentCmp (>) <?> "Block (indented)"

-- align :: IMegaparsec ()
-- align = indentCmp (==) <?> "Block (same indentation)"

-- runIndentParser
--   :: Stream Token Identity a
--   => SourceName
--   -> IMegaparsec a
--   -> Token
--   -> Either ParseError a
-- runIndentParser filePath p = runParser p initParseState filePath
