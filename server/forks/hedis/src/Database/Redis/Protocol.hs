{-# LANGUAGE CPP #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Database.Redis.Protocol (Reply(..), reply, renderRequest) where

import Prelude hiding (error, take)
#if __GLASGOW_HASKELL__ < 710
import Control.Applicative
#endif
import Control.DeepSeq
import Scanner (Scanner)
import qualified Scanner
import Data.ByteString.Char8 (ByteString)
import GHC.Generics
import qualified Data.ByteString.Char8 as B
import qualified Data.Text.Encoding as Text
import qualified Data.Text.Read as Text
import Control.Monad (replicateM)

-- |Low-level representation of replies from the Redis server.
data Reply = SingleLine ByteString
           | Error ByteString
           | Integer Integer
           | Bulk (Maybe ByteString)
           | MultiBulk (Maybe [Reply])
         deriving (Eq, Show, Generic)

instance NFData Reply

------------------------------------------------------------------------------
-- Request
--
renderRequest :: [ByteString] -> ByteString
renderRequest req = B.concat (argCnt:args)
  where
    argCnt = B.concat ["*", showBS (length req), crlf]
    args   = map renderArg req

renderArg :: ByteString -> ByteString
renderArg arg = B.concat ["$",  argLen arg, crlf, arg, crlf]
  where
    argLen = showBS . B.length

showBS :: (Show a) => a -> ByteString
showBS = B.pack . show

crlf :: ByteString
crlf = "\r\n"

------------------------------------------------------------------------------
-- Reply parsers
--
{-# INLINE reply #-}
reply :: Scanner Reply
reply = do
  c <- Scanner.anyChar8
  case c of
    '+' -> string
    '-' -> error
    ':' -> integer
    '$' -> bulk
    '*' -> multi
    _ -> fail "Unknown reply type"

{-# INLINE string #-}
string :: Scanner Reply
string = SingleLine <$> line

{-# INLINE error #-}
error :: Scanner Reply
error = Error <$> line

{-# INLINE integer #-}
integer :: Scanner Reply
integer = Integer <$> integral

{-# INLINE bulk #-}
bulk :: Scanner Reply
bulk = Bulk <$> do
  len <- integral
  if len < 0
    then return Nothing
    else Just <$> Scanner.take len <* eol

-- don't inline it to break the circle between reply and multi
{-# NOINLINE multi #-}
multi :: Scanner Reply
multi = MultiBulk <$> do
  len <- integral
  if len < 0
    then return Nothing
    else Just <$> replicateM len reply

{-# INLINE integral #-}
integral :: Integral i => Scanner i
integral = do
  str <- line
  case Text.signed Text.decimal (Text.decodeUtf8 str) of
    Left err -> fail (show err)
    Right (l, _) -> return l

{-# INLINE line #-}
line :: Scanner ByteString
line = Scanner.takeWhileChar8 (/= '\r') <* eol

{-# INLINE eol #-}
eol :: Scanner ()
eol = do
  Scanner.char8 '\r'
  Scanner.char8 '\n'
