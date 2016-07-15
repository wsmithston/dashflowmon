{-# LANGUAGE OverloadedStrings #-}
import           Data.Char                         (ord)
--import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Base64 as B64
import qualified Data.ByteString as B
import           Data.Attoparsec.ByteString as A

import Control.Applicative

skipWhile1 p = skip p *> A.skipWhile p


data Config = Config User Password deriving (Show)

data User = User B.ByteString deriving (Show)
data Password = Password B.ByteString deriving (Show)

whitespace :: B.ByteString
whitespace = " \n\t"

parseConfig :: Parser Config
parseConfig = do
    u <-  parseUser
    A.skipWhile (A.inClass "\t\n ")
    p <-  parsePassword
    return (Config u p)

parseUser :: Parser User
parseUser = do
    A.string "rpcuser="
    --x <- takeTill ((=='\t') <||> (== '\n'))
    --x <- Atto.takeTill (\x -> x `B.elem` whitespace)
    x <- A.takeTill (A.inClass "\t\n ")
    return (User x)

parsePassword :: Parser Password
parsePassword =  A.string "rpcpassword=" >>
                 A.takeTill (A.inClass "\t\n ") >>= \x ->
                 return (Password x)
