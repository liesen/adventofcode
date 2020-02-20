{-# LANGUAGE OverloadedStrings #-}
import Data.Char
import Data.List
import Data.Monoid
import qualified Data.ByteString as B
import qualified Data.ByteString.Base16 as Base16
import qualified Crypto.Hash.MD5 as MD5 -- https://hackage.haskell.org/package/cryptohash-md5

input = "ckczppom"

toByteString = B.pack . map (fromIntegral . ord)

hexhash :: Show a => a -> B.ByteString
hexhash = Base16.encode . MD5.hash . mappend input . toByteString . show

main = do
    -- Part 1
    let i = until (B.isPrefixOf (B.pack [48, 48, 48, 48, 48]) . hexhash) (+ 1) 1
    print i

    -- Part 2
    print $ until (B.isPrefixOf (B.pack [48, 48, 48, 48, 48, 48]) . hexhash) (+ 1) i
