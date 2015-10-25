{-# LANGUAGE OverloadedStrings #-}

module Network.Blockchain
( module Network.Blockchain.Types
, getBlockByHash
) where

import Data.Aeson
import Data.ByteString (ByteString(), append)
import qualified Data.ByteString.Char8 as C8
import Data.Default (def)
import Network.Blockchain.Types
import Network.HTTP.Conduit

baseReq = def
    { secure = True
    , host = "blockchain.info"
    , port = 443
    }

getBlockByHash :: ByteString -> IO (Either String Block)
getBlockByHash hash = do
    r <- withManager $ httpLbs $ baseReq { path = "/rawblock/" `append` hash }
    let b = responseBody r
    return $ eitherDecode b
