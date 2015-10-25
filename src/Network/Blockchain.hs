{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE StandaloneDeriving #-}

module Network.Blockchain
( module Network.Blockchain.Types
, ChartRange(..)
, ChartType(..)
, getBlockByHash
, getChartData
) where

import Data.Aeson
import Data.ByteString (ByteString(), append)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import Data.Default (def)
import Network.Blockchain.Types
import Network.HTTP.Conduit

data ChartType :: * -> * -> * where
    MarketPrice :: ChartType BTCTime Double
    -- TODO add other constructors

deriving instance (Show a, Show b) => Show (ChartType a b)

data ChartRange
    = Days30
    | Days60
    | Days180
    | Years1
    | Years2
    | AllTime
    deriving (Show)

-- | Convert a chart range into a ByteString usable as part of a URL for a
-- Blockchain.info API resource.
renderChartRange :: ChartRange -> ByteString
renderChartRange r = case r of
    Days30 -> "30days"
    Days60 -> "60days"
    Days180 -> "180days"
    Years1 -> "1year"
    Years2 -> "2year"
    AllTime -> "all"

-- | Convert a chart type into a ByteString usable as part of a URL for a
-- Blockchain.info API resource.
renderChartType :: ChartType a b -> ByteString
renderChartType t = case t of
    MarketPrice -> "market-price"

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

getChartData :: (FromJSON a, FromJSON b)
             => ChartRange -> ChartType a b -> IO (Either String [ChartPoint a b])
getChartData range ctype = do
    let srange = renderChartRange range
    let stype = renderChartType ctype
    r <- withManager $
         httpLbs $
         baseReq
            { path = "/charts/" `append` stype
            , queryString = BS.concat [ "?timespan="
                                      , srange
                                      , "&format=json"
                                      ]
            }
    let b = responseBody r
    return $ do
        Chart { chartValues = ps } <- eitherDecode b
        return ps
