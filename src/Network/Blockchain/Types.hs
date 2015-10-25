{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Network.Blockchain.Types where

import Control.Applicative
import Data.Aeson
import Data.ByteString (ByteString())
import qualified Data.ByteString as BS
import Data.Time.Clock
import Data.Time.Format
import Data.Text.Encoding (encodeUtf8)
import Data.Ratio

-- | A wrapper around UTCTime providing a FromJSON instance that parses a
-- Unix timestamp.
newtype BTCTime = BTCTime { btcTime :: UTCTime }
    deriving (Eq, Ord, Show)

timefmt = "%s"
timeParse = parseTimeM True defaultTimeLocale timefmt

instance FromJSON BTCTime where
    parseJSON (Number n) = BTCTime <$> timeParse (show (numerator $ toRational n))

data Block = Block
    { blkHash :: ByteString
    , blkVersion :: Int
    , blkPrevious :: ByteString
    , blkMerkleRoot :: ByteString
    , blkTime :: BTCTime
    , blkBits :: Int
    , blkNonce :: Int
    , blkTxCount :: Int
    , blkSize :: Int
    , blkBlockIndex :: Int
    , blkMainChain :: Bool
    , blkHeight :: Int
    , blkTransactions :: [Transaction]
    }
    deriving (Show)

data Transaction = Transaction
    { txHash :: ByteString
    , txVer :: Int
    , txIn :: Int
    , txOut :: Int
    , txLockTime :: BTCTime
    , txSize :: Int
    , txRelayedBy :: ByteString
    , txTime :: BTCTime
    , txIndex :: Int
    , txInputs :: [TxInput]
    , txOutputs :: [TxOutput]
    }
    deriving (Show)

data TxInput = TxInput
    { txinSequence :: Int
    -- , txinPrevOut :: TxOutput
    , txinScript :: ByteString
    }
    deriving (Show)

data TxOutput = TxOutput
    { txoutSpent :: Bool
    , txoutIndex :: Int
    , txoutType :: Int
    , txoutAddr :: ByteString
    , txoutNumber :: Int
    , txoutScript :: ByteString
    }
    deriving (Show)

data Chart a b = Chart
    { chartValues :: [ChartPoint a b]
    }
    deriving (Show)

data ChartPoint a b = ChartPoint
    { chartX :: a
    , chartY :: b
    }
    deriving (Show)

instance FromJSON ByteString where
    parseJSON (String s) = pure $ encodeUtf8 s

instance FromJSON Block where
    parseJSON (Object v) =
        Block
        <$> v .: "hash"
        <*> v .: "ver"
        <*> v .: "prev_block"
        <*> v .: "mrkl_root"
        <*> v .: "time"
        <*> v .: "bits"
        <*> v .: "nonce"
        <*> v .: "n_tx"
        <*> v .: "size"
        <*> v .: "block_index"
        <*> v .: "main_chain"
        <*> v .: "height"
        <*> v .: "tx"

instance FromJSON Transaction where
    parseJSON (Object v) =
        Transaction
        <$> v .: "hash"
        <*> v .: "ver"
        <*> v .: "vin_sz"
        <*> v .: "vout_sz"
        <*> v .: "lock_time"
        <*> v .: "size"
        <*> v .: "relayed_by"
        <*> v .: "time"
        <*> v .: "tx_index"
        <*> v .: "inputs"
        <*> v .: "out"

instance FromJSON TxInput where
    parseJSON (Object v) =
        TxInput
        <$> v .: "sequence"
        -- <*> v .: "prev_out"
        <*> v .: "script"

instance FromJSON TxOutput where
    parseJSON (Object v) =
        TxOutput
        <$> v .: "spent"
        <*> v .: "tx_index"
        <*> v .: "type"
        <*> v .: "addr"
        <*> v .: "n"
        <*> v .: "script"

instance (FromJSON a, FromJSON b) => FromJSON (Chart a b) where
    parseJSON (Object v) =
        Chart
        <$> v .: "values"

instance (FromJSON a, FromJSON b) => FromJSON (ChartPoint a b) where
    parseJSON (Object v) =
        ChartPoint
        <$> v .: "x"
        <*> v .: "y"
