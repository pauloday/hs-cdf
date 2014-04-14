import qualified Data.ByteString.Lazy
import Data.Functor
import Data.Int
import Data.Word
import Data.Binary.Get

data CDF = CDF { secSz		:: Int -- Sector size in bytes
	       , shortSecSz	:: Int -- Short sector size in bytes
	       , sat		:: [Int] -- Sector Allocation Table
	       , shortStreamSz	:: Int -- Maximum short stream size
	       , ssat		:: [Int] -- Short Sector Allocation Table
	       , file		:: ByteString -- File data
	       } -- TODO: Add dirstream info

readCDF :: Get CDF
readCDF = do
	skip 30 -- Skip standardized info (version, endianess, etc)
	ssz <- (2^) <$> fromIntegral <$> getWord16le
	sssz <- (2^) <$> fromIntegral <$> getWord16le
	skip 10
	satSz <- fromIntegral <$> getWord32le
	skip 36
	stsize <- fromIntegral <$> getWord32le
	`
	
	return CDF { secSz = ssz
		     , shortSecSize = sssz
		     , sat = sat
