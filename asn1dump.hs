module Main where

import qualified Data.ByteString as B
import Data.ASN1.Encoding
import Data.ASN1.BinaryEncoding
import Data.ASN1.Types
import Data.ASN1.BitArray
import Data.PEM
import Control.Applicative
import qualified Data.ByteString.Lazy as L
import System.Environment

import Numeric

hexdump :: B.ByteString -> String
hexdump bs = concatMap hex $ B.unpack bs
    where hex n
            | n > 0xa   = showHex n ""
            | otherwise = "0" ++ showHex n ""

hexdump' = hexdump

showASN1 :: Int -> [ASN1] -> IO ()
showASN1 at = prettyPrint at
  where
    indent n = putStr (replicate n ' ')

    prettyPrint n []                 = return ()
    prettyPrint n (x@(Start _) : xs) = indent n >> p x >> putStrLn "" >> prettyPrint (n+1) xs
    prettyPrint n (x@(End _) : xs)   = indent (n-1) >> p x >> putStrLn "" >> prettyPrint (n-1) xs
    prettyPrint n (x : xs)           = indent n >> p x >> putStrLn "" >> prettyPrint n xs

    p (Boolean b)            = putStr ("bool: " ++ show b)
    p (IntVal i)             = putStr ("int: " ++ showHex i "")
    p (BitString bits)       = putStr ("bitstring: " ++ (hexdump $ bitArrayGetData bits))
    p (OctetString bs)       = putStr ("octetstring: " ++ hexdump bs)
    p (Null)                 = putStr "null"
    p (OID is)               = putStr ("OID: " ++ show is)
    p (Real d)               = putStr "real"
    p (Enumerated _)         = putStr "enum"
    p (Start Sequence)       = putStr "sequence"
    p (End Sequence)         = putStr "end-sequence"
    p (Start Set)            = putStr "set"
    p (End Set)              = putStr "end-set"
    p (Start _)              = putStr "container"
    p (End _)                = putStr "end-container"
    p (ASN1String cs)        = putCS cs
    p (ASN1Time TimeUTC time tz)      = putStr ("utctime: " ++ show time)
    p (ASN1Time TimeGeneralized time tz) = putStr ("generalizedtime: " ++ show time)
    p (Other tc tn x)        = putStr ("other(" ++ show tc ++ "," ++ show tn ++ ")")

    putCS (ASN1CharacterString UTF8 t)         = putStr ("utf8string:" ++ show t)
    putCS (ASN1CharacterString Numeric bs)     = putStr "numericstring:"
    putCS (ASN1CharacterString Printable t)    = putStr ("printablestring: " ++ show t)
    putCS (ASN1CharacterString T61 bs)         = putStr "t61string:"
    putCS (ASN1CharacterString VideoTex bs)    = putStr "videotexstring:"
    putCS (ASN1CharacterString IA5 bs)         = putStr "ia5string:"
    putCS (ASN1CharacterString Graphic bs)     = putStr "graphicstring:"
    putCS (ASN1CharacterString Visible bs)     = putStr "visiblestring:"
    putCS (ASN1CharacterString General bs)     = putStr "generalstring:"
    putCS (ASN1CharacterString UTF32 t)        = putStr ("universalstring:" ++ show t)
    putCS (ASN1CharacterString Character bs)   = putStr "characterstring:"
    putCS (ASN1CharacterString BMP t)          = putStr ("bmpstring: " ++ show t)

showASN1Simple (ASN1String s) =
    case asn1CharacterToString s of
        Nothing -> error ("cannot decode ASN1String " ++ show s)
        Just ds -> "ASN1String " ++ ds
showASN1Simple x = show x

parseAndPrint a = do
    r <- head . either error id . pemParseLBS <$> L.readFile a
    let v = either (error . show) id $ decodeASN1' BER (pemContent r)
    mapM_ (putStrLn . showASN1Simple) v

main = do
    args <- getArgs
    mapM_ parseAndPrint args
