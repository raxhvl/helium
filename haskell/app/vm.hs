{-# LANGUAGE BangPatterns #-}
module VM where

import Data.Word (Word8, Word16, Word256)
import Data.Vector.Unboxed (Vector, (!))
import qualified Data.Vector.Unboxed as V
import Data.Bits (shiftL, (.|.))

data Transaction = Transaction {
    txData :: !(Vector Word8)  -- Made strict
} deriving (Show, Eq)

data State = State {
    stack :: ![Word256],      -- Made strict
    memory :: !(Vector Word8)  -- Made strict
} deriving (Show, Eq)

-- Define the PUSH opcodes
data Opcode = PUSH1 Word8 | PUSH2 Word16 | PUSH32 Word256
    deriving (Show, Eq)

-- Ipsilon is the state transition function that takes a 
-- transaction and the current state and returns a new state.
ipsilon :: Transaction -> State -> Either String State
ipsilon transaction currentState = 
    case decodeOpcode (txData transaction) of
        Left err -> Left err
        Right opcode -> Right (executeOpcode opcode currentState)

-- Optimized decode function with explicit type signature
decodeOpcode :: Vector Word8 -> Either String Opcode
decodeOpcode txData
    | V.null txData = Left "Transaction data is empty"
    | otherwise = case txData ! 0 of
        0x60 -> decodePush1 txData
        0x61 -> decodePush2 txData
        0x7f -> decodePush32 txData
        op   -> Left $ "Unknown opcode: " ++ show op

-- Specialized decode functions for better performance
decodePush1 :: Vector Word8 -> Either String Opcode
decodePush1 v | V.length v >= 2 = Right $ PUSH1 (v ! 1)
              | otherwise = Left "Insufficient data for PUSH1"

decodePush2 :: Vector Word8 -> Either String Opcode
decodePush2 v | V.length v >= 3 = Right $ PUSH2 (toWord16 (v ! 1) (v ! 2))
              | otherwise = Left "Insufficient data for PUSH2"

decodePush32 :: Vector Word8 -> Either String Opcode
decodePush32 v | V.length v >= 33 = Right $ PUSH32 (toWord256 $ V.slice 1 32 v)
               | otherwise = Left "Insufficient data for PUSH32"

-- Optimized word conversion functions
{-# INLINE toWord16 #-}
toWord16 :: Word8 -> Word8 -> Word16
toWord16 hi lo = (fromIntegral hi `shiftL` 8) .|. fromIntegral lo

{-# INLINE toWord256 #-}
toWord256 :: Vector Word8 -> Word256
toWord256 = V.foldl' (\(!acc) x -> (acc `shiftL` 8) .|. fromIntegral x) 0

-- Strict state updates
executeOpcode :: Opcode -> State -> State
executeOpcode opcode !state = case opcode of
    PUSH1 !value -> state { stack = fromIntegral value : stack state }
    PUSH2 !value -> state { stack = fromIntegral value : stack state }
    PUSH32 !value -> state { stack = value : stack state }
