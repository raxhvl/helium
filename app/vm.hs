module VM where

import Data.Word (Word8, Word16, Word256)
import Data.Vector (Vector, (!))
import qualified Data.Vector as V
import Data.Bits (shiftL, (.|.))

-- Define the types for Transaction and State
data Transaction = Transaction {
    txData :: Vector Word8 -- Transaction data
} deriving (Show, Eq)

data State = State {
    stack :: [Word256], -- Stack to hold values
    memory :: Vector Word8 -- Memory for the VM
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

-- Decode the opcode from the transaction data
decodeOpcode :: Vector Word8 -> Either String Opcode
decodeOpcode txData
    | V.null txData = Left "Transaction data is empty"
    | otherwise = case txData ! 0 of
        0x60 -> decodePush 2 PUSH1
        0x61 -> decodePush 3 (PUSH2 . toWord16)
        0x7f -> decodePush 33 (PUSH32 . toWord256)
        _    -> Left "Unknown opcode"
  where
    decodePush n constructor
        | V.length txData >= n = Right (constructor (V.slice 1 (n - 1) txData))
        | otherwise = Left $ "Insufficient data for " ++ show (txData ! 0) ++ " opcode"

    toWord16 v = fromIntegral (v ! 0) `shiftL` 8 .|. fromIntegral (v ! 1)
    toWord256 = V.foldl' (\acc x -> acc `shiftL` 8 .|. fromIntegral x) 0

-- Execute the opcode and update the state
executeOpcode :: Opcode -> State -> State
executeOpcode opcode state = case opcode of
    PUSH1 value -> push value
    PUSH2 value -> push value
    PUSH32 value -> push value
  where
    push value = state { stack = fromIntegral value : stack state }
