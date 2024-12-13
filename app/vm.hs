module VM where

import Data.Word (Word8, Word256)
import Data.Vector (Vector, (!), (//))
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
    let txData = VM.txData transaction
    in case decodeOpcode txData of
         Left err -> Left err
         Right opcode -> Right (executeOpcode opcode currentState)

-- Decode the opcode from the transaction data
decodeOpcode :: Vector Word8 -> Either String Opcode
decodeOpcode txData
    | V.null txData = Left "Transaction data is empty"
    | V.length txData < 2 = Left "Transaction data is too short for opcode"
    | otherwise = case txData ! 0 of
        0x60 -> if V.length txData >= 2
                then Right (PUSH1 (txData ! 1))
                else Left "Insufficient data for PUSH1 opcode"
        0x61 -> if V.length txData >= 3
                then Right (PUSH2 (fromIntegral (txData ! 1) `shiftL` 8 .|. fromIntegral (txData ! 2)))
                else Left "Insufficient data for PUSH2 opcode"
        0x7f -> if V.length txData >= 33
                then Right (PUSH32 (foldl (\acc x -> acc `shiftL` 8 .|. fromIntegral x) 0 (V.toList (V.slice 1 32 txData))))
                else Left "Insufficient data for PUSH32 opcode"
        _    -> Left "Unknown opcode"

-- Execute the opcode and update the state
executeOpcode :: Opcode -> State -> State
executeOpcode opcode state = case opcode of
    PUSH1 value -> state { stack = fromIntegral value : stack state }
    PUSH2 value -> state { stack = fromIntegral value : stack state }
    PUSH32 value -> state { stack = value : stack state }

-- Helper function to convert a list of Word8 to Word256
word8ListToWord256 :: [Word8] -> Word256
word8ListToWord256 = foldl (\acc x -> acc `shiftL` 8 .|. fromIntegral x) 0