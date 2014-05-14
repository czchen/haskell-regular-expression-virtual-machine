module Re (
    match
) where

import Data.Char

import Re.Internal.Scanner

data OpCode = OpChar Char
            | OpMatch
            | OpJmp Int
            | OpSplit Int Int
            deriving(Show)

data VirtualMachine = VirtualMachine {
    opcode :: [OpCode],
    pc :: Int,
    string :: String,
    ptr :: Int
} deriving(Show)

createOpcodeFromChar :: Char -> String -> Maybe [OpCode]
createOpcodeFromChar x last =
    case createOpCodeFromPattern last of
        Just opcodeLast -> Just (OpChar x: opcodeLast)
        _ -> Nothing

createOpCodeFromStar :: String -> String -> Maybe [OpCode]
createOpCodeFromStar pattern last =
    case (createOpCodeFromPattern pattern, createOpCodeFromPattern last) of
        (Just opcodeRepeat, Just opcodeLast) ->
            let lenRepeat = length opcodeRepeat in
                Just $ (OpSplit 1 (lenRepeat + 2)) : (opcodeRepeat ++ [OpJmp (-1 - lenRepeat)] ++ opcodeLast)
        _ -> Nothing

createOpCodeFromPlus :: String -> String -> Maybe [OpCode]
createOpCodeFromPlus pattern last =
    case (createOpCodeFromPattern pattern, createOpCodeFromPattern last) of
        (Just opcodeRepeat, Just opcodeLast) ->
            let lenRepeat = length opcodeRepeat in
                Just $ opcodeRepeat ++ [OpSplit (-lenRepeat) 1] ++ opcodeLast
        _ -> Nothing

createOpCodeFromQuestionMark :: String -> String -> Maybe [OpCode]
createOpCodeFromQuestionMark pattern last =
    case (createOpCodeFromPattern pattern, createOpCodeFromPattern last) of
        (Just opcodeRepeat, Just opcodeLast) ->
            let lenRepeat = length opcodeRepeat in
                Just $ (OpSplit 1 (lenRepeat + 1)) : (opcodeRepeat ++ opcodeLast)
        _ -> Nothing

createOpCodeFromAlternative :: String -> String -> Maybe [OpCode]
createOpCodeFromAlternative left right =
    case (createOpCodeFromPattern left, createOpCodeFromPattern right) of
        (Just opcodeLeft, Just opcodeRight) ->
            let
                lenLeft = length opcodeLeft
                lenRight = length opcodeRight
            in
                Just $ [OpSplit 1 (lenLeft + 2)] ++ opcodeLeft ++ [OpJmp (lenRight + 1)] ++ opcodeRight
        _ -> Nothing

extractFirstBlock :: String -> Maybe [String]
extractFirstBlock pattern =
    if length pattern == 0 then Just []
    else
        let c = head pattern in
            if isAlphaNum c then Just $ [head pattern]: [(tail pattern)]
            else if c == '(' then Nothing --  FIXME: not implemented, find the whole block
            else Nothing -- FIXME: if there is a | or ( inside pattern, we need to extract the whole block before it

createOpCodeFromBlock :: String -> String -> Maybe [OpCode]
createOpCodeFromBlock pattern last =
    if length pattern == 1 then createOpcodeFromChar (head pattern) last
    else
        case ((createOpCodeFromPattern pattern), (createOpCodeFromPattern last)) of
            (Just opcode, Just opcodeLast) -> Just $ opcode ++ opcodeLast
            _ -> Nothing

createOpCodeFromPattern :: String -> Maybe [OpCode]
createOpCodeFromPattern pattern =
    case extractFirstBlock pattern of
        Just [] -> Just []
        Just (x:y:_) ->
            if length y /= 0 then
                case head y of
                    '*' -> createOpCodeFromStar x (tail y)
                    '+' -> createOpCodeFromPlus x (tail y)
                    '?' -> createOpCodeFromQuestionMark x (tail y)
                    '|' -> createOpCodeFromAlternative x (tail y)
                    _ -> createOpCodeFromBlock x y
            else createOpCodeFromBlock x y
        Nothing -> Nothing

createOpCodeFromRe :: String -> Maybe[OpCode]
createOpCodeFromRe pattern =
    case createOpCodeFromPattern pattern of
        Just opcode -> Just $ opcode ++ [OpMatch]
        _ -> Nothing

runVirtualMachine :: VirtualMachine -> Bool
runVirtualMachine (VirtualMachine opcode pc string ptr) =
    case opcode!!pc of
        OpChar c ->
            if (ptr >= length string) || (string!!ptr /= c) then False
            else runVirtualMachine $ VirtualMachine opcode (pc + 1) string (ptr + 1)
        OpMatch -> True
        OpJmp j -> runVirtualMachine $ VirtualMachine opcode (pc + j) string ptr
        OpSplit x y ->
            if runVirtualMachine $ VirtualMachine opcode (pc + x) string ptr then True
            else runVirtualMachine $ VirtualMachine opcode (pc + y) string ptr

match :: String -> String -> Maybe Bool
match pattern string =
    case createOpCodeFromRe pattern of
        Just opcode -> Just $ runVirtualMachine $ VirtualMachine opcode 0 string 0
        Nothing -> Nothing
