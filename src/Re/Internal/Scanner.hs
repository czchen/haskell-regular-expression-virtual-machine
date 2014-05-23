module Re.Internal.Scanner
( tokenize
, Token(..)
) where

data Token = Simple Char
           | AnyChar
           | ZeroOrOne
           | ZeroOrMore
           | OneOrMore
           | OpenParenthesis
           | CloseParenthesis
           | Alternative
    deriving(Eq, Show)

tokenize :: String -> Either String [Token]
tokenize input =
    let
        tokenizeIter :: Int -> [Token] -> Either String [Token]
        tokenizeIter pos res =
            if pos >= length input then Right res
            else
                case input!!pos of
                    '?' -> tokenizeIterValue ZeroOrOne (pos + 1) res
                    '*' -> tokenizeIterValue ZeroOrMore (pos + 1) res
                    '+' -> tokenizeIterValue OneOrMore (pos + 1) res
                    '(' -> tokenizeIterValue OpenParenthesis (pos + 1) res
                    ')' -> tokenizeIterValue CloseParenthesis (pos + 1) res
                    '|' -> tokenizeIterValue Alternative (pos + 1) res
                    '.' -> tokenizeIterValue AnyChar (pos + 1) res
                    '\\' ->
                        if pos + 1 >= length input then Left $ "Escape error at pos " ++ show(pos)
                        else tokenizeIterValue (Simple $ input!!(pos + 1)) (pos + 2) res
                    _ -> tokenizeIterValue (Simple $ input!!pos) (pos + 1) res
            where
                tokenizeIterValue token next res =
                    case tokenizeIter next res of
                        Left err -> Left err
                        Right res -> Right $ token:res
    in
        case tokenizeIter 0 [] of
            Left err -> Left err
            Right res -> Right res
