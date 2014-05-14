module Re.Internal.Scanner
( tokenize
, Token(..)
) where

data Token = Simple Char
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
            let
                tokenizeIterValue :: Token -> Int -> [Token] -> Either String [Token]
                tokenizeIterValue token next res =
                    case tokenizeIter next res of
                        Left err -> Left err
                        Right res -> Right $ token:res
            in
                if pos >= length input then Right res
                else
                    case input!!pos of
                        '?' -> tokenizeIterValue ZeroOrOne (pos + 1) res
                        _ -> tokenizeIterValue (Simple $ input!!pos) (pos + 1) res

    in
        case tokenizeIter 0 [] of
            Left err -> Left err
            Right res -> Right res
