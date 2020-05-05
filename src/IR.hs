module IR where

import Data.Char

tmpToReg :: String -> String
tmpToReg tmp
    | head tmp == 'T' && all isDigit (tail tmp) = 'r' : tail tmp
    | otherwise = tmp

operation :: String -> String -> String -> String -> [String]
operation kind arg1 arg2 arg3 = 
    [
        unwords ["move", arg1, arg3],
        unwords [kind, arg2, arg3]
    ]

translateIR :: String -> [String]
translateIR ir =
    let toks = map tmpToReg $ words ir
        inst = head toks
        arg1 = toks !! 1
        arg2 = toks !! 2
        arg3 = toks !! 3
    in case inst of
        "VAR" -> ["var " ++ arg1]
        "STR" -> ["str " ++ arg1 ++ " " ++ arg2]
        "ADDI" -> operation "addi" arg1 arg2 arg3
        "ADDF" -> operation "addr" arg1 arg2 arg3
        "SUBI" -> operation "subi" arg1 arg2 arg3
        "SUBF" -> operation "subr" arg1 arg2 arg3
        "MULTI" -> operation "muli" arg1 arg2 arg3
        "MULTF" -> operation "mulr" arg1 arg2 arg3
        "DIVI" -> operation "divi" arg1 arg2 arg3
        "DIVF" -> operation "divr" arg1 arg2 arg3
        "WRITEI" -> ["sys writei " ++ arg1]
        "WRITEF" -> ["sys writer " ++ arg1]
        "WRITES" -> ["sys writes " ++ arg1]
        "READI" -> ["sys readi " ++ arg1]
        "READF" -> ["sys readr " ++ arg1]
        "STOREI" -> [unwords ["move", arg1, arg2]]
        "STOREF" -> [unwords ["move", arg1, arg2]]
        _ -> ["ERROR - Unknown instruction: " ++ inst]