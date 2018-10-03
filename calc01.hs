-- simple calculator thing

import Numeric.Natural

-- defining a data type for the operations we can have
data OpType = Plus

stringifyOp :: OpType -> String
stringifyOp Plus = "+"

instance Show OpType where
  show op = stringifyOp op

-- defining a data type for the types of tokens we can have, where EOF means
-- end of file.
data Token = Nat Natural | Op OpType | EOF

stringifyToken :: Token -> String
stringifyToken (Nat x) = "Token(Nat," ++ show x ++ ")"
stringifyToken (Op x)  = "Token(Op," ++ show x ++ ")"
stringifyToken EOF     = "Token(EOF,EOF)"

instance Show Token where
  show token = stringifyToken token
