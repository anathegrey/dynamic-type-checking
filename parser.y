{
  module Grammar where
    import Data
    }

%name calc
%tokentype { Token }
%error { parseError }

%token
int { TokenInt }
float { TokenFloat }
bool { TokenBool }
dyn { TokenDyn }
var { TokenVar $$ }
label { TokenLabel }
"\\" { TokenLambda }
"==" { TokenEq }
">=" { TokenBiggerEq }
"<=" { TokenLessEq }
"->" { TokenArrow }
'+' { TokenAdd }
'-' { TokenSub }
'*' { TokenMul }
'/' { TokenDiv }
'<' { TokenLess }
'>' { TokenBigger }
'=' { TokenEquals }
'.' { TokenDot }
':' { TokenColon }
',' { TokenComma }
'(' { TokenOBrack }
')' { TokenCBrack }
'[' { TokenOSquare }
']' { TokenCSquare }
"if" { TokenIf }
"then" { TokenThen }
"else" { TokenElse }

%%

Label : label { $1 }

Expr : int { ConstI $1 Int }
| '[' int ']' { ConstI $2 Dyn }
| float { ConstF $1 Float }
| '[' float ']' { ConstF $2 Dyn }
| bool { ConstB $1 Bool }
| '[' bool ']' { ConstB $2 Dyn }
| var { VarE $1 }
| Expr '+' Expr { Add $1 $3 }
| Expr '-' Expr { Sub $1 $3 }
| Expr '*' Expr { Mul $1 $3 }
| Expr '/' Expr { Div $1 $3 }
| Expr '<' Expr { Less $1 $3 }
| Expr '>' Expr { Bigger $1 $3 }
| Expr "<=" Expr { LessEq $1 $3 }
| Expr ">=" Expr { BiggerEq $1 $3 }
| Expr '<' Expr { Less $1 $3 }
| Expr '>' Expr { Bigger $1 $3 }
| Expr "==" Expr { Eq $1 $3 }
| "if" Expr "then" Expr "else" Expr { If $2 $4 $6 }
| Expr Expr { AppE $1 $2 }
| "\\" var '.' Type ':' Expr { FuncE $2 $4 $6 }
| '<' Type "<=" Type ',' label '>' Expr { ExprC $8 $4 $2 $6 } 


Type : int { $1 }
| float { $1 }
| bool { $1 }
| dyn { $1 }
| Type "->" Type { FuncT $1 $3 }



{

  parseError :: [Token] -> a
    parseError _ = error "Parse error"

    data Token
    = TokenInt
    | TokenFloat
    | TokenBool
    | TokenDyn
    | TokenVar String
    | TokenLabel
    | TokenEq
    | TokenBiggerEq
    | TokenLessEq
    | TokenLess
    | TokenBigger
    | TokenLambda
    | TokenArrow
    | TokenAdd
    | TokenSub
    | TokenMul
    | TokenDiv
    | TokenEquals
    | TokenDot
    | TokenColon
    | TokenComma
    | TokenOBrack
    | TokenCBrack
    | TokenOSquare
    | TokenCSquare
    | TokenIf
    | TokenThen
    | TokenElse
    deriving (Show, Eq)
    
    lexer :: String -> [Token]
    lexer [] = []
    lexer (c:cs) 
    | isSpace c = lexer cs
    | isAlpha c = lexVar (c:cs)
    | isDigit c = lexNum (c:cs)
    lexer ("==":cs) = TokenEq : lexer cs
    lexer ("<=":cs) = TokenLessEq : lexer cs
    lexer (">=":cs) = TokenBiggerEq : lexer cs
    lexer ("->":cs) = TokenArrow : lexer cs
    lexer ('=':cs) = TokenEquals : lexer cs
    lexer ('+':cs) = TokenAdd : lexer cs
    lexer ('-':cs) = TokenSub : lexer cs
    lexer ('*':cs) = TokenMul : lexer cs
    lexer ('/':cs) = TokenDiv : lexer cs
    lexer (':':cs) = TokenColon : lexer cs
    lexer ('.':cs) = TokenDot : lexer cs
    lexer (',':cs) = TokenComma : lexer cs
    lexer ("\\":cs) = TokenLambda : lexer cs
    lexer ('(':cs) = TokenOBrack : lexer cs
    lexer (')':cs) = TokenCBrack : lexer cs
    lexer ('[':cs) = TokenOSquare : lexer cs
    lexer (']':cs) = TokenCSquare : lexer cs

    lexNum cs = TokenInt (read num) : lexer rest
    where (num,rest) = span isDigit cs
    
    lexVar cs =
    case span isAlpha cs of
    ("if",rest) -> TokenIf : lexer rest
    ("then",rest)  -> TokenThen : lexer rest
    ("else",rest) -> TokenElse : lexer rest
    (var,rest)   -> TokenVar var : lexer rest
    (label,rest) -> TokenLabel : lexer rest
    
    
    main = getContents >>= print . calc . lexer
    }
