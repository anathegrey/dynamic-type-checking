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
var { TokenVar $$ }
label { TokenLabel $$ }
"Int" { TokenStringInt }
"Float" { TokenStringFloat }
"Bool" { TokenStringBool }
"Dyn" { TokenStringDyn }
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

%nonassoc '<' '>' "<=" ">="
%left '+' '-'
%left '*' '/'
%%

Expr : Expr1 { $1 }
| ExprArith { $1 }
| ExprBool { $1 }

Expr1 :  int { ConstI $1 Int }
| '[' int ']' { ConstI $2 Dyn }
| float { ConstF $1 Float }
| '[' float ']' { ConstF $2 Dyn }
| bool { ConstB $1 Bool }
| '[' bool ']' { ConstB $2 Dyn }
| var { VarE $1 }
| '(' Expr ')' { $2 }
| '(' Expr ')''(' Expr ')' { AppE $1 $2 }
| "if" ExprBool "then" Expr1 "else" Expr1 { If $2 $4 $6 }
|  "\\" var '.' Type ':' Expr1 { FuncE $2 $4 $6 }
| '<' Type "<=" Type ',' Label '>' Expr1 { ExprC $8 $4 $2 $6 }

ExprArith : Expr '+' Expr1 { Add $1 $3 }
| Expr '-' Expr1 { Sub $1 $3 }
| Expr '*' Expr1 { Mul $1 $3 }
| Expr '/' Expr1 { Div $1 $3 }

ExprBool : Expr "<=" Expr1 { LessEq $1 $3 }
| Expr ">=" Expr1 { BiggerEq $1 $3 }
| Expr '<' Expr1 { Less $1 $3 }
| Expr '>' Expr1 { Bigger $1 $3 }
| Expr "==" Expr1 { Eq $1 $3 }


Label : label { Label $1 }

Type : "Int" { $1 }
| "Float" { $1 }
| "Bool" { $1 }
| "Dyn" { $1 }
| Type "->" Type1 { FuncT $1 $3  }

Type1 : "Int" { $1 }
| "Float" { $1 }
| "Bool" { $1 }
| "Dyn" { $1 }

{

  parseError :: [Token] -> a
    parseError _ = error "Parse error"

    data Token
    = TokenInt
    | TokenFloat
    | TokenBool
    | TokenVar String
    | TokenLabel
    | TokenEq
    | TokenBiggerEq
    | TokenLessEq
    | TokenLess
    | TokenBigger
    | TokenLambda
    | TokenArrow
    | TokenStringInt
    | TokenStringFloat
    | TokenStringDyn
    | TokenStringBool
    | TokenAdd
    | TokenSub
    | TokenMul
    | TokenDiv
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
    lexer ('<':cs) = TokenLess : lexer cs
    lexer ('>':cs) = TokenBigger : lexer cs
    lexer ("->":cs) = TokenArrow : lexer cs
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

    lexNum cs = let (num,rest) = span isDigit cs
    in if (filter (\x -> x == '.')) cs == [] then TokenInt (read num) : lexer rest else TokenFloat (read num) : '.' : lexNum (tail rest)
    
    lexVar cs =
    case span isAlpha cs of
    ("if",rest) -> TokenIf : lexer rest
    ("then",rest)  -> TokenThen : lexer rest
    ("else",rest) -> TokenElse : lexer rest
    (var,rest)   -> TokenVar var : lexer rest
    (label,rest) -> TokenLabel label : lexer rest
    ("Int",rest) -> TokenStringInt : lexer rest
    ("Bool",rest) -> TokenStringBool : lexer rest
    ("Float",rest) -> TokenStringFloat : lexer rest
    ("Dyn",rest) -> TokenStringDyn : lexer rest
    
    main = getContents >>= print . calc . lexer
    }
