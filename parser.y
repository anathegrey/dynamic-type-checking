%name calc
%tokentype { Token }
%error { parseError }

%token 
int             { TokenInt $$ }
float           { TokenFloat $$ }
bool            { TokenBool $$ }
dyn             { TokenDyn $$ }
var             { TokenVar $$ }
if              { TokenIf }
then            { TokenThen }
 else           { TokenElse }
blame           { TokenBlame }
label           { TokenLabel $$ }
'='             { TokenEq }
'+'             { TokenAdd }
'-'             { TokenSub }
'*'             { TokenMul }
'/'             { TokenDiv }
'=='            { TokenEqual }
'<'             { TokenLess }
'>'             { TokenBigger }
'<='            { TokenLessEq }
'>='            { TokenBiggerEq }
'('             { TokenOB }
')'             { TokenCB }
'['             { TokenOSB }
']'             { TokenCSB }
'->'            { TokenArrow }
'\'             { TokenLambda }
'.'             { TokenDot }
':'             { TokenColon }
','             { TokenComma }

%left '+' '-'
%left '*' '/'
%%

Type : int  { Int $1 }
| bool  { Bool $1 }
| float { Float $1 }
| dyn   { Dyn $1 }
| Type '->' Type { FuncT $1 $3 }
| NoneType { NoneType }

Label : label { Label $1 }

Expr : int       { ConstI $1 Int }
| bool           { ConstB $1 Bool }
| float          { ConstF $1 Float }
| '[' int ']'    { ConstI $1 Dyn }
| '[' float ']'  { ConstF $1 Dyn }
| var            { VarE $1 }
| Expr '+' Expr  { Add $1 $3 }
| Expr '-' Expr  { Sub $1 $3 }
| Expr '*' Expr  { Mul $1 $3 }
| Expr '/' Expr  { Div $1 $3 }
| Expr '<' Expr  { Less $1 $3 }
| Expr '>' Expr  { Bigger $1 $3 }
| Expr '<=' Expr { LessEq $1 $3 }
| Expr '>=' Expr { BiggerEq $1 $3 }
| Expr '==' Expr { Eq $1 $3 }
| if Expr then Expr else Expr  { If $2 $4 $6 }
| '\' var '.' Type ':' Expr { FuncE $2 $4 $6 }
| Expr Expr      { AppE $1 $2 }
| '<' Type '<' '=' Type ',' label '>' Expr { ExprC $9 $5 $2 $7 }
| blame Type label { Blame $2 $3 }
 
{

 dataToken
 = TokenInt Int
 | TokenFloat Float
 | TokenBool Bool
 | TokenDyn  Dyn
 | TokenVar  String
 | TokenIf
 | TokenThen
 | TokenElse
 | TokenBlame
 | TokenLabel String
 | TokenEq
 | TokenAdd
 | TokenSub
 | TokenMul
 | TokenDiv
 | TokenEqual
 | TokenLess
 | TokenBigger
 | TokenLessEq
 | TokenBiggerEq
 | TokenOB
 | TokenCB
 | TokenOSB
 | TokenCSB
 | TokenArrow
 | TokenLambda
 | TokenDot
 | TokenColon
 | TokenComma
 deriving(Show, Eq)

 parseError :: [Token] -> a
 parseError _ = error "Parse error"

 lexer :: String -> [Token]
 lexer [] = []
 lexer (c:cs) 
 | isSpace c = lexer cs
 | isAlpha c = lexVar (c:cs)
 | isDigit c = lexNum (c:cs)
 lexer ('=':cs) = TokenEq : lexer cs
 lexer ('+':cs) = TokenAdd : lexer cs
 lexer ('-':cs) = TokenSub : lexer cs
 lexer ('*':cs) = TokenMul : lexer cs
 lexer ('/':cs) = TokenDiv : lexer cs
 lexer ('<':cs) = TokenLess : lexer cs
 lexer ('>':cs) = TokenBigger : lexer cs
 lexer ('<=':cs) = TokenLessEq : lexer cs
 lexer ('>=':cs) = TokenBiggerEq : lexer cs
 lexer ('(':cs) = TokenOB : lexer cs
 lexer (')':cs) = TokenCB : lexer cs
 lexer (':':cs) = TokenColon : lexer cs
 lexer ('.':cs) = TokenDot : lexer cs
 lexer (',':cs) = TokenComma : lexer cs
 lexer ('\':cs) = TokenLambda : lexer cs

 lexNum cs = TokenInt { read num } : lexer rest
 where (num, rest) = span isDigit cs

 lexVar cs =
 case span isAlpha cs of
 ("if", rest) -> TokenIf : lexer rest
 ("then", rest) -> TokenThen : lexer rest
 ("else", rest) -> TokenElse : lexer rest
 ("blame", rest) -> TokenBlame : lexer rest
 (var, rest) -> TokenVar : lexer rest
 ("label", rest) -> TokenLabel : lexer rest

 main = getContents >>= print . calc . lexer
}
