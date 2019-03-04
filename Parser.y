{
module Parser where

import AbsPascal
import Lexer
}

%name parser prog

%tokentype {Token}

%error {parseError}

%right EQ ADDE SUBE MULTE DIVE DIVINTE MODE

--Operatori Relazionali
%nonassoc OR
%nonassoc AND
%nonassoc NOT
%nonassoc EQU NEQ
%nonassoc LEQ LES GRT GEQ

--Operatori Aritmetici
%left ADD SUB 
%left DIV DIVINT MOD MULT

%right PARCLOSE ELSE

%token

IDENT		{Tkn _ (TokenIdent $$)}
CONSTNAME 	{Tkn _ (TokenConstName $$)}

CONSTINT			{Tkn _ (TokenConstInteger $$)}
CONSTFLOAT			{Tkn _ (TokenConstFloat $$)}
CONSTCHAR			{Tkn _ (TokenConstChar $$)}
CONSTSTRING			{Tkn _ (TokenConstString $$)}
CONSTBOOLEANFALSE	{Tkn _ (TokenConstBooleanFalse $$)}
CONSTBOOLEANTRUE	{Tkn _ (TokenConstBooleanTrue $$)}

PAROPEN		{Tkn _ TokenParO}
PARCLOSE	{Tkn _ TokenParC}
QPAROPEN	{Tkn _ TokenParQO}
QPARCLOSE	{Tkn _ TokenParQC}
BLOCKOPEN	{Tkn _ TokenBckO}
BLOCKCLOSE	{Tkn _ TokenBckC}

PROG 		{Tkn _ TokenProgram}
CONST		{Tkn _ TokenConst}
VAR			{Tkn _ TokenVar}
BEGIN		{Tkn _ TokenBegin}
END			{Tkn _ TokenEnd}
FUN         {Tkn _ (TokenFunction)}
PROC        {Tkn _ (TokenProcedure)}
ARRAY 		{Tkn _ (TokenArray)}

INT			{Tkn _ TokenInt}
FLOAT		{Tkn _ TokenFloat}
CHAR		{Tkn _ TokenChar}
BOOLEAN		{Tkn _ TokenBoolean}
STRING		{Tkn _ TokenString}

COLON		{Tkn _ TokenColon}
SEMICOLON	{Tkn _ TokenSemiColon}
COMMA		{Tkn _ TokenComma}
DOT			{Tkn _ TokenDot}
POINTER     {Tkn _ TokenPointer}

EQ			{Tkn _ TokenEq}
LT			{Tkn _ TokenLt}
GT			{Tkn _ TokenGt}
LET			{Tkn _ TokenLet}
GET			{Tkn _ TokenGet}
NEQ			{Tkn _ TokenNeq}
ECMP		{Tkn _ TokenEcmp}
ADD			{Tkn _ TokenAdd}
SUB			{Tkn _ TokenSub}
DIV			{Tkn _ TokenDiv}
DIVINT		{Tkn _ TokenDivInt}
MULT		{Tkn _ TokenMult}
MOD			{Tkn _ TokenMod}
AND			{Tkn _ TokenAnd}
OR			{Tkn _ TokenOr}
NOT			{Tkn _ TokenNot}

IF			{Tkn _ TokenIf} 				
THEN 		{Tkn _ TokenThen}
ELSE		{Tkn _ TokenElse}
WHILE		{Tkn _ TokenWhile}
FOR			{Tkn _ TokenFor}
TO 			{Tkn _ TokenTo}
DO 			{Tkn _ TokenDoCmd}
OF			{Tkn _ TokenOf}
BREAK		{Tkn _ TokenBreak}
CONTINUE	{Tkn _ TokenContinue}
%%

--Punto di partenza
prog :: { Program } 
	: PROG IDENT SEMICOLON block DOT { Prog $2 $4 }

--Lista di statement
statementList :: { [Statement] }
	: { [] }
	| statement statementList { $1:$2 }

--Statements
statement :: { Statement } 
	: block {StatementBlock $1 }
	| declaration SEMICOLON{ StatementDec $1 }
	| assignment SEMICOLON { StatementAss $1 }
	| rexpr { StatementRexpr $1 }
	| condition { StatementCond $1 }
	| function { StatementFunc $1 }
	| loop { StatementLoop $1 }

--Blocco iniziale
block :: { [Statement] }
	: BLOCKOPEN statementList BLOCKCLOSE { $2 }

--Tipi di assegnamento
assignment :: { Assignment }
	: IDENT EQ rexpr { AssSimple $1 AssEqual $3 }
	| IDENT POINTER EQ rexpr { PointerAssSimple $1 AssEqual $4 }
	| IDENT QPAROPEN rexpr QPARCLOSE EQ rexpr { ArrayAssSimple $1 $3 AssEqual $6 }

--Tipi di dichiarazione
declaration :: { Declaration }
	: VAR IDENT COLON type ECMP rexpr { VarDec $2 $4 $6}
	| CONST CONSTNAME ECMP const { ConstDec $2 $4 }
	| VAR IDENT COLON POINTER type { PointerDec $2 $5 }
	| VAR IDENT COLON ARRAY QPAROPEN rexpr DOT DOT rexpr QPARCLOSE OF type { ArrayDec $2 $6 $9 $12 }

--Definizione di funzioni/procedure
function :: { FunDecl }
	: FUN IDENT PAROPEN prs PARCLOSE COLON type SEMICOLON BEGIN statementList END SEMICOLON { Fun $2 $4 $7 $10 }
	| PROC IDENT PAROPEN prs PARCLOSE SEMICOLON BEGIN statementList END SEMICOLON { Proc $2  $4 $8 }

--Parametri
prs :: { [Parameter] }
	: { [] }
	| listPar { $1 }

--Lista di parametri
listPar :: { [Parameter] }
	: par { [$1] }
	| par SEMICOLON listPar { $1:$3 }

--Parametro (passato per valore e o riferimento)
par :: { Parameter }
	: IDENT COLON type { Param $1 $3 }
	| VAR IDENT COLON type{ Param $2 $4 }

--Condizioni 
condition :: { Condition }
	: if { $1 }
	| ifBreak { $1 }
	| ifContinue { $1 }
	| ifElse { $1 }
	| ifElseBreak { $1 }
	| ifElseContinue { $1 }

if :: { Condition }
	: IF PAROPEN rexpr PARCLOSE THEN statement { IfNoElse $3 $6 }
	
ifBreak :: { Condition }
	: IF PAROPEN rexpr PARCLOSE THEN BREAK SEMICOLON{ IfNoElseBreak $3 }

ifContinue :: { Condition }
	: IF PAROPEN rexpr PARCLOSE THEN CONTINUE SEMICOLON { IfNoElseContinue $3 }	

ifElseBreak :: { Condition }
	: IF PAROPEN rexpr PARCLOSE THEN statement ELSE BREAK SEMICOLON { IfElseBreak $3 $6 }

ifElseContinue :: { Condition }
	: IF PAROPEN rexpr PARCLOSE THEN statement ELSE CONTINUE SEMICOLON{ IfElseContinue $3 $6 }

ifElse :: { Condition }
	: IF PAROPEN rexpr PARCLOSE THEN statement ELSE statement { IfElse $3 $6 $8 }

--Ciclo
loop :: { Loop }
	: WHILE PAROPEN rexpr PARCLOSE DO BEGIN statement END SEMICOLON { While $3 $7 }
	| FOR assignment TO rexpr DO BEGIN statement END SEMICOLON { For $2 $4 $7 }
	| WHILE PAROPEN rexpr PARCLOSE DO BEGIN BREAK SEMICOLON END SEMICOLON { WhileBreak $3 } 
	| WHILE PAROPEN rexpr PARCLOSE DO BEGIN CONTINUE SEMICOLON END SEMICOLON { WhileContinue $3 } 
	| FOR assignment TO rexpr DO BEGIN BREAK SEMICOLON END SEMICOLON { ForBreak $2 $4 }
	| FOR assignment TO rexpr DO BEGIN CONTINUE SEMICOLON END SEMICOLON { ForContinue $2 $4 }

--Tipi di R-expression
rexpr :: { RExpr }
	: rexpr AND rexpr { ROpBin $1 And $3 }
	| rexpr OR rexpr  { ROpBin $1 Or $3 }
	| NOT rexpr { ROpUn Not $2 }
	| rexpr LT rexpr  { ROpBin $1 LessThan $3 }
	| rexpr GT rexpr { ROpBin $1 GreaterThan $3 }
	| rexpr LET rexpr { ROpBin $1 LessEq $3 }
	| rexpr GET rexpr { ROpBin $1 GreaterEq $3 }
	| rexpr ECMP rexpr { ROpBin $1 EqComp $3 }
	| rexpr NEQ rexpr {ROpBin $1 Neq $3 }
	| rexpr ADD rexpr { ROpBin $1 Add $3 }
	| rexpr SUB rexpr { ROpBin $1 Sub $3 }
	| rexpr DIV rexpr { ROpBin $1 Div $3 }
	| rexpr DIVINT rexpr { ROpBin $1 DivInt $3 }
	| rexpr MOD rexpr { ROpBin $1 Mod $3 }
	| rexpr MULT rexpr { ROpBin $1 Mult $3 }
	| SUB constNeg { $2 }
	| PAROPEN rexpr PARCLOSE { Par $2 }
	| lValue { RxLx $1 }
	| const { $1 }

--L-expression
lValue :: { LExpr }
	: IDENT { LId $1 }
	| lValue POINTER { LPt $1 }
	| IDENT QPAROPEN rexpr QPARCLOSE { LArr $1 $3 }

--Tipi
type :: { Type }
	: INT { SInt }
	| CHAR { SChar }
	| FLOAT { SFloat }
	| BOOLEAN { SBool }
	| STRING { SString }
   
--Costanti
const :: { RExpr } 
	: CONSTBOOLEANTRUE { RBool $1 }
	| CONSTBOOLEANFALSE { RBool $1 }  
	| CONSTCHAR { RChar $1 }
	| CONSTSTRING { RString $1 } 
	| CONSTINT { RInt $1 } 
	| CONSTFLOAT { RFloat $1 }
	
--Costanti negative
constNeg :: { RExpr }
	: CONSTINT { RInt (-$1) } 
	| CONSTFLOAT { RFloat (-$1) }

{

myparse s = parser $ alexScanTokens s

parseError ((Tkn (l,c) t ):_) = error $ "Errore di parsing alla (linea,colonna): (" ++ (show l) ++ "," ++ (show c) ++ ") . Token inatteso: " ++ (show t)
parseError [] = error "Errore di parsing alla fine di una riga o del file."

}