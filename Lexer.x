{
module Lexer where
}

%wrapper "posn"

$digit = [0-9]
$letter = [a-zA-Z]
$stringchar = [^"]
--Le variabili del programma iniziano con la lettera minuscola
@ident = [a-z][a-zA-Z0-9_]*
@constName = [A-Z][A-Z_]*

tokens :-

--Dichiarazioni

program				{\(AlexPn _ l c) _ -> Tkn (l,c) TokenProgram}
const				{\(AlexPn _ l c) _ -> Tkn (l,c) TokenConst}
var					{\(AlexPn _ l c) _ -> Tkn (l,c) TokenVar}
procedure			{\(AlexPn _ l c) _ -> Tkn (l,c) TokenProcedure}
function			{\(AlexPn _ l c) _ -> Tkn (l,c) TokenFunction}
array				{\(AlexPn _ l c) _ -> Tkn (l,c) TokenArray}

--Tipi

integer				{\(AlexPn _ l c) _ -> Tkn (l,c) TokenInt}
real				{\(AlexPn _ l c) _ -> Tkn (l,c) TokenFloat}
char				{\(AlexPn _ l c) _ -> Tkn (l,c) TokenChar}
boolean				{\(AlexPn _ l c) _ -> Tkn (l,c) TokenBoolean}
string				{\(AlexPn _ l c) _ -> Tkn (l,c) TokenString}

--Parentesi

"{" 			{\(AlexPn _ l c) _ -> Tkn (l,c) TokenBckO}
"}"				{\(AlexPn _ l c) _ -> Tkn (l,c) TokenBckC}
"("				{\(AlexPn _ l c) _ -> Tkn (l,c) TokenParO}
")"				{\(AlexPn _ l c) _ -> Tkn (l,c) TokenParC}
"["				{\(AlexPn _ l c) _ -> Tkn (l,c) TokenParQO}
"]"				{\(AlexPn _ l c) _ -> Tkn (l,c) TokenParQC}
"^"				{\(AlexPn _ l c) _ -> Tkn (l,c) TokenPointer}

--Blocchi

begin			{\(AlexPn _ l c) _ -> Tkn (l,c) TokenBegin}
end				{\(AlexPn _ l c) _ -> Tkn (l,c) TokenEnd}

--Punteggiatura

":"				{\(AlexPn _ l c) _ -> Tkn (l,c) TokenColon}
";"				{\(AlexPn _ l c) _ -> Tkn (l,c) TokenSemiColon}
","				{\(AlexPn _ l c) _ -> Tkn (l,c) TokenComma}
"."				{\(AlexPn _ l c) _ -> Tkn (l,c) TokenDot}

--Operatori

":="				{\(AlexPn _ l c) _ -> Tkn (l,c) TokenEq}
"<"					{\(AlexPn _ l c) _ -> Tkn (l,c) TokenLt}
">"					{\(AlexPn _ l c) _ -> Tkn (l,c) TokenGt}
"<="				{\(AlexPn _ l c) _ -> Tkn (l,c) TokenLet}
">="				{\(AlexPn _ l c) _ -> Tkn (l,c) TokenGet}
"<>"				{\(AlexPn _ l c) _ -> Tkn (l,c) TokenNeq}
"="					{\(AlexPn _ l c) _ -> Tkn (l,c) TokenEcmp}
"+"					{\(AlexPn _ l c) _ -> Tkn (l,c) TokenAdd}
"-"					{\(AlexPn _ l c) _ -> Tkn (l,c) TokenSub}
"/"					{\(AlexPn _ l c) _ -> Tkn (l,c) TokenDiv}
"div"				{\(AlexPn _ l c) _ -> Tkn (l,c) TokenDivInt}
"*"					CONST{\(AlexPn _ l c) _ -> Tkn (l,c) TokenMult}
"mod"				{\(AlexPn _ l c) _ -> Tkn (l,c) TokenMod}
"and"				{\(AlexPn _ l c) _ -> Tkn (l,c) TokenAnd}
"or"				{\(AlexPn _ l c) _ -> Tkn (l,c) TokenOr}
"not"				{\(AlexPn _ l c) _ -> Tkn (l,c) TokenNot}

--Costrutti

if					{\(AlexPn _ l c) _ -> Tkn (l,c) TokenIf}
then 				{\(AlexPn _ l c) _ -> Tkn (l,c) TokenThen}
else				{\(AlexPn _ l c) _ -> Tkn (l,c) TokenElse}
while				{\(AlexPn _ l c) _ -> Tkn (l,c) TokenWhile}
for					{\(AlexPn _ l c) _ -> Tkn (l,c) TokenFor}
to          		{\(AlexPn _ l c) _ -> Tkn (l,c) TokenTo}
do  				{\(AlexPn _ l c) _ -> Tkn (l,c) TokenDoCmd}
of					{\(AlexPn _ l c) _ -> Tkn (l,c) TokenOf}
break				{\(AlexPn _ l c) _ -> Tkn (l,c) TokenBreak}
continue			{\(AlexPn _ l c) _ -> Tkn (l,c) TokenContinue}

--Valori

"false"						{\(AlexPn _ l c) s -> Tkn (l,c) (TokenConstBooleanFalse False)}
"true"						{\(AlexPn _ l c) s -> Tkn (l,c) (TokenConstBooleanTrue True)}
"'" $printable "'"			{\(AlexPn _ l c) s -> Tkn (l,c) (TokenConstChar (read s)) }
\" ($stringchar|\")* \"  	{\(AlexPn _ l c) s -> Tkn (l,c) (TokenConstString s)} 
$digit+						{\(AlexPn _ l c) s -> Tkn (l,c) (TokenConstInteger (read s))} 
$digit+ \. $digit*			{\(AlexPn _ l c) s -> Tkn (l,c) (TokenConstFloat (read (s++"0")))} 
@ident 						{\(AlexPn _ l c) s -> Tkn (l,c) (TokenIdent s) }
@constName					{\(AlexPn _ l c) s -> Tkn (l,c) (TokenConstName s) }

$white+ 									;


"{".*"}"									;
"{*"([ . # \*] | \*[ . # \/] | \n )*"*}"	; 
.				;

{

data Token = Tkn Posizione TokenClass 
   deriving (Eq,Show)
   
type Posizione = (Int,Int)   

data TokenClass = TokenIdent String |
	TokenConstName String |
	TokenConstInteger Int |
	TokenConstFloat Float |
	TokenConstChar Char |
	TokenConstString String |
	TokenConstBooleanFalse Bool |
	TokenConstBooleanTrue Bool |
	TokenBckO |
	TokenBckC |
	TokenParO |
	TokenParC |
	TokenParQO |
	TokenParQC |
	TokenArray |
	TokenProgram |
	TokenConst |
	TokenProcedure |
	TokenFunction |
	TokenVar |
	TokenBegin |
	TokenEnd |
	TokenInt |
	TokenFloat |
	TokenChar |
	TokenBoolean |
	TokenString |
	TokenColon |
	TokenSemiColon |
	TokenComma |
	TokenDot |
	TokenPointer |
	TokenEq |
	TokenLt |
	TokenGt |
	TokenLet |
	TokenGet |
	TokenNeq |
	TokenEcmp |
	TokenAdd |
	TokenSub |
	TokenDiv |
	TokenDivInt |
	TokenMult |
	TokenMod |
	TokenAnd |
	TokenOr |
	TokenNot |
	TokenIf |
	TokenThen |
	TokenElse |
	TokenBreak |
	TokenContinue |
	TokenWhile |
	TokenDoCmd |
	TokenOf |
	TokenTo |
	TokenFor
	deriving(Eq,Show)

}