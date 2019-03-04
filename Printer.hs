module Printer where

import AbsPascal
import Lexer
import Parser

prettyPrinter :: Program->String

--La funzione stampa la prima linea del programma e richiama il prettyPrinter su tutti gli statements contenuti in xs
prettyPrinter (Prog name xs) = "program " ++ name ++";\n" ++ "{\n"++ foldl progprint "" xs  ++ "}." 
	where
		progprint z1 st = z1 ++ (printer 1 st)

--Classe printer per la stampa (a terminale)
class Printer a where
	printer :: Int -> a -> String

--Istanza di stampa per statements
instance Printer Statement where
	printer i stat = case stat of
		StatementBlock stmts -> (indentation i) ++ "begin" ++ (foldl (\x y -> x ++ (printer (i+1) y)) "" stmts ) ++ (indentation i) ++ "end;\n"
		StatementDec dichiarazione -> (indentation i) ++ (printer i dichiarazione)
		StatementAss assegnamento -> (indentation i) ++ (printer i assegnamento) ++ " \n"
		StatementRexpr espressione -> (indentation i) ++ (printer i espressione)++" ;\n"
		StatementCond condizione -> (indentation i) ++ (printer i condizione)
		StatementLoop ciclo -> (indentation i) ++ (printer i ciclo)
		StatementFunc funz -> (indentation i) ++ (printer i funz)

--Istanza di stampa per R-expression
instance Printer RExpr where
	printer i rexpr = case rexpr of
		RInt val -> (indentation i) ++ (show val)
		RFloat val -> (indentation i) ++ (show val)
		RBool val -> (indentation i) ++ (show val)
		RChar val -> (indentation i) ++ (show val)
		RString val -> (indentation i) ++ val
		Par val -> (indentation i) ++ "( " ++ (printer 0 val) ++ " )"
		RxLx left -> (indentation i) ++ (printer 0 left)
		ROpUn op a ->  (indentation 0) ++ (printer 0 op)++(printer 0 a)
		ROpBin a1 op a2 -> (indentation 0) ++ (printer 0 a1)++(printer 0 op)++(printer 0 a2)

--Istanza di stampa per L-expression
instance Printer LExpr where
	printer i lexpr = case lexpr of
		LId v -> v
		LPt left -> (printer 0 left) ++"^"
		
--Istanza di stampa per operatori unari
instance Printer UnaryOperator where
	printer _ op = case op of
		Not		->" not "

--Istanza di stampa per operatori binari
instance Printer BinaryOperator where
	printer _ op = case op of
		Add 		-> " + "
		Sub			-> " - "
		Mod			-> " mod "
		Div			-> " / "
		DivInt		-> " div "
		Mult		-> " * "
		And 		-> " and "
		Or			-> " or "
		LessThan	-> " < "
		GreaterThan	-> " > "
		LessEq		-> " <= "
		GreaterEq	-> " >= "
		EqComp		-> " = "
		Neq			-> " <> "

--Istanza di stampa per stampa di cicli While e For (includendo anche break e continue)
instance Printer Loop where
	printer i ciclo = case ciclo of
		While cond stmts-> "while ( " ++ (printer 0 cond) ++ " ) do\n" ++ (indentation i) ++"begin\n"++ (printBlock (i+1) stmts) ++(indentation i) ++"end;\n"
		WhileBreak cond-> "while ( " ++ (printer 0 cond) ++ " ) do\n" ++ (indentation i) ++"begin\n"++(indentation i)++ "break;\n"++(indentation i) ++"end;\n"
		WhileContinue cond-> "while ( " ++ (printer 0 cond) ++ " ) do\n" ++ (indentation i) ++"begin\n"++(indentation i)++ "continue;\n" ++(indentation i) ++"end;\n"
		For ass cond stmts-> "for " ++ (printer 0 ass) ++ " to " ++ (printer 0 cond) ++ " do\n" ++ (indentation i) ++"begin\n"++ (printBlock (i+1) stmts) ++(indentation i) ++"end;\n"
		ForBreak ass cond-> "for " ++ (printer 0 ass) ++ " to " ++ (printer 0 cond) ++ " do\n" ++ (indentation i) ++"begin\n"++(indentation i)++ "break;\n" ++(indentation i) ++"end;\n"
		ForContinue ass cond-> "for " ++ (printer 0 ass) ++ " to " ++ (printer 0 cond) ++ " do\n" ++ (indentation i) ++"begin\n"++(indentation i)++ "continue;\n" ++(indentation i) ++"end;\n"

--Istanza di stampa del tipo (tipi semplici e complessi:array e puntatori)
instance Printer Type where
	printer _ tipo = case tipo of
		SInt 			-> "integer"
		SFloat 			-> "real"
		SChar 			-> "char"
		SString 		-> "string"
		SBool 			-> "boolean"

--Istanza di stampa per le dichiarazioni
instance Printer Declaration where
	printer i dichiarazione = case dichiarazione of
		VarDec ident rexpr tipo ->  "var "++ident++" : "++ (printer 0 rexpr) ++ " = " ++(printer 0 tipo)++" ;\n"
		ConstDec ident rexpr ->  "const "++ident++" = "++(printer 0 rexpr)++" ;\n"
		PointerDec ident tipo  -> "var " ++ident++" : "++ "^"++(printer 0 tipo) ++" ;\n"
		ArrayDec ident rexprI rexprF tipo -> "var " ++ident++ " = "++"array ["++(printer 0 rexprI)++" .. "++(printer 0 rexprF)++"] of "++(printer 0 tipo)++" ;\n"

--Istanza di stampa per gli assegnamenti
instance Printer Assignment where
	printer i assegnamento = case assegnamento of
		AssSimple ident _ rexpr ->  ident++" := "++(printer 0 rexpr)++" ;"
		PointerAssSimple ident _ rexprV -> ident ++ "^ := " ++ (printer 0 rexprV)++ " ;"
		ArrayAssSimple ident rexprI _ rexprV -> ident ++ "["++(printer 0 rexprI)++"] := " ++ (printer 0 rexprV) ++ " ;" 

--Istanza di stampa gli If-IfElse (includendo anche break e continue)
instance Printer Condition where
	printer i condizione = case condizione of
		IfNoElse cond statements -> "if ( " ++ (printer 0 cond) ++ " ) then\n" ++ (printBlock (i+1) statements)
		IfElse cond statements1 statements2 -> "if ( " ++ (printer 0 cond) ++ " ) then\n" ++ (printBlock (i+1) statements1) ++ (indentation i) ++ "else\n" ++ (printBlock (i+1) statements2)
		IfNoElseBreak cond -> "if ( " ++ (printer 0 cond) ++ " ) then\n" ++(indentation i)++ "break;\n"
		IfNoElseContinue cond -> "if ( " ++ (printer 0 cond) ++ " ) then\n" ++(indentation i)++ "continue;\n"
		IfElseContinue cond statements -> "if ( " ++ (printer 0 cond) ++ " ) then\n" ++ (printBlock (i+1) statements) ++ (indentation i) ++ "else\n" ++(indentation i)++ "continue;\n"
		IfElseBreak cond statements -> "if ( " ++ (printer 0 cond) ++ " ) then\n" ++ (printBlock (i+1) statements) ++ (indentation i) ++ "else\n" ++(indentation i)++ "break;\n"

--Istanza di stampa per funzioni e procedure
instance Printer FunDecl where
	printer i funz = case funz of
		Fun ident pars tipo stmts -> "function "++ident++" ( "++(foldl (\x y -> x ++ (printer (i+1) y)) "" pars )++" ) : "++(printer 0 tipo)++" ;\n"++ (indentation i) ++"begin\n "++(foldl (\x y -> x ++ (printer (i+1) y)) "" stmts) ++(indentation i)++ "end;\n\n"
		Proc ident pars stmts -> "procedure "++ident++" ( "++(foldl (\x y -> x ++ (printer (i+1) y)) "" pars)++" ) ;\n" ++(indentation i)++ "begin\n" ++(foldl (\x y -> x ++ (printer (i+1) y)) "" stmts)++(indentation i) ++"end;\n\n"

--Istanza di stampa dei parametri di una funzione/procedura
instance Printer Parameter where
	printer i pars = case pars of
		Param ident tipo -> ident++" : "++(printer 0 tipo) ++" ; "

--Metodo di gestione degli spazi e rientri
indentation  :: Int->String
indentation 0 = ""
indentation i = "  "++(indentation (i-1) )

--Funzione di stampa dei blocchi (StatementBlock)
printBlock :: Int -> Statement -> String
printBlock i (StatementBlock x) = (printer i (StatementBlock x))
printBlock i stat = (printer (i+1) stat)