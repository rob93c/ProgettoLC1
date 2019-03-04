module AbsPascal where


--Identificatore delle variabili
type Ident = String

data Program = Prog Ident [Statement]
	deriving (Eq, Show)

--Istruzioni dentro un block
data Statement = StatementBlock [Statement]
	| StatementDec Declaration
	| StatementAss Assignment
	| StatementRexpr RExpr
	| StatementCond Condition
	| StatementFunc FunDecl
	| StatementLoop Loop
	deriving (Eq, Show)

--L-expression
data LExpr = LId Ident
	| LPt LExpr
	| LArr Ident RExpr
	deriving (Eq, Show)

--dichiarazione di variabili, costanti, puntatori e array
data Declaration = VarDec Ident Type RExpr
	| ConstDec Ident RExpr
	| PointerDec Ident Type
	| ArrayDec Ident RExpr RExpr Type
	deriving (Eq, Show)

--dichiarazione delle funzioni e procedure
data FunDecl = Fun Ident [Parameter] Type [Statement]
	| Proc Ident [Parameter] [Statement]
	deriving (Eq, Show)

--dichiarazione dei parametri di una funzione/procedura
data Parameter = Param Ident Type
	deriving (Eq, Show)

--Dichiarazione degli assegnamenti (semplici, puntatori, array)
data Assignment = AssSimple Ident AssignmentOperator RExpr
		| PointerAssSimple Ident AssignmentOperator RExpr
		| ArrayAssSimple Ident RExpr AssignmentOperator RExpr
	deriving(Eq, Show)

--Dichiarazione degli operatori di assegnamento
data AssignmentOperator = AssEqual
	deriving (Eq,Show)

--dichiarazione if/ifElse
data Condition = IfNoElse RExpr Statement
		| IfElse RExpr Statement Statement
		| IfNoElseBreak RExpr
		| IfElseBreak RExpr Statement
		| IfNoElseContinue RExpr
		| IfElseContinue RExpr Statement
	deriving (Eq, Show)

--Dichiarazione dei cicli
data Loop = While RExpr Statement
		| For Assignment RExpr Statement
		| WhileContinue RExpr
		| WhileBreak RExpr
		| ForContinue Assignment RExpr
		| ForBreak Assignment RExpr
	deriving (Eq,Show)

--Valori delle variabili
data RExpr = RInt Int
	| RFloat Float
	| RBool Bool
	| RChar Char
	| RString String
	| ROpBin RExpr BinaryOperator RExpr
	| ROpUn UnaryOperator RExpr
	| Par RExpr
	| RxLx LExpr
	deriving (Eq,Show)

--Dichiarazione degli operatori unari
data UnaryOperator = Not
	deriving (Eq,Show)

--Dichiarazione degli operatori binari
data BinaryOperator = Add
		| Sub
		| Mult
		| Mod
		| Div
		| DivInt
		| And
		| Or
		| LessThan
		| GreaterThan
		| LessEq
		| GreaterEq
		| EqComp
		| Neq
	deriving (Eq,Show)

--Tipi semplici
data Type = SInt
	| SChar
	| SFloat
	| SBool
	| SString
	| SFunction
	| SProcedure
	--Tipi costanti
	| CInt
	| CChar
	| CFloat
	| CBool
	| CString
	--tipi accettati dalle funzioni
	| FInt
	| FChar
	| FFloat
	| FBool
	| FString
	deriving (Eq, Show)








