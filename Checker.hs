module Checker where

import AbsPascal
import Lexer
import Parser
import qualified Data.Map as Map

--Dichiarazione dell'ambiente
type Env = Map.Map Ident Type

--Dichiarazione di Stato
data State = Correct | Errors [String] | RightType Type
  deriving(Eq,Show)

--Creazione ambiente di default con le funzioni (Read, Write) (Int, Real, Char, String)
defEnv =  foldl1 (Map.union) [ Map.singleton "readInt" FInt,Map.singleton "readReal" FFloat,Map.singleton "readChar" FChar,Map.singleton "readString" FString,Map.singleton "writeInt" SProcedure,Map.singleton "writeReal" SProcedure,Map.singleton "writeChar" SProcedure,Map.singleton "writeString" SProcedure]

--Punto di partenza del TypeChecker
execution :: Program -> [String]
execution (Prog _ statements) = case createEnvironment statements of
				(env, Correct) -> case checkStatementsList statements (env, Correct) of
							(_, Errors listErr) -> listErr
							(_, _)  -> []
				(_, Errors listErr) -> listErr

--Creazione dell'ambiente iniziale formato dalla coppia (ambiente,Stato)
createEnvironment statements = (defEnv, Correct)

--Controlli sulla lista degli statements
checkStatementsList [] envState = envState
checkStatementsList ((StatementDec   x):xs) envState@(env, st) 	= checkStatementsList xs (saveErrors envState (dichiarazione x (env, Correct)))
checkStatementsList ((StatementRexpr x):xs) envState@(env, st)  = checkStatementsList xs (saveErrors envState (rexpression   x (env, Correct)))
checkStatementsList ((StatementAss   x):xs) envState@(env, st)  = checkStatementsList xs (saveErrors envState (assegnamento  x (env, Correct)))
checkStatementsList ((StatementCond  x):xs) envState@(env, st) 	= checkStatementsList xs (saveErrors envState (condizioni    x (env, Correct)))
checkStatementsList ((StatementLoop  x):xs) envState@(env, st) 	= checkStatementsList xs (saveErrorsBlock envState (ciclo	 x (env, Correct)))
checkStatementsList ((StatementFunc  x):xs) envState@(env, st) 	= checkStatementsList xs (saveErrors envState (Funzione 	 x (env, Correct)))

--Controlli su blocchi singoli
checkBlocks (RBool _) stmts envState@(env, st)		 = checkStatementsList stmts envState
checkBlocks (RInt _) stmts envState@(env, st) 		 = checkStatementsList stmts envState
checkBlocks (RFloat _) stmts envState@(env, st) 	 = checkStatementsList stmts envState
checkBlocks (RChar _) stmts envState@(env, st) 	 	 = (env,(Errors ["Il tipo (Char) della condizione non è valido"]))
checkBlocks (RString _) stmts envState@(env, st) 	 = (env,(Errors ["Il tipo (String) della condizione non è valido"]))
checkBlocks (RxLx (LId a)) stmts envState@(env, st)  = case (Map.lookup a env) of
						(Just SBool)  -> checkStatementsList stmts envState
						(Just SFloat) -> checkStatementsList stmts envState
						(Just SInt)   -> checkStatementsList stmts envState
						Nothing 	  -> (env, (Errors ["Il tipo della variabile non è compatibile"]))
checkBlocks ex@(ROpBin a1 op a2) stmts envState@(env, st) = case (getRexprType ex envState) of
						(RightType SBool)  -> checkStatementsList stmts envState
						(RightType SFloat) -> checkStatementsList stmts envState
						(RightType SInt)   -> checkStatementsList stmts envState
						_ -> (env, (Errors ["La condizione non è corretta"]))					
checkBlocks _ stmts envState = checkStatementsList stmts envState

--Controlli su blocchi singoli
checkCondition (RBool _) envState@(env, st)   = envState
checkCondition (RInt _) envState@(env, st)    = envState
checkCondition (RFloat _) envState@(env, st)  = envState
checkCondition (RChar _) envState@(env, st)   = (env,(Errors ["Il tipo (Char) della condizione non è valido"]))
checkCondition (RString _) envState@(env, st) = (env,(Errors ["Il tipo (String) della condizione non è valido"]))
checkCondition (RxLx (LId a)) envState@(env, st) = case (Map.lookup a env) of
						(Just SBool)  -> envState
						(Just SFloat) -> envState
						(Just SInt)   -> envState
						Nothing 	  -> (env, (Errors ["Il tipo della variabile non è compatibile"]))
checkCondition ex@(ROpBin a1 op a2) envState@(env, st) = case (getRexprType ex envState) of
						(RightType SBool)  -> envState
						(RightType SFloat) -> envState
						(RightType SInt)   -> envState
						_ -> (env, (Errors ["La condizione non è corretta"]))					
checkCondition _ envState = envState

				

--Controlli sulle funzioni con blocco statements
funzione (Fun ident parametri tipo stmts) envState = let envState2=(addFunction ident (getFunctionType tipo) envState) in
								case (snd envState2) of 
									Correct -> (saveErrorsBlock envState2 (checkStatementsList stmts (getParam parametri envState2)))
									_ -> envState2

--Controlli sulle procedure con blocco statements
funzione (Proc ident parametri stmts) envState = let envState2=(addFunction ident SProcedure envState) in
							case (snd envState2) of 
								Correct ->  (saveErrorsBlock envState2 (checkStatementsList stmts (getParam parametri envState2)))
								_ -> envState2						

--Controlli del tipo di funzione
getFunctionType tipo = case tipo of
		SInt 	-> FInt
		SChar 	-> FChar
		SFloat 	-> FFloat
		SBool 	-> FBool
		SString -> FString
	
--Estrazione di un parametro dalla lista dei parametri
getParam [] envState = envState
getParam (x:xs) envState = getParam xs (addParam x envState)

--Inserimento del parametro estratto nell'ambiente della funzione/procedura
addParam (Param ident tipo) envState@(env, st) = case (Map.lookup ident env) of
							    (Just SProcedure) -> (env, (Errors ["Identificatore \""++ident++"\" già presente"]))
							    (Just FInt)       -> (env, (Errors ["Identificatore \""++ident++"\" già presente"]))
							    (Just FChar)   	  -> (env, (Errors ["Identificatore \""++ident++"\" già presente"]))
							    (Just FFloat)  	  -> (env, (Errors ["Identificatore \""++ident++"\" già presente"]))
							    (Just FBool)      -> (env, (Errors ["Identificatore \""++ident++"\" già presente"]))
							    (Just FString)    -> (env, (Errors ["Identificatore \""++ident++"\" già presente"]))
							    _ -> ((Map.union (Map.singleton ident tipo) env), st)

--Inserimento della procedura nell'ambiente
addFunction ident SProcedure envState@(env, st) = if (Map.notMember ident env) 
							then ((Map.union (Map.singleton ident SProcedure) env), st)
							else (env, (Errors ["Identificatore \""++ident++"\" già presente"]))
							
--Inserimento della funzione nell'ambiente
addFunction ident tipof envState@(env, st) = if (Map.notMember ident env) 
							then ((Map.union (Map.singleton ident tipof) env), st)
							else (env, (Errors ["Identificatore \""++ident++"\" già presente"]))

--Controlli if/if-else
condizioni (IfNoElse x (StatementBlock stmts)) envState = checkBlocks x stmts envState
condizioni (IfNoElse x stmts) envState = checkBlocks x [stmts] envState
condizioni (IfElse x (StatementBlock stmts1) (StatementBlock stmts2)) envState = saveErrors (checkBlocks x stmts1 envState) (checkStatementsList stmts2 envState)

--Controlli if/if-else + continue/break
condizioni (IfNoElseBreak x) envState 		 = checkCondition x envState
condizioni (IfElseBreak x stmts) envState 	 = checkCondition x  envState
condizioni (IfNoElseContinue x) envState 	 =  checkCondition x envState
condizioni (IfElseContinue x stmts) envState = checkCondition x envState

--Controlli sui cicli
ciclo (While x (StatementBlock stmts)) envState   = checkBlocks x stmts envState
ciclo (While x stmts) envState 					  = checkBlocks x [stmts] envState
ciclo (For ass x (StatementBlock stmts)) envState = checkBlocks x stmts envState
ciclo (For ass x stmts) envState 				  = checkBlocks x [stmts] envState

--Controlli sui cicli + continue/break
ciclo (WhileBreak x) envState 		= checkCondition x envState
ciclo (WhileContinue x) envState 	= checkCondition x envState
ciclo (ForBreak ass x) envState 	= checkCondition x envState
ciclo (ForContinue ass x) envState  = checkCondition x envState

--Controlli sugli assegnamenti semplici
assegnamento (AssSimple a _ b) envState@(env, st) = if (Map.member a env)
						then if (checkTypeAssignment (getVariableType a envState) (getType b envState))
							then envState
							else case (getVariableType a envState) of
								RightType SProcedure -> (env, Errors ["Non si possono fare assegnamenti a una procedura"])
								RightType FInt 	 	 -> (env, Errors ["Non si possono fare assegnamenti a una funzione"])
								RightType FChar 	 -> (env, Errors ["Non si possono fare assegnamenti a una funzione"])
								RightType FFloat 	 -> (env, Errors ["Non si possono fare assegnamenti a una funzione"])
								RightType FBool 	 -> (env, Errors ["Non si possono fare assegnamenti a una funzione"])
								RightType FString 	 -> (env, Errors ["Non si possono fare assegnamenti a una funzione"])
								RightType CInt 		 -> (env, Errors ["Non si possono fare assegnamenti ad una costante"])
								RightType CChar 	 -> (env, Errors ["Non si possono fare assegnamenti ad una costante"])
								RightType CFloat 	 -> (env, Errors ["Non si possono fare assegnamenti ad una costante"])
								RightType CBool 	 -> (env, Errors ["Non si possono fare assegnamenti ad una costante"])
								RightType CString 	 -> (env, Errors ["Non si possono fare assegnamenti ad una costante"])
								_ -> (env, (mergeErrors st (Errors ["La R-expression della variabile \""++a++"\" non è di tipo compatibile"])))
						else (env,Errors["Variabile \""++a++"\" non dichiarata"])

--Controlli sugli assegnamenti dei puntatori
assegnamento (PointerAssSimple a _ b) envState@(env, st) =  if (Map.member a env)
						then if (checkTypePointerAssignment (getVariableType a envState) (getType b envState)) 
						 	then envState
							else (env, (mergeErrors st (Errors ["La R-expression assegnata al puntatore non è di tipo compatibile"])))
						else (env,Errors["Puntatore \""++a++"\" non dichiarato"])

--Controlli sugli assegnamenti degli array
assegnamento (ArrayAssSimple a b _ c) envState@(env, st) = 
						 if (checkTypeArrayAssignment (getVariableType a envState) (getType c envState)) 
						 	then envState
							else (env, (mergeErrors st (Errors ["La R-expression assegnata all'elemento dell'array non è di tipo compatibile"])))


--Controlli sulle R-expressions, ritorna la coppia (ambiente, Stato)
rexpression (ROpBin a1 op a2) envState@(env, st) = if (checkType (getType a1 envState) (getType a2 envState))
						then (env, (getGeneralType (getType a1 envState) (getType a2 envState) st))
						else (env, (mergeErrors st (Errors ["Tipi incompatibili presenti nella R-expression! "++(printType (getType a1 envState))++(printOp op)++(printType (getType a2 envState))]))) 
rexpression a envState@(env, st) = (env, Errors ["R-expression inaspettata."])

--Controlli sulle R-expressions, ritorna lo Stato	
getRexprType (ROpBin a1 op a2) envState = if (checkType (getType a1 envState) (getType a2 envState))
						then (getGeneralType (getType a1 envState) (getType a2 envState) (snd envState))
						else Errors ["Tipi incompatibili presenti nella R-expression!"]

--Funzione che ritorna il tipo più generico tra due tipi (a e b)
getGeneralType a b env = case (a,b) of
			 (RightType SInt, RightType SFloat)   -> RightType SFloat
			 (RightType SFloat, RightType SInt)   -> RightType SFloat
			 (RightType SInt, RightType SInt) 	  -> RightType SInt
			 (RightType SFloat, RightType SFloat) -> RightType SFloat
			 (_,_) -> mergeErrors env (Errors ["Tipi incompatibili presenti nella R-expression!"])

--Controllo le dichiarazioni delle variabili
dichiarazione (VarDec ident t1 t2) envState@(env, st)= case (Map.lookup ident env) of
							(Just SProcedure) -> (env, (Errors ["Identificatore \""++ident++"\" già presente"]))
							(Just FInt) 	  -> (env, (Errors ["Identificatore \""++ident++"\" già presente"]))
							(Just FChar) 	  -> (env, (Errors ["Identificatore \""++ident++"\" già presente"]))
							(Just FFloat) 	  -> (env, (Errors ["Identificatore \""++ident++"\" già presente"]))
							(Just FBool) 	  -> (env, (Errors ["Identificatore \""++ident++"\" già presente"]))
							(Just FString) 	  -> (env, (Errors ["Identificatore \""++ident++"\" già presente"]))
							(Just CInt) 	  -> (env, (Errors ["Identificatore \""++ident++"\" già presente"]))
							(Just CChar) 	  -> (env, (Errors ["Identificatore \""++ident++"\" già presente"]))
							(Just CFloat) 	  -> (env, (Errors ["Identificatore \""++ident++"\" già presente"]))
							(Just CBool) 	  -> (env, (Errors ["Identificatore \""++ident++"\" già presente"]))
							(Just CString) 	  -> (env, (Errors ["Identificatore \""++ident++"\" già presente"]))
							_ -> if checkType (RightType t1) (getType t2 envState)
									then ((Map.union (Map.singleton ident t1) env), (RightType t1))
									else ((Map.union (Map.singleton ident t1) env), (Errors ["La dichiarazione è errata, var \""++ident++"\" : "++(printType (RightType t1))++" = "++(printType (getType t2 envState))]))

--Controllo le dichiarazioni dell'array
dichiarazione (ArrayDec ident _ _ t) envState@(env, st) = ((Map.union (Map.singleton ident t) env), (RightType t))

--Controllo le dichiarazioni delle costanti 
dichiarazione (ConstDec ident t) envState@(env, st)     = (env, Correct)

--Controllo le dichiarazione dei puntatori
dichiarazione (PointerDec ident t) envState@(env, st)   = ((Map.union (Map.singleton ident t) env), (RightType t))

--Unione degli errori
mergeErrors (Errors x1) (Errors x2) = Errors (x1 ++ x2)
mergeErrors _ (Errors x) 		    = Errors x
mergeErrors (Errors x) _ 			= Errors x
mergeErrors _ (RightType a) 		= RightType a
mergeErrors _ Correct 				= error "_ Corretto"

--Unione dei due ambienti (env1 e env2) e degli errori correlati
saveErrors (env1, a) (env2, Correct) = ((Map.union env2 env1), a)
saveErrors (env1, st1) (env2, st2)   = ((Map.union env2 env1), (mergeErrors st1 st2))

--ritorna il primo ambiente (env1) e procede con l'unione degli errori (st1 e st2)
saveErrorsBlock (env1, a) (env2, Correct) = (env1, a)
saveErrorsBlock (env1, st1) (env2, st2)   = (env1, (mergeErrors st1 st2))

--Controllo della compatibilità dei tipi (t1 e t2) di una R-Expression
checkType t1 t2 = ( t1 == t2 ) || case (t1, t2) of
			   (RightType SInt,RightType SFloat) -> True
			   (RightType SFloat,RightType SInt) -> True
			   (_,_) -> False	

--Controllo della correttezza di un assegnamento
checkTypeAssignment t1 t2 = ( t1 == t2 ) || case (t1, t2) of
			   (RightType SFloat,RightType SInt) -> True
			   (_,_) -> False 

--Controllo della correttezza di un assegnamento (array)
checkTypeArrayAssignment t1 t2 = (t1 == t2) || case (t1, t2) of
				(RightType SInt,RightType SInt) 	  -> True
				(RightType SFloat,RightType SFloat)   -> True
				(RightType SString,RightType SString) -> True
				(RightType SChar,RightType SChar)     -> True
				(RightType SBool,RightType SBool)     -> True
				(_,_) -> False 

--Controllo della correttezza di un assegnamento (puntatori)
checkTypePointerAssignment t1 t2 = if ( t1 == t2 ) then True
						else False 

--Controllo che l'identificatore sia presente nell'ambiente
getVariableType a envState = case (Map.lookup a (fst envState)) of
						  (Just c) -> (mergeErrors (snd envState) (RightType c))
						  Nothing -> Errors ["Variabile o funzione/procedura \""++a++"\" non dichiarata -- Usata prima della dichiarazione?"]

--Funzione che ritorna il tipo della R-expression: caso variabile
getType (RInt _ ) _ 	= RightType SInt
getType (RFloat _ ) _ 	= RightType SFloat
getType (RBool _ ) _ 	= RightType SBool
getType (RChar _ ) _ 	= RightType SChar
getType (RString _ ) _  = RightType SString
getType (RxLx (LId a)) envState@(env, st) = case (Map.lookup a env) of 
						    (Just b) -> RightType b
						    Nothing  -> Errors ["Tipi incompatibili presenti nella R-expression!"]
getType (ROpUn Not x) envState@(env, st)			  = getType x envState
getType x@(ROpBin a1 op a2 ) envState@(env, st) 	  = (snd (rexpression x envState))
getType (Par x@(ROpBin a1 op a2 )) envState@(env, st) = (snd (rexpression x envState))


--Funzione che ritorna il tipo della R-expression: caso costante
getTypeConst (RInt _ ) _    = RightType CInt
getTypeConst (RFloat _ ) _  = RightType CFloat
getTypeConst (RBool _ ) _   = RightType CBool
getTypeConst (RChar _ ) _   = RightType CChar
getTypeConst (RString _ ) _ = RightType CString

--Funzionche stampa la stringa corrispondente al tipo (t)
printType t = case t of 
	RightType SInt		 -> "integer"
	RightType SBool		 -> "boolean"
	RightType SFloat	 -> "real"
	RightType SChar		 -> "char"
	RightType SString	 -> "string"
	RightType SFunction	 -> "function"
	RightType SProcedure -> "procedure"
	RightType CInt		 -> "integer"
	RightType CBool		 -> "boolean"
	RightType CFloat	 -> "real"
	RightType CChar		 -> "char"
	RightType CString	 -> "string"
	_ 					 -> "Tipo non riconosciuto"

--In caso di errori di compatibilità, stampa gli operatori
printOp op = case op of 
	Add 		-> "+"
	Sub			-> "-"
	Mult		-> "*"
	Mod			-> "mod"
	Div			-> "/"
	DivInt		-> "div"
	And			-> "and"
	Or			-> "or"
	LessThan	-> "<"
	GreaterThan	-> ">"
	LessEq		-> "<="
	GreaterEq	-> ">="
	EqComp		-> "="
	Neq			-> "<>"
	