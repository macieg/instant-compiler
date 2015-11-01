module JvmCompiler where

import Data.List
import Data.List.Split
import Data.Map as M
import Control.Monad.State
import AbsInstant

type LocVars = M.Map String Int
type St = State LocVars  --local vars indexes

varinc = "0"
initialState = M.singleton varinc 1

cmplExpGen :: Exp -> Exp -> String -> St (Int, String) -- St(Depth, Code)
cmplExpGen e1 e2 op = do
	(d1, c1) <- cmplExp e1
	(d2, c2) <- cmplExp e2
	if d1 >= d2 then
		return (1 + max d1 d2, c1 ++ c2 ++ op)
	else
		return (1 + max d1 d2, c2 ++ c1 ++ op)

cmplExp :: Exp -> St (Int, String) -- St(Depth, Code)
cmplExp (ExpAdd exp1 exp2) = cmplExpGen exp1 exp2 iadd
cmplExp (ExpSub exp1 exp2) = cmplExpGen exp1 exp2 isub
cmplExp (ExpMul exp1 exp2) = cmplExpGen exp1 exp2 imul
cmplExp (ExpDiv exp1 exp2) = cmplExpGen exp1 exp2 idiv

cmplExp (ExpLit lit) = return (1, expLitStr lit)

cmplExp (ExpVar (Ident var)) = do
	varm <- get
	case M.lookup var varm of
		Just loc -> return (1, expVarStr loc)
		Nothing -> error $ "no such variable " ++ var

cmplStmt :: Stmt -> St String
cmplStmt (SAss (Ident var) exp) = do
	(_, code) <- cmplExp exp
	varm <- get
	case M.lookup varinc varm of
		Just ind ->
			case M.lookup var varm of
				Just loc -> return $ code ++ (stmtAss loc)
				Nothing -> do
					put (M.insert var ind (M.insert varinc (ind + 1) varm))
					return $ code ++ (stmtAss ind)
		Nothing -> error "unexpected error in cmplStmt"

cmplStmt (SExp exp) = do
	(_, code) <- cmplExp exp
	return $ code ++ getPrintStream ++ "\n" ++ swap ++ printInt ++ "\n"

cmplStmts :: [Stmt] -> St String
cmplStmts [] = return ""
cmplStmts (s:ss) = do
	code1 <- cmplStmt s
	code2 <- cmplStmts ss
	return $ code1 ++ code2

compileStmts :: [Stmt] -> String
compileStmts stmts = do
	let (code, varm) = runState (cmplStmts stmts) initialState
	(limitStack $ calcStackSize code) ++ (limitLocals $ M.size varm) ++ code

compileProgram :: Program -> String -> String
compileProgram (Prog stmts) className =
	".class  public " ++ className ++ "\n" ++ header ++ (compileStmts stmts) ++ footer

calcStackSize :: String -> Int
calcStackSize code = maximum (
	Prelude.foldl (\acc x -> ((head acc) + stackChangeNo x) : acc) [0] (splitOn "\n" code))

stackChangeNo :: String -> Int
stackChangeNo s =  if or (Prelude.foldr (\x acc -> (isPrefixOf x s) : acc) [] [iconst, iload, ldc, getPrintStream])
	then 1 else if isPrefixOf swap s then 0 else -1



iadd = "iadd\n"
idiv = "idiv\n"
isub = "isub\n"
imul = "imul\n"
iconst = "iconst_"
ldc = "ldc "
iload = "iload "
istore = "istore "
swap = "swap\n"
printInt = "invokevirtual java/io/PrintStream/println(I)V"
getPrintStream = "getstatic java/lang/System/out Ljava/io/PrintStream;"
header = intercalate "\n" [".super  java/lang/Object",
	"",
	"; standard initializer",
	".method public <init>()V",
	"aload_0",
	"invokespecial java/lang/Object/<init>()V",
	"return",
	".end method",
	"",
	".method public static main([Ljava/lang/String;)V",
	""]
footer = intercalate "\n" ["return", ".end method"]

limitStack :: Int -> String
limitStack i = ".limit stack " ++ show i ++ "\n"

limitLocals :: Int -> String
limitLocals i = if i > 1 then ".limit locals " ++ show i ++ "\n" else ""

expLitStr :: Integer -> String
expLitStr e =
	let x = if e < 6 then iconst else ldc
	in x ++ show e ++ "\n"

expVarStr :: Int -> String
expVarStr e = iload ++ show e ++ "\n"

stmtAss :: Int -> String
stmtAss e = istore ++ show e ++ "\n"

--testy
program1 = Prog [SExp (ExpAdd (ExpMul (ExpLit 2) (ExpLit 3)) (ExpSub (ExpLit 4) (ExpDiv (ExpLit 6) (ExpLit 2))))]
program2 = Prog [SExp (ExpMul (ExpLit 2) (ExpLit 3))]
program3 = Prog [SExp (ExpLit 2)]
program4 = Prog [SExp (ExpMul (ExpMul (ExpMul (ExpMul (ExpLit 1) (ExpLit 2)) (ExpLit 3)) (ExpLit 4)) (ExpLit 5))]
program5 = Prog [SExp (ExpAdd (ExpLit 1) (ExpSub (ExpMul (ExpLit 2) (ExpLit 3)) (ExpDiv (ExpLit 4) (ExpLit 5))))]
program6 = Prog [SAss (Ident "a") (ExpLit 1),SAss (Ident "b") (ExpLit 2),SExp (ExpAdd (ExpVar (Ident "b")) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpLit 1) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpLit 1) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpVar (Ident "b")) (ExpAdd (ExpLit 1) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpLit 1) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpLit 1) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpLit 1) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpLit 1) (ExpAdd (ExpVar (Ident "a")) (ExpVar (Ident "b"))))))))))))))))))))))))))))))))))))))))]
program7 = Prog [SExp (ExpAdd (ExpLit 1) (ExpAdd (ExpSub (ExpLit 1) (ExpLit 1)) (ExpAdd (ExpSub (ExpLit 1) (ExpLit 1)) (ExpSub (ExpLit 1) (ExpLit 1)))))]
program8 = Prog [SExp (ExpAdd (ExpDiv (ExpLit 1) (ExpLit 2)) (ExpAdd (ExpSub (ExpMul (ExpLit 5) (ExpLit 1)) (ExpLit 1)) (ExpAdd (ExpDiv (ExpLit 4) (ExpLit 1)) (ExpAdd (ExpSub (ExpLit 4) (ExpMul (ExpLit 4) (ExpLit 1))) (ExpAdd (ExpSub (ExpLit 8) (ExpMul (ExpLit 3) (ExpLit 1))) (ExpAdd (ExpSub (ExpLit 5) (ExpDiv (ExpMul (ExpLit 3) (ExpLit 1)) (ExpLit 5))) (ExpLit 5)))))))]

