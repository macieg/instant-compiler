module LlvmCompiler where

import Data.List
import Data.List.Split
import Data.Set as S
import Control.Monad.State
import AbsInstant

type St = State (Int, Set String)  --local vars indexes

initialState = (0, empty)

cmplExpGen :: Exp -> Exp -> (Int -> String -> String -> String) -> St (String, String)
cmplExpGen e1 e2 op = do
	(reg1, code1) <- cmplExp e1
	(reg2, code2) <- cmplExp e2
	(globReg, s) <- get
	let newGlobReg = globReg +  1
	put (newGlobReg, s)
	return ("%" ++ (show newGlobReg), code1 ++ code2 ++ (op newGlobReg reg1 reg2))

cmplExp :: Exp -> St (String, String)
cmplExp (ExpAdd exp1 exp2) = cmplExpGen exp1 exp2 add
cmplExp (ExpSub exp1 exp2) = cmplExpGen exp1 exp2 sub
cmplExp (ExpMul exp1 exp2) = cmplExpGen exp1 exp2 mul
cmplExp (ExpDiv exp1 exp2) = cmplExpGen exp1 exp2 LlvmCompiler.div

cmplExp (ExpLit lit) = return $ (show lit, "")

cmplExp (ExpVar (Ident var)) = do
	(reg, s) <- get
	case member var s of
		True -> do
			let newReg = reg + 1
			put (newReg, s)
			return ("%" ++ (show newReg), load var newReg)
		False -> error $ "no such variable " ++ var

cmplStmt :: Stmt -> St String
cmplStmt (SAss (Ident var) exp) = do
	(reg, code) <- cmplExp exp
	(r, s) <- get
	case member var s of
		True -> return $ code ++ (store var reg)
		False -> do
			put (r, S.insert var s)
			return $ code ++ (alloca var) ++ (store var reg)

cmplStmt (SExp exp) = do
	(reg, code) <- cmplExp exp
	(r, s) <- get
	put (r+1, s)
	return $ code ++ (printf reg)

cmplStmts :: [Stmt] -> St String
cmplStmts [] = return ""
cmplStmts (s:ss) = do
	code1 <- cmplStmt s
	code2 <- cmplStmts ss
	return $ code1 ++ code2

compileStmts :: [Stmt] -> String
compileStmts stmts = let (code, _) = runState (cmplStmts stmts) initialState in code

compileProgram :: Program -> String
compileProgram (Prog stmts) = header ++ (compileStmts stmts) ++ footer

alloca var = "%" ++ var ++ " = alloca i32\n"
store var val = "store i32 " ++ val ++ ", i32* %" ++ var ++ "\n"
load var ind = "%" ++ (show ind) ++ " = load i32* %" ++ var ++ "\n"
printf var = "call i32 (i8*, ...)* @printf(i8* getelementptr inbounds ([4 x i8]* @.str, i32 0, i32 0), i32 " ++ var ++ ")\n"
add newind arg1 arg2 = "%" ++ (show newind) ++ " = add nsw i32 " ++ arg1 ++ ", " ++ arg2 ++ "\n"
sub newind arg1 arg2 = "%" ++ (show newind) ++ " = sub nsw i32 " ++ arg1 ++ ", " ++ arg2 ++ "\n"
mul newind arg1 arg2 = "%" ++ (show newind) ++ " = mul nsw i32 " ++ arg1 ++ ", " ++ arg2 ++ "\n"
div newind arg1 arg2 = "%" ++ (show newind) ++ " = sdiv i32 " ++ arg1 ++ ", " ++ arg2 ++ "\n"
header = intercalate "\n" ["@.str = private unnamed_addr constant [4 x i8] c\"%d\\0A\\00\"",
	"declare i32 @printf(i8*, ...)",
	"",
	"define i32 @main() {",
	""]
footer = intercalate "\n" ["ret i32 0", "}"]


--testy
program1 = Prog [SExp (ExpAdd (ExpMul (ExpLit 2) (ExpLit 3)) (ExpSub (ExpLit 4) (ExpDiv (ExpLit 6) (ExpLit 2))))]
program2 = Prog [SExp (ExpMul (ExpLit 2) (ExpLit 3))]
program3 = Prog [SExp (ExpLit 2)]
program4 = Prog [SExp (ExpMul (ExpMul (ExpMul (ExpMul (ExpLit 1) (ExpLit 2)) (ExpLit 3)) (ExpLit 4)) (ExpLit 5))]
program5 = Prog [SExp (ExpAdd (ExpLit 1) (ExpSub (ExpMul (ExpLit 2) (ExpLit 3)) (ExpDiv (ExpLit 4) (ExpLit 5))))]
program6 = Prog [SAss (Ident "a") (ExpLit 1),SAss (Ident "b") (ExpLit 2),SExp (ExpAdd (ExpVar (Ident "b")) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpLit 1) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpLit 1) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpVar (Ident "b")) (ExpAdd (ExpLit 1) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpLit 1) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpLit 1) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpLit 1) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpVar (Ident "a")) (ExpAdd (ExpLit 1) (ExpAdd (ExpVar (Ident "a")) (ExpVar (Ident "b"))))))))))))))))))))))))))))))))))))))))]
program7 = Prog [SExp (ExpAdd (ExpLit 1) (ExpAdd (ExpSub (ExpLit 1) (ExpLit 1)) (ExpAdd (ExpSub (ExpLit 1) (ExpLit 1)) (ExpSub (ExpLit 1) (ExpLit 1)))))]
program8 = Prog [SExp (ExpAdd (ExpDiv (ExpLit 1) (ExpLit 2)) (ExpAdd (ExpSub (ExpMul (ExpLit 5) (ExpLit 1)) (ExpLit 1)) (ExpAdd (ExpDiv (ExpLit 4) (ExpLit 1)) (ExpAdd (ExpSub (ExpLit 4) (ExpMul (ExpLit 4) (ExpLit 1))) (ExpAdd (ExpSub (ExpLit 8) (ExpMul (ExpLit 3) (ExpLit 1))) (ExpAdd (ExpSub (ExpLit 5) (ExpDiv (ExpMul (ExpLit 3) (ExpLit 1)) (ExpLit 5))) (ExpLit 5)))))))]

