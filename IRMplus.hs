
{-
Describes the intermediate representation tree for M+,
deriving Generic for pretty printing, and functions for
generation of the IR from the abstract syntax tree
(using error monad from BNF Converter during checking)
-}

{-# LANGUAGE DeriveGeneric #-}

module IRMplus where

import SymTableMplus
import SymTypesMplus
import ASTMplus
import GenericPretty
import ErrM

{- Intermediate representation tree datatype -}

data I_Prog  = IPROG ([I_Fnbody],Int,[(Int,[I_Expr])],[I_Stmt])
            deriving (Eq, Generic)

instance Out I_Prog

data I_Fnbody = IFUN (String,[I_Fnbody],Int,Int,[(Int,[I_Expr])],[I_Stmt])
            deriving (Eq, Generic)

instance Out I_Fnbody

data I_Stmt = IASS (Int,Int,[I_Expr],I_Expr)
            | IBLOCK ([I_Fnbody],Int,[(Int,[I_Expr])],[I_Stmt])
            | IRETURN I_Expr
            | IWHILE (I_Expr,I_Stmt)
            | ICOND (I_Expr,I_Stmt,I_Stmt)
            | IREAD_F (Int,Int,[I_Expr])
            | IREAD_I (Int,Int,[I_Expr])
            | IREAD_B (Int,Int,[I_Expr])
            | IPRINT_F I_Expr
            | IPRINT_I I_Expr
            | IPRINT_B I_Expr
            deriving (Eq, Generic)

instance Out I_Stmt
            
data I_Expr =  IID (Int,Int,[I_Expr],M_Type)
            | IAPP (I_Op,[I_Expr])
            | IINT Integer
            | IREAL Double
            | IBOOL Bool
            | ISIZE (Int,Int,Int)
            deriving (Eq, Generic)
      
instance Out I_Expr

data I_Op = ICALL (String,Int)
           | IADD 
           | ISUB 
           | INEG
           | IMUL 
           | IDIV 
           | IEQ
           | ILT  
           | IGT 
           | ILE   
           | IGE  
           | IAND 
           | IOR 
           | INOT 
           | IFLOAT 
           | ICEIL 
           | IFLOOR 
           deriving (Eq, Generic)

instance Out I_Op
           
{- Functions for generating intermediate representation from AST -}

irProg :: M_Prog -> Err I_Prog
irProg (M_Prog (decls,stmts)) = case (irDecls empty decls) of
    Ok (table,vars,args,dim) -> case irStmts table stmts of
        Ok stmts2 -> case irFns table decls of
            Ok body -> Ok (IPROG (body,vars,dim,stmts2))
            Bad b -> Bad b
        Bad b -> Bad b
    Bad b -> Bad b

irDecls :: ST -> [M_Decl] -> Err (ST,Int,Int,[(Int,[I_Expr])])
irDecls table [] = Ok (table,vars,args,[]) where
    (_,_,vars,args,_) = getScope table
irDecls table (decl:decls) = case decl of
    (M_Var (id,expr,t)) -> case insertSymbol table (VARIABLE (id,t,length expr)) of
        Ok table2 -> case irDecls table2 decls of
            Ok (x,vars,args,z) -> case irExprs table expr of
                Ok a -> Ok (x,vars,args,z)
            Bad b -> Bad b
        Bad b -> Bad ("Exception: Invalid declaration in " ++ show (M_Var(id,expr,t)))
    (M_Fun (id,args,rt,fdecls,fstmts)) ->
        case insertSymbol table (FUNCTION (id,(map (\(_,x,table) -> (table,x)) args),rt)) of
            Ok table2 -> irDecls (fst (removeScope table2)) decls
            otherwise -> Bad ("Exception: Invalid declaration in " 
                    ++ show (M_Fun (id,args,rt,fdecls,fstmts)))

irArgs :: ST -> [(String,Int,M_Type)] -> Err ST
irArgs table [] = Ok table
irArgs table ((id,x,t):rest) = 
    case insertSymbol table (ARGUMENT (id,t,x)) of
        Ok table2 -> irArgs table2 rest
        Bad b -> Bad b

irBody :: I_Fnbody -> [I_Stmt]
irBody (IFUN (_,body,_,_,_,stmts)) = stmts ++ (concatMap irBody body)

irStmts :: ST -> [M_Stmt] -> Err [I_Stmt]
irStmts _ [] = Ok []
irStmts table (stmt:stmts) = case irErrStmt table stmt of
    Ok any -> case irStmt table stmt of
        Ok stmt2 -> case irStmts table stmts of
            Ok stmts2 -> Ok(stmt2:stmts2)
            Bad b -> Bad b
        Bad b -> Bad b
    Bad b -> Bad b
    
irErrStmts :: ST -> [M_Stmt] -> Err Bool
irErrStmts table stmts
    | irAcceptList ss = Ok True
    | otherwise = Bad (irGetEx ss)
        where ss = (map (\s -> irErrStmt table s) stmts)
        
irGetEx ((Bad b):as) = b
irGetEx ((Ok _):as) = irGetEx as

irStmt :: ST -> M_Stmt -> Err I_Stmt
irStmt table (M_Ass (id,expr,exprs)) = case lookupSymbol table id of
    Ok(I_VARIABLE (x,y,t,z)) -> case irExprs table expr of
        Ok exprs2 -> case irExpr table exprs of
            Ok i -> Ok (IASS ((getScopeN table)-x,y,exprs2,i))
            Bad b -> Bad b
        Bad b -> Bad b
    Bad b -> Bad b
irStmt table (M_While (test,body)) = case irExpr table test of
    Ok cond -> case irStmt table body of
        Ok body2 -> Ok (IWHILE (cond,body2))
        Bad b -> Bad b
    Bad b -> Bad b
irStmt table (M_Cond (test,expr1,expr2)) = case irExpr table test of
    Ok cond -> case irStmt table expr1 of
        Ok body1 -> case irStmt table expr2 of
            Ok body2 -> Ok (ICOND (cond,body1,body2))
            Bad b -> Bad b
        Bad b -> Bad b
    Bad b -> Bad b
irStmt table (M_Read(id,expr)) = case lookupSymbol table id of
    Ok(I_VARIABLE(x,y,t,z)) -> case irExprs table expr of
        Ok body -> case (t,z-length body) of
            (M_Int, 0) -> Ok (IREAD_I ((getScopeN table)-x,y,body))
            (M_Real,0) -> Ok (IREAD_F ((getScopeN table)-x,y,body))
            (M_Bool,0) -> Ok (IREAD_B ((getScopeN table)-x,y,body))
            otherwise -> Bad "Exception: Invalid type"
        Bad b -> Bad b
    Bad b -> Bad b
irStmt table (M_Print expr) = case irExpr table expr of
    Ok cont -> case irErrType table expr of
        Ok(M_Int,0) -> Ok (IPRINT_I cont)
        Ok(M_Real,0) -> Ok (IPRINT_F cont)
        Ok(M_Bool,0) -> Ok (IPRINT_B cont)
        otherwise -> Bad "Exception: Invalid type"
    Bad b -> Bad b
irStmt table (M_Return expr) = case irExpr table expr of
    Ok cont -> Ok (IRETURN cont)
    Bad b -> Bad b
irStmt table (M_Block (decls,stmts)) = case irDecls (newScope L_BLK table) decls of
    Ok (table2,vars,_,as) -> case irFns table2 decls of
        Ok body -> case irStmts table2 stmts of
            Ok block -> Ok (IBLOCK (body,vars,as,block))
            Bad b -> Bad b
        Bad b -> Bad b
    Bad b -> Bad b
        
irAccept :: Err a -> Bool
irAccept (Bad b) = False
irAccept (Ok a) = True

irAcceptList :: [Err a] -> Bool
irAcceptList ((Ok _):as) = irAcceptList as
irAcceptList ((Bad _):as) = False
irAcceptList [] = True
        
irFormBlock :: I_Stmt -> [I_Stmt]
irFormBlock stmt = case stmt of
    (IBLOCK (body,_,_,stmts)) -> 
        stmt:((concatMap irFormBlock (concatMap irBody body))
            ++ (concatMap irFormBlock stmts))
    (ICOND (test,body1,body2)) -> 
        stmt:((irFormBlock body1) ++ (irFormBlock body2))
    (IWHILE (test,body)) -> 
        stmt:(irFormBlock body)
    otherwise -> []
        
irErrStmt :: ST -> M_Stmt -> Err Bool
irErrStmt table (M_Ass (id, exprs, expr)) = case lookupSymbol table id of
    Ok(I_VARIABLE(x,y,t,z)) -> ok where
        b1 = irErrExprs table exprs
        b2 = irErrExpr table expr
        b3 = integers (M_Ass (id,exprs,expr)) (irErrTypes table exprs) 
        b4 = case irErrType table expr of
            Ok v -> if v == (t,z-length exprs)
                then Ok True
                else Bad ("Exception: Invalid type in assignment " 
                        ++ show (M_Ass (id,exprs,expr)))
            Bad b -> Bad b
        ok = if irAcceptList [b1,b2,b3,b4]
            then Ok True
            else Bad (irGetEx [b1,b2,b3,b4])
    Bad b -> Bad ("Exception: Invalid assignment in " 
                ++ show (M_Ass (id,exprs,expr)))
    otherwise -> Bad ("Exception: Invalid type in assignment " ++ id)
irErrStmt table (M_Block (decls,stmts)) = case irDecls (newScope L_BLK table) decls of
    Ok (table2,_,_,_) -> irErrStmts table2 stmts
    Bad b -> Bad b
irErrStmt table (M_Return v) = case irErrType table v of
    Ok(rt1,0) -> case scopeReturn table of
        Ok rt2 -> if rt1 == rt2 then Ok True
            else Bad ("Exception: Invalid return value in " ++ show (M_Return (v)))
        Bad b -> Bad b
    Bad b -> Bad b
irErrStmt table (M_Cond (test,body1,body2)) = 
    if irAcceptList [b1,b2,b3]
        then Ok True
        else Bad (irGetEx [b1,b2,b3]) where
            b1 = irErrExpr table test
            b2 = irErrStmt table body1
            b3 = irErrStmt table body2
irErrStmt table (M_While(test,body)) = 
    if irAcceptList [b1,b2]
        then Ok True
        else Bad (irGetEx [b1,b2]) where
            b1 = irErrExpr table test
            b2 = irErrStmt table body
irErrStmt table (M_Read (id,v)) = case lookupSymbol table id of
    Ok(I_VARIABLE (x,y,t,z)) -> case (t,z-length v) of
        (M_Bool,0) -> Ok True
        (M_Real,0) -> Ok True
        (M_Int,0) -> Ok True
        otherwise -> Bad ("Exception: Invalid type in statement " ++ show (M_Read (id,v)))
    Bad b -> Bad b
irErrStmt table (M_Print v) = case irErrExpr table v of
    Ok any -> case irErrType table v of
        Ok(M_Int,0) -> Ok True
        Ok(M_Real,0) -> Ok True
        Ok(M_Bool,0) -> Ok True
        Bad b -> Bad b
        otherwise -> Bad ("Exception: Invalid type in statement " ++ show (M_Print (v)))
    Bad b -> Bad b
    
integers :: M_Stmt -> Err [(M_Type,Int)] -> Err Bool
integers stmt (Ok (t:ts)) = case t of
    (M_Int,0) -> integers stmt (Ok ts)
    otherwise -> Bad "Exception: Invalid type"
integers _ (Bad b) = Bad b
integers _ (Ok []) = Ok True

irFns :: ST -> [M_Decl] -> Err [I_Fnbody]
irFns _ [] = Ok []
irFns table (decl:decls) = case decl of
    (M_Var _) -> irFns table decls
    (M_Fun decl) -> case irFn table (M_Fun decl) of
        Ok body1 -> case irFns table decls of
            Ok body2 -> Ok (body1:body2)
            Bad b -> Bad b
        Bad b -> Bad b

irFn :: ST -> M_Decl -> Err I_Fnbody
irFn table (M_Fun (id,args,rt,decls,stmts)) = case lookupSymbol table id of
    Ok (I_FUNCTION (off,fid,_,_)) -> case irArgs (newScope (L_FUN rt) table) args of
        Ok s1 -> case irDecls s1 decls of
            Ok (s2,vars,args,dims) -> case irStmts s2 stmts of
                Ok fstmts -> case irFns s2 decls of
                    Ok body -> Ok (IFUN (fid,body,vars,args,dims,fstmts))
                    Bad b -> Bad b
                Bad b -> Bad b
            Bad b -> Bad b
        Bad b -> Bad b
    otherwise -> Bad ("Exception: Invalid function " 
                ++ show (M_Fun (id,args,rt,decls,stmts)))
    
irExprs :: ST -> [M_Expr] -> Err [I_Expr]
irExprs _ [] = Ok []
irExprs table (expr:exprs) = case irErrExpr table expr of
    Ok any -> case irExpr table expr of
        Ok e -> case irExprs table exprs of
            Ok es -> Ok(e:es)
            Bad b -> Bad b
        Bad b -> Bad b
    Bad b -> Bad b
    
irErrExprs :: ST -> [M_Expr] -> Err Bool
irErrExprs table exprs
    | irAcceptList es = Ok True
    | otherwise = Bad (irGetEx es) where
        es = (map (\expr->irErrExpr table expr) exprs)

irExpr :: ST -> M_Expr -> Err I_Expr
irExpr table (M_App (f,expr)) = case irErrTypes table expr of
    Ok ts -> case irErrTypeOp table f ts of
        Ok ft -> case irOp table f of
            Ok op -> case irExprs table expr of
                Ok exprs -> Ok (IAPP (op,exprs))
                Bad b -> Bad b
            Bad b -> Bad b
        Bad b -> Bad "Exception: Invalid type"
    otherwise -> Bad "Exception: Invalid type"
irExpr table (M_Bval v) = Ok (IBOOL v)
irExpr table (M_Rval v) = Ok (IREAL v)
irExpr table (M_Ival v) = Ok (IINT v)
irExpr table (M_Size (id,i)) = case lookupSymbol table id of
    Ok (I_VARIABLE (x,y,t,z)) -> Ok (ISIZE ((getScopeN table)-x,y,z))
    Bad b -> Bad b
    otherwise -> Bad ("Exception: Invalid function application in " ++ show (M_Size (id,i)))
irExpr table (M_Id (name,v)) = case lookupSymbol table name of
    Ok (I_VARIABLE (x,y,t,z)) -> case irExprs table v of
        Ok b -> if z == (length b)
            then Ok (IID ((getScopeN table)-x,y,b,t))
            else Bad ("Exception: Invalid array dimensions in " 
                    ++ show (M_Id (name,v)))
    otherwise -> Bad ("ID is undefined " ++ show (M_Id (name,v)))
irExpr _ _ = Bad ("Exception: Invalid expression")
    
irErrExpr :: ST -> M_Expr -> Err Bool
irErrExpr table (M_App (f,v)) = 
    if (irAcceptList [b1,b2])
        then Ok True
        else Bad (irGetEx [b1,b2]) where
            b1 = irErrOp table f
            b2 = irErrExprs table v
irErrExpr _ (M_Bval _) = Ok True
irErrExpr _ (M_Rval _) = Ok True
irErrExpr _ (M_Ival _) = Ok True
irErrExpr table (M_Size (id,v)) = case (lookupSymbol table id) of
    Ok (I_VARIABLE (_,_,_,z)) -> case (irErrType table (M_Size (id,v))) of
        Ok any -> Ok True
        Bad b -> Bad b
    otherwise -> Bad ("Exception: Invalid function application in " 
                ++ show (M_Size (id,v)))
irErrExpr table (M_Id (id,v)) = 
    if (irAcceptList[b1,b2])
        then Ok True
        else Bad (irGetEx [b1,b2]) where
        b1 = case (lookupSymbol table id) of
            Ok (I_VARIABLE (x,y,t,z)) -> case irErrType table (M_Id (id,v)) of
                Ok any -> Ok True
                Bad b -> Bad b
            otherwise -> Bad ("Exception: Invalid identifier in " 
                            ++ show (M_Id( id,v)))
        b2 = irErrExprs table v
irErrExpr _ _ = Bad ("Exception: Invalid expression")

irOp :: ST -> M_Operation -> Err I_Op
irOp table o = case o of
        M_Add -> Ok IADD
        M_Sub -> Ok ISUB
        M_Mul -> Ok IMUL
        M_Div -> Ok IDIV
        M_Neg -> Ok INEG
        M_LT -> Ok ILT
        M_LE -> Ok ILE
        M_GT -> Ok IGT
        M_GE -> Ok IGE
        M_EQ -> Ok IEQ
        M_Not -> Ok INOT
        M_And -> Ok IAND
        M_Or -> Ok IOR
        M_Float -> Ok IFLOAT
        M_Floor -> Ok IFLOOR
        M_Ceil -> Ok ICEIL
        M_Fn id -> case lookupSymbol table id of
            Ok (I_FUNCTION (x,fid,_,_)) -> Ok (ICALL (fid,(getScopeN table)-x))
            otherwise -> Bad ("Exception: Invalid function application in " 
                        ++ show (M_Fn id))
        
irErrOp :: ST -> M_Operation -> Err Bool
irErrOp table (M_Fn f) = case (lookupSymbol table f) of
    Ok (I_FUNCTION _) -> Ok True
    otherwise -> Bad ("Exception: Invalid operation " ++ show (M_Fn f))
irErrOp _ _ = Ok True
        
irTypeOp :: [(M_Type,Int)] -> Err M_Type
irTypeOp ((t,0):[]) = Ok t
irTypeOp (t:ts) = case (t,irTypeOp ts) of
    ((M_Bool,0),Ok M_Bool) -> Ok M_Bool
    ((M_Real,0),Ok M_Real) -> Ok M_Real
    ((M_Int,0),Ok M_Int) -> Ok M_Int
    otherwise -> Bad "Exception: Invalid type"
    
irErrTypeOp :: ST -> M_Operation -> [(M_Type,Int)] -> Err M_Type
irErrTypeOp table f [] = Bad "Exception: Invalid function arguments"
irErrTypeOp table (M_Fn id) ts = case lookupSymbol table id of
    Ok (I_FUNCTION (_,fid,args,rt)) -> case chkA args ts of
        Ok any -> Ok rt
        Bad b -> Bad b
        where
            chkA [] [] = Ok True
            chkA (arg:args) (a:as) = if arg == a
                then chkA args as
                else Bad "Exception: Invalid function arguments"
            chkA _ _ = Bad "Exception: Invalid function"
    Bad b -> Bad b
irErrTypeOp table op es = case irTypeOp es of
    Ok x ->
        if elem op [M_Neg,M_Add,M_Sub,M_Mul,M_Div] then case x of
            M_Real -> Ok M_Real
            M_Int -> Ok M_Int
            otherwise -> Bad "Exception: Invalid number value in arithmetic expression"
        else if elem op [M_And,M_Or,M_Not] then case x of
            M_Bool -> Ok M_Bool
            otherwise -> Bad "Exception: Invalid boolean value in boolean expression"
        else if elem op [M_EQ,M_LE,M_GE,M_LT,M_GT] then case x of
            M_Bool -> Ok M_Bool
            M_Real -> Ok M_Bool
            M_Int -> Ok M_Bool
        else if (op == M_Float) then case x of
            M_Real -> Ok M_Real
            M_Int -> Ok M_Real
            otherwise -> Bad "Exception: Invalid number value in function application"
        else if elem op [M_Floor,M_Ceil] then case x of
            M_Real -> Ok M_Real
            M_Int -> Ok M_Int
            otherwise -> Bad "Exception: Invalid number value in function application"
        else Bad ("Exception: Invalid operation in " ++ show op)
    Bad b -> Bad b
    
irErrTypes :: ST -> [M_Expr] -> Err [(M_Type,Int)]
irErrTypes _ [] = Ok []
irErrTypes table (expr:exprs) = case irErrType table expr of
    Ok t -> case irErrTypes table exprs of
        Ok ts -> Ok(t:ts)
        Bad b -> Bad b
    Bad b -> Bad b
    
irErrType :: ST -> M_Expr -> Err (M_Type,Int)
irErrType _ (M_Rval _) = Ok (M_Real,0)
irErrType _ (M_Bval _) = Ok (M_Bool,0)
irErrType _ (M_Ival _) = Ok (M_Int,0)
irErrType table (M_Size(id,v)) = case lookupSymbol table id of
    Ok (I_VARIABLE (_,_,t,z)) -> Ok (M_Int,0)
    otherwise -> Bad ("Exception: Invalid type in function " ++ show (M_Size (id,v)))
irErrType table (M_Id(id,v)) = case (lookupSymbol table id) of
    Ok(I_VARIABLE (_,_,t,_)) -> Ok(t,0)
    otherwise -> Bad ("Exception: Invalid type in function " ++ show (M_Id (id,v)))
irErrType table (M_App(f,v)) = case irErrTypes table v of
    Ok ts -> case irErrTypeOp table f ts of
        Ok t -> Ok(t,0)
        Bad b -> Bad("Exception: Invalid function application in " ++ show (M_App (f,v)))
    Bad b -> Bad b

