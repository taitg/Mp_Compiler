
-- Haskell module generated by the BNF converter
-- Modified to produce AST for M+ from parse tree

module SkelMplus where

import AbsMplus
import ErrM
import ASTMplus
type Result = Err String

failure :: Show a => a -> Result
failure x = Bad $ "Undefined case: " ++ show x

transIdent :: Ident -> String
transIdent x = case x of
  Ident string -> string
  
transProg :: Prog -> ASTMplus.M_Prog
transProg x = case x of
  Prog1 block -> ASTMplus.M_Prog(transBlock block)
  
transBlock :: Block -> ([ASTMplus.M_Decl],[ASTMplus.M_Stmt])
transBlock x = case x of
  Block1 decls progbody ->
    (map transDecl decls, transProg_body progbody)
  
transDecl :: Decl -> ASTMplus.M_Decl
transDecl x = case x of
  Decl1 vardecl -> transVar_decl vardecl
  Decl2 fundecl -> transFun_decl fundecl
  
transVar_decl :: Var_decl -> ASTMplus.M_Decl
transVar_decl x = case x of
  Var_decl1 ident arraydims type_ ->
    ASTMplus.M_Var (transIdent ident, transArray_dims arraydims, transType type_)
  
transType :: Type -> ASTMplus.M_Type
transType x = case x of
  Type1 -> ASTMplus.M_Int
  Type2 -> ASTMplus.M_Real
  Type3 -> ASTMplus.M_Bool
  
transArray_dims :: Array_dims -> [ASTMplus.M_Expr]
transArray_dims x = case x of
  Array_dims1 expr arraydims -> transExpr expr : transArray_dims arraydims
  Array_dims2 -> []
  
transFun_decl :: Fun_decl -> ASTMplus.M_Decl
transFun_decl x = case x of
  Fun_decl1 ident paramlist type_ funblock ->
    ASTMplus.M_Fun(transIdent ident, transParam_list paramlist, transType type_, fst (transFun_block funblock), snd (transFun_block funblock))
  
transFun_block :: Fun_block -> ([ASTMplus.M_Decl],[ASTMplus.M_Stmt])
transFun_block x = case x of
  Fun_block1 decls funbody ->
    ((map transDecl decls), transFun_body funbody)
  
transParam_list :: Param_list -> [(String,Int,ASTMplus.M_Type)]
transParam_list x = case x of
  Param_list1 params -> map transParam params
  
transParam :: Param -> (String,Int,ASTMplus.M_Type)
transParam x = case x of
  Param1 basicdecl -> transBasic_decl basicdecl
  
transBasic_decl :: Basic_decl -> (String,Int,ASTMplus.M_Type)
transBasic_decl x = case x of
  Basic_decl1 ident basicarraydims type_ ->
    (transIdent ident, transBasic_array_dims basicarraydims, transType type_)
  
transBasic_array_dims :: Basic_array_dims -> Int
transBasic_array_dims x = case x of
  Basic_array_dims1 basicarraydims -> 0
  Basic_array_dims2 -> 0
  
transProg_body :: Prog_body -> [ASTMplus.M_Stmt]
transProg_body x = case x of
  Prog_body1 stmts -> map transStmt stmts
  
transFun_body :: Fun_body -> [ASTMplus.M_Stmt]
transFun_body x = case x of
  Fun_body1 stmts expr ->
    (map transStmt stmts) ++ [ASTMplus.M_Return(transExpr expr)]
  
transStmt :: Stmt -> ASTMplus.M_Stmt
transStmt x = case x of
  Stmt1 expr stmt1 stmt2 ->
    ASTMplus.M_Cond(transExpr expr, transStmt stmt1, transStmt stmt2)
  Stmt2 expr stmt ->
    ASTMplus.M_While (transExpr expr, transStmt stmt)
  Stmt3 identifier ->
    ASTMplus.M_Read (transIdentifier identifier)
  Stmt4 identifier expr ->
    ASTMplus.M_Ass (fst (transIdentifier identifier), snd (transIdentifier identifier), transExpr expr)
  Stmt5 expr ->
    ASTMplus.M_Print (transExpr expr)
  Stmt6 block ->
    ASTMplus.M_Block (transBlock block)
  
transIdentifier :: Identifier -> (String, [ASTMplus.M_Expr])
transIdentifier x = case x of
  Identifier1 ident arraydims ->
    (transIdent ident, transArray_dims arraydims)
  
transExpr :: Expr -> ASTMplus.M_Expr
transExpr x = case x of
  Expr1 expr bintterm ->
    ASTMplus.M_App(ASTMplus.M_Or,[transExpr expr, transBint_term bintterm])
  Expr2 bintterm -> 
    transBint_term bintterm
  
transBint_term :: Bint_term -> ASTMplus.M_Expr
transBint_term x = case x of
  Bint_term1 bintterm bintfactor -> 
    ASTMplus.M_App(ASTMplus.M_And,[transBint_term bintterm, transBint_factor bintfactor])
  Bint_term2 bintfactor -> 
    transBint_factor bintfactor
  
transBint_factor :: Bint_factor -> ASTMplus.M_Expr
transBint_factor x = case x of
  Bint_factor1 bintfactor -> 
    ASTMplus.M_App(ASTMplus.M_Not,[transBint_factor bintfactor])
  Bint_factor2 intexpr1 compareop intexpr2 -> 
    ASTMplus.M_App(transCompare_op compareop,[transInt_expr intexpr1, transInt_expr intexpr2])
  Bint_factor3 intexpr -> 
    transInt_expr intexpr
  
transCompare_op :: Compare_op -> ASTMplus.M_Operation
transCompare_op x = case x of
  Compare_op1 -> ASTMplus.M_EQ
  Compare_op2 -> ASTMplus.M_LT
  Compare_op3 -> ASTMplus.M_GT
  Compare_op4 -> ASTMplus.M_LE
  Compare_op5 -> ASTMplus.M_GE
  
transInt_expr :: Int_expr -> ASTMplus.M_Expr
transInt_expr x = case x of
  Int_expr1 intexpr addop intterm -> 
    ASTMplus.M_App(transAdd_op addop,[transInt_expr intexpr, transInt_term intterm])
  Int_expr2 intterm -> 
    transInt_term intterm
  
transAdd_op :: Add_op -> ASTMplus.M_Operation
transAdd_op x = case x of
  Add_op1 -> ASTMplus.M_Add
  Add_op2 -> ASTMplus.M_Sub
  
transInt_term :: Int_term -> ASTMplus.M_Expr
transInt_term x = case x of
  Int_term1 intterm mulop intfactor -> 
    ASTMplus.M_App(transMul_op mulop, [transInt_term intterm, transInt_factor intfactor])
  Int_term2 intfactor -> 
    transInt_factor intfactor
  
transMul_op :: Mul_op -> ASTMplus.M_Operation
transMul_op x = case x of
  Mul_op1 -> ASTMplus.M_Mul
  Mul_op2 -> ASTMplus.M_Div
  
transInt_factor :: Int_factor -> ASTMplus.M_Expr
transInt_factor x = case x of
  Int_factor1 expr ->
    transExpr expr
  Int_factor2 ident basicarraydims -> 
    ASTMplus.M_Size(transIdent ident, transBasic_array_dims basicarraydims)
  Int_factor3 expr -> 
    ASTMplus.M_App(ASTMplus.M_Float, [transExpr expr])
  Int_factor4 expr -> 
    ASTMplus.M_App(ASTMplus.M_Floor, [transExpr expr])
  Int_factor5 expr -> 
    ASTMplus.M_App(ASTMplus.M_Ceil, [transExpr expr])
  Int_factor6 ident modlist -> 
    idOrFun (transMod_list modlist) (transIdent ident)
  Int_factor7 integer -> 
    ASTMplus.M_Ival (integer)
  Int_factor8 double -> 
    ASTMplus.M_Rval (double)
  Int_factor9 -> 
    ASTMplus.M_Bval (True)
  Int_factor10 -> 
    ASTMplus.M_Bval (False)
  Int_factor11 intfactor -> 
    ASTMplus.M_App(ASTMplus.M_Neg, [transInt_factor intfactor])
  
transMod_list :: Mod_list -> ASTMplus.M_Expr
transMod_list x = case x of
  Mod_list1 args -> 
    ASTMplus.M_App (ASTMplus.M_Fn "tmp", map transArg args)
  Mod_list2 arraydims -> 
    ASTMplus.M_Id ("tmp", transArray_dims arraydims)
  
transArg :: Arg -> ASTMplus.M_Expr
transArg x = case x of
  Arg1 expr -> transExpr expr

idOrFun (ASTMplus.M_Id (_,exprs)) n = (ASTMplus.M_Id(n,exprs))
idOrFun (ASTMplus.M_App (_,exprs)) n = (ASTMplus.M_App((ASTMplus.M_Fn n),exprs))