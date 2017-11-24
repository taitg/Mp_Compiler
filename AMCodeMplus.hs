
{-
Module for generating AM stack machine code from the IR tree

Based on example code given in the AM specification
-}

module AMCodeMplus where

import IRMplus
import ASTMplus
import GenericPretty
  
   
codeInit :: String -> String -> String
codeInit vs vs2 = "LOAD_R %sp     % access pointer for program\n" 
                ++ "LOAD_R %sp     % set frame pointer\n"
                ++ "STORE_R %fp\n"
                ++ "ALLOC " ++ vs 
                ++ "     % allocate stack space for variables\n"
                
codeFuncInit :: String -> String -> String
codeFuncInit vs vs2  = "LOAD_R %sp     % begin function header\n" 
                    ++ "STORE_R %fp     % set new FP to top of stack\n"
                    ++ "ALLOC " ++ vs ++ "     % allocate void cells\n" 
                    ++ "LOAD_I -" ++ vs2 ++ "     % initialize dealloc counter\n"

codeBlockInit :: String -> String -> String
codeBlockInit vs vs3 = "LOAD_R %fp     % begin block header\n"
                        ++ "ALLOC 2\n"
                        ++ "LOAD_R %sp\n"
                        ++ "STORE_R %fp\n"
                        ++ "ALLOC " ++ vs ++ "\n"
                        ++ "LOAD_I " ++ vs3 ++ "     % load dealloc counter\n"
                
codeFuncReturn :: String -> String -> String -> String -> String
codeFuncReturn vs1 a a2 a3 = "LOAD_R %fp     % load frame pointer\n"
                            ++ "STORE_O " ++ a3 
                            ++ "     % store result at beginning\n"
                            ++ "LOAD_R %fp     % load frame pointer\n"
                            ++ "LOAD_O 0     % load return\n"
                            ++ "LOAD_R %fp \n"
                            ++ "STORE_O " ++ a2 
                            ++ "     % place return below result\n"
                            ++ "LOAD_R %fp\n"
                            ++ "LOAD_O " ++ vs1 
                            ++ "     % retrieve dealloc pointer\n"
                            ++ "ALLOC_S     % deallocate local storage\n"
                            ++ "STORE_R %fp     % restore old FP\n"
                            ++ "ALLOC -" ++ a 
                            ++ "     % remove top " ++ a ++ " stack elements\n"
                            ++ "JUMP_S     % end function, jump to top of stack addr\n"
                            
codeBlockReturn :: String -> String
codeBlockReturn vs1 = "LOAD_R %fp\n"
                    ++ "LOAD_O " ++ vs1 ++ "     % load dealloc counter\n"
                    ++ "APP NEG     % negate it\n"
                    ++ "ALLOC_S     % deallocate block's space\n"
                    ++ "STORE_R %fp     % end block\n"
                            
checkReal :: [I_Expr] -> Bool
checkReal (expr:[]) = case expr of
    IREAL _ -> True
    IID (_,_,_,t) -> case t of
        M_Real -> True
        otherwise -> False
    otherwise -> False
checkReal (expr:exprs) = this && (checkReal exprs)
    where
        this = case expr of
            IREAL _ -> True
            IID (_,_,_,t) -> case t of
                M_Real -> True
                otherwise -> False
            otherwise -> False
            
fixBool :: String -> String
fixBool s = map rep s
    where
        rep 'T' = 't'
        rep 'F' = 'f'
        rep c = c
        
codeProg :: I_Prog -> Int -> String
codeProg (IPROG (body,vars,dims,stmts)) n1 = let
    code1 = codeStatements stmts n1
    code2 = codeFunctions body (fst code1) stmts
    in (codeInit (show vars) (show (vars+2))) ++ snd code1
        ++ "ALLOC -" ++ show (vars+1) ++ "     % deallocate program's stack space\n" 
        ++ "HALT     % end of execution\n" ++ snd code2

codeFunctions :: [I_Fnbody] -> Int -> [I_Stmt] -> (Int,String)
codeFunctions [] n _ = (n,"")
codeFunctions (fbody:fbodies) n1 stmts = let
    code1 = codeFunction fbody n1
    code2 = codeFunctions fbodies (fst code1) []
    in (fst code2,snd code1 ++ snd code2
        ++ snd (codeNestFun stmts (fst code2)))
codeFunction :: I_Fnbody -> Int -> (Int,String)
codeFunction (IFUN (id,fbody,vars,args,dims,stmts)) n1 = let
    code1 = codeStatements stmts n1
    code2 = codeFunctions fbody (fst code1) []
    code3 = codeNestFun stmts (fst code2)
    in (fst code3,funL id ++ (codeFuncInit (show vars) (show (vars+2))) 
        ++ snd code1
        ++ (codeFuncReturn (show (vars+1)) (show args) (show (-args-2)) (show (-args-3))) 
        ++ "\n" ++ snd code2 ++ snd code3)

codeNestFun :: [I_Stmt] ->  Int -> (Int,String)
codeNestFun [] n = (n,"")
codeNestFun stmts n = codeNestFunHelper (concatMap irFormBlock stmts) n
codeNestFunHelper :: [I_Stmt] -> Int -> (Int,String)
codeNestFunHelper [] n = (n,"")
codeNestFunHelper ((IBLOCK (body,_,_,_)):stmts) n1 = let
    code1 = codeFunctions body n1 []
    code2 = codeNestFunHelper stmts (fst code1)
    in (fst code2,snd code1 ++ snd code2)
codeNestFunHelper (stmt:stmts) n = codeNestFunHelper stmts n

codeStatements:: [I_Stmt] -> Int -> (Int,String)
codeStatements [] n = (n,"")
codeStatements (stmt:stmts) n1 = let
    code1 = codeStatement stmt n1
    code2 = codeStatements stmts (fst code1)
    in (fst code2,snd code1 ++ snd code2)
codeStatement :: I_Stmt -> Int -> (Int,String)
codeStatement(ICOND (test,s1,s2)) n1 = let
    code1 = codeStatement s1 n1
    label1 = createL (fst code1)
    code2 = codeStatement s2 (fst label1)
    label2 = createL (fst code2)
    in (fst label2,codeExpression test ++ "JUMP_C l_" ++ (show (fst label1))
        ++ "     % if test\n" ++ snd code1 ++ "JUMP l_" ++ (show (fst label2)) 
        ++ "     % else branch\n" ++ snd label1 ++ snd code2 ++ snd label2)
codeStatement (IWHILE (test, body)) n1 = let
    label1 = createL n1
    code1 = codeStatement body (fst label1)
    label2 = createL (fst code1)
    in (fst label2,"JUMP l_" ++ show (fst label2) ++ "     % while loop\n"
        ++ snd label1 ++ snd code1 ++ snd label2 ++ codeExpression test
        ++ "APP NOT\n"
        ++ "JUMP_C l_" ++ show (fst label1) ++ "     % while test\n")
codeStatement (IASS (x,y,_,value)) n
    = (n,codeExpression value ++ codeLink x
        ++ "STORE_O " ++ (show y) ++ "     % variable assignment\n")
codeStatement (IPRINT_B val) n = (n,codeExpression val 
    ++ "PRINT_B     % print boolean\n")
codeStatement (IPRINT_I val) n = (n,codeExpression val 
    ++ "PRINT_I     % print integer\n")
codeStatement (IPRINT_F val) n = (n,codeExpression val 
    ++ "PRINT_F     % print real\n")
codeStatement (IREAD_I (x,y,_)) n = (n,"READ_I     % input integer\n" ++ codeLink x
                                ++ "STORE_O " ++ show y ++ "\n")
codeStatement (IREAD_F (x,y,[])) n = (n,"READ_F     % input real\n" ++ codeLink x
                                ++ "STORE_O " ++ show y ++ "\n")
codeStatement (IREAD_B (x,y,[])) n = (n,"READ_B     % input boolean\n" ++ codeLink x
                                ++ "STORE_O " ++ show y ++ "\n")
codeStatement (IBLOCK (_,vars,_,stmts)) n1 = let
    code1 = codeStatements stmts n1
    in (fst code1,codeBlockInit (show vars) (show (vars+3)) ++ snd code1 
        ++ codeBlockReturn (show (vars+1)))
codeStatement (IRETURN val) n = (n,codeExpression val)
codeStatement s _ = error ("Statement error (unrecognized):\n" ++ (pretty s))

codeOperation :: I_Op -> [I_Expr] -> String
codeOperation IAND exprs = codeExpressions exprs ++ "APP AND     % perform AND op\n"
codeOperation IOR exprs = codeExpressions exprs ++ "APP OR     % perform OR op\n"
codeOperation INOT exprs = codeExpressions exprs ++ "APP NOT     % perform NOT op\n"
codeOperation IFLOAT exprs = codeExpressions exprs ++ "APP FLOAT     % perform FLOAT op\n"
codeOperation IFLOOR exprs = codeExpressions exprs ++ "APP FLOOR     % perform FLOOR op\n"
codeOperation ICEIL exprs = codeExpressions exprs ++ "APP CEIL     % perform CEIL op\n"
codeOperation IADD exprs = case checkReal exprs of 
    True -> codeExpressions exprs ++ "APP ADD_F     % perform float addition\n"
    False -> codeExpressions exprs ++ "APP ADD     % perform integer addition\n"
codeOperation ISUB exprs = case checkReal exprs of 
    True -> codeExpressions exprs ++ "APP SUB_F     % perform float subtraction\n"
    False -> codeExpressions exprs ++ "APP SUB     % perform integer subtraction\n"
codeOperation INEG exprs = case checkReal exprs of 
    True -> codeExpressions exprs ++ "APP NEG_F     % perform float negation\n"
    False -> codeExpressions exprs ++ "APP NEG     % perform integer negation\n"
codeOperation IMUL exprs = case checkReal exprs of 
    True -> codeExpressions exprs ++ "APP MUL_F     % perform float multiplication\n" 
    False -> codeExpressions exprs ++ "APP MUL     % perform integer multiplication\n"
codeOperation IDIV exprs = case checkReal exprs of 
    True -> codeExpressions exprs ++ "APP DIV_F     % perform float division\n"
    False -> codeExpressions exprs ++ "APP DIV     % perform integer division\n"
codeOperation IEQ exprs = case checkReal exprs of 
    True -> codeExpressions exprs ++ "APP EQ_F     % perform float equality check\n"
    False -> codeExpressions exprs ++ "APP EQ     % perform integer equality check\n"
codeOperation ILT exprs = case checkReal exprs of 
    True -> codeExpressions exprs ++ "APP LT_F     % perform float LT check\n"
    False -> codeExpressions exprs ++ "APP LT     % perform integer LT check\n"
codeOperation ILE exprs = case checkReal exprs of 
    True -> codeExpressions exprs ++ "APP LE_F     % perform float LE check\n"
    False -> codeExpressions exprs ++ "APP LE     % perform integer LE check\n"
codeOperation IGT exprs = case checkReal exprs of 
    True -> codeExpressions exprs ++ "APP GT_F     % perform float GT check\n"
    False -> codeExpressions exprs ++ "APP GT     % perform integer GT check\n"
codeOperation IGE exprs = case checkReal exprs of 
    True -> codeExpressions exprs ++ "APP GE_F     % perform float GE check\n"
    False -> codeExpressions exprs ++ "APP GE     % perform integer GE check\n"
codeOperation (ICALL (id,n)) exprs = codeExpressions exprs 
                            ++ "ALLOC 1     % void on stack\n" ++ codeLink n
                            ++ "LOAD_R %fp\n" ++ "LOAD_R %cp\n" ++ "JUMP fun_" ++ id 
                            ++ "     % jump to " ++ id ++ " function\n"

codeExpressions :: [I_Expr] -> String
codeExpressions [] = ""
codeExpressions (expr:[]) = codeExpression expr
codeExpressions (expr:exprs) = (codeExpression expr) ++ (codeExpressions exprs)
codeExpression :: I_Expr -> String
codeExpression (IAPP (ICALL call, exprs)) = codeOperation (ICALL call) (reverse exprs)
codeExpression (IAPP (op,exprs)) = codeOperation op exprs
codeExpression (ISIZE s) = ""
codeExpression (IID (x,y,_,_)) = codeLink x ++ "LOAD_O " ++ (show y) 
                                ++ "     % load identifier " ++ (show x) ++ " value\n"
codeExpression (IINT i) = "LOAD_I " ++ (show i) ++ "     % load integer value\n"
codeExpression (IREAL r) = "LOAD_F " ++ (show r) ++ "     % load real value\n"
codeExpression (IBOOL b) = "LOAD_B " ++ fixBool (show b) ++ "     % load boolean value\n"
codeExpression s = error ("Expression error (unrecognized):\n" ++ (pretty s))

codeLink :: Int -> String
codeLink n = let
    s = "LOAD_O -2\n"
    in "LOAD_R %fp\n" ++ (concat (take n (repeat s)))

createL :: Int -> (Int,String)
createL n = (n+1,"\nl_" ++ show (n+1) ++ ":\n")
funL :: String -> String
funL s = "\nfun_" ++ s ++ ":     % begin " ++ s ++ " function definition\n"