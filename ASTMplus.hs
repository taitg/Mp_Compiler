
{-
Describes the abstract syntax tree for M+,
deriving Generic for pretty printing
-}

{-# LANGUAGE DeriveGeneric #-}

module ASTMplus where

import GenericPretty

data M_Prog = M_Prog ([M_Decl],[M_Stmt])
            deriving (Eq,Show,Generic)
            
instance Out M_Prog

data M_Decl = M_Var (String,[M_Expr],M_Type)
            | M_Fun (String,[(String,Int,M_Type)],M_Type,[M_Decl],[M_Stmt])
            deriving (Eq,Show,Generic)
            
instance Out M_Decl
            
data M_Stmt = M_Ass (String,[M_Expr],M_Expr)
            | M_While (M_Expr,M_Stmt)
            | M_Cond (M_Expr,M_Stmt,M_Stmt)
            | M_Read (String,[M_Expr])
            | M_Print (M_Expr)
            | M_Return (M_Expr)
            | M_Block ([M_Decl],[M_Stmt])
            deriving (Eq,Show,Generic)
            
instance Out M_Stmt

data M_Type = M_Int
            | M_Real
            | M_Bool
            deriving (Eq,Show,Generic)
            
instance Out M_Type

data M_Expr = M_Ival Integer
            | M_Rval Double
            | M_Bval Bool
            | M_Size (String,Int)
            | M_Id (String,[M_Expr])
            | M_App (M_Operation,[M_Expr])
            deriving (Eq,Show,Generic)
            
instance Out M_Expr
            
data M_Operation = M_Fn String
                | M_Add
                | M_Sub
                | M_Mul
                | M_Div
                | M_Neg
                | M_LT
                | M_GT
                | M_LE
                | M_GE
                | M_EQ
                | M_Not
                | M_And
                | M_Or
                | M_Float
                | M_Floor
                | M_Ceil
                deriving (Eq,Show,Generic)
            
instance Out M_Operation
