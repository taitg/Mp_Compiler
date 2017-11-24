
{-
Describes the symbol table datatypes for M+,
deriving Generic for pretty printing
-}

{-# LANGUAGE DeriveGeneric #-}

module SymTypesMplus where

import ASTMplus
import GenericPretty

data SYM_DESC = ARGUMENT (String,M_Type,Int)
                | VARIABLE (String,M_Type,Int)
                | FUNCTION (String,[(M_Type,Int)],M_Type)
                -- DATATYPE String
                -- CONSTRUCTOR (String,[M_Type],String)
                deriving (Eq,Generic)
                
data SYM_I_DESC = I_VARIABLE (Int,Int,M_Type,Int)
                | I_FUNCTION (Int,String,[(M_Type,Int)],M_Type)
                -- I_CONSTRUCTOR (Int,[M_Type],String)
                -- I_TYPE [String]
                deriving (Eq,Generic)

data SYM_ATTR = ATTR(SYM_I_DESC, Int)
                deriving (Eq,Generic)
                
type Scope = ([SYM_DESC], Int, Int, Int, (Maybe M_Type))

data ScopeType = L_PROG
                | L_FUN M_Type
                | L_BLK
                deriving (Eq,Generic)