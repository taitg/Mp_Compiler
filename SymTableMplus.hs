
{-
Describes building the symbol table and associated functions for M+
-}

{-# LANGUAGE DeriveGeneric #-}

module SymTableMplus (ST, empty, newScope, getScope, getScopeN, 
                removeScope, insertSymbol, lookupSymbol, scopeReturn )
                where

import ASTMplus
import SymTypesMplus
import ErrM
import qualified Data.Map.Lazy as DML(Map,empty,insert,lookup)

data ST = ST([Scope], Int, (DML.Map String [SYM_ATTR]))

newScope :: ScopeType -> ST -> ST
newScope stype table = case stype of
    L_PROG -> newScopePB table
    L_FUN t -> newScopeF t table
    L_BLK -> newScopePB table
    otherwise -> error "Exception: Invalid scope type"

newScopePB :: ST -> ST
newScopePB (ST([],_,_)) = error "Exception: Could not expand upon empty scope"
newScopePB (ST(scope1:scopes,i,attr)) =
    (ST(([], x+1, 0, 0, Nothing):(scope1:scopes),i,attr)) where
        (d, x, vars, args, ret) = scope1

newScopeF :: M_Type -> ST -> ST
newScopeF _ (ST([],_,_)) = error "Exception: Could not expand upon empty scope"
newScopeF t (ST(scope1:scopes,i,attr)) =
    (ST(([], x+1, 0, 0, Just t):(scope1:scopes),i,attr)) where
        (d, x, vars, args, ret) = scope1
        
newScopeHelper :: Scope
newScopeHelper = ([],0,0,0,Nothing)

removeScope :: ST -> (ST,Int)
removeScope (ST([],d,_)) = error "Exception: Could not expand upon empty scope"
removeScope (ST(_:[],d,_)) = error "Exception: Invalid scope"
removeScope (ST(scope:scopes,d,attr)) =
    removeScopeHelper (ST(scopes,d,attr)) scope

removeScopeHelper :: ST -> Scope -> (ST,Int)
removeScopeHelper (ST([],_,_)) _ = error "Exception: Invalid Scope"
removeScopeHelper (ST(scopes,x1,attr)) scope =
    (rem descs1 (ST(scopes,x1,attr)),vars) where
        (descs2,x2,vars,_,_) = scope
        rem [] table1 = table1
        rem (desc:descs3) table1 = rem descs3 (removeSymbol table1 scope desc)
        descs1 = foldr1 (++) (map (\(d,_,_,_,_)-> d) scopes)

insertSymbol :: ST -> SYM_DESC -> Err ST
insertSymbol (ST([],_,_)) _ = Bad "Exception: invalid scope - could not insert"
insertSymbol (ST (scope:scopes, x, sattr)) desc
    | not (elem (symbolId desc) (symbolIds (scope:scopes) i1))
        = Ok(ST (scopes2,x,DML.insert ins [attr] sattr))
    | otherwise = insertHelper (ST((scope:scopes),x,sattr)) i2 desc attr 
        where
            (descs,i1,vars,args,rt) = scope
            i2 = case desc of
                (FUNCTION _) -> i1+1
                otherwise -> i1
            (attr,scopes2,ins) = case desc of
                (ARGUMENT (id,t,d))
                    -> ((ATTR ((I_VARIABLE (i1, (-(args+4)),t,d)), i1)),
                        (desc:descs,i1,vars,args+1,rt):scopes, id)
                (VARIABLE (id,t,d))
                    -> ((ATTR ((I_VARIABLE (i1, (vars+1),t,d)), i1)),
                        (desc:descs,i1,vars+1,args,rt):scopes, id)
                (FUNCTION (id,rts,t))
                    -> ((ATTR ((I_FUNCTION (i1,id,rts,t)), i1)),
                        ([],i1+1,0,0,Just t):((desc:descs,i1,vars,args,rt):scopes), id)

insertHelper :: ST -> Int -> SYM_DESC -> SYM_ATTR -> Err ST
insertHelper (ST(scopes,x,attr2)) i desc attr1
    | not(elem (symbolId desc) (inScope scopes i))
        = Ok(ST(scope2,x,sattr))
    | otherwise
        = Bad (show (symbolId desc) ++ "Exception: symbol occurs more than once within scope" )
            where
                attrs = parseElement $ DML.lookup (symbolId desc) attr2
                sattr = DML.insert (symbolId desc) (attr1:attrs) attr2
                (descs,_,vars,args,t) = head scopes
                scope2 = case desc of
                    (FUNCTION (_,_,p)) -> 
                        ([],i+1,0,0,Just p):((desc:descs,i,vars,args,t):(tail scopes))
                    (ARGUMENT _) -> (desc:descs,i,vars,args+1,t):(tail scopes)
                    (VARIABLE _) -> (desc:descs,i,vars+1,args,t):(tail scopes)
                
lookupSymbol :: ST -> String -> Err SYM_I_DESC
lookupSymbol (ST(_,_,attr)) id =
    case (DML.lookup id attr) of
        Just (attr:_) -> Ok (getDesc attr)
        Nothing -> Bad ("Exception: " ++ id ++ " not in scope")
        otherwise -> Bad "Exception: Invalid symbol"

symbolId :: SYM_DESC -> String
symbolId (ARGUMENT(id,_,_)) = id
symbolId (VARIABLE(id,_,_)) = id
symbolId (FUNCTION(id,_,_)) = id

symbolIds :: [Scope] -> Int -> [String]
symbolIds scopes i = foldr (\(desc,_,_,_,_) scope ->
    (map symbolId desc) ++ scope) [] scopes

inScope :: [Scope] -> Int -> [String]
inScope scopes i = symbolIds (filter (\(_,x,_,_,_) -> x==i) scopes) i

getDesc :: SYM_ATTR -> SYM_I_DESC
getDesc (ATTR(desc,_)) = desc

removeSymbol :: ST -> Scope -> SYM_DESC -> ST
removeSymbol (ST([],_,_)) _ _ = error "Exception: invalid scope - could not remove symbol"
removeSymbol (ST(scopes,i,attr)) scope desc
    | not inSc = (ST(scopes,i,attr))
    | otherwise = (ST(scopes,i,sattr)) where
        inSc = elem desc descs
        sattr = DML.insert (symbolId desc) (filter (\x-> symbolScope x /= n) val) attr
        val = parseElement $ DML.lookup (symbolId desc) attr
        (descs,n,_,_,_) = scope
        
parseElement :: Maybe a -> a
parseElement (Just a) = a
parseElement Nothing = error "Exception: invalid value detected"

empty :: ST
empty = ST([newScopeHelper], 0, (DML.empty))

symbolScope :: SYM_ATTR -> Int
symbolScope (ATTR(_,i)) = i

getScope :: ST -> Scope
getScope (ST((scope:scopes),_,_)) = scope

scopeReturn :: ST -> Err M_Type
scopeReturn (ST ([],_,_)) = Bad "Exception: scope contains no values"
scopeReturn (ST (scope:scopes,_,attr)) = returnHelper scope

returnHelper :: Scope -> Err M_Type
returnHelper(_,_,_,_,rt) = case rt of
    Just a -> Ok a
    otherwise -> Bad "Exception: no type for return value"
    
getScopeN :: ST -> Int
getScopeN (ST((_,i,_,_,_):scopes,_,_)) = i

