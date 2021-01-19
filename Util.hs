-----------------------------------------------------------------
--    COMP90045 Programming Language Implementation, Stage 3
--
--                         Wallaby
--
--                    Karl Flores 760493
--                   Marco Marasco 834482
--                  Austen McClernon 834063
-----------------------------------------------------------------
-----------------------------------------------------------------
-- This file contains helper functions used to convert a
-- Roo program to Oz code.
-----------------------------------------------------------------
module Util where

import Control.Monad.State
import Data.Map (Map, (!))
import qualified Data.Map as Map
import PrettyRoo
import RooAST
import SymbolTable

-----------------------------------------------------------------
-- Helper function to check if pass by reference.
-----------------------------------------------------------------
passByRef :: ParMode -> Bool
passByRef PInt = True
passByRef PBoolean = True
passByRef (PAlias _) = True
passByRef _ = False

-----------------------------------------------------------------
-- Convert reference type to Roo type.
-----------------------------------------------------------------
refTypeToRooType :: RefType -> RooType
refTypeToRooType (VarType v) = v
refTypeToRooType (ArrayType _ v) = v

-----------------------------------------------------------------
-- Convert parse mode to Roo type.
-----------------------------------------------------------------
parModeToRefType :: ParMode -> State SymbolTable RefType
parModeToRefType PBoolean = return (VarType BoolType)
parModeToRefType PBoolV = return (VarType BoolType)
parModeToRefType PInt = return (VarType IntType)
parModeToRefType PIntV = return (VarType IntType)
parModeToRefType (PAlias a) =
  do
    table <- get
    case Map.lookup a (arrays table) of
      -- Variable of given identifier is undefined
      Nothing ->
        do
          case Map.lookup a (records table) of
            Nothing -> error $ concat $ ["Unknown alias ", a]
            Just (ARecord _ _ _ size) -> return (VarType (RecordType a))
      Just (rooType, len, size) -> return (ArrayType len rooType)

-----------------------------------------------------------------
-- Helper function to convert local declarations to array types.
-----------------------------------------------------------------
convertRefType :: LocalDecl -> State SymbolTable LocalDecl
convertRefType local@(LocalDecl pos idents ref@(VarType rooType)) =
  do
    table <- get
    case rooType of
      BoolType -> return local
      IntType -> return local
      (RecordType s) ->
        do
          case Map.lookup s (arrays table) of
            -- Variable of given identifier is undefined
            Nothing -> return local
            Just (rooType, len, size) ->
              return (LocalDecl pos idents (ArrayType len rooType))

-----------------------------------------------------------------
-- Returns stack size of Roo type.
-----------------------------------------------------------------
getRooTypeSize :: RooType -> State SymbolTable Integer
getRooTypeSize BoolType = return 1
getRooTypeSize IntType = return 1
getRooTypeSize (RecordType s) =
  do
    table <- get
    case Map.lookup s (records table) of
      Nothing -> error $ concat $ ["Unknown alias ", s]
      Just (ARecord _ _ _ n) -> return n

-----------------------------------------------------------------
-- Convert binary operator to Oz instruction.
-----------------------------------------------------------------
binopToOz :: Binop -> String
binopToOz (Arop Op_add) = "add_int"
binopToOz (Arop Op_sub) = "sub_int"
binopToOz (Arop Op_mul) = "mul_int"
binopToOz (Arop Op_div) = "div_int"
binopToOz (Logop Op_or) = "or"
binopToOz (Logop Op_and) = "and"
binopToOz (Logop Op_not) = "not"
binopToOz (Relop Op_eq) = "cmp_eq_int"
binopToOz (Relop Op_ne) = "cmp_ne_int"
binopToOz (Relop Op_ge) = "cmp_ge_int"
binopToOz (Relop Op_le) = "cmp_le_int"
binopToOz (Relop Op_gt) = "cmp_gt_int"
binopToOz (Relop Op_lt) = "cmp_lt_int"

-----------------------------------------------------------------
-- Extract position from expression.
-----------------------------------------------------------------
getPosition :: Exp -> Position
getPosition (Lval pos _) = pos
getPosition (ExpConst pos _) = pos
getPosition (UnaryMinus pos _) = pos
getPosition (Not pos _) = pos
getPosition (BinOpExp pos _ _ _) = pos

-----------------------------------------------------------------
-- Return parameter for procedure.
-----------------------------------------------------------------
getProcedureParam :: String -> Integer -> State SymbolTable (Bool, RefType)
getProcedureParam ident idx =
  do
    table <- get
    return $ (procedures table) ! ident !! (fromInteger idx)

-----------------------------------------------------------------
-- Insert array to symbol table.
-----------------------------------------------------------------
insertArray :: Array -> State SymbolTable ()
insertArray arr@(Array pos ident (ArrayType n rooType)) =
  do
    table <- get
    -- Check if duplicated array exists
    if Map.member ident (arrays table)
      then
        error $
          concat $
            [ writeSourcePos pos,
              "Duplicated array definition ",
              ident
            ]
      else do
        rooTypeSize <- getRooTypeSize rooType
        put $
          table
            { arrays =
                Map.insert
                  ident
                  (rooType, n, rooTypeSize)
                  (arrays table)
            }
insertArray _ = error $ "Error: attempted to insert an array."

-----------------------------------------------------------------
-- Convert a record to an ARecord, it helps to keep track of
-- the record stack frame size.
-----------------------------------------------------------------
convertRecord :: Record -> ARecord
convertRecord (Record pos decls ident) =
  ARecord
    pos
    offsets
    ident
    (toInteger $ length offsets)
  where
    offsets = getFieldOffsets 0 decls

-----------------------------------------------------------------
-- Return field offset.
-----------------------------------------------------------------
getFieldOffset :: RooType -> Ident -> State SymbolTable Integer
getFieldOffset (RecordType record) field =
  do
    fields <- getRecordFields record
    (_, ret, _) <- getField field fields
    return ret
getFieldOffset _ _ = error $ "Attempted to offset a non-field."

-----------------------------------------------------------------
-- Collect offsets of record fields.
-----------------------------------------------------------------
getFieldOffsets :: Integer -> [FieldDecl] -> [(Ident, Integer, RooType)]
getFieldOffsets _ [] = []
getFieldOffsets n ((FieldDecl _ name refType) : ds) =
  (name, n, refTypeToRooType refType) : (getFieldOffsets (n + 1) ds)

-----------------------------------------------------------------
-- Return Field type.
-----------------------------------------------------------------
getFieldType :: RooType -> Ident -> State SymbolTable RooType
getFieldType (RecordType record) field =
  do
    fields <- getRecordFields record
    (_, _, ret) <- getField field fields
    return ret
getFieldType _ field = error $ concat $ ["Unknown field ", field]

-----------------------------------------------------------------
-- Return's the information of a given field.
-----------------------------------------------------------------
getFieldInfo :: Ident -> Ident -> State SymbolTable (Ident, Integer, RooType)
getFieldInfo record field =
  do
    fields <- getRecordFields record
    ret <- getField field fields
    return ret

-----------------------------------------------------------------
-- Returns a field's info.
-----------------------------------------------------------------
getField ::
  Ident ->
  [(Ident, Integer, RooType)] ->
  State SymbolTable (Ident, Integer, RooType)
getField field [] = error $ concat $ ["Unknown field ", field]
getField field ((ident, n, r) : xs)
  | field == ident = return (ident, n, r)
  | otherwise =
    do
      ret <- getField field xs
      return ret

-----------------------------------------------------------------
-- Get all fields for a record.
-----------------------------------------------------------------
getRecordFields :: Ident -> State SymbolTable [(Ident, Integer, RooType)]
getRecordFields record =
  do
    table <- get
    case Map.lookup record (records table) of
      -- Variable of given identifier is undefined
      Nothing -> error $ concat $ ["Unknown alias record ", record]
      Just (ARecord _ fields _ _) -> return fields

-----------------------------------------------------------------
-- Get full stack size required for an alias type.
-----------------------------------------------------------------
aliasToFullStackSize :: String -> State SymbolTable Integer
aliasToFullStackSize a =
  do
    table <- get
    case Map.lookup a (arrays table) of
      Nothing ->
        do
          case Map.lookup a (records table) of
            Nothing -> error $ concat $ ["Unknown alias ", a]
            Just (ARecord _ _ _ size) -> return size
      Just (_, len, size) -> return (len * size)

-----------------------------------------------------------------
-- Check if Alias is array
-----------------------------------------------------------------
aliasIsArray :: String -> State SymbolTable Bool
aliasIsArray a =
  do
    table <- get
    case Map.lookup a (arrays table) of
      -- Variable of given identifier is undefined
      Nothing ->
        do
          case Map.lookup a (records table) of
            Nothing -> error $ concat $ ["Unknown alias ", a]
            Just (ARecord _ _ _ size) -> return False
      Just (rooType, len, size) -> return True

-----------------------------------------------------------------
-- Function inserts Roo statement to Oz code as a comment.
-----------------------------------------------------------------
writeStatementComment :: Stmt -> State SymbolTable ()
writeStatementComment stmt =
  do
    writeOzCode $ concat [indent, "# ", (concat $ ppStatement stmt)]

-----------------------------------------------------------------
-- Write required Oz function for reading.
-----------------------------------------------------------------
putReadCodeType :: RooType -> State SymbolTable ()
putReadCodeType baseType =
  do
    case baseType of
      BoolType -> writeOzCode $ concat [indent, "call_builtin read_bool"]
      IntType -> writeOzCode $ concat [indent,  "call_builtin read_int"]
      _ -> error $ "Cannot write a value that is not an integer or boolean."

-----------------------------------------------------------------
-- Check if parameter is passed by reference.
-----------------------------------------------------------------
checkPassByReference :: FormalParamSpec -> State SymbolTable (Bool, RefType)
checkPassByReference (FormalParamSpec _ parmode _) =
  do
    let pass_type = passByRef parmode
    refType <- parModeToRefType parmode
    return (pass_type, refType)

-----------------------------------------------------------------
-- Scan parameters for reference types and then insert to table.
-----------------------------------------------------------------
prepareProcedures :: Procedure -> State SymbolTable ()
prepareProcedures (Procedure pos ident params _ _) =
  do
    types <- mapM checkPassByReference params
    insertProcedure ident types pos

