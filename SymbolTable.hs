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
-- This file contains a symbol table and relevant helper
-- functions used to compile a Roo program to Oz code.
-----------------------------------------------------------------
module SymbolTable where

import Control.Monad.State
import Data.List (group)
import Data.Map (Map, (!))
import qualified Data.Map as Map
import RooAST

-----------------------------------------------------------------
-- Flag to assert if varaible contains multiple pieces of data.
-- E.g. Integer is single, a record is considered multiple.
-----------------------------------------------------------------
data DataType
  = SingleData
  | MultiData Integer
  deriving (Show, Eq)

-----------------------------------------------------------------
-- Symbol table.
-----------------------------------------------------------------
data SymbolTable = SymbolTable
  { slotCount :: Integer,
    registerCount :: Integer,
    labelCount :: Integer,
    records :: Map String (ARecord),
    arrays :: Map String (RooType, Integer, Integer), -- Type, length, rooType size
    procedures :: Map String ([(Bool, RefType)]),
    variables :: Map String (Bool, RefType, DataType, Integer),
    code :: String
  }
  deriving (Show)

-----------------------------------------------------------------
-- Returns compiled Oz Code.
-----------------------------------------------------------------
getCode :: ((), SymbolTable) -> String
getCode (_, table) = code table

-----------------------------------------------------------------
-- Add new Oz code to symbol table collector.
-----------------------------------------------------------------
writeOzCode :: String -> State SymbolTable ()
writeOzCode str =
  do
    let final_str = concat [str, "\n"]
    table <- get
    put $ table {code = (code table) ++ final_str}

-----------------------------------------------------------------
-- Returns integer of next available register.
-----------------------------------------------------------------
getNextRegister :: State SymbolTable Integer
getNextRegister =
  do
    table <- get
    let reg = registerCount table
    if reg == 1024
      then error $ " Maximumum number of registers exceeded."
      else do
        put $ table {registerCount = (reg + 1)}
        return reg

-----------------------------------------------------------------
-- Set integer of next available register.
-----------------------------------------------------------------
setRegisterCounter :: Integer -> State SymbolTable ()
setRegisterCounter n =
  do
    table <- get
    put $ table {registerCount = n}

-----------------------------------------------------------------
-- Resets register counter.
-----------------------------------------------------------------
resetRegisterCounter :: State SymbolTable ()
resetRegisterCounter =
  do
    table <- get
    put $ table {registerCount = 0}

-----------------------------------------------------------------
-- Returns next stack slot.
-----------------------------------------------------------------
getNextSlot :: State SymbolTable Integer
getNextSlot =
  do
    table <- get
    let slot = slotCount table
    put $ table {slotCount = (slot + 1)}
    return slot

-----------------------------------------------------------------
-- Increments stack slot counter.
-----------------------------------------------------------------
incrSlotCounter :: Integer -> State SymbolTable ()
incrSlotCounter 0 = return ()
incrSlotCounter n =
  do
    getNextSlot
    incrSlotCounter (n - 1)

-----------------------------------------------------------------
-- Increment slot counter and initialise values.
-----------------------------------------------------------------
incrSlotCounterInit :: Integer -> Integer -> State SymbolTable ()
incrSlotCounterInit _ 0 = return ()
incrSlotCounterInit r n =
  do
    slot <- getNextSlot
    writeOzCode $ concat $ [indent, "store ", show slot, ", r", show r]
    incrSlotCounterInit r (n - 1)

-----------------------------------------------------------------
-- Returns integer of next label.
-----------------------------------------------------------------
getNextLabel :: State SymbolTable String
getNextLabel =
  do
    table <- get
    let label = labelCount table
    put $ table {labelCount = (label + 1)}
    return $ "label_" ++ show label

-----------------------------------------------------------------
-- Clears current symbol table.
-----------------------------------------------------------------
clearSymbolTable :: State SymbolTable ()
clearSymbolTable =
  do
    table <- get
    put $ table {slotCount = 0}
    table <- get
    put $ table {registerCount = 0}
    table <- get
    put $ table {variables = Map.empty}

-----------------------------------------------------------------
-- Inserts a record into the symbol table.
-----------------------------------------------------------------
insertRecord :: ARecord -> State SymbolTable ()
insertRecord arecord@(ARecord pos fields ident _) =
  do
    uniqueFields pos fields
    table <- get
    if Map.member ident (records table)
      then error $ concat $ [writeSourcePos pos, "Duplicated record ", ident]
      else put $ table {records = Map.insert ident arecord (records table)}

fst3 :: (a, b, c) -> a
fst3 (x, _, _) = x

-----------------------------------------------------------------
-- Asserts record fields are unique.
-----------------------------------------------------------------
uniqueFields :: Position -> [(Ident, Integer, RooType)] -> State SymbolTable ()
uniqueFields pos fields =
  do
    let names = filter ((> 1) . length) (group $ map fst3 fields)

    case length names of
      0 -> return ()
      _ -> error $ concat $ [writeSourcePos pos, "Duplicated record field ", show ((names !! 0) !! 0)]

    return ()

-----------------------------------------------------------------
-- Return procedure parameters from symbol table.
-----------------------------------------------------------------
getProcedure :: String -> Position -> State SymbolTable ([(Bool, RefType)])
getProcedure ident pos =
  do
    table <- get
    case Map.lookup ident (procedures table) of
      Nothing ->
        error $ concat $ [writeSourcePos pos, "Undefined procedure ", ident]
      Just procedure ->
        return procedure

-----------------------------------------------------------------
-- Insert a procedure to the symbol table.
-----------------------------------------------------------------
insertProcedure ::
  String ->
  [(Bool, RefType)] ->
  Position ->
  State SymbolTable ()
insertProcedure ident types pos =
  do
    table <- get
    if Map.member ident (procedures table)
      then error $ concat $ [writeSourcePos pos, "Duplicate procedure ", ident]
      else put $ table {procedures = Map.insert ident types (procedures table)}

-----------------------------------------------------------------
-- Return variable from symbol table.
-----------------------------------------------------------------
getVariable ::
  String ->
  DataType ->
  Position ->
  State SymbolTable (Bool, RefType, DataType, Integer)
getVariable ident vs pos =
  do
    table <- get
    case Map.lookup ident (variables table) of
      Nothing ->
        error $ concat $ [writeSourcePos pos, "Undefined variable ", ident]
      Just (isRef, baseType, dataType, slot) -> return (isRef, baseType, dataType, slot)

-----------------------------------------------------------------
-- Insert variable into symbol table.
-----------------------------------------------------------------
insertVariable ::
  String ->
  (Bool, RefType, DataType, Integer) ->
  Position ->
  State SymbolTable ()
insertVariable ident (isRef, bt, vs, slot) pos =
  do
    table <- get
    if Map.member ident (variables table)
      then error $ concat $ [writeSourcePos pos, "Variable ", ident, " declared more than once."]
      else do
        put $
          table
            { variables =
                Map.insert ident (isRef, bt, vs, slot) (variables table)
            }

-----------------------------------------------------------------
-- Write error location details.
-----------------------------------------------------------------
writeSourcePos :: Position -> String
writeSourcePos (line, column) =
  concat $
    ["Error at line ", show line, " column ", show column, "\n"]

-----------------------------------------------------------------
-- Write an indent for the Oz Code.
-----------------------------------------------------------------
indent :: String
indent = "    "