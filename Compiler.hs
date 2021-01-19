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
-- This file contains a Haskell program that converts a parsed
-- Roo program to equivalent Oz code.
-----------------------------------------------------------------
module Compiler where

import Control.Monad.State
import Data.List (intersect)
import Data.Map (Map, keys, keysSet, toList, (!))
import qualified Data.Map as Map
import PrettyRoo
import RooAST
import SymbolTable
import Util

-----------------------------------------------------------------
-- Initialises symbol table and compiles Oz code.
-----------------------------------------------------------------
compileRoo :: Program -> ((), SymbolTable)
compileRoo prog =
  runState
    (compileProgram prog)
    ( SymbolTable
        { labelCount = 0,
          slotCount = 0,
          registerCount = 0,
          code = "",
          procedures = Map.empty,
          variables = Map.empty,
          records = Map.empty,
          arrays = Map.empty
        }
    )

-----------------------------------------------------------------
-- Compiles a Roo program to Oz code.
-----------------------------------------------------------------
compileProgram :: Program -> State SymbolTable ()
compileProgram (Program records arrays procs) =
  do
    -- Call main procedure
    writeOzCode $ concat [indent, "call proc_main"]
    -- Stop the program
    writeOzCode $ concat [indent, "halt"]

    -- Insert records
    mapM_ insertRecord $ map convertRecord records

    -- Insert arrays
    mapM_ insertArray arrays

    -- Assert array and record names are unique
    assertUniqueIdents

    -- Put procedures into map
    mapM_ prepareProcedures procs

    -- Check main procedure exists
    assertMain

    -- Compile all procedures
    mapM_ compileProcedure procs

-----------------------------------------------------------------
-- Asserts array and record names are unique..
-----------------------------------------------------------------
assertUniqueIdents :: State SymbolTable ()
assertUniqueIdents =
  do
    table <- get
    let array_names = (keys (arrays table))
    let record_names = (keys (records table))
    let duplicates = intersect array_names record_names
    case (length duplicates) of
      0 -> return ()
      _ ->
        error $
          concat $
            [ show (duplicates !! 0),
              " is used for both an array and record declaration."
            ]

-----------------------------------------------------------------
-- Asserts a main procedure with zero parameters exists.
-----------------------------------------------------------------
assertMain :: State SymbolTable ()
assertMain =
  do
    table <- get
    case Map.lookup "main" (procedures table) of
      Nothing ->
        do
          error $ "Please insert a main procedure."
      Just params ->
        do
          -- Assert zero parametres
          if (length params) /= 0
            then error $ "Main procedure should have 0 parameters."
            else return ()

-----------------------------------------------------------------
-- Function compiles a Roo procedure to Oz code.
-----------------------------------------------------------------
compileProcedure :: Procedure -> State SymbolTable ()
compileProcedure (Procedure _ ident params decls stmts) =
  do
    -- Insert procedure label.
    writeOzCode $ concat $ ["proc_", ident, ":"]

    -- Convert local array declarations to ArrayTypes if required.
    decls <- mapM convertRefType decls

    -- Insert prologue.
    stackSize <- writePrologue params decls

    -- Comment to pass parameters if required.
    unless (null params) (writeOzCode $ concat [indent, "# Writing parameters"])

    -- Insert parameters to Oz code.
    mapM_ storeProcParameter params

    -- Reset register counter in symbol table.
    resetRegisterCounter

    -- Create a register with value zero for initialising declarations.
    zero_reg <- getNextRegister
    writeOzCode $ concat $ [indent, "int_const r", show zero_reg, ", 0"]

    -- Insert local declarations.
    mapM_ (\a -> storeLocalDecl zero_reg a) decls

    -- Reset register counter in symbol table.
    resetRegisterCounter

    -- Convert Roo statements to Oz code.
    mapM_ prepStatement stmts

    -- Reset register counter in symbol table.
    resetRegisterCounter

    -- Insert epilogue.
    writeEpilogue stackSize

    -- Clear symbol table - MAY NOT BE NEEDED.
    clearSymbolTable

    return ()

-----------------------------------------------------------------
-- Function inserts the prologue for the Oz code.
-----------------------------------------------------------------
writePrologue :: [FormalParamSpec] -> [LocalDecl] -> State SymbolTable Integer
writePrologue params decls =
  do
    writeOzCode $ concat [indent, "# prologue"]

    -- Require at most 1 stack slot per parameter
    let params_size = toInteger $ length params

    -- Get stack slots required for local declarations.
    sizes <- mapM getDeclarationStackSlots decls
    let decls_size = sum sizes

    -- Calculate total required stack space.
    let full_size = params_size + decls_size

    -- Push the required stack frame.
    writeOzCode $ concat $ [indent, "push_stack_frame ", show (full_size)]

    -- Return stack size for procedure epilogue.
    return (full_size)

-----------------------------------------------------------------
-- Function returns the number of stack slots required to
-- intialise a local variable(s) declaration.
-----------------------------------------------------------------
getDeclarationStackSlots :: LocalDecl -> State SymbolTable Integer
getDeclarationStackSlots (LocalDecl _ ids reftype) =
  case reftype of
    (ArrayType n rooType) ->
      do
        -- Get size of underlying Roo type.
        rooTypeSize <- getRooTypeSize rooType

        -- Stack size for single array.
        let array_size = (fromIntegral $ n * rooTypeSize)

        -- Check if multiple arrays declared on 1 line.
        let full_size = (toInteger $ length ids) * array_size

        return full_size
    (VarType rooType) ->
      do
        -- Get stack size of Roo type.
        rooTypeSize <- getRooTypeSize rooType

        -- Check if multiple variables declared on 1 line.
        let full_size = (toInteger $ length ids) * rooTypeSize

        return full_size

-----------------------------------------------------------------
-- Function stores passed parameters to local stack slots.
-----------------------------------------------------------------
storeProcParameter :: FormalParamSpec -> State SymbolTable ()
storeProcParameter (FormalParamSpec pos parmode ident) =
  case parmode of
    (PAlias a) ->
      do
        -- Prepare next slot.
        slot <- getNextSlot

        -- Get register containing parameter.
        reg <- getNextRegister

        -- Get reference type.
        refType <- parModeToRefType parmode

        -- Allocate stack space for element -- MAY NOT NEED
        stack_size <- aliasToFullStackSize a

        -- Insert variable.
        insertVariable
          ident
          ( passByRef parmode,
            refType,
            (MultiData stack_size),
            slot
          )
          pos

        -- Write store code.
        writeOzCode $ concat $ [indent, "store ", show slot, ", r", show reg]
    _ ->
      do
        -- Prepare next slot.
        slot <- getNextSlot

        -- Get register containing parameter.
        reg <- getNextRegister

        -- Get reference type.
        refType <- parModeToRefType parmode

        -- Insert variable.
        insertVariable ident (passByRef parmode, refType, SingleData, slot) pos

        -- Write store code.
        writeOzCode $ concat $ [indent, "store ", show slot, ", r", show reg]

-----------------------------------------------------------------
-- Function initialises local variable declarations.
-----------------------------------------------------------------
storeLocalDecl :: Integer -> LocalDecl -> State SymbolTable ()
storeLocalDecl _ (LocalDecl _ [] _) = return ()
storeLocalDecl r (LocalDecl pos (ident : ids) refType) =
  do
    -- Insert declaration comment.
    writeOzCode $
      concat $
        [ indent,
          "# initialise ",
          (show ident),
          " val ",
          (show refType)
        ]

    -- Prepare next stack slot.
    slot <- getNextSlot

    case refType of
      (VarType rooType@(RecordType record_name)) ->
        do
          table <- get

          -- Check if record type is defined.
          case Map.lookup record_name (records table) of
            Nothing ->
              error $
                concat $
                  [ writeSourcePos pos,
                    "No defined record of type ",
                    ident
                  ]
            Just (ARecord _ _ _ n) ->
              do
                -- Insert into symbol table.
                insertVariable
                  ident
                  (False, (VarType rooType), (MultiData n), slot)
                  pos

                -- Intialise to zero.
                writeOzCode $
                  concat $
                    [ indent, "store ",
                      show slot,
                      ", r",
                      show r
                    ]

                -- Initialise rest of stack slot requried for record with 0.
                incrSlotCounterInit r (n - 1)

                -- Continue with rest of declarations in same line if required.
                storeLocalDecl r (LocalDecl pos ids refType)
      (VarType rooType) ->
        do
          -- Insert into symbol table.
          insertVariable ident (False, (VarType rooType), SingleData, slot) pos

          -- Intialise to zero.
          writeOzCode $ concat $ [indent, "store ", show slot, ", r", show r]

          --- Continue with rest of declarations in same line if required.
          storeLocalDecl r (LocalDecl pos ids refType)
      (ArrayType n rooType) ->
        do
          -- Insert into symbol table.
          insertVariable
            ident
            (False, (ArrayType n rooType), (MultiData n), slot)
            pos

          -- Intialise to zero.
          writeOzCode $ concat $ [indent, "store ", show slot, ", r", show r]

          -- Initialise rest of stack slot array with 0.
          roo_size <- getRooTypeSize rooType
          incrSlotCounterInit r ((n * roo_size) - 1)

          --- Continue with rest of declarations in same line if required.
          storeLocalDecl r (LocalDecl pos ids refType)

-----------------------------------------------------------------
-- Helper function to compile Roo statements to Oz code.
-----------------------------------------------------------------
prepStatement :: Stmt -> State SymbolTable ()
prepStatement stmt =
  do
    writeRooStatement stmt

    -- Reset register counter.
    resetRegisterCounter

-----------------------------------------------------------------
-- Function compiles multiple Roo statements to Oz code.
-----------------------------------------------------------------
compileRooStatements :: [Stmt] -> State SymbolTable ()
compileRooStatements [] =
  do
    resetRegisterCounter
    return ()
compileRooStatements (stmt : stmts) =
  do
    resetRegisterCounter
    writeRooStatement stmt
    compileRooStatements stmts

-----------------------------------------------------------------
-- Function compiles complex Array and Record assignments into Oz code.
-- <id> <- <id>
-----------------------------------------------------------------
specialAssign :: LValue -> Exp -> State SymbolTable Bool
specialAssign lval@(LId pos1 ident1) exp@(Lval pos2 (LId pos3 ident2)) =
  do
    -- Load variables.
    (isRef1, baseType1, _, slot1) <-
      getVariable ident1 (MultiData 0) pos1
    (isRef2, baseType2, _, slot2) <-
      getVariable ident2 (MultiData 0) pos2

    if ( isRef1 && isRef2
           && (refTypeToRooType baseType1 == refTypeToRooType baseType2)
       )
      then do
        lhs_size <- getStackSize baseType1
        rhs_size <- getStackSize baseType2

        mapM_ (copySlot slot2 slot1) [(i, i) | i <- [0 .. lhs_size -1]]

        return True
      else return False

-- <id>[exp] <- <id>
specialAssign
  lval@(LIdArrayRef pos1 ident1 (ExpConst _ (IntConst n)))
  exp2@(Lval pos2 (LId pos3 ident2)) =
    do
      -- Load variables.
      (isRef1, baseType1, _, slot1) <-
        getVariable ident1 (MultiData 0) pos1
      (isRef2, baseType2, _, slot2) <-
        getVariable ident2 (MultiData 0) pos2

      if ( isRef1 && isRef2
             && (refTypeToRooType baseType1 == refTypeToRooType baseType2)
         )
        then do
          let rooType = refTypeToRooType baseType1
          cell_size <- getRooTypeSize rooType
          mapM_
            (copySlot slot2 slot1)
            [(i, i + n * cell_size) | i <- [0 .. cell_size -1]]

          return True
        else return False

-- <id>[exp] <- <id>[exp]
specialAssign
  lval@(LIdArrayRef pos1 ident1 (ExpConst _ (IntConst n)))
  exp2@(Lval pos2 (LIdArrayRef pos3 ident2 (ExpConst _ (IntConst m)))) =
    do
      -- Load variables.
      (isRef1, baseType1, _, slot1) <- getVariable ident1 (MultiData 0) pos1
      (isRef2, baseType2, _, slot2) <- getVariable ident2 (MultiData 0) pos2

      if (isRef1 && isRef2 && (refTypeToRooType baseType1 == refTypeToRooType baseType2))
        then do
          let rooType = refTypeToRooType baseType1
          cell_size <- getRooTypeSize rooType
          mapM_
            (copySlot slot2 slot1)
            [(i + m * cell_size, i + n * cell_size) | i <- [0 .. cell_size -1]]

          return True
        else return False

-- <id> <- <id>[exp]
specialAssign
  lval@(LId pos1 ident1)
  exp2@(Lval pos2 (LIdArrayRef pos3 ident2 (ExpConst _ (IntConst m)))) =
    do
      -- Load variables.
      (isRef1, baseType1, _, slot1) <- getVariable ident1 (MultiData 0) pos1
      (isRef2, baseType2, _, slot2) <- getVariable ident2 (MultiData 0) pos2

      if (isRef1 && isRef2 && (refTypeToRooType baseType1 == refTypeToRooType baseType2))
        then do
          let rooType = refTypeToRooType baseType2
          cell_size <- getRooTypeSize rooType
          mapM_ (copySlot slot2 slot1) [(i + m * cell_size, i) | i <- [0 .. cell_size -1]]

          return True
        else return False
specialAssign _ _ = return False

copySlot :: Integer -> Integer -> (Integer, Integer) -> State SymbolTable ()
copySlot startSlot destSlot (startOffset, destOffset) =
  do
    startReg <- getNextRegister
    destReg <- getNextRegister
    startOffsetReg <- getNextRegister
    destOffsetReg <- getNextRegister

    writeOzCode $
      concat
        [ indent, "# copy at startOffset ",
          show startOffset,
          " to destOffset ",
          show destOffset
        ]
    -- Load offset into offset register
    writeOzCode $ concat [indent, "int_const r", show startOffsetReg, ", ", show startOffset]
    writeOzCode $ concat [indent, "int_const r", show destOffsetReg, ", ", show destOffset]

    -- offSet start register
    writeOzCode $ concat [indent, "load r", show startReg, ", ", show startSlot]
    writeOzCode $
      concat
        [ indent, "sub_offset r",
          show startReg,
          ", r",
          show startReg,
          ", r",
          show startOffsetReg
        ]

    -- offSet destination register
    writeOzCode $ concat [indent, "load r", show destReg, ", ", show destSlot]
    writeOzCode $
      concat
        [ indent, "sub_offset r",
          show destReg,
          ", r",
          show destReg,
          ", r",
          show destOffsetReg
        ]

    -- Load startSlot + offset value into start register
    writeOzCode $ concat [indent, "load_indirect r", show startReg, ", r", show startReg]

    -- Store startSlot + offset value into offsetted destSlot + offset
    writeOzCode $ concat [indent, "store_indirect r", show destReg, ", r", show startReg]

    return ()

getStackSize :: RefType -> State SymbolTable Integer
getStackSize refType =
  do
    case refType of
      (ArrayType n rooType) ->
        do
          -- Get size of underlying Roo type.
          rooTypeSize <- getRooTypeSize rooType

          -- Stack size for single array.
          let array_size = (fromIntegral $ n * rooTypeSize)

          return array_size
      (VarType rooType) ->
        do
          -- Get stack size of Roo type.
          rooTypeSize <- getRooTypeSize rooType

          return rooTypeSize

-----------------------------------------------------------------
-- Function compiles Roo statements to Oz code.
-----------------------------------------------------------------
writeRooStatement :: Stmt -> State SymbolTable ()
-- Assign statement
writeRooStatement assign@(Assign pos lvalue expr) =
  do
    -- Insert comment.
    writeStatementComment assign

    -- Check if special assignment
    ret <- specialAssign lvalue expr
    if ret
      then writeOzCode $ concat [indent, "# finish <id> assign"]
      else do
        -- Prepare next register.
        regThis <- getNextRegister

        -- Get type of RHS.
        exprType <- writeRooExpression regThis expr

        -- Get Type of LHS.
        stmtType <- getLvalRooType lvalue

        -- Check if types match.
        if stmtType == (refTypeToRooType exprType)
          then writeAssignmentCode lvalue regThis
          else error $ concat $ [writeSourcePos pos, "Assignment types are mismatched."]

-- Read statement
writeRooStatement (Read pos lvalue) =
  do
    -- Prepare next register.
    reg <- getNextRegister

    -- Get Roo type.
    rooType <- getLvalRooType lvalue

    -- Insert comment.
    writeStatementComment (Read pos lvalue)

    -- Insert oz code to read for required type.
    putReadCodeType rooType

    -- Assign read value to passed lvalue.
    writeAssignmentCode lvalue reg

-- Write statement for constants.
writeRooStatement exp@(Write _ (ExpConst _ c)) =
  do
    --- Prepare register.
    reg <- getNextRegister

    -- Insert comment.
    writeStatementComment exp

    -- Intialise register with passed constant value.
    case c of
      BoolConst b ->
        writeOzCode $
          concat $
            [ indent, "int_const r",
              show reg,
              ", ",
              (show (if b then 1 else 0))
            ]
      IntConst i -> writeOzCode $ concat $ [indent, "int_const r", show reg, ", ", (show i)]
      StrConst s -> writeOzCode $ concat $ [indent, "string_const r", show reg, ", \"", s, "\""]

    -- Find required Oz builtin function.
    let oz_func = case c of
          BoolConst _ -> "print_bool"
          IntConst _ -> "print_int"
          StrConst _ -> "print_string"
    -- Call built-in print function
    writeOzCode $ concat $ [indent, "call_builtin ", oz_func]

-- Write statement.
writeRooStatement (Write pos lvalue) =
  do
    -- Prepare register.
    reg <- getNextRegister

    -- Insert comment.
    writeStatementComment (Write pos lvalue)

    -- Insert expression value to register.
    refType <- writeRooExpression reg lvalue

    case (refTypeToRooType refType) of
      BoolType ->
        do
          writeOzCode $ concat [indent, "call_builtin print_bool"]
      IntType ->
        do
          writeOzCode $ concat [indent, "call_builtin print_int"]
      _ ->
        do
          error $ concat $ [writeSourcePos pos, "Cannot write passed value."]

-- Writeln statement
writeRooStatement (Writeln pos lvalue) =
  do
    -- Write lvalue.
    writeRooStatement (Write pos lvalue)

    -- Insert newline.
    writeOzCode $ concat [indent, "string_const r0, \"\\n\""]
    writeOzCode $ concat $ [indent, "call_builtin print_string"]

-- Call statement
writeRooStatement call@(Call pos ident params) =
  do
    -- Prepare register
    reg <- getNextRegister

    -- Get procedure Oz label.
    proc_name <- getProcedure ident pos

    -- Assert parameter length are matched.
    if (length proc_name) == (length params)
      then do
        -- Insert comment.
        writeStatementComment call

        -- Insert parameters to registers.
        compileRooExps ident reg params

        -- Insert Oz code to call procedure.
        writeOzCode $ concat $ [indent, "call proc_", ident]
      else
        error $
          concat $
            [ writeSourcePos pos,
              "Procedure ",
              ident,
              " requires ",
              show (length proc_name),
              " parameters, received ",
              show (length params),
              "."
            ]

-- If statement.
writeRooStatement (If pos expr stmts) =
  do
    -- Prepare register.
    reg <- getNextRegister

    -- Prepare label to branch if condition is broken.
    falseLabel <- getNextLabel

    -- Insert comment for condition.
    writeOzCode $ concat $ [indent, "# if ", ppExp expr]

    -- Compile condition.
    exprType <- writeRooExpression reg expr

    -- Assert condition is boolean.
    if (refTypeToRooType exprType) == BoolType
      then do
        -- Branch on false.
        writeOzCode $ concat $ [indent, "branch_on_false r0, ", falseLabel]

        writeOzCode $ concat [indent, "# then"]

        -- Compile true branch statements
        compileRooStatements stmts

        -- Exit if statement.
        writeOzCode $ concat $ [indent, "branch_uncond ", falseLabel]
        writeOzCode $ concat[indent, "# fi"]

        -- Insert label for branch_on_false
        writeOzCode $ concat $ [falseLabel, ":"]
      else
        error $
          concat $
            [writeSourcePos pos, "If statements require boolean condition."]

-- If/Else statment
writeRooStatement (IfElse pos expr trueStmts elseStmts) =
  do
    -- Prepare register.
    reg <- getNextRegister

    -- Labels for branch on false or exiting statement.
    falseLabel <- getNextLabel
    exitLabel <- getNextLabel

    writeOzCode $ concat $ [indent, "# if ", ppExp expr]

    -- Compile if condition,
    exprType <- writeRooExpression reg expr

    -- Assert condition is boolean
    if (refTypeToRooType exprType) == BoolType
      then do
        -- Add code for branch on false
        writeOzCode $ concat $ [indent, "branch_on_false r0, ", falseLabel]
        writeOzCode $ concat [indent, "# then"]

        -- Compile statements for true branch.
        compileRooStatements trueStmts

        -- Exit if statement from true.
        writeOzCode $ concat $ [indent, "branch_uncond ", exitLabel]

        -- Insert label for else
        writeOzCode $ concat $ [falseLabel, ":"]
        writeOzCode $ concat [indent, "# else"]

        -- Compile else statements.
        compileRooStatements elseStmts
        writeOzCode $ concat [indent, "# fi"]

        -- Exit if/else statement.
        writeOzCode $ concat $ [indent, "branch_uncond ",  exitLabel]
        writeOzCode $ concat $ [exitLabel, ":"]
      else
        error $
          concat $
            [writeSourcePos pos, "If statements require boolean condition."]

-- While statement
writeRooStatement (While pos expr stmts) =
  do
    -- Prepare register
    reg <- getNextRegister

    -- Prepare labels
    trueLabel <- getNextLabel
    falseLabel <- getNextLabel

    -- Insert guard comment.
    writeOzCode $ concat $ [indent, "# if ", ppExp expr]

    -- Insert true label.
    writeOzCode $ concat $ [trueLabel, ":"]

    -- Compile while guard.
    guardType <- writeRooExpression reg expr

    -- Assert guard is a boolean statement
    if (refTypeToRooType guardType) == BoolType
      then do
        -- Add code for branch on false
        writeOzCode $ concat $ [indent, "branch_on_false r0, ", falseLabel]
        writeOzCode $ concat [indent, "# do"]

        -- Compile while statements.
        compileRooStatements stmts

        -- Go back to start of loop.
        writeOzCode $ concat $ [indent, "branch_uncond ",  trueLabel]
        writeOzCode $ concat [indent, "# od"]

        -- Label to exit loop
        writeOzCode $ concat $ [falseLabel, ":"]
      else
        error $
          concat $
            [writeSourcePos pos, "While statements requires a boolean guard condition."]

-----------------------------------------------------------------
-- Function return the Roo type of an lvalue.
-----------------------------------------------------------------
getLvalRooType :: LValue -> State SymbolTable RooType
getLvalRooType (LId pos ident) =
  do
    -- let test_pos = newPos "test" line col
    (_, btype, _, _) <- getVariable ident (SingleData) pos
    return (refTypeToRooType btype)

-- Array
getLvalRooType (LIdArrayRef pos ident _) =
  do
    (_, btype, _, _) <- getVariable ident (MultiData 0) pos
    return (refTypeToRooType btype)

-- Record field
getLvalRooType (LIdFieldRef pos ident field) =
  do
    -- Get RefType
    (_, btype, _, _) <- getVariable ident (MultiData 0) pos

    -- Extract RecordType
    let rooType = refTypeToRooType btype

    -- Get type of record field
    ret <- getFieldType rooType field

    return ret

-- Record field inside array.
getLvalRooType (LIdArrayFieldRef pos ident _ field) =
  do
    -- Get RefType
    (_, btype, _, _) <- getVariable ident (MultiData 0) pos

    -- Extract RecordType
    let rooType = refTypeToRooType btype

    -- Get type of record field
    ret <- getFieldType rooType field

    return ret

-----------------------------------------------------------------
-- Function compiles a Roo expression to Oz code.
-----------------------------------------------------------------
writeRooExpression :: Integer -> Exp -> State SymbolTable RefType
-- Boolean constant.
writeRooExpression reg (ExpConst _ (BoolConst bool)) =
  do
    let boolInt = if bool then 1 else 0
    -- Initial bool constant
    writeOzCode $ concat $ [indent, "int_const r", show reg, ", ", show boolInt]
    return (VarType BoolType)

-- Integer constant.
writeRooExpression reg (ExpConst _ (IntConst i)) =
  do
    -- Initial integer constant
    writeOzCode $ concat $ [indent, "int_const r", show reg, ", ", show i]
    return (VarType IntType)

-- Compile binary operations
writeRooExpression reg (BinOpExp pos op expr1 expr2) =
  case op of
    (Arop _) -> writeArop (binopToOz op) reg expr1 expr2 pos
    (Relop Op_eq) -> writeEqop (binopToOz op) reg expr1 expr2 pos
    (Relop Op_ne) -> writeEqop (binopToOz op) reg expr1 expr2 pos
    (Relop _) -> writeRelop (binopToOz op) reg expr1 expr2 pos
    (Logop Op_or) -> writeLogop "or" "true" reg expr1 expr2 pos
    (Logop Op_and) -> writeLogop "and" "false" reg expr1 expr2 pos
-- Negating a boolean expression.
writeRooExpression reg (Not pos exp) =
  do
    -- Get expression Roo type.
    rooType <- writeRooExpression reg exp

    -- Assert boolean.
    if (refTypeToRooType rooType) == BoolType
      then do
        -- Negate value.
        writeOzCode $ concat $ [indent, "not r", show reg, ", r", show reg]
        return (VarType BoolType)
      else error $ concat $ [writeSourcePos pos, "Can't negate non-boolean value."]

-- Unary minus
writeRooExpression reg (UnaryMinus pos expr) =
  do
    rooType <- writeRooExpression reg expr

    -- Assert integer.
    if (refTypeToRooType rooType) == IntType
      then do
        -- Reverse sign.
        writeOzCode $ concat $ [indent, "neg_int r", show reg, ", r", show reg]
        return rooType
      else error $ concat $ [writeSourcePos pos, "Can't use unary minus on non-integer value."]

-- Lvalue of form <id>
writeRooExpression reg (Lval pos (LId _ ident)) =
  do
    -- Look up variable
    (isRef, baseType, dataType, slot) <- getVariable ident (SingleData) pos

    if dataType == SingleData
      then
        if not isRef
          then do
            -- Load into register.
            writeOzCode $ concat $ [indent, "load r", show reg, ", ", show slot]
            return baseType
          else -- Call by reference.
          do
            -- Load value from slot into register
            writeOzCode $ concat $ [indent, "load r", show reg, ", ", show slot]
            -- Load address value from register into register
            writeOzCode $ concat $ [indent, "load_indirect r", show reg, ", r", show reg]
            return baseType
      else
        if not isRef
          then do
            error $ concat $ [writeSourcePos pos, "Expected a referenced variable."]
          else -- Call by reference
          do
            -- Load value from slot into register
            writeOzCode $ concat $ [indent, "load r", show reg, ", ", show slot]
            -- Load address value from register into register
            writeOzCode $ concat $ [indent, "load_indirect r", show reg, ", r", show reg]
            return baseType

-- Lvalue of form <id>[<exp>]
writeRooExpression reg (Lval pos (LIdArrayRef _ ident exp)) =
  do
    -- Get variable.
    (isRef, baseType, dataType, slot) <- getVariable ident (MultiData 0) pos

    -- Prepare register.
    offsetReg <- getNextRegister

    -- Get Roo type of array.
    let rooType = refTypeToRooType baseType

    -- Size of 1 cell in array.
    cell_size <- getRooTypeSize rooType

    -- Compile stack slot number into register, slot number = cell_size * <exp>.
    exprType <-
      writeRooExpression
        offsetReg
        (BinOpExp pos (Arop Op_mul) exp (ExpConst pos (IntConst cell_size)))

    case dataType of
      (MultiData _n) ->
        do
          -- Check array index type
          if (refTypeToRooType exprType) == IntType
            then do
              ret <- writeLoadOffsetToRegister isRef rooType reg offsetReg slot
              return ret
            else error $ concat $ [writeSourcePos pos, "Require array index to be an integer."]
      SingleData ->
        error $ concat $ [writeSourcePos pos, "Expected an array."]

-- Lvalue of form <id>.<id>
writeRooExpression reg (Lval pos (LIdFieldRef _ ident field)) =
  do
    -- Load variable.
    (isRef, baseType, dataType, slot) <- getVariable ident (MultiData 0) pos

    -- Prepare an offset register
    offsetReg <- getNextRegister

    -- Get the type of the record
    let recordRooType = refTypeToRooType baseType

    -- Find offset of field within record.
    fieldOffset <- getFieldOffset recordRooType field

    -- Prepare registers to provide offset.
    exprType <- writeRooExpression offsetReg (ExpConst pos (IntConst fieldOffset))

    -- Get underlying type of field.
    rooType <- getFieldType recordRooType field

    case dataType of
      (MultiData _) ->
        do
          -- Check array index type
          if exprType == (VarType IntType)
            then do
              ret <- writeLoadOffsetToRegister isRef rooType reg offsetReg slot
              return ret
            else error $ concat $ [writeSourcePos pos, "Array index must be integer."]
      SingleData ->
        error $ concat $ [writeSourcePos pos, "Expected a record type"]

-- Lvalue of form <id>[exp].<id>
writeRooExpression reg (Lval pos (LIdArrayFieldRef _ ident expr field)) =
  do
    -- Load the array
    (isRef, baseType, dataType, slot) <- getVariable ident (MultiData 0) pos

    -- Prepare an offset register
    offsetReg <- getNextRegister

    -- Get the type of the record
    let recordRooType = refTypeToRooType baseType

    -- Find offset of field within record
    field_offset <- getFieldOffset recordRooType field

    -- Size of array cells.
    cell_size <- getRooTypeSize recordRooType

    -- Offset to start of desired cell in array.
    let array_offset = (BinOpExp pos (Arop Op_mul) expr (ExpConst pos (IntConst cell_size)))

    -- Compile access to required stack slot = array_offset + field_offset
    exprType <-
      writeRooExpression
        offsetReg
        (BinOpExp pos (Arop Op_add) array_offset (ExpConst pos (IntConst field_offset)))

    -- Get underlying type of field
    rooType <- getFieldType recordRooType field

    case dataType of
      (MultiData _) ->
        do
          -- Check array index type
          if exprType == (VarType IntType)
            then do
              ret <- writeLoadOffsetToRegister isRef rooType reg offsetReg slot
              return ret
            else error $ concat $ [writeSourcePos pos, "Array index must be an integer."]
      SingleData ->
        error $ concat $ [writeSourcePos pos, "Expected a record type."]

-----------------------------------------------------------------
-- Function assigns an lvalue to a register value.
-----------------------------------------------------------------
writeAssignmentCode :: LValue -> Integer -> State SymbolTable ()
-- Lvalue of form <id>
writeAssignmentCode (LId pos ident) reg =
  do
    -- Get variable
    (isRef, _, _, slot) <- getVariable ident (SingleData) pos
    if isRef
      then do
        -- Call by reference.
        nextReg <- getNextRegister
        -- Load address to register
        writeOzCode $ concat $ [indent, "load r", show nextReg, ", ", show slot]
        -- Store indirect to address
        writeOzCode $ concat $ [indent, "store_indirect r", show nextReg, ", r", show reg]
      else writeOzCode $ concat $ [indent, "store ", show slot, ", r", show reg]

-- Lvalue of form <id>[exp]
writeAssignmentCode (LIdArrayRef pos ident index) reg =
  do
    -- Get variable.
    (isRef, refType, _, slot) <- getVariable ident (MultiData 0) pos

    -- Prepare an offset register
    offsetReg <- getNextRegister

    -- Get size of array in cell.
    let rooType = refTypeToRooType refType
    cell_size <- getRooTypeSize rooType

    -- Prepare register for offset == index * cell_size
    exprType <-
      writeRooExpression
        offsetReg
        (BinOpExp pos (Arop Op_mul) index (ExpConst pos (IntConst cell_size)))

    -- Assert index is integer.
    if (refTypeToRooType exprType) == IntType
      then writeAssignmentOffsetCode offsetReg slot reg isRef
      else error $ concat $ [writeSourcePos pos, "Array index must be an integer."]

-- Lvalue of form <id>.<id>
writeAssignmentCode (LIdFieldRef pos ident field) reg =
  do
    -- Get variable.
    (isRef, refType, _, slot) <- getVariable ident (MultiData 0) pos

    -- Prepare offset regsiter
    offsetReg <- getNextRegister

    -- Get rootype of field
    let rooType = refTypeToRooType refType

    -- Get offset value of field with element
    fieldOffset <- getFieldOffset rooType field

    -- Prepare offset access to field.
    writeRooExpression offsetReg (ExpConst pos (IntConst fieldOffset))

    -- Assign value.
    writeAssignmentOffsetCode offsetReg slot reg isRef

-- Lvalue of form <id>[exp].<id>
writeAssignmentCode (LIdArrayFieldRef pos ident exp field) reg =
  do
    -- Get variable
    (isRef, baseType, _, slot) <- getVariable ident (MultiData 0) pos

    -- Prepare an offset register.
    offsetReg <- getNextRegister

    -- Get the type of the record.
    let recordRooType = refTypeToRooType baseType

    -- Find offset of field within record.
    fieldOffset <- getFieldOffset recordRooType field

    -- Get offset of cell within array.
    cell_size <- getRooTypeSize recordRooType
    let array_offset = (BinOpExp pos (Arop Op_mul) exp (ExpConst pos (IntConst cell_size)))

    -- Compile offset register to access required slot offset by array index and record field.
    exprType <-
      writeRooExpression
        offsetReg
        (BinOpExp pos (Arop Op_add) array_offset (ExpConst pos (IntConst fieldOffset)))

    -- Check if index is int type
    if (refTypeToRooType exprType) == IntType
      then writeAssignmentOffsetCode offsetReg slot reg isRef
      else error $ concat $ [writeSourcePos pos, "Array index must be an integer."]

-----------------------------------------------------------------
-- Assigns a register's value to a stack slot.
-----------------------------------------------------------------
writeAssignmentOffsetCode :: Integer -> Integer -> Integer -> Bool -> State SymbolTable ()
writeAssignmentOffsetCode offsetReg startSlot reg isRef =
  do
    -- Prepare a register
    nextReg <- getNextRegister

    if isRef
      then writeOzCode $ concat $ [indent, "load r", show nextReg, ", ", show startSlot]
      else writeOzCode $ concat $ [indent, "load_address r", show nextReg, ", ", show startSlot]

    -- Subtract address with offset
    writeOzCode $
      concat $
        [ indent, "sub_offset r",
          show nextReg,
          ", r",
          show nextReg,
          ", r",
          show offsetReg
        ]
    -- Store indirect to address
    writeOzCode $ concat $ [indent, "store_indirect r", show nextReg, ", r", show reg]

-----------------------------------------------------------------
-- Function load an address to a register.
-----------------------------------------------------------------
writeLoadAddress :: LValue -> Integer -> State SymbolTable ()
-- Lvalue of form <id>
writeLoadAddress (LId pos ident) dest =
  do
    -- Load variable
    (isRef, _, _, slot) <- getVariable ident (SingleData) pos
    if isRef
      then -- Load slot to register.
        writeOzCode $ concat $ [indent, "load r", show dest, ", ", show slot]
      else -- Load address to register.
        writeOzCode $ concat $ [indent, "load_address r", show dest, ", ", show slot]

-- Lvalue of form <id>[exp]
writeLoadAddress (LIdArrayRef pos ident exp) destReg =
  do
    -- Load variable.
    (isRef, refType, _, slot) <- getVariable ident (MultiData 0) pos

    -- Prepare register.
    offsetReg <- getNextRegister

    -- Get Roo type of array.
    let rooType = refTypeToRooType refType

    -- Size of 1 cell in array.
    cell_size <- getRooTypeSize rooType

    -- Compile stack slot number into register, slot number = cell_size * <exp>.
    exprType <-
      writeRooExpression
        offsetReg
        (BinOpExp pos (Arop Op_mul) exp (ExpConst pos (IntConst cell_size)))

    -- Assert integer is index
    if (refTypeToRooType exprType) == IntType
      then writeStoreOffsetAddress offsetReg slot destReg isRef
      else error $ concat $ [writeSourcePos pos, "Array index must be an integer."]

-- Lvalue of form <id>.<id>
writeLoadAddress (LIdFieldRef pos ident field) destReg =
  do
    -- Load variable.
    (isRef, refType, _, slot) <- getVariable ident (MultiData 0) pos

    -- Prepare offset regsiter
    offsetReg <- getNextRegister

    -- Get rootype of field
    let rooType = refTypeToRooType refType

    -- Get offset value of field with element
    fieldOffset <- getFieldOffset rooType field

    -- Prepare offset access
    writeRooExpression offsetReg (ExpConst pos (IntConst fieldOffset))

    writeStoreOffsetAddress offsetReg slot destReg isRef

-- Lvalue of form <id>[exp].<id>
writeLoadAddress (LIdArrayFieldRef pos ident exp field) destReg =
  do
    -- Load variable.
    (isRef, refType, _, slot) <- getVariable ident (MultiData 0) pos

    -- Prepare an offset register
    reg1 <- getNextRegister

    -- Get the type of the record
    let recordRooType = refTypeToRooType refType

    -- Find offset of field within record
    fieldOffset <- getFieldOffset recordRooType field

    -- Get stack offset for desired array cell.
    cell_size <- getRooTypeSize recordRooType
    let array_offset = (BinOpExp pos (Arop Op_mul) exp (ExpConst pos (IntConst cell_size)))

    -- Move up the array and across the field offset
    exprType <-
      writeRooExpression
        reg1
        ( BinOpExp
            pos
            (Arop Op_add)
            array_offset
            (ExpConst pos (IntConst fieldOffset))
        )

    if (refTypeToRooType exprType) == IntType
      then writeStoreOffsetAddress reg1 slot destReg isRef
      else error $ concat $ [writeSourcePos pos, "Array index must be an integer."]

-----------------------------------------------------------------
-- Function load a register into another register that has
-- been offsetted.
-----------------------------------------------------------------
writeLoadOffsetToRegister ::
  Bool ->
  RooType ->
  Integer ->
  Integer ->
  Integer ->
  State SymbolTable RefType
writeLoadOffsetToRegister isRef rooType reg offsetReg slot =
  do
    if isRef
      then -- Load address from offsetted slot into register

        writeOzCode $ concat $ [indent, "load r", show reg, ", ", show slot]
      else -- Load address from slot into register

        writeOzCode $ concat $ [indent, "load_address r", show reg, ", ", show slot]

    -- Calculate address using offset
    writeOzCode $
      concat $
        [ indent, "sub_offset r",
          show reg,
          ", r",
          show reg,
          ", r",
          show offsetReg
        ]

    -- Load value indirectly to address
    writeOzCode $ concat $ [indent, "load_indirect r", show reg, ", r", show reg]

    return (VarType rooType)

-----------------------------------------------------------------
-- Function offsets a register.
-----------------------------------------------------------------
writeStoreOffsetAddress :: Integer -> Integer -> Integer -> Bool -> State SymbolTable ()
writeStoreOffsetAddress offsetReg startSlot destReg isRef =
  do
    if isRef
      then writeOzCode $ concat $ [indent, "load r", show destReg, ", ", show startSlot]
      else -- Load address of start slot

        writeOzCode $ concat $ [indent, "load_address r", show destReg, ", ", show startSlot]
    -- Calculate the offset
    writeOzCode $
      concat $
        [ indent, "sub_offset r",
          show destReg,
          ", r",
          show destReg,
          ", r",
          show offsetReg
        ]

-----------------------------------------------------------------
-- Function compiles Roo expressions to Oz code.
-----------------------------------------------------------------
compileRooExps :: String -> Integer -> [Exp] -> State SymbolTable ()
compileRooExps _ _ [] = return ()
compileRooExps ident n (e : es) =
  do
    let pos = getPosition e

    -- Get variable.
    (isRef, baseType) <- getProcedureParam ident n

    if not isRef
      then -- Call by value
      do
        exprType <- writeRooExpression n e
        setRegisterCounter (n + 1)

        if exprType == baseType
          then do
            n1 <- getNextRegister
            compileRooExps ident n1 es
          else error $ concat $ [writeSourcePos pos, "Procedure parameters do not match."]
      else -- Call by reference
      case e of
        (Lval _ lvalue) ->
          do
            -- Get roo type of lvalue.
            stmtVarType <- getLvalRooType lvalue

            -- Assert same types.
            if stmtVarType == (refTypeToRooType baseType)
              then do
                -- Load lvalue into register.
                writeLoadAddress lvalue n

                -- Adjust symbol table register counter.
                setRegisterCounter (n + 1)

                nextReg <- getNextRegister
                -- Compile rest of expressions.
                compileRooExps ident nextReg es
              else error $ concat $ [writeSourcePos pos, "Procedure parameters do not match."]
        _ ->
          do
            error $ concat $ [writeSourcePos pos, "Cannot pass a non-lvalue to procedure."]

-----------------------------------------------------------------
-- Function compiles arithmetic Roo expressions to Oz code.
-----------------------------------------------------------------
writeArop ::
  String ->
  Integer ->
  Exp ->
  Exp ->
  Position ->
  State SymbolTable RefType
writeArop s reg lexp rexp pos =
  do
    nextReg <- getNextRegister

    -- Compile lhs/rhs into separate register.
    lhs <- writeRooExpression reg lexp
    rhs <- writeRooExpression nextReg rexp

    if ((refTypeToRooType lhs) == (refTypeToRooType rhs)) && (refTypeToRooType lhs) == IntType
      then do
        -- Insert Oz code.
        writeOzCode $
          concat $
            [ indent, "",
              s,
              " r",
              show reg,
              ", r",
              show reg,
              ", r",
              show nextReg
            ]
        return (VarType IntType)
      else
        error $
          concat $
            [ writeSourcePos pos,
              "Can not ",
              s,
              " type ",
              show lhs,
              " with type ",
              show rhs
            ]

-----------------------------------------------------------------
-- Function compiles logical expressions.
-----------------------------------------------------------------
writeLogop ::
  String ->
  String ->
  Integer ->
  Exp ->
  Exp ->
  Position ->
  State SymbolTable RefType
writeLogop operator short_circuit reg lexp rexp pos =
  do
    -- Prepare next register.
    nextReg <- getNextRegister

    -- Prepare label.
    shortLabel <- getNextLabel

    -- Compile left hand side.
    lhs <- writeRooExpression reg lexp

    -- Insert short circuit branch
    writeOzCode $
      concat $
        [ indent, "branch_on_",
          short_circuit,
          " r",
          show reg,
          ", ",
          shortLabel
        ]

    -- Compile right hand size.
    rhs <- writeRooExpression nextReg rexp

    -- Assert both sides are boolean.
    if (refTypeToRooType lhs) == BoolType && (refTypeToRooType rhs) == BoolType
      then do
        -- Insert Oz code for logical expression.
        writeOzCode $
          concat $
            [ indent, "",
              operator,
              " r",
              show reg,
              ", r",
              show reg,
              ", r",
              show nextReg
            ]

        -- Short circuit label.
        writeOzCode $ concat $ [shortLabel, ":"]

        return (VarType BoolType)
      else
        error $
          concat $
            [ writeSourcePos pos,
              operator,
              " operation can not be used with types ",
              show lhs,
              " and ",
              show rhs
            ]

-----------------------------------------------------------------
-- Function compiles non equivalence relational expressions.
-----------------------------------------------------------------
writeRelop ::
  String ->
  Integer ->
  Exp ->
  Exp ->
  Position ->
  State SymbolTable RefType
writeRelop operator reg lexp rexp pos =
  do
    nextReg <- getNextRegister
    lhs <- writeRooExpression reg lexp
    rhs <- writeRooExpression nextReg rexp
    if (refTypeToRooType lhs) == (refTypeToRooType rhs)
      && (refTypeToRooType lhs == IntType || refTypeToRooType lhs == BoolType)
      then do
        -- Insert Oz code.
        writeOzCode $
          concat $
            [ indent, "",
              operator,
              " r",
              show reg,
              ", r",
              show reg,
              ", r",
              show nextReg
            ]

        return (VarType BoolType)
      else
        error $
          concat $
            [ writeSourcePos pos,
              "Can not compare",
              operator,
              " with type ",
              show lhs,
              " and type ",
              show rhs
            ]

-----------------------------------------------------------------
-- Function compiles equivalence Roo expressions to Oz code.
-----------------------------------------------------------------
writeEqop ::
  String ->
  Integer ->
  Exp ->
  Exp ->
  Position ->
  State SymbolTable RefType
writeEqop s reg lexp rexp pos =
  do
    nextReg <- getNextRegister
    lhs <- writeRooExpression reg lexp
    rhs <- writeRooExpression nextReg rexp

    if ((refTypeToRooType lhs) == (refTypeToRooType rhs))
      && (refTypeToRooType lhs == IntType || refTypeToRooType lhs == BoolType)
      then do
        -- Insert Oz code
        writeOzCode $
          concat $
            [ indent, "",
              s,
              " r",
              show reg,
              ", r",
              show reg,
              ", r",
              show nextReg
            ]
        return (VarType BoolType)
      else
        error $
          concat $
            [ writeSourcePos pos,
              "Can not compare ",
              s,
              " with type ",
              show lhs,
              " and type ",
              show rhs
            ]

-----------------------------------------------------------------
-- Function insert epilogue to Oz code.
-----------------------------------------------------------------
writeEpilogue :: Integer -> State SymbolTable ()
writeEpilogue n =
  do
    writeOzCode $ concat [indent, "# epilogue"]
    -- Pop the total size
    writeOzCode $ concat $ [indent, "pop_stack_frame ", show n]
    writeOzCode $ concat [indent, "return"]
    return ()
