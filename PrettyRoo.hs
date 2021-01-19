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
-- This file contains a Haskell program that outputs a
-- pretty-printed version of a Roo source code file, using an
-- AST generated for a file.
-----------------------------------------------------------------

module PrettyRoo (prettyPrint, ppStatement, ppExp) where

import Data.List (intercalate)
import RooAST
import RooParser (ast)



-----------------------------------------------------------------
-- Converts an input Program to a pretty printed String.
-----------------------------------------------------------------
prettyPrint :: Program -> String
prettyPrint (Program records arrays procedures) =
  ppfile
  where
    ppfile =
      intercalate "\n\n" $
        filter (not . null) [file_header, prettyProcedures]

    -- Space records and arrays across separate lines.
    -- Remove empty definitions if they exist.
    file_header =
      intercalate "\n" $
        filter (not . null) [formatRecords, prettyArrays]

    -- Format records, spaced with a blank line between them.
    formatRecords = intercalate "\n" (map ppRecord records)

    -- Format arrays, printed onto separate lines.
    prettyArrays = intercalate "\n" (map ppArray arrays)

    -- Format procedures, spaced with a blank line between them.
    prettyProcedures = intercalate "\n\n" (map ppProcedure procedures)


-----------------------------------------------------------------
-- Pretty prints a Roo record.
-----------------------------------------------------------------
ppRecord :: Record -> String
ppRecord (Record _ declarations name) =
  intercalate "\n" ["record", body]
  where
    -- Format field declarations.
    body = concat ["    { ", decls, "\n    } ", name, ";"]
    decls = ppFieldDeclBody declarations


-----------------------------------------------------------------
-- Formats and collects record field declarations.
-----------------------------------------------------------------
ppFieldDeclBody :: [FieldDecl] -> String
ppFieldDeclBody decls = intercalate "\n    ; " (map ppFieldDecl decls)


-----------------------------------------------------------------
-- Pretty print each record field declarations
-----------------------------------------------------------------
ppFieldDecl :: FieldDecl -> String
ppFieldDecl (FieldDecl _ ident rootype) =
  concat [show rootype, " ", ident]


-----------------------------------------------------------------
-- Formats a Roo array definition.
-----------------------------------------------------------------
ppArray :: Array -> String
ppArray (Array _ ident rootype) =
  concat ["array", show rootype, " ", ident, ";"]


-----------------------------------------------------------------
-- Pretty prints a Roo procedure.
-----------------------------------------------------------------
ppProcedure :: Procedure -> String
ppProcedure (Procedure _ ident formals declarations stmts) =
  if null decls
    then -- No local variable declarations.
      intercalate "\n" [header, body]
    else
      intercalate "\n" [header, decls, body]
  where
    header = concat ["procedure ", ident, " ", ppFormals formals]
    decls = intercalate "\n" (ppLocalDeclarations declarations)
    body = concat ["{\n", intercalate "\n" (ppStatements stmts), "\n}"]


-----------------------------------------------------------------
-- Returns a pretty printed list of formal parameters.
-----------------------------------------------------------------
ppFormals :: [FormalParamSpec] -> String
ppFormals formals =
  '(' : (intercalate ", " (map ppFormal formals) ++ ")")


-----------------------------------------------------------------
-- Pretty prints a formal parameter.
-----------------------------------------------------------------
ppFormal :: FormalParamSpec -> String
ppFormal (FormalParamSpec _ mode ident) =
  concat [show mode, " ", ident]
  -- where
  --   mode = ppMode passingMode

-----------------------------------------------------------------
-- Returns a list of pretty printed declarations.
-----------------------------------------------------------------
ppLocalDeclarations :: [LocalDecl] -> [String]
ppLocalDeclarations decls =
  indentation . (map ppLocalDeclaration) $ decls


-----------------------------------------------------------------
-- Pretty prints a Roo local variable declaration.
-----------------------------------------------------------------
ppLocalDeclaration :: LocalDecl -> String
ppLocalDeclaration (LocalDecl _ idents rooType) =
  concat [show rooType, " ", ppIdents, ";"]
  where
    -- Comma separated variable identifiers
    ppIdents = intercalate ", " idents


-----------------------------------------------------------------
-- Returns a pretty printed, indented list of statements.
-----------------------------------------------------------------
ppStatements :: [Stmt] -> [String]
ppStatements stmts =
  indentation . concat . (map ppStatement) $ stmts


-----------------------------------------------------------------
-- Pretty prints a statement into a list of strings.
-----------------------------------------------------------------
ppStatement :: Stmt -> [String]
ppStatement stmt =
  case stmt of
    -- Assignment statement.
    Assign _ lval exp ->
      [concat [ppLvalue lval, " <- ", ppExp exp, ";"]]
    -- Read statement.
    Read _ lval ->
      [concat ["read ", ppLvalue lval, ";"]]
    -- Write statement.
    Write _ exp ->
      [concat ["write ", ppExp exp, ";"]]
    -- Writeln statement.
    Writeln _ exp ->
      [concat ["writeln ", ppExp exp, ";"]]
    -- Call statement.
    Call _ p exps ->
      [concat ["call ", p, "(", ppExpList exps, ");"]]
    -- If statement.
    If _ exp stmts ->
      concat ["if ", ppExp exp, " then"] :
      (ppStatements stmts) ++ ["fi"]
    -- If/else statement.
    IfElse _ exp ifStmts elseStmts ->
      concat ["if ", ppExp exp, " then"] :
      (ppStatements ifStmts) ++ ["else"]
        ++ (ppStatements elseStmts)
        ++ ["fi"]
    -- While statement.
    While _ exp stmts ->
      concat ["while ", ppExp exp, " do"] :
      (ppStatements stmts) ++ ["od"]


-----------------------------------------------------------------
-- Pretty prints a Roo lvalue.
-----------------------------------------------------------------
ppLvalue :: LValue -> String
ppLvalue lvalue =
  case lvalue of
    -- <id>
    LId _ ident ->
      ident
    -- <id>.<id>
    LIdArrayRef _ ident exp ->
      concat [ident, "[", ppExp exp, "]"]
    -- <id>[ <exp> ]
    LIdFieldRef _ ident field ->
      concat [ident, ".", field]
    -- <id>[ <exp> ].<id>
    LIdArrayFieldRef _ ident exp field ->
      concat [ident, "[", ppExp exp, "].", field]


-----------------------------------------------------------------
-- Returns a pretty printed list of expressions.
-----------------------------------------------------------------
ppExpList :: [Exp] -> String
ppExpList exps =
  intercalate ", " (map ppExp exps)


-----------------------------------------------------------------
-- Pretty prints an expression.
-----------------------------------------------------------------
ppExp :: Exp -> String
ppExp exp =
  case exp of
    UnaryMinus _ n ->
      case n of
        (BinOpExp _ op e1 e2) -> "-(" ++ ppExp n ++ ")"
        _ -> '-' : ppExp n
    Not _ e ->
      case e of
        (BinOpExp _ op e1 e2) -> "not (" ++ ppExp e ++ ")"
        _ -> "not " ++ ppExp e
    (BinOpExp _ op e1 e2) ->
      concat [left1, ppExp e1, left2, show op, right1, ppExp e2, right2]
      where
        left1 = if leftP then "(" else ""
        left2 = if leftP then ")" else ""
        right1 = if rightP then "(" else ""
        right2 = if rightP then ")" else ""
        leftP = needParensLeft op e1
        rightP = needParensRight op e2
    Lval _ lvalue ->
      ppLvalue lvalue
    ExpConst _ const ->
      show const

-----------------------------------------------------------------
-- Determines if a left sub-expression of an expression
-- needs a parentheses.
-----------------------------------------------------------------
needParensLeft :: Binop -> Exp -> Bool
needParensLeft op exp =
  case exp of
    (BinOpExp _ x e1 e2) ->
      (opToInt x) < (opToInt op)
    (Not _ _) ->
      (opToInt op) > 1
    _ ->
      False


-----------------------------------------------------------------
-- Determines if a right sub-expression of an expression
-- needs a parentheses.
-----------------------------------------------------------------
needParensRight :: Binop -> Exp -> Bool
needParensRight op exp =
  case exp of
    (BinOpExp _ x e1 e2) ->
      (opToInt x) <= (opToInt op)
    (Not _ _) ->
      (opToInt op) <= 1
    _ ->
      False


-----------------------------------------------------------------
-- Returns an integer value for precedence ordering of
-- binary operators, where smallest precedence = 0, and largest
-- precedence = 4.
-----------------------------------------------------------------
opToInt :: Binop -> Int
opToInt op =
  case op of
    (Logop Op_or) -> 0
    (Logop Op_and) -> 1
    (Relop _) -> 2
    (Arop Op_sub) -> 3
    (Arop Op_add) -> 3
    (Arop Op_mul) -> 4
    (Arop Op_div) -> 4


-----------------------------------------------------------------
-- Returns a higher-order function that maps an indentation to a
-- list of strings.
-----------------------------------------------------------------
indentation :: [String] -> [String]
indentation =
  map ("    " ++)
