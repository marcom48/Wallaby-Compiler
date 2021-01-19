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
-- This file contains a the data types to create an AST for the
-- Roo language.
-----------------------------------------------------------------

module RooAST where

-- Type alias for identifer.
type Ident = String

-- Type alias for source code position (row, column).
type Position =
  (Int, Int)


-----------------------------------------------------------------
-- Roo data types.
-----------------------------------------------------------------
data RooType
  = BoolType            -- Boolean type
  | IntType             -- Integer type
  | RecordType String   -- Record type
  deriving (Eq)
instance Show RooType where
    show (BoolType) = "boolean"
    show (IntType) = "integer"
    show (RecordType t) = t

-----------------------------------------------------------------
-- Roo reference types.
-----------------------------------------------------------------
data RefType
  = VarType RooType             -- Varaible
  | ArrayType Integer RooType   -- Array
  deriving (Eq)
instance Show RefType where
    show (VarType t) = show t
    show (ArrayType n t) = concat ["[", show n, "] ", show t]

-----------------------------------------------------------------
-- Roo lvalues.
-----------------------------------------------------------------
data LValue
  = LId Position Ident
  | LIdFieldRef Position Ident Ident
  | LIdArrayRef Position Ident Exp
  | LIdArrayFieldRef Position Ident Exp Ident
  deriving (Show, Eq)


-----------------------------------------------------------------
-- Roo constants.
-----------------------------------------------------------------
data Const
  = BoolConst Bool      -- Boolean constant
  | IntConst Integer    -- Integer Constant
  | StrConst String     -- String Constant
  deriving (Eq)
instance Show Const where
    show (BoolConst False) = "false"
    show (BoolConst True) = "true"
    show (IntConst n) = show n
    show (StrConst str) = '\"' : (str ++ "\"")

-----------------------------------------------------------------
-- Binary operators.
-----------------------------------------------------------------
data Binop
  = Arop Arop     -- Arithmetic operators
  | Logop Logop   -- Logical operators
  | Relop Relop   -- Relational operators
  deriving (Eq)
instance Show Binop where
    show (Logop op) = show op
    show (Arop op) = show op
    show (Relop op) = show op

-----------------------------------------------------------------
-- Arithmetic operators.
-----------------------------------------------------------------
data Arop
  = Op_sub -- Subtraction (-)
  | Op_add -- Addition (+)
  | Op_mul -- Multiplication (*)
  | Op_div -- Division (/)
  deriving (Eq)
instance Show Arop where
    show (Op_sub) = " - "
    show (Op_add) = " + "
    show (Op_mul) = " * "
    show (Op_div) = " / "


-----------------------------------------------------------------
-- Logical operators.
-----------------------------------------------------------------
data Logop
  = Op_or     -- OR operator
  | Op_and    -- AND operator
  | Op_not
  deriving (Eq)
instance Show Logop where
    show (Op_and) = " and "
    show (Op_or) = " or "


-----------------------------------------------------------------
-- Relational operators.
-----------------------------------------------------------------
data Relop
  = Op_eq -- Equals (=)
  | Op_ne -- Not equals (!=)
  | Op_ge -- Greater than or equal to (\>=) (Ignore slash)
  | Op_le -- Less than (<=)
  | Op_gt -- Greater than (>)
  | Op_lt -- Less than (<)
  deriving (Eq)
instance Show Relop where
    show (Op_eq) = " = "
    show (Op_ne) = " != "
    show (Op_gt) = " > "
    show (Op_lt) = " < "
    show (Op_ge) = " >= "
    show (Op_le) = " <= "


-----------------------------------------------------------------
-- Roo expressions.
-----------------------------------------------------------------
data Exp
  = Lval Position LValue -- Lvalue reference
  | ExpConst Position Const -- Constant expression
  | UnaryMinus Position Exp -- Unary minus expression
  | Not Position Exp -- Negation expression
  | BinOpExp Position Binop Exp Exp -- Binary operator expression
  deriving (Show, Eq)

-----------------------------------------------------------------
-- Roo statements.
-----------------------------------------------------------------
data Stmt
  = Assign Position LValue Exp -- Assignment
  | Read Position LValue -- Read
  | Write Position Exp -- Write
  | Writeln Position Exp -- Write line
  | Call Position Ident [Exp] -- Call
  | If Position Exp [Stmt] -- If statement
  | IfElse Position Exp [Stmt] [Stmt] -- If/else statement
  | While Position Exp [Stmt] -- While loop
  deriving (Show, Eq)

-----------------------------------------------------------------
-- Roo parameter type/mode indicator types.
-----------------------------------------------------------------
data ParMode
  = PAlias String
  | PBoolean
  | PInt
  | PBoolV
  | PIntV
  deriving (Eq)
instance Show ParMode where
    show (PAlias a) = a
    show (PBoolean) = "boolean"
    show (PInt) = "integer"
    show (PBoolV) = "boolean val"
    show (PIntV) = "integer val"
    
-----------------------------------------------------------------
-- Roo formal parameter. Consists of a parameter type/mode
-- indicator, and an identifier.
-----------------------------------------------------------------
data FormalParamSpec
  = FormalParamSpec Position ParMode Ident
  deriving (Show, Eq)

-----------------------------------------------------------------
-- Roo prodecure. Consists of zero or more formal parameters,
-- zero or more local variable declarations, and a non-empty
-- list of statements.
-----------------------------------------------------------------
data Procedure
  = Procedure Position Ident [FormalParamSpec] [LocalDecl] [Stmt]
  deriving (Show, Eq)

-----------------------------------------------------------------
-- Roo local variable declaration. Consists of an identifier,
-- and a Roo type.
-----------------------------------------------------------------
data LocalDecl
  = LocalDecl Position [Ident] RefType
  deriving (Show, Eq)

-----------------------------------------------------------------
-- Roo array definition. Consists of an identifier,
-- and a Roo type.
-----------------------------------------------------------------
data Array
  = Array Position Ident RefType
  deriving (Show, Eq)

-----------------------------------------------------------------
-- Roo record field declaration. Consists of an identifier,
-- and a Roo type.
-----------------------------------------------------------------
data FieldDecl
  = FieldDecl Position Ident RefType
  deriving (Show, Eq)

-----------------------------------------------------------------
-- Roo record. Consists of a list of field decalarations,
-- and an identifier.
-----------------------------------------------------------------
data Record
  = Record Position [FieldDecl] Ident
  deriving (Show, Eq)

data ARecord
  = ARecord Position [(Ident, Integer, RooType)] Ident Integer
  deriving (Show, Eq)

-----------------------------------------------------------------
-- Roo program. A program consists of:
--  * Zero or more records
--  * Zero or more array declarations
--  * One or more procedures
-----------------------------------------------------------------
data Program
  = Program [Record] [Array] [Procedure]
  deriving (Show, Eq)