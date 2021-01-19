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
-- This file contains a Haskell program that uses the Parsec
-- library to parse and generate an AST for Roo source code
-- files.
-----------------------------------------------------------------

module RooParser (ast) where


import RooAST
import Text.Parsec
import Text.Parsec.Expr
    ( buildExpressionParser,
      Assoc(AssocNone, AssocLeft),
      Operator(Prefix, Infix) )
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Q

type Parser a =
  Parsec String Int a

scanner :: Q.TokenParser Int
scanner =
  Q.makeTokenParser
    ( emptyDef
        { Q.commentLine = "#",
          Q.nestedComments = True,
          Q.identStart = letter,
          Q.identLetter = alphaNum <|> oneOf "'_",
          Q.opStart = oneOf "+-*/=<>",
          Q.opLetter = oneOf "=",
          Q.reservedNames = rooReserved,
          Q.reservedOpNames = rooOpNames
        }
    )

-----------------------------------------------------------------
-- Parsec symbol aliases used for parsing.
-----------------------------------------------------------------
whiteSpace = Q.whiteSpace scanner

natural = Q.natural scanner

decimal = Q.decimal scanner

identifier = Q.identifier scanner

semi = Q.semi scanner

comma = Q.comma scanner

dot = Q.dot scanner

parens = Q.parens scanner

braces = Q.braces scanner

squares = Q.squares scanner

reserved = Q.reserved scanner

reservedOp = Q.reservedOp scanner

-----------------------------------------------------------------
-- Reserved words and operator symbols.
-----------------------------------------------------------------
rooReserved, rooOpNames :: [String]
rooReserved =
  [ "and",
    "array",
    "boolean",
    "call",
    "do",
    "else",
    "false",
    "fi",
    "if",
    "integer",
    "not",
    "od",
    "or",
    "procedure",
    "read",
    "record",
    "then",
    "true",
    "val",
    "while",
    "write",
    "writeln",
    ";"
  ]
rooOpNames =
  [ "or",
    "and",
    "not",
    "=",
    "!=",
    "<",
    "<=",
    ">",
    ">=",
    "+",
    "-",
    "*",
    "/",
    "-",
    "<-"
  ]

-----------------------------------------------------------------
--  pExp parses expressions. It is built using Parsec's powerful
--  buildExpressionParser and takes into account the operator
--  precedences and associativity specified in 'opTable' below.
-----------------------------------------------------------------
pExp :: Parser Exp
pExp =
  buildExpressionParser opTable pFac
    <?> "an expression"

pFac :: Parser Exp
pFac =
  choice [parens pExp, pNum, pBool, pLVExp, pString, pNot]
    <?> "an expression, number, boolean, lvalue, or string."

-- Sublists sorted by descending precedence.
opTable =
  [ 
    [prefix "-" UnaryMinus],
    [ binary "*" (Arop Op_mul),
      binary "/" (Arop Op_div)
    ],
    [ binary "+" (Arop Op_add),
      binary "-" (Arop Op_sub)
    ],
    [ relation "=" (Relop Op_eq),
      relation "!=" (Relop Op_ne),
      relation "<" (Relop Op_lt),
      relation "<=" (Relop Op_le),
      relation ">" (Relop Op_gt),
      relation ">=" (Relop Op_ge)
    ],
    [prefix "not" Not],
    [binary "and" (Logop Op_and)],
    [binary "or" (Logop Op_or)]
  ]

-- Binary operator definition.
binary name op =
  Infix
    ( do
        pos <- getPosition
        reservedOp name
        return (BinOpExp (filepos pos) op)
    )
    AssocLeft


-- Binary relational definition. No association.
relation name rel =
  Infix
    ( do
        pos <- getPosition
        reservedOp name
        return (BinOpExp (filepos pos) rel)
    )
    AssocNone

prefix name f =
  Prefix
    ( do
        
        pos <- getPosition
        reservedOp name
        return (f (filepos pos))
    )

-----------------------------------------------------------------
-- Function takes an input source code string and returns
-- either a parsed program, or a parse error.
-----------------------------------------------------------------
ast :: String -> Either ParseError Program
ast input =
  runParser rooParse 0 "" input


-----------------------------------------------------------------
-- Control function, begins parsing over an input file.
-----------------------------------------------------------------
rooParse :: Parser Program
rooParse =
  do
    -- Consume all whitespace before source code.
    whiteSpace
    p <- pProgram
    eof
    return p


-----------------------------------------------------------------
-- pProgram is the topmost parsing function. It parses the
-- source code body for zero or more records, followed by zero or
-- more arrays, followed by one or more procedures.
-----------------------------------------------------------------
pProgram :: Parser Program
pProgram =
  do
    -- Zero or more records.
    records <- many pRecord

    -- Zero or more arrays.
    arrays <- many pArray

    -- One or more procedures.
    procs <- many1 pProc

    return (Program records arrays procs)


-----------------------------------------------------------------
-- Parses a Roo record. Looks for keyword "record", followed by
-- one or more semicolon separated field declarations,
-- then an identifier, closed with a semicolon.
-----------------------------------------------------------------
pRecord :: Parser Record
pRecord =
  do
    reserved "record"
    pos <- getPosition

    -- Parse record field declarations.
    fielddecs <- braces (sepBy1 pFieldDecl semi)

    -- Parse recorder name.
    ident <- pIdent

    semi
    return (Record (filepos pos) fielddecs ident)


-----------------------------------------------------------------
-- Parses a record field declaration.
-----------------------------------------------------------------
pFieldDecl :: Parser FieldDecl
pFieldDecl =
  do
    -- Parse field type. Only allow boolean/integer.
    fieldtype <- pRooType True

    pos <- getPosition
    ident <- pIdent

    return (FieldDecl (filepos pos) ident (VarType fieldtype))


-----------------------------------------------------------------
-- Parses a Roo array definition. Looks for keyword "array",
-- followed by a size parameter, a type, then an identifier,
-- closed with a semicolon.
-----------------------------------------------------------------
pArray :: Parser Array
pArray =
  do
    reserved "array"
    pos <- getPosition

    -- Size is a natural positive number
    size <- squares (decimal)

    -- Allow boolean/integer/type alias.
    basetype <- pRooType False

    ident <- pIdent
    semi

    return (Array (filepos pos) ident (ArrayType (fromInteger size) basetype))


-----------------------------------------------------------------
-- Parses a Roo procedure. Lokos for keyword "procedure",
-- followed by the prodecure header and prodecure body.
-----------------------------------------------------------------
pProc :: Parser Procedure
pProc =
  do
    reserved "procedure"
    pos <- getPosition

    -- Parse header
    (ident, argspecs) <- pProcHead

    -- Parse Body.
    (localdecls, stmts) <- pProcbody

    return (Procedure (filepos pos) ident argspecs localdecls stmts)


-----------------------------------------------------------------
-- Parses a Roo procedure header. Requires an identigier,
-- followed by a comma-separated list of zero or more formal
-- parameters within a pair of parentheses.
-----------------------------------------------------------------
pProcHead :: Parser (Ident, [FormalParamSpec])
pProcHead =
  do
    ident <- pIdent

    -- Comma separated arguments
    argspecs <- parens (sepBy pArg comma)

    return (ident, argspecs)


-----------------------------------------------------------------
-- Parses a Roo formal parameter. Requires a parameter
-- type/mode indicator, followed by an identifier.
-----------------------------------------------------------------
pArg :: Parser FormalParamSpec
pArg =
  do
    -- Type/mode indicator.
    mode <- pParMode

    pos <- getPosition

    ident <- pIdent

    return (FormalParamSpec (filepos pos) mode ident)
    <?> "a formal argument - a parameter type/mode indicator,\
        \ which is one of these five: a type alias, boolean,\
        \ integer, boolean val, or integer val, followed by an identifier."


-----------------------------------------------------------------
-- Parses a Roo procedure formal parameter. One of: type alias,
-- boolean, integer, boolean val, integer val.
-----------------------------------------------------------------
pParMode :: Parser ParMode
pParMode =
      try (do reserved "boolean"; reserved "val"; return PBoolV)<|> 
      
      (try(do reserved "integer"; reserved "val"; return PIntV) <|>

      (try(do reserved "boolean"; return PBoolean) <|>
      (try(do reserved "integer"; return PInt) <|>
      try(do ident <- pIdent; return (PAlias ident)) )))
    <?> "a type/mode indicator, which is one of these five:\
        \ a type alias, boolean, integer, boolean val, or integer val."


-----------------------------------------------------------------
-- Parses a Roo procedure body. Consists of zero or more
-- local variable declarations, followed by a non-empty sequence
-- of statements, the statements enclosed in braces.
-----------------------------------------------------------------
pProcbody :: Parser ([LocalDecl], [Stmt])
pProcbody =
  do
    -- Declarations
    decls <- many pLocalDecl

    -- Statements
    stmts <- braces (many1 pStmt)

    return (decls, stmts)


-----------------------------------------------------------------
-- Parses a procedure body local variable declaration.
-- Requires a type name (boolean/integer/type alias), followed
-- by a non-empty list of comma-separated list of idenifiers,
-- the list terminated by a semicolon.
-----------------------------------------------------------------
pLocalDecl :: Parser LocalDecl
pLocalDecl =
  do
    pos <- getPosition

    -- Allow boolean/integer/type alias.
    rootype <- pRooType False

    -- One or more idenifiers.
    ident <- sepBy1 pIdent comma

    semi

    return (LocalDecl (filepos pos) ident (VarType rootype))
    <?> "a local variable declaration. A variable declaration consists of\
        \ a type name (boolean, integer, or a type alias), followed by a\
        \ non-empty comma-separated list of identifiers, the list terminated\
        \ with a semicolon. There may be any number of variable declarations,\
        \ in any order."


-----------------------------------------------------------------
-- Returns a type for a parse type name. If true, only allows
-- Roo base types.
-----------------------------------------------------------------
pRooType :: Bool -> Parser RooType
pRooType base =
  if base
    then
      do reserved "boolean"; return BoolType
        <|> do reserved "integer"; return IntType
        <?> "boolean or integer."
    else
      do reserved "boolean"; return BoolType
        <|> do reserved "integer"; return IntType
        <|> do ident <- pIdent; return (RecordType ident)
        <?> "boolean, integer, or a record alias."


-----------------------------------------------------------------
-- pStmt is the main parser for statements. Provides a control
-- flow for all types of Roo statements.
-----------------------------------------------------------------
pStmt, pAsg, pRead, pWrite, pWriteln, pCall, pIf, pWhile, pComp :: Parser Stmt
pStmt =
  choice [pAsg, pRead, pWrite, pWriteln, pCall, pComp]
-----------------------------------------------------------------
-- Roo assignment statement: <lvalue> <- <exp> ;
-----------------------------------------------------------------
pAsg =
  do
    pos <- getPosition
    lvalue <- pLvalue
    reservedOp "<-"
    rvalue <- pExp
    semi
    return (Assign (filepos pos) lvalue rvalue)
    <?> "an assignment statement."

-----------------------------------------------------------------
-- Roo read statement: read <lvalue> ;
-----------------------------------------------------------------
pRead =
  do
    pos <- getPosition
    reserved "read"
    lvalue <- pLvalue
    semi
    return (Read (filepos pos) lvalue)
    <?> "a read statement."

-----------------------------------------------------------------
-- Roo write statement: write <exp> ;
-----------------------------------------------------------------
pWrite =
  do
    pos <- getPosition
    reserved "write"
    exp <- (pExp)
    semi
    return (Write (filepos pos) exp)
    <?> "a write statement."

-----------------------------------------------------------------
-- Roo writeln statement: writeln <exp> ;
-----------------------------------------------------------------
pWriteln =
  do
    pos <- getPosition
    reserved "writeln"
    exp <- (pExp)
    semi
    return (Writeln (filepos pos) exp)
    <?> "a writeln statement."

-----------------------------------------------------------------
-- Roo call statement: call <id> ( <exp> ) ;
-----------------------------------------------------------------
pCall =
  do
    reserved "call"
    pos <- getPosition
    ident <- pIdent

    -- Expressions are zero or more comma-separated expressions.
    exps <- parens (sepBy pExp comma)
    semi
    return (Call (filepos pos) ident exps)
    <?> "a call statement."

-----------------------------------------------------------------
-- Control flow for Roo composite statements. Composite
-- statements have three forms:

-- * if <exp> then <stmt-list> fi

-- * if <exp> then <stmt-list> else <stmt-list> fi

-- * while <exp> do <stmt-list> od

-- <stmt-list> is a non-empty sequence of atomic, if,
-- if/else, while statements.
-----------------------------------------------------------------
pComp =
  do
    -- If statement.
    reserved "if"
    stmt <- pIf
    return stmt
    <|> do
      -- While statement.
      reserved "while"
      stmt <- pWhile
      return stmt
    <?> "a conditional (if/while) statement."

-----------------------------------------------------------------
-- Roo if statement. See pComp for format.
-----------------------------------------------------------------
pIf =
  do
    pos <- getPosition

    -- First expression guard.
    exp <- pExp
    reserved "then"

    -- Expression list from if guard.
    if_stmts <- many1 pStmt

    -- Check if there is an "else" statement.
    else_check <- pElse

    case else_check of
      Nothing -> return (If (filepos pos) exp if_stmts)
      Just else_stmts -> return (IfElse (filepos pos) exp if_stmts else_stmts)

-----------------------------------------------------------------
-- Parses for an "else" statement. Requires keyword "else"
-- followed by a non-empty sequence of atomic, if,
-- if/else, while statements.
-----------------------------------------------------------------
pElse :: Parser (Maybe [Stmt])
pElse =
  do
    reserved "else"
    stmts <- many1 pStmt
    reserved "fi"
    return (Just stmts)
    <|> do
      reserved "fi"
      return Nothing

-----------------------------------------------------------------
-- Roo while statement. See pComp for format.
-----------------------------------------------------------------
pWhile =
  do
    pos <- getPosition

    -- While guard
    guard <- pExp

    reserved "do"
    stmts <- many1 pStmt
    reserved "od"
    return (While (filepos pos) guard stmts)


-----------------------------------------------------------------
-- Parsers for string/integer/boolean literals, and Lvalues.
-----------------------------------------------------------------
pString, pNum, pBool, pLVExp, pNot :: Parser Exp
-----------------------------------------------------------------
-- A string literal is a sequence of characters between double
-- quotes. The sequence itself cannot contain double quotes or
-- newline/tab characters. It may, however, contain ‘\"’, ‘\n’,
-- and ‘\t’, respectively, to represent those characters.
-----------------------------------------------------------------
pString =
  do
    pos <- getPosition
    char '"'
    -- Ensure strings do not have tab, newline, or double
    -- quotation characters.
    str <- many (satisfy (`notElem` "\"\t\n"))
    char '"'
    whiteSpace
    return (ExpConst (filepos pos) (StrConst str))
    <?> "a string - a sequence of characters between\
        \ double quotes. The sequence itself cannot contain\
        \ double quotes or newline/tab characters. It may,\
        \ however, contain ‘\"’, ‘\n’, and ‘\t’, respectively,\
        \ to represent those characters."

-----------------------------------------------------------------
-- An integer literal is a sequence of digits, possibly
-- preceded by a minus sign.
-----------------------------------------------------------------
pNum =
  do
    pos <- getPosition
    n <- natural <?> ""
    return (ExpConst (filepos pos) (IntConst (fromInteger n :: Integer)))
    <|> do
      -- Parsing a negative number.
      pos <- getPosition
      reservedOp "-"
      exp <- pFac
      return (UnaryMinus (filepos pos) exp)
    <?> "a sequence of digits, possibly preceded by a minus sign."

-----------------------------------------------------------------
-- A boolean literal is false or true.
-----------------------------------------------------------------
pBool =
  do
    pos <- getPosition
    reserved "true"
    let ret = (BoolConst True)
    return (ExpConst (filepos pos) (BoolConst True))
    <|> do
      pos <- getPosition
      reserved "false"
      return (ExpConst (filepos pos) (BoolConst False))
    <?> "true or false."

-----------------------------------------------------------------
-- Returns a parsed lvalue wrapped within an Exp data type.
-----------------------------------------------------------------
pLVExp =
  do
    pos <- getPosition
    lvalue <- pLvalue
    return (Lval (filepos pos) lvalue)

-----------------------------------------------------------------
-- Parses a negation. Added in to compensate for Piazza's
-- "can of worms". XXXXXXX's developers recognise the 
-- redundancy of this function in most use cases.
-----------------------------------------------------------------
pNot = 
  do
    
    pos <- getPosition
    reserved "not"
    exp <- pExp
    return (Not (filepos pos) exp)


-----------------------------------------------------------------
-- Parses an lvalue. Requires a helper function to identify
-- the lvalue format type.
-----------------------------------------------------------------
pLvalue :: Parser LValue
pLvalue =
  do
    pos <- getPosition

    -- Get first identifier.
    ident <- pIdent

    -- Check format of lvalue
    lvalue <- pLvalueFormatCheck ident (filepos pos)

    return lvalue
    <?> "an lvalue"



-----------------------------------------------------------------
-- Checks lvalue format type from four options:
-- * <id>
-- * <id> . <id>
-- * <id> [ <exp> ]
-- * <id> [ <exp> ] . <id>
-- See do statements for format type they check.
-----------------------------------------------------------------
pLvalueFormatCheck :: String -> Position -> Parser LValue
pLvalueFormatCheck ident pos =
  do
    -- <id>.<id>
    dot
    field <- pIdent
    return (LIdFieldRef pos ident field)
    <|> do
      -- <id>[ <exp> ] || <id>[ <exp> ].<id>

      -- Get array size.
      exp <- squares (pExp)

      -- Check if field reference too.
      maybeField <- pMaybeArrayField

      -- Empty string -> no field reference
      case maybeField of
        Nothing -> return (LIdArrayRef pos ident exp)
        (Just field) -> return (LIdArrayFieldRef pos ident exp field)
    <|> do
      -- <id>
      return (LId pos ident)


-----------------------------------------------------------------
-- Checks if lvalue array reference is followed by a
-- field idenifier.
-----------------------------------------------------------------
pMaybeArrayField :: Parser (Maybe Ident)
pMaybeArrayField =
  do
    dot
    field <- pIdent
    return (Just field)
    <|> return Nothing


-----------------------------------------------------------------
-- Parses a Roo identifier. A non-empty sequence of alphanumeric
-- characters, underscore and apostrophe (’), and it must start
--  with a (lower or upper case) letter."
-----------------------------------------------------------------
pIdent :: Parser String
pIdent =
  do
    ident <- identifier
    return ident
    <?> "an identifier - a non-empty sequence of alphanumeric characters,\
        \ underscore and apostrophe (’), and it must start\
        \ with a (lower or upper case) letter."


-----------------------------------------------------------------
-- Convert file position to (line, column).
-----------------------------------------------------------------
filepos :: SourcePos -> (Int, Int)
filepos pos =
  (sourceLine pos, sourceColumn pos)
