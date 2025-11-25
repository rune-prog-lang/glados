{-# LANGUAGE LambdaCase #-}

module Rune.AST.Parser (parseRune) where

import Control.Applicative (Alternative (..), optional)
import Data.Either (partitionEithers)
import Rune.AST.Nodes
import Rune.AST.ParserHelper
import Rune.AST.ParserTypes (Parser (..), ParserState (..))
import qualified Rune.Lexer.Tokens as T

--
-- public
--

parseRune :: FilePath -> [T.Token] -> Either String Program
parseRune filepath tokens =
  case runParser parseProgram (ParserState tokens 0 filepath) of
    Left err -> Left err
    Right (prog, _) -> Right prog

--
-- private
--

--
-- Program
--

parseProgram :: Parser Program
parseProgram =
  Program
    <$> (psFilePath <$> getParserState)
    <*> parseTopLevels
    <* expect T.EOF

parseTopLevels :: Parser [TopLevelDef]
parseTopLevels = do
  isEof <- check T.EOF
  if isEof
    then pure []
    else do
      def <- parseTopLevelDef
      defs <- parseTopLevels
      pure (def : defs)

--
-- Top Level
--

parseTopLevelDef :: Parser TopLevelDef
parseTopLevelDef = do
  t <- peek
  case T.tokenKind t of
    T.KwDef -> parseFunction
    T.KwStruct -> parseStruct
    T.KwOverride -> parseOverride
    _ -> failParse "Expected top-level definition (def, struct, override)"

parseFunction :: Parser TopLevelDef
parseFunction = do
  _ <- expect T.KwDef
  name <- parseIdentifier
  params <- withContext ("parameters of function '" ++ name ++ "'") parseParams
  retType <- withContext ("return type of function '" ++ name ++ "'") parseReturnType
  body <- withContext ("body of function '" ++ name ++ "'") parseBlock
  pure $ DefFunction name params retType body

parseStruct :: Parser TopLevelDef
parseStruct = do
  name <- expect T.KwStruct *> parseIdentifier
  _ <- expect T.LBrace
  (fields, methods) <- withContext ("body of struct '" ++ name ++ "'") parseStructBody
  pure $ DefStruct name fields methods

parseStructBody :: Parser ([Field], [TopLevelDef])
parseStructBody = do
  items <- parseStructItemsLoop
  pure $ partitionEithers items

parseStructItemsLoop :: Parser [Either Field TopLevelDef]
parseStructItemsLoop = do
  isEnd <- check T.RBrace
  if isEnd
    then advance >> pure []
    else do
      item <- parseStructItem
      rest <- parseStructItemsLoop
      pure (item : rest)

parseStructItem :: Parser (Either Field TopLevelDef)
parseStructItem = do
  t <- peek
  case T.tokenKind t of
    T.KwDef -> Right <$> parseFunction
    T.Identifier _ -> Left <$> parseField <* expect T.Semicolon
    _ -> failParse "Expected struct field or method"

parseOverride :: Parser TopLevelDef
parseOverride = do
  _ <- expect T.KwOverride *> expect T.KwDef
  name <- parseIdentifier
  params <- withContext ("parameters of override '" ++ name ++ "'") parseParams
  retType <- withContext ("return type of override '" ++ name ++ "'") parseReturnType
  body <- withContext ("body of override '" ++ name ++ "'") parseBlock
  pure $ DefOverride name params retType body

parseParams :: Parser [Parameter]
parseParams = between (expect T.LParen) (expect T.RParen) (sepBy parseParameter (expect T.Comma))

parseParameter :: Parser Parameter
parseParameter = parseSelfParam <|> parseTypedParam

parseSelfParam :: Parser Parameter
parseSelfParam =
  Parameter "self" TypeAny <$ expectIdent "self"

parseTypedParam :: Parser Parameter
parseTypedParam =
  Parameter
    <$> parseIdentifier
    <*> (expect T.Colon *> parseType)
      <|> failParse "Expected typed parameter (name: type)"

parseReturnType :: Parser Type
parseReturnType =
  (expect T.OpArrow <|> expect T.OpSquigArrow) *> parseType

parseField :: Parser Field
parseField = Field <$> parseIdentifier <*> (expect T.Colon *> parseType)

--
-- Statements
--

parseBlock :: Parser Block
parseBlock = expect T.LBrace *> parseBlockLoop

parseBlockLoop :: Parser Block
parseBlockLoop = do
  isEnd <- check T.RBrace
  if isEnd
    then advance >> pure []
    else do
      stmt <- parseStatement
      stmts <- parseBlockLoop
      pure (stmt : stmts)

parseStatement :: Parser Statement
parseStatement = do
  t <- peek
  case T.tokenKind t of
    T.KwReturn -> withContext "Return statement" parseReturn
    T.KwIf -> withContext "If statement" parseIf
    T.KwFor -> withContext "For loop" parseFor
    T.Identifier _ -> parseVarDeclOrExpr
    _ -> withContext "Statement" parseExprStmt

parseReturn :: Parser Statement
parseReturn =
  expect T.KwReturn
    *> (StmtReturn <$> optional (withContext "return value" parseExpression))
    <* expect T.Semicolon

parseIf :: Parser Statement
parseIf =
  StmtIf
    <$> (expect T.KwIf *> withContext "if condition" parseExpression)
    <*> withContext "if block" parseBlock
    <*> optional (expect T.KwElse *> (withContext "else block" (parseBlock <|> (pure <$> parseIf))))

parseFor :: Parser Statement
parseFor = do
  _ <- expect T.KwFor
  name <- parseIdentifier
  t <- peek
  case T.tokenKind t of
    T.OpAssign -> parseForRangeRest name
    T.KwIn -> parseForEachRest name
    _ -> failParse "Expected '=' or 'in' after for loop variable"

parseForRangeRest :: String -> Parser Statement
parseForRangeRest var = do
  _ <- expect T.OpAssign
  start <- withContext "start index" parseExpression
  _ <- expect T.KwTo
  end <- withContext "end index" parseExpression
  body <- withContext "for block" parseBlock
  pure $ StmtFor var start end body

parseForEachRest :: String -> Parser Statement
parseForEachRest var = do
  _ <- expect T.KwIn
  iterable <- withContext "iterable expression" parseExpression
  body <- withContext "for-each block" parseBlock
  pure $ StmtForEach var iterable body

parseVarDeclOrExpr :: Parser Statement
parseVarDeclOrExpr = do
  isDecl <- lookAheadIsVarDecl
  if isDecl
    then withContext "Variable declaration" parseVarDecl
    else withContext "Expression statement" parseExprStmt

lookAheadIsVarDecl :: Parser Bool
lookAheadIsVarDecl = Parser $ \s ->
  let p = do
        _ <- parseIdentifier
        t <- peek
        pure $
          T.tokenKind t
            `elem` [ T.Colon,
                     T.OpAssign,
                     T.OpAddAssign,
                     T.OpSubAssign,
                     T.OpMulAssign,
                     T.OpDivAssign,
                     T.OpModAssign
                   ]
   in case runParser p s of
        Right (result, _) -> Right (result, s)
        Left _ -> Right (False, s)

parseVarDecl :: Parser Statement
parseVarDecl = do
  name <- parseIdentifier
  typeAnnot <- optional (expect T.Colon *> parseType)

  t <- peek
  (_, binOp) <- case T.tokenKind t of
    T.OpAssign -> advance >> pure (T.OpAssign, Nothing)
    T.OpAddAssign -> advance >> pure (T.OpAddAssign, Just Add)
    T.OpSubAssign -> advance >> pure (T.OpSubAssign, Just Sub)
    T.OpMulAssign -> advance >> pure (T.OpMulAssign, Just Mul)
    T.OpDivAssign -> advance >> pure (T.OpDivAssign, Just Div)
    T.OpModAssign -> advance >> pure (T.OpModAssign, Just Mod)
    _ -> failParse "Expected assignment operator"

  val <- withContext "assigned value" parseExpression
  _ <- expect T.Semicolon

  let finalExpr = case binOp of
        Nothing -> val
        Just op -> ExprBinary op (ExprVar name) val

  pure $ StmtVarDecl name typeAnnot finalExpr

parseExprStmt :: Parser Statement
parseExprStmt = do
  expr <- parseExpression
  choice
    [ StmtExpr expr <$ expect T.Semicolon,
      do
        isEnd <- check T.RBrace
        if isEnd
          then pure (StmtReturn (Just expr))
          else empty,
      failParse "Expected ';' after expression"
    ]

--
-- Expressions
--

parseExpression :: Parser Expression
parseExpression = parseLogicalOr

parseLogicalOr :: Parser Expression
parseLogicalOr = chainl1 parseLogicalAnd (ExprBinary Or <$ expect T.OpOr)

parseLogicalAnd :: Parser Expression
parseLogicalAnd = chainl1 parseEquality (ExprBinary And <$ expect T.OpAnd)

parseEquality :: Parser Expression
parseEquality = chainl1 parseComparison op
  where
    op = (ExprBinary Eq <$ expect T.OpEq) <|> (ExprBinary Neq <$ expect T.OpNeq)

parseComparison :: Parser Expression
parseComparison = chainl1 parseTerm op
  where
    op =
      choice
        [ ExprBinary Lt <$ expect T.OpLt,
          ExprBinary Lte <$ expect T.OpLte,
          ExprBinary Gt <$ expect T.OpGt,
          ExprBinary Gte <$ expect T.OpGte
        ]

parseTerm :: Parser Expression
parseTerm = chainl1 parseFactor op
  where
    op = (ExprBinary Add <$ expect T.OpPlus) <|> (ExprBinary Sub <$ expect T.OpMinus)

parseFactor :: Parser Expression
parseFactor = chainl1 parseUnary op
  where
    op =
      choice
        [ ExprBinary Mul <$ expect T.OpMul,
          ExprBinary Div <$ expect T.OpDiv,
          ExprBinary Mod <$ expect T.OpMod
        ]

parseUnary :: Parser Expression
parseUnary =
  choice
    [ ExprUnary Negate <$ expect T.OpMinus <*> parseUnary,
      ExprUnary PrefixInc <$ expect T.OpInc <*> parseUnary,
      ExprUnary PrefixDec <$ expect T.OpDec <*> parseUnary,
      parsePostfix
    ]

parsePostfix :: Parser Expression
parsePostfix = chainPostfix parsePrimary op
  where
    op =
      choice
        [ do
            args <- between (expect T.LParen) (expect T.RParen) (sepBy (withContext "argument" parseExpression) (expect T.Comma))
            pure $ \e -> ExprCall (getExprName e) args,
          do
            f <- expect T.Dot *> parseIdentifier
            pure $ \e -> ExprAccess e f,
          do
            _ <- expect T.OpErrorProp
            pure $ \e -> ExprUnary PropagateError e,
          do
            _ <- expect T.OpInc
            pure $ \e -> ExprUnary PostfixInc e,
          do
            _ <- expect T.OpDec
            pure $ \e -> ExprUnary PostfixDec e
        ]

getExprName :: Expression -> String
getExprName (ExprVar name) = name
getExprName (ExprAccess _ field) = field
getExprName _ = ""

parsePrimary :: Parser Expression
parsePrimary =
  choice
    [ parseLitInt,
      parseLitFloat,
      parseLitString,
      parseLitBool,
      ExprLitNull <$ expect T.LitNull,
      parseStructInitOrVar,
      between (expect T.LParen) (expect T.RParen) (withContext "parenthesized expression" parseExpression)
    ]
    <|> failParse "Expected expression (literal, variable, or '('...)"

parseLitInt :: Parser Expression
parseLitInt =
  tokenMap $ \case
    T.LitInt n -> Just (ExprLitInt n)
    _ -> Nothing

parseLitFloat :: Parser Expression
parseLitFloat =
  tokenMap $ \case
    T.LitFloat f -> Just (ExprLitFloat f)
    _ -> Nothing

parseLitString :: Parser Expression
parseLitString =
  tokenMap $ \case
    T.LitString s -> Just (ExprLitString s)
    _ -> Nothing

parseLitBool :: Parser Expression
parseLitBool =
  tokenMap $ \case
    T.LitBool b -> Just (ExprLitBool b)
    _ -> Nothing

tokenMap :: (T.TokenKind -> Maybe a) -> Parser a
tokenMap f = do
  t <- peek
  case f (T.tokenKind t) of
    Just val -> advance >> pure val
    Nothing -> empty

parseStructInitOrVar :: Parser Expression
parseStructInitOrVar = try parseStructInit <|> (ExprVar <$> parseIdentifier)

parseStructInit :: Parser Expression
parseStructInit = do
  name <- parseIdentifier
  fields <- between (expect T.LBrace) (expect T.RBrace) parseStructFields
  pure $ ExprStructInit name fields

parseStructFields :: Parser [(String, Expression)]
parseStructFields = sepEndBy parseStructField (expect T.Comma)

parseStructField :: Parser (String, Expression)
parseStructField = do
  name <- parseIdentifier
  _ <- expect T.Colon
  val <- withContext ("value of field '" ++ name ++ "'") parseExpression
  pure (name, val)

--
-- Types & Identifiers
--

parseType :: Parser Type
parseType =
  choice
    [ TypeI8 <$ expect T.TypeI8,
      TypeI16 <$ expect T.TypeI16,
      TypeI32 <$ expect T.TypeI32,
      TypeI64 <$ expect T.TypeI64,
      TypeF32 <$ expect T.TypeF32,
      TypeF64 <$ expect T.TypeF64,
      TypeBool <$ expect T.TypeBool,
      TypeU8 <$ expect T.TypeU8,
      TypeU16 <$ expect T.TypeU16,
      TypeU32 <$ expect T.TypeU32,
      TypeString <$ expect T.TypeString,
      TypeAny <$ expect T.TypeAny,
      TypeNull <$ expect T.TypeNull,
      TypeCustom <$> parseIdentifier
    ]
    <|> failParse "Expected type"

parseIdentifier :: Parser String
parseIdentifier =
  tokenMap $ \case
    T.Identifier s -> Just s
    _ -> Nothing
