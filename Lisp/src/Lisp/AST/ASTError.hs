module Lisp.AST.ASTError (
    reservedSymbolError,
    invalidDefineError,
    invalidLambdaError,
    invalidIfError,
    emptySExprError,
    undefinedVariableError,
    divisionByZeroError,
    argumentMustBeIntegerError,
    unknownOperatorError,
    operatorRequiresTwoArgumentsError,
    conditionMustBeBooleanError,
    functionExpectsArgumentsError,
    invalidListExpressionError,
    emptyListError
) where

-- Error message definitions for SExpr to AST conversion

reservedSymbolError :: String -> String
reservedSymbolError name =
    "'" ++ name ++ "' is a reserved symbol and cannot be redefined." ++ "\n"

invalidDefineError :: String
invalidDefineError = "Invalid 'define' syntax. " ++
    "Expected (define name value) or (define (name args) body)." ++ "\n"

invalidLambdaError :: String
invalidLambdaError = "Invalid 'lambda' syntax. Expected (lambda (args) body)." ++ "\n"

invalidIfError :: String
invalidIfError = "Invalid 'if' syntax. " ++
    "Expected (if condition then-expression else-expression)." ++ "\n"

emptySExprError :: String
emptySExprError = "Empty expression cannot be converted to AST." ++ "\n"

-- Error message definitions for AST evaluation
undefinedVariableError :: String -> String
undefinedVariableError name = "Undefined variable: " ++ name ++ "\n"

divisionByZeroError :: String
divisionByZeroError = "Division by zero." ++ "\n"

emptyListError :: String
emptyListError = "Attempted to evaluate an empty list." ++ "\n"

argumentMustBeIntegerError :: String
argumentMustBeIntegerError = "Argument must be an integer." ++ "\n"

unknownOperatorError :: String -> String
unknownOperatorError op = "Unknown operator: " ++ op ++ "\n"

operatorRequiresTwoArgumentsError :: String
operatorRequiresTwoArgumentsError = "Operator requires exactly 2 arguments." ++ "\n"

conditionMustBeBooleanError :: String
conditionMustBeBooleanError = "Condition must be a boolean value." ++ "\n"

functionExpectsArgumentsError :: String -> Int -> Int -> String
functionExpectsArgumentsError funcName expected actual =
    "Function '" ++ funcName ++ "' expects " ++ show expected ++
    " argument(s) but got " ++ show actual ++ "\n"

invalidListExpressionError :: String
invalidListExpressionError = "Invalid list expression." ++ "\n"