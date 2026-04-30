module Lang.TypeChecker.TypeChecker where

import Lang.Syntax.Syntax
import Lang.TypeChecker.Helper
import Lang.TypeChecker.Types
import qualified Data.Map as Map
import Control.Monad (forM_, unless, foldM)

-- Static analysis tool. Walks through AST before the program runs
-- TypeChecker infers the result without actually computing (compile-time)
checkExpr :: TypeEnv -> Expr -> Either TypeError Type
checkExpr env expr =
    case expr of 
        -- | Let Binding (eg: x = 5 in x + 1, name = x, bindexpr = IntLit 5, x + 1)
        Let name bindExpr bodyExpr -> do 
            bindType <- checkExpr env bindExpr -- | checkExpr env (IntLit 5) → Right TInt
            let env' = Map.insert name bindType env -- | env' = {"x" → TInt}
            checkExpr env' bodyExpr

        -- Literals
        IntLit _    -> Right TInt
        FloatLit _  -> Right TFloat
        DoubleLit _ -> Right TDouble
        BoolLit _   -> Right TBool
        CharLit _   -> Right TChar
        StringLit _ -> Right TString
        NullLit     -> Right TNull

        -- Variable lookup
        Var name    -> case Map.lookup name env of
            Just t  -> Right t
            Nothing -> Left (UndefinedVariable name)

        -- Unary Operations
        Negate e -> do
            t <- checkExpr env e
            if isNumeric t 
                then Right t 
                else Left (ExpectedNumeric t e)
        
        Not e -> do
            t <- checkExpr env e
            if isCompatibleType t TBool
                then Right TBool
                else Left (ExpectedTypeBool t e)

        -- Binary Operations
        BinOp op e1 e2 -> 
            case op of
                -- Arithmetic: Both numeric
                Add -> numericBinOp env e1 e2
                Sub -> numericBinOp env e1 e2
                Mul -> numericBinOp env e1 e2
                Div -> numericBinOp env e1 e2

                -- Mod: Both integers
                Mod -> integerBinOp env e1 e2

                -- Bitwise: Both Int
                BitAnd    -> integerBinOp env e1 e2
                BitOr     -> integerBinOp env e1 e2
                BitXor    -> integerBinOp env e1 e2
                BitLShift -> integerBinOp env e1 e2
                BitRShift -> integerBinOp env e1 e2

                -- Comparison: Both same type, result Bool
                Eq  -> compareBinOp env e1 e2
                Neq -> compareBinOp env e1 e2
                
                -- Numeric comparisons: Both numeric, result Bool
                Lt  -> numericComparison env e1 e2
                Lte -> numericComparison env e1 e2
                Gt  -> numericComparison env e1 e2
                Gte -> numericComparison env e1 e2
                
                -- Logical: Both Bool, result Bool
                And -> logicalBinOp env e1 e2
                Or  -> logicalBinOp env e1 e2
        
        -- List Operations
        -- (forM_ is a loop for monadic actions that discards the result)
        -- it checks each element but doesn't keep their type
        ListLit (e:es) -> do
            t <- checkExpr env e
            forM_ es $ \ei -> do
                ti <- checkExpr env ei
                unless (isCompatibleType t ti) $
                    Left (TypeMismatch t ti ei)
            Right (TList t)

        ListLit [] -> Left (EmptyList expr)

        ListIndex listExpr indexExpr -> do
            indexType <- checkExpr env indexExpr
            unless (indexType == TInt) $
                Left (ExpectedTypeInt indexType indexExpr)
            
            listType <- checkExpr env listExpr
            case listType of
                TList elementType -> Right elementType
                TDynamic -> Right TDynamic
                _                 -> Left (ExpectedList listType listExpr)
        
        ListRange e1 e2 -> do 
            t1 <- checkExpr env e1
            t2 <- checkExpr env e2
            if t1 /= TInt
                then Left (ExpectedTypeInt t1 e1)
            else if t2 /= TInt 
                then Left (ExpectedTypeInt t2 e2)
            else 
                Right (TList TInt)

        ListSlice listExpr (Slice start stop step) -> do
            listType <- checkExpr env listExpr 

            elementType <- case listType of
                TList t -> Right t 
                TDynamic -> Right TDynamic
                _       -> Left (ExpectedList listType listExpr)

            -- Helper to check if each slice is an int
            let checkInt e =
                    case e of
                        Nothing -> Right ()
                        Just ex -> do
                             t <- checkExpr env ex 
                             if t == TInt 
                                then Right ()
                                else Left (ExpectedTypeInt t ex)
            
            checkInt start
            checkInt stop
            checkInt step

            Right (TList elementType) -- do block final return val

        Call funcExpr args -> 
            case funcExpr of
                Var funcName -> do
                    -- String to function
                    func <- case funcConvertString funcName of
                        Just f -> Right f 
                        Nothing -> Left (UnknownFunction funcName)

                    -- Check num of arguments (aka arity)
                    let expectedArg = functionArgs func 
                        actualArg   = length args 

                    -- log can take either 1 or 2 arguments
                    let logArgs = expectedArg == actualArg ||
                                (func == FLog && (actualArg == 1 || actualArg == 2))
                    
                    if not logArgs
                        then Left (ArgsMismatch funcName expectedArg actualArg)
                        else return ()
                    
                    argType <- mapM (checkExpr env) args

                    let checkNumeric (arg, t) =
                            if isNumeric t 
                                then Right ()
                            else 
                                Left (ExpectedNumeric t arg)
                    
                    let checkInt (arg, t) =
                            if t == TInt 
                                then Right ()
                            else 
                                Left (ExpectedTypeInt t arg)
                    
                    let checkNumericList =
                            case (args, argType) of
                                ([_], [TList t]) | isNumeric t -> Right ()
                                ([arg], [TList t]) -> Left (ExpectedNumeric t arg)
                                ([arg], [t])       -> Left (ExpectedList t arg)
                                _                  -> Left (ArgsMismatch funcName 1 actualArg)
                    
                    case func of
                        FMean    -> checkNumericList
                        FMedian  -> checkNumericList
                        FMode    -> checkNumericList
                        FSum     -> checkNumericList
                        FProduct -> checkNumericList
                        FMin     -> checkNumericList
                        FMax     -> checkNumericList
                        FStddev  -> checkNumericList

                        FFact  -> mapM_ checkInt (zip args argType)
                        FFact2 -> mapM_ checkInt (zip args argType) 
                        FFib   -> mapM_ checkInt (zip args argType) 

                        FComb -> mapM_ checkInt (zip args argType) 
                        FPerm -> mapM_ checkInt (zip args argType) 
                        FGcd  -> mapM_ checkInt (zip args argType) 
                        FLcm  -> mapM_ checkInt (zip args argType) 

                        FAbs   -> mapM_ checkNumeric (zip args argType) 
                        FCeil  -> mapM_ checkNumeric (zip args argType) 
                        FFloor -> mapM_ checkNumeric (zip args argType) 
                        FRound -> mapM_ checkNumeric (zip args argType) 
                        FSqrt  -> mapM_ checkNumeric (zip args argType) 
                        FLog   -> mapM_ checkNumeric (zip args argType) 
                        FExp   -> mapM_ checkNumeric (zip args argType) 
                        FSin   -> mapM_ checkNumeric (zip args argType) 
                        FCos   -> mapM_ checkNumeric (zip args argType) 
                        FTan   -> mapM_ checkNumeric (zip args argType) 

                        FLength -> case (args, argType) of
                            (_, [TList _]) -> Right ()
                            (_, [TString]) -> Right ()
                            ([arg], [t])   -> Left (ExpectedList t arg)
                            _              -> Left (ArgsMismatch funcName 1 actualArg)
                    

                        _    -> mapM_ checkNumeric (zip args argType)
                    
                    Right (funcReturnType func)

                
                _ -> Left (NotFunction funcExpr)

                    



-- Helper functions for BinOp
numericBinOp :: TypeEnv -> Expr -> Expr -> Either TypeError Type
numericBinOp env e1 e2 = do
    t1 <- checkExpr env e1 
    t2 <- checkExpr env e2
    if not (isNumeric t1)
        then Left (ExpectedNumeric t1 e1)
    else if not (isNumeric t2)
        then Left (ExpectedNumeric t2 e2)
    else
        Right (numericCoercion t1 t2)

integerBinOp :: TypeEnv -> Expr -> Expr -> Either TypeError Type
integerBinOp env e1 e2 = do
    t1 <- checkExpr env e1
    t2 <- checkExpr env e2
    if not (t1 == TInt || t1 == TDynamic)
        then Left (ExpectedTypeInt t1 e1)
    else if not (t2 == TInt || t2 == TDynamic)
        then Left (ExpectedTypeInt t2 e2)
    else
        Right TInt

logicalBinOp :: TypeEnv -> Expr -> Expr -> Either TypeError Type
logicalBinOp env e1 e2 = do
    t1 <- checkExpr env e1
    t2 <- checkExpr env e2
    if not (isCompatibleType t1 TBool)
        then Left (ExpectedTypeBool t1 e1)
    else if not (isCompatibleType t2 TBool)
        then Left (ExpectedTypeBool t2 e2)
    else
        Right TBool  

compareBinOp :: TypeEnv -> Expr -> Expr -> Either TypeError Type
compareBinOp env e1 e2 = do
    t1 <- checkExpr env e1
    t2 <- checkExpr env e2
    if isCompatibleType t1 t2
        then Right TBool
    else 
        Left (TypeMismatch t1 t2 e2)

numericComparison :: TypeEnv -> Expr -> Expr -> Either TypeError Type
numericComparison env e1 e2 = do
    t1 <- checkExpr env e1
    t2 <- checkExpr env e2
    if not (isNumeric t1)
        then Left (ExpectedNumeric t1 e1)
    else if not (isNumeric t2)
        then Left (ExpectedNumeric t2 e2)
    else 
        Right TBool

getStmtType :: TypeEnv -> Stmt -> Either TypeError Type
getStmtType env stmt =
    case stmt of 
        Assign _ expr -> checkExpr env expr

        ExprStmt expr -> checkExpr env expr

        AssignOp _ _ expr -> checkExpr env expr

        Block [] -> Right TNull
        Block stmts -> do
            let (initStmts, lastStmt) = (init stmts, last stmts)
            cumulativeEnv <- foldM checkStmt env initStmts
            getStmtType cumulativeEnv lastStmt

        If condition thenStmt elseStmt -> do

            conditionType <- checkExpr env condition
            if conditionType /= TBool
                then Left (ExpectedTypeBool conditionType condition)
                else return ()

            thenType <- getStmtType env thenStmt

            case elseStmt of
                Nothing -> Right thenType
                Just elseStatement -> do
                    elseType <- getStmtType env elseStatement
                    if isCompatibleType thenType elseType
                    then Right thenType
                    else Left (BranchTypeMismatch thenType elseType elseStatement)

        Decl _ _ expr -> checkExpr env expr


checkStmt :: TypeEnv -> Stmt -> Either TypeError TypeEnv
checkStmt env stmt = 
    case stmt of
        Assign name expr -> do
            actualType <- checkExpr env expr 
            case Map.lookup name env of
                Just varType ->
                    if isCompatibleType varType actualType
                        then Right env
                        else Left (TypeMismatch varType actualType expr)
                Nothing -> Right (Map.insert name TDynamic env)
        
        AssignOp op name expr -> do
            exprType <- checkExpr env expr

            varType <- case Map.lookup name env of
                Just t  -> Right t
                Nothing -> Left (UndefinedVariable name)

            let requireNumeric = 
                    if isNumeric varType && isNumeric exprType
                        then Right()
                        else Left (ExpectedNumeric exprType expr)

                requireInt =
                    if (varType == TInt || varType == TDynamic) && exprType == TInt
                        then Right ()
                        else if exprType /= TInt
                            then Left (ExpectedTypeInt exprType expr)
                        else 
                            Left (ExpectedTypeInt varType (Var name))

            case op of
                AddEq -> requireNumeric
                SubEq -> requireNumeric
                MulEq -> requireNumeric
                DivEq -> requireNumeric
                ModEq -> requireInt
                BitAndEq    -> requireInt
                BitOrEq     -> requireInt
                BitXorEq    -> requireInt
                BitLShiftEq -> requireInt
                BitRShiftEq -> requireInt
            
            Right env

        If cond thenStmt elseStmt -> do
            conditionType <- checkExpr env cond 
            if conditionType /= TBool
                then Left (ExpectedTypeBool conditionType cond)
                else return ()

            _ <- getStmtType env (If cond thenStmt elseStmt)
            Right env

        ExprStmt expr -> do
            _         <- checkExpr env expr
            Right env

        Decl t1 name expr -> do
            t2 <- checkExpr env expr
            if isCompatibleType t1 t2 
                then Right (Map.insert name t1 env)
                else Left (TypeMismatch t1 t2 expr)
        
        Block stmts -> foldM checkStmt env stmts

-- To type check the entire program (all the statements) 
runCheckProgram :: [Stmt] -> Either TypeError TypeEnv
runCheckProgram = foldM checkStmt Map.empty

envCheckProgram :: TypeEnv -> [Stmt] -> Either TypeError TypeEnv
envCheckProgram = foldM checkStmt