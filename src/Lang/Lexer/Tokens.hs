module Lang.Lexer.Tokens
  ( TokenPos (..),
    TokenType (..),
    Token (..),
  )
where

data Token = Token
  { tokenType :: TokenType, -- For differenciating tokens
    tokenPos :: TokenPos    -- For error feedback
  }
  deriving (Show, Eq)

data TokenPos = TokenPos
  { line :: !Int,
    column :: !Int
  }
  deriving (Show, Eq)

data TokenType
  = TokEOF            -- End of program
  | TokError String   -- Error handled by lexer
  | TokIdent String   -- Variable names

  -- Comments
  | TokLComment | TokRComment
  
  -- Values
  | TokBool Bool | TokNull
  | TokInt Int | TokFloat Float
  | TokChar Char | TokString String

  -- Special characters
  | TokSemiColon
  | TokDot | TokComma

  -- Control Structures Variables
  | TokIf | TokThen | TokElse 
  | TokLet | TokIn
  | TokFor | TokWhile                                                               
  | TokSwitch | TokCase 

  -- Scoping               
  | TokLBrack | TokRBrack | TokLSqBrack | TokRSqBrack | TokLCBrack | TokRCBrack

  -- Logical Op
  | TokNot | TokAnd | TokOr                                                                   

  -- Comparison Op
  | TokEq | TokNeq | TokLte | TokLt | TokGte | TokGt                                          

  -- Arithmetic Op
  | TokAdd | TokSub | TokMul | TokDiv | TokMod

  -- Binary Arithmetic Op
  | TokBinAnd | TokBinOr | TokBinXor | TokBinLShift | TokBinRShift                            

  -- Operation Assignments
  | TokAssign
  | TokAddAssign | TokSubAssign | TokMulAssign | TokDivAssign | TokModAssign 
  | TokBinAndAssign | TokBinOrAssign | TokBinXorAssign | TokBinLShiftAssign | TokBinRShiftAssign

  -- Hyperbolic Functions
  | TokSinh | TokCosh | TokTanh 
  | TokCsch | TokSech | TokCoth 
  | TokAsinh | TokAcosh

  -- Statistical Functions
  | TokMean | TokMedian | TokMode 
  | TokSum | TokProduct 
  | TokMin | TokMax | TokStddev

  -- Power and Root Functions
  | TokSqrt | TokCbrt | TokPow | TokExp | TokSquare | TokCube | TokExp10
        
  -- Trigonometric Functions
  | TokSin | TokCos | TokTan 
  | TokAsin | TokAcos | TokAtan | TokAtan2 
  | TokSec | TokCsc | TokCot 
  | TokVersin | TokExsec

  -- Logarithmic Functions
  | TokLn | TokLog10 | TokLog2 | TokLog | TokLog1p
        
  -- Combinatorial Functions
  | TokFact | TokFact2 | TokComb | TokPerm 
  | TokGcd | TokLcm | TokFib | TokGamma
  deriving (Show, Eq)