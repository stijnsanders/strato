unit stratoPred;

interface

type
  TPrecedence=(
      p___,
    pCodeBlock,

      p_Statement,
    pThrow,pDefer,pCatch,
    pBrackets,
    pParentheses,

    pUnTypedVar,
    pAssign,

      p_Juxta,

    pIterationX,pIterationY,pIterationZ,
    pIfThen,pIfElse,
    
    pRange,
    pCast,
    pLogicalOr,
    pLogicalXor,
    pLogicalAnd,
    pBitwiseOr,
    pBitwiseXor,
    pBitwiseAnd,
    pEqual,
    pComparative,
    pShift,
    pAddSub,
    pMulDiv,
    pUnary,pTypeOf,pSizeOf,pAddressOf
  );

implementation

end.
