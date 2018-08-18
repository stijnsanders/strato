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

    pIterationX,pIterationY,pIterationZ,
    pIfThen,pIfElse,

      p_Juxta,

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

      p_POpen,

    pUnary,pTypeOf,pSizeOf,pAddressOf
  );

implementation

end.
