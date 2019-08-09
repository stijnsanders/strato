unit stratoPred;

interface

type
  TSyntaxClass=(
    //scHeader, //see ParseHeader
    scDeclarative,
    scDeclarative_Record,
    scImperative,
    sc_Unknown
  );

  TPrecedence=(
      p___,

      p_Delarative,

    pDeclaration,

      p_Imperative,
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
