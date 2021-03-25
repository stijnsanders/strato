unit stratoPred;

interface

type
  TSyntaxClass=(
    scDeclarative,
    scDeclarative_ArgList,
    scDeclarative_Record,
    scImperative,
    sc_Unknown
  );

  TPrecedence=(
    p___,

{//TODO: redo ParseDeclaration/ParseLiteral with these

    p_Declarative,

      pDeclaration,
      pDeclareType,
      pDeclareDefinition,
      pDeclareArgList,
        pArgument,pArgByRef,
      pDeclareRecord,
        pField,pOffset,
      pDeclareCodeBlock,

      pDeclareDestructor, //stTilde,stOpMin
      pDeclareInterface, //stQuestionMark

      pDeclareCast,
      pDeclareLogicalOr,
      pDeclareLogicalXor,
      pDeclareLogicalAnd,
      pDeclareBitwiseOr,
      pDeclareBitwiseXor,
      pDeclareBitwiseAnd,
      pDeclareEqual,
      pDeclareComparative,
      pDeclareShift,
      pDeclareAddSub,
      pDeclareMulDiv,
}

    p_Imperative,
      pCodeBlock,

    p_Statement,
      pThrow,pDefer,pCatch,
      pBrackets,
      pParentheses,

      pUntypedVar,
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
