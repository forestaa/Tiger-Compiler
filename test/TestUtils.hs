module TestUtils where

import Data.Extensible
import Data.Extensible.Effect
import RIO
import Tiger.Semant
import Tiger.Semant.Translate
import Tiger.Semant.TypeCheck
import Unique

fetchTwoLabel :: (Label, Label)
fetchTwoLabel = leaveEff . runUniqueEff @"label" $ (,) <$> newLabel <*> newLabel

isExpectedVariable :: TypeCheckError -> Bool
isExpectedVariable ExpectedVariable {} = True
isExpectedVariable _ = False

isUndefinedVariable :: TypeCheckError -> Bool
isUndefinedVariable Tiger.Semant.TypeCheck.VariableUndefined {} = True
isUndefinedVariable _ = False

isUnknownType :: TypeCheckError -> Bool
isUnknownType UnknownType {} = True
isUnknownType _ = False

isExpectedExpression :: TypeCheckError -> Bool
isExpectedExpression ExpectedExpression {} = True
isExpectedExpression _ = False

isExpectedIntType :: TypeCheckError -> Bool
isExpectedIntType ExpectedIntType {} = True
isExpectedIntType _ = False

isExpectedUnitType :: TypeCheckError -> Bool
isExpectedUnitType ExpectedUnitType {} = True
isExpectedUnitType _ = False

isExpectedArrayType :: TypeCheckError -> Bool
isExpectedArrayType ExpectedArrayType {} = True
isExpectedArrayType _ = False

isExpectedRecordType :: TypeCheckError -> Bool
isExpectedRecordType ExpectedRecordType {} = True
isExpectedRecordType _ = False

isExpectedType :: TypeCheckError -> Bool
isExpectedType ExpectedType {} = True
isExpectedType _ = False

isExpectedTypes :: TypeCheckError -> Bool
isExpectedTypes ExpectedTypes {} = True
isExpectedTypes _ = False

isExpectedFunction :: TypeCheckError -> Bool
isExpectedFunction ExpectedFunction {} = True
isExpectedFunction _ = False

isExpectedTypeForRecordField :: TypeCheckError -> Bool
isExpectedTypeForRecordField ExpectedTypeForRecordField {} = True
isExpectedTypeForRecordField _ = False

isExtraRecordFieldInConstruction :: TypeCheckError -> Bool
isExtraRecordFieldInConstruction ExtraRecordFieldInConstruction {} = True
isExtraRecordFieldInConstruction _ = False

isMissingRecordFieldInConstruction :: TypeCheckError -> Bool
isMissingRecordFieldInConstruction MissingRecordFieldInConstruction {} = True
isMissingRecordFieldInConstruction _ = False

isMissingRecordField :: TypeCheckError -> Bool
isMissingRecordField MissingRecordField {} = True
isMissingRecordField _ = False

isNotDeterminedNilType :: TypeCheckError -> Bool
isNotDeterminedNilType NotDeterminedNilType = True
isNotDeterminedNilType _ = False

isInvalidRecTypeDeclaration :: TypeCheckError -> Bool
isInvalidRecTypeDeclaration InvalidRecTypeDeclaration {} = True
isInvalidRecTypeDeclaration _ = False

isMultiDeclaredName :: TypeCheckError -> Bool
isMultiDeclaredName MultiDeclaredName {} = True
isMultiDeclaredName _ = False

isBreakOutsideLoop :: TranslateError -> Bool
isBreakOutsideLoop BreakOutsideLoop = True
isBreakOutsideLoop _ = False

isTypeCheckErrorAnd :: (TypeCheckError -> Bool) -> SemantAnalysisError -> Bool
isTypeCheckErrorAnd p (TypeCheckError e) = p e
isTypeCheckErrorAnd _ _ = False

isTypeCheckErrorAndExpectedVariable :: SemantAnalysisError -> Bool
isTypeCheckErrorAndExpectedVariable = isTypeCheckErrorAnd isExpectedVariable

isTypeCheckErrorAndUndefinedVariable :: SemantAnalysisError -> Bool
isTypeCheckErrorAndUndefinedVariable = isTypeCheckErrorAnd isUndefinedVariable

isTypeCheckErrorAndUnknownType :: SemantAnalysisError -> Bool
isTypeCheckErrorAndUnknownType = isTypeCheckErrorAnd isUnknownType

isTypeCheckErrorAndExpectedExpression :: SemantAnalysisError -> Bool
isTypeCheckErrorAndExpectedExpression = isTypeCheckErrorAnd isExpectedExpression

isTypeCheckErrorAndExpectedIntType :: SemantAnalysisError -> Bool
isTypeCheckErrorAndExpectedIntType = isTypeCheckErrorAnd isExpectedIntType

isTypeCheckErrorAndExpectedUnitType :: SemantAnalysisError -> Bool
isTypeCheckErrorAndExpectedUnitType = isTypeCheckErrorAnd isExpectedUnitType

isTypeCheckErrorAndExpectedArrayType :: SemantAnalysisError -> Bool
isTypeCheckErrorAndExpectedArrayType = isTypeCheckErrorAnd isExpectedArrayType

isTypeCheckErrorAndExpectedRecordType :: SemantAnalysisError -> Bool
isTypeCheckErrorAndExpectedRecordType = isTypeCheckErrorAnd isExpectedRecordType

isTypeCheckErrorAndExpectedType :: SemantAnalysisError -> Bool
isTypeCheckErrorAndExpectedType = isTypeCheckErrorAnd isExpectedType

isTypeCheckErrorAndExpectedTypes :: SemantAnalysisError -> Bool
isTypeCheckErrorAndExpectedTypes = isTypeCheckErrorAnd isExpectedTypes

isTypeCheckErrorAndExpectedFunction :: SemantAnalysisError -> Bool
isTypeCheckErrorAndExpectedFunction = isTypeCheckErrorAnd isExpectedFunction

isTypeCheckErrorAndExpectedTypeForRecordField :: SemantAnalysisError -> Bool
isTypeCheckErrorAndExpectedTypeForRecordField = isTypeCheckErrorAnd isExpectedTypeForRecordField

isTypeCheckErrorAndExtraRecordFieldInConstruction :: SemantAnalysisError -> Bool
isTypeCheckErrorAndExtraRecordFieldInConstruction = isTypeCheckErrorAnd isExtraRecordFieldInConstruction

isTypeCheckErrorAndMissingRecordFieldInConstruction :: SemantAnalysisError -> Bool
isTypeCheckErrorAndMissingRecordFieldInConstruction = isTypeCheckErrorAnd isMissingRecordFieldInConstruction

isTypeCheckErrorAndMissingRecordField :: SemantAnalysisError -> Bool
isTypeCheckErrorAndMissingRecordField = isTypeCheckErrorAnd isMissingRecordField

isTypeCheckErrorAndNotDeterminedNilType :: SemantAnalysisError -> Bool
isTypeCheckErrorAndNotDeterminedNilType = isTypeCheckErrorAnd isNotDeterminedNilType

isTypeCheckErrorAndInvalidRecTypeDeclaration :: SemantAnalysisError -> Bool
isTypeCheckErrorAndInvalidRecTypeDeclaration = isTypeCheckErrorAnd isInvalidRecTypeDeclaration

isTypeCheckErrorAndMultiDeclaredName :: SemantAnalysisError -> Bool
isTypeCheckErrorAndMultiDeclaredName = isTypeCheckErrorAnd isMultiDeclaredName

isTranslateErrorAnd :: (TranslateError -> Bool) -> SemantAnalysisError -> Bool
isTranslateErrorAnd p (TranslateError e) = p e
isTranslateErrorAnd _ _ = False

isTranslateErrorAndBreakOutsideLoop :: SemantAnalysisError -> Bool
isTranslateErrorAndBreakOutsideLoop = isTranslateErrorAnd isBreakOutsideLoop
