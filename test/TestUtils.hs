module TestUtils where

import Data.Extensible
import RIO

import Unique

import Tiger.Semant.TypeCheck


fetchTwoLabel :: (Label, Label)
fetchTwoLabel = leaveEff . runUniqueEff @"label" $ (,) <$> newLabel <*> newLabel

isExpectedVariable :: TranslateError -> Bool
isExpectedVariable ExpectedVariable{} = True
isExpectedVariable _ = False
isUndefinedVariable :: TranslateError -> Bool
isUndefinedVariable VariableUndefined{} = True
isUndefinedVariable _ = False
isUnknownType :: TranslateError -> Bool
isUnknownType UnknownType{} = True
isUnknownType _ = False
isExpectedExpression :: TranslateError -> Bool
isExpectedExpression ExpectedExpression{} = True
isExpectedExpression _ = False
isExpectedIntType :: TranslateError -> Bool
isExpectedIntType ExpectedIntType{} = True
isExpectedIntType _ = False
isExpectedUnitType :: TranslateError -> Bool
isExpectedUnitType ExpectedUnitType{} = True
isExpectedUnitType _ = False
isExpectedArrayType :: TranslateError -> Bool
isExpectedArrayType ExpectedArrayType{} = True
isExpectedArrayType _ = False
isExpectedRecordType :: TranslateError -> Bool
isExpectedRecordType ExpectedRecordType{} = True
isExpectedRecordType _ = False
isExpectedType :: TranslateError -> Bool
isExpectedType ExpectedType{} = True
isExpectedType _ = False
isExpectedTypes :: TranslateError -> Bool
isExpectedTypes ExpectedTypes{} = True
isExpectedTypes _ = False
isExpectedFunction :: TranslateError -> Bool
isExpectedFunction ExpectedFunction{} = True
isExpectedFunction _ = False
isExpectedTypeForRecordField :: TranslateError -> Bool
isExpectedTypeForRecordField ExpectedTypeForRecordField{} = True
isExpectedTypeForRecordField _ = False
isExtraRecordFieldInConstruction :: TranslateError -> Bool
isExtraRecordFieldInConstruction ExtraRecordFieldInConstruction{} = True
isExtraRecordFieldInConstruction _ = False
isMissingRecordFieldInConstruction :: TranslateError -> Bool
isMissingRecordFieldInConstruction MissingRecordFieldInConstruction{} = True
isMissingRecordFieldInConstruction _ = False
isMissingRecordField :: TranslateError -> Bool
isMissingRecordField MissingRecordField{} = True
isMissingRecordField _ = False
isBreakOutsideLoop :: TranslateError -> Bool
isBreakOutsideLoop BreakOutsideLoop = True
isBreakOutsideLoop _ = False
isNotDeterminedNilType :: TranslateError -> Bool
isNotDeterminedNilType NotDeterminedNilType = True
isNotDeterminedNilType _ = False
isInvalidRecTypeDeclaration :: TranslateError -> Bool
isInvalidRecTypeDeclaration InvalidRecTypeDeclaration{} = True
isInvalidRecTypeDeclaration _ = False
isMultiDeclaredName :: TranslateError -> Bool
isMultiDeclaredName MultiDeclaredName{} = True
isMultiDeclaredName _ = False

