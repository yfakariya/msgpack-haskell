{-# LANGUAGE QuasiQuotes, RecordWildCards, OverloadedStrings #-}

module Language.MessagePack.IDL.CodeGen.CliXml (
  Config(..),
  generate,
  ) where

import qualified Data.Map as M
import qualified Data.Text as T
import System.FilePath
import qualified Filesystem.Path.CurrentOS as P
import qualified Text.XML as X

import Language.MessagePack.IDL.Syntax

data Config
  = Config
    { configFilePath :: FilePath
    }
  deriving (Show, Eq)

generate :: Config -> Spec -> IO()
generate Config {..} spec = do
    X.writeFile X.def{X.rsPretty= True} outFilePath $ genDoc spec
    where outFilePath = toSystemFilePath $ replaceExtension (takeFileName configFilePath) "xml"

{-
 - Top level elements
 -}
genDoc :: Spec -> X.Document
genDoc spec = X.Document (X.Prologue [] Nothing []) (X.Element "MessagePackIDL" M.empty (genIdlXml spec)) []

genIdlXml :: Spec -> [X.Node]
genIdlXml spec =
  map genDeclXml spec

genDeclXml :: Decl -> X.Node
genDeclXml MPMessage {..} =
    X.NodeElement $ X.Element "Message" (M.singleton "Name" msgName) $ (genTypeParamsElement msgParam) ++ (genFieldsElement msgFields)
genDeclXml MPException {..} =
    X.NodeElement $ X.Element "Exception" (M.fromList ([("Name", excName)] ++ (genExceptionBaseAttr excSuper))) $ (genTypeParamsElement excParam) ++ (genFieldsElement excFields)
genDeclXml MPType {..} =
    X.NodeElement $ X.Element "TypeAreas" (M.singleton "Name" tyName) $ genTypeElement tyType
genDeclXml MPEnum {..} =
    X.NodeElement $ X.Element "Enum" (M.singleton "Name" enumName) $ genEnumMembersElement enumMem
genDeclXml MPService {..} =
    X.NodeElement $ X.Element "Service" (M.fromList ([("Name", serviceName)] ++ (genServiceVersionAttr serviceVersion)) ) $ genMethodsElement serviceMethods

genExceptionBaseAttr :: Maybe T.Text -> [(X.Name, T.Text)]
genExceptionBaseAttr Nothing = []
genExceptionBaseAttr (Just excSuper) = [(xName "Base", excSuper)]

genServiceVersionAttr :: Maybe Int -> [(X.Name, T.Text)]
genServiceVersionAttr Nothing = []
genServiceVersionAttr (Just serviceVersion) = [(xName "Version", T.pack (show serviceVersion))]

{-
 - Common elements
 -}
genTypeParamsElement :: [T.Text] -> [X.Node]
genTypeParamsElement [] = []
genTypeParamsElement typeParams = 
    [X.NodeElement $ X.Element "TypeParams" M.empty $ genTypeParamElements typeParams]

genTypeParamElements :: [T.Text] -> [X.Node]
genTypeParamElements [] = []
genTypeParamElements typeParams = 
    map (\ typeParam -> X.NodeElement $ X.Element "TypeParameter" (M.singleton "Name" typeParam) [] ) typeParams

genFieldsElement :: [Field] -> [X.Node]
genFieldsElement [] = []
genFieldsElement fields =
    [X.NodeElement $ X.Element "Fields" M.empty $ genFieldElements fields]

genFieldElements :: [Field] -> [X.Node]
genFieldElements [] = []
genFieldElements fields = 
    map (\ Field {..} -> X.NodeElement $ X.Element "Field" (M.fromList [("Id", (T.pack (show fldId))), ("Name", fldName)]) ((genTypeElement fldType) ++ (genLiteralElement fldDefault "FieldValue"))) fields

-- genTypeElement
genTypeElement :: Type -> [X.Node]
genTypeElement TBool = genTypeElementCore "bool" Nothing False []
genTypeElement TRaw = genTypeElementCore "raw" Nothing False []
genTypeElement TString = genTypeElementCore "string" Nothing False []
genTypeElement TVoid = genTypeElementCore "void" Nothing False []
genTypeElement ( TFloat True ) = genTypeElementCore "double" Nothing False []
genTypeElement ( TFloat False ) = genTypeElementCore "float" Nothing False []
genTypeElement ( TList item ) = genTypeElementCore "list" Nothing False [item]
genTypeElement ( TMap key value ) = genTypeElementCore "map" Nothing False [key, value]
genTypeElement TObject = genTypeElementCore "object" Nothing False []
genTypeElement ( TUserDef identifier typeArguments ) = genTypeElementCore "udt" (Just $ T.unpack identifier) False typeArguments
genTypeElement ( TTuple items ) = genTypeElementCore "tuple" Nothing False items
genTypeElement ( TInt True 8 ) = genTypeElementCore "sbyte" Nothing False []
genTypeElement ( TInt True 16 ) = genTypeElementCore "short" Nothing False []
genTypeElement ( TInt True 32 ) = genTypeElementCore "int" Nothing False []
genTypeElement ( TInt True 64 ) = genTypeElementCore "long" Nothing False []
genTypeElement ( TInt False 8 ) = genTypeElementCore "byte" Nothing False []
genTypeElement ( TInt False 16 ) = genTypeElementCore "ushort" Nothing False []
genTypeElement ( TInt False 32 ) = genTypeElementCore "uint" Nothing False []
genTypeElement ( TInt False 64 ) = genTypeElementCore "ulong" Nothing False []
genTypeElement ( TInt _ _ ) = genTypeElementCore "bigint" Nothing False []
genTypeElement ( TNullable TBool ) = genTypeElementCore "bool" Nothing True []
genTypeElement ( TNullable TRaw ) = genTypeElementCore "raw" Nothing True []
genTypeElement ( TNullable TString ) = genTypeElementCore "string" Nothing True []
genTypeElement ( TNullable TVoid ) = error "Cannot create Nullable Void."
genTypeElement ( TNullable ( TFloat True ) ) = genTypeElementCore "double" Nothing True []
genTypeElement ( TNullable ( TFloat False ) ) = genTypeElementCore "float" Nothing True []
genTypeElement ( TNullable ( TList item ) ) = genTypeElementCore "list" Nothing True [item]
genTypeElement ( TNullable ( TMap key value ) ) = genTypeElementCore "map" Nothing True [key, value]
genTypeElement ( TNullable TObject ) = genTypeElementCore "object" Nothing True []
genTypeElement ( TNullable ( TUserDef identifier typeArguments ) ) = genTypeElementCore "udt" (Just $ T.unpack identifier) True typeArguments
genTypeElement ( TNullable ( TTuple types ) ) = genTypeElementCore "tuple" Nothing True types
genTypeElement ( TNullable ( TInt True 8 ) ) = genTypeElementCore "sbyte" Nothing True []
genTypeElement ( TNullable ( TInt True 16 ) ) = genTypeElementCore "short" Nothing True []
genTypeElement ( TNullable ( TInt True 32 ) ) = genTypeElementCore "int" Nothing True []
genTypeElement ( TNullable ( TInt True 64 ) ) = genTypeElementCore "long" Nothing True []
genTypeElement ( TNullable ( TInt False 8 ) ) = genTypeElementCore "byte" Nothing True []
genTypeElement ( TNullable ( TInt False 16 ) ) = genTypeElementCore "ushort" Nothing True []
genTypeElement ( TNullable ( TInt False 32 ) ) = genTypeElementCore "uint" Nothing True []
genTypeElement ( TNullable ( TInt False 64 ) ) = genTypeElementCore "ulong" Nothing True []
genTypeElement ( TNullable ( TInt _ _ ) ) = genTypeElementCore "bigint" Nothing True []
genTypeElement ( TNullable ( TNullable _ ) ) = error "Nested Nullable is not valid."

genTypeElementCore :: String -> Maybe String -> Bool -> [Type] -> [X.Node]
genTypeElementCore kind typeName nullable types =
    [X.NodeElement $ X.Element "Type" (M.fromList ([("Kind", (T.pack kind)), ("IsNullable", (T.pack (show nullable)))] ++ (genTypeNameAttr typeName))) $ genTypeArgumentsElement types]

genTypeNameAttr :: Maybe String -> [(X.Name, T.Text)]
genTypeNameAttr Nothing = []
genTypeNameAttr (Just typeName) = [("TypeName", (T.pack typeName))]

genTypeArgumentsElement :: [Type] -> [X.Node]
genTypeArgumentsElement [] = []
genTypeArgumentsElement types =
    [X.NodeElement $ X.Element "TypeArguments" M.empty $ concat (map (\t -> genTypeElement t) types)]

genLiteralElement :: Maybe Literal -> String -> [X.Node]
genLiteralElement Nothing _ = []
genLiteralElement (Just val) elementName = genLiteralElementCore elementName val

genLiteralElementCore :: String -> Literal -> [X.Node]
genLiteralElementCore elementName (LInt val) =
    [X.NodeElement $ X.Element (xName elementName) (M.fromList [("Kind", "bigint"), ("Value", (T.pack (show val)))]) [] ]
genLiteralElementCore elementName (LFloat val) = 
    [X.NodeElement $ X.Element (xName elementName) (M.fromList [("Kind", "double"), ("Value", (T.pack (show val)))]) [] ]
genLiteralElementCore elementName (LBool val) = 
    [X.NodeElement $ X.Element (xName elementName) (M.fromList [("Kind", "bool"), ("Value", (T.pack (show val)))]) [] ]
genLiteralElementCore elementName LNull = 
    [X.NodeElement $ X.Element (xName elementName) (M.singleton "Kind" "null") [] ]
genLiteralElementCore elementName (LString val) = 
    [X.NodeElement $ X.Element (xName elementName) (M.fromList [("Kind", "string"), ("Value", val)]) [] ]

genEnumMembersElement :: [(Int, T.Text)] -> [X.Node]
genEnumMembersElement [] = []
genEnumMembersElement members =
    [X.NodeElement $ X.Element "EnumMembers" M.empty $ genEnumMemberElements members]

genEnumMemberElements :: [(Int, T.Text)] -> [X.Node]
genEnumMemberElements [] = []
genEnumMemberElements members =
    map (\ (value, name) -> X.NodeElement $ X.Element "Member" (M.fromList [("Name", name), ("Value", (T.pack (show value)))]) [] ) members

genMethodsElement :: [Method] -> [X.Node]
genMethodsElement [] = []
genMethodsElement methods = 
    [X.NodeElement $ X.Element "Methods" M.empty $ genMethodElements methods]

genMethodElements :: [Method] -> [X.Node]
genMethodElements [] = []
genMethodElements methods = 
    map genMethodElement methods

genMethodElement :: Method -> X.Node
genMethodElement Function{..} =
    X.NodeElement $ X.Element "Method" (M.fromList [("Name", methodName), ("IsInherited", (T.pack (show methodInherit)))]) $ (genParametersElement methodArgs) ++ [(X.NodeElement (X.Element "Return" M.empty (genTypeElement methodRetType)))]
genMethodElement (InheritName name) =
    X.NodeElement $ X.Element "Method" (M.fromList [("Name", name), ("IsInherited", "true")]) []
genMethodElement InheritAll =
    X.NodeElement $ X.Element "InheritAll" M.empty []

genParametersElement :: [Field] -> [X.Node]
genParametersElement [] = []
genParametersElement params =
    [X.NodeElement $ X.Element "Parameters" M.empty $ genFieldElements params]

{-
 - Utilities 
 -}

-- Note: System.FilePath.FilePath is "type FilePath = String"
toSystemFilePath :: FilePath -> P.FilePath
toSystemFilePath path = P.fromText $ T.pack path

xName :: String -> X.Name
xName str = X.Name (T.pack str) Nothing Nothing