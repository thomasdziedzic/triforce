{-# LANGUAGE OverloadedStrings #-}

import qualified Data.Map as M
import qualified Data.Set as S
import Data.List (isSuffixOf, intercalate)
import Data.Char (toLower)
import System.Directory (createDirectoryIfMissing, setCurrentDirectory)
import Types
import Definitions

import Data.String (fromString)
import qualified Data.ByteString.Lazy as BS
import qualified Data.Text as T
import qualified Data.Text.Lazy.Encoding as E

import Data.Text.Template

context :: [(T.Text, T.Text)] -> Context
context assocs x = maybe err id . lookup x $ assocs
  where err = error $ "Could not find key: " ++ T.unpack x

-- TODO move this into types?
type Field = String
type Fields = S.Set Field

convertRelatedObjectToField :: String -> String
convertRelatedObjectToField s
    | "__r" `isSuffixOf` s = init s ++ "c"
    | otherwise = s ++ "Id"

relationshipSet :: Relationship -> S.Set Relationship
relationshipSet oneLevelDeep@(Relationship [] field) = undefined
relationshipSet oneLevelDeep@(Relationship (so:[]) field) = S.singleton oneLevelDeep
relationshipSet topLevel@(Relationship sos field) = (S.singleton topLevel) `S.union` nextRelationShip
  where
    lastRelatedObject = last sos
    relatedFieldName = convertRelatedObjectToField $ show lastRelatedObject
    nextRelationShip = relationshipSet (Relationship (init sos) relatedFieldName)

assignmentToRelationshipSet :: Assignment -> S.Set Relationship
assignmentToRelationshipSet (lRelationship := rRelationship) = (relationshipSet lRelationship) `S.union` (relationshipSet rRelationship)

constraintToRelationshipSet :: Constraint -> S.Set Relationship
constraintToRelationshipSet (Constraint r1 _ (Rel r2)) = (relationshipSet r1) `S.union` (relationshipSet r2)
constraintToRelationshipSet (Constraint r _ _)         = relationshipSet r

constraintsToRelationshipSet :: [Constraint] -> S.Set Relationship
constraintsToRelationshipSet []     = S.empty
constraintsToRelationshipSet (x:xs) = (constraintToRelationshipSet x) `S.union` (constraintsToRelationshipSet xs)

definitionToRelationshipSet :: Definition -> S.Set Relationship
definitionToRelationshipSet (constraints, assignment) = (constraintsToRelationshipSet constraints) `S.union` (assignmentToRelationshipSet assignment)

compileSet :: S.Set Relationship -> M.Map [SObjectType] Fields
compileSet relationshipSet = relationshipMap
  where
    relationshipMap = S.foldr test M.empty relationshipSet
    test (Relationship sobjects fieldname) y = 
        case (M.lookup sobjects y) of
            Nothing -> M.insert sobjects (S.singleton fieldname) y
            Just fieldnames -> M.insert sobjects (S.singleton fieldname `S.union` fieldnames) y

-- TODO get rid of the need to generate intermediate sets of relationships and just generate the map directly
compileDefinition :: Definition -> M.Map [SObjectType] Fields
compileDefinition definition = compileSet . definitionToRelationshipSet $ definition

relationshipToStringQuery :: Relationship -> String
relationshipToStringQuery (Relationship (x:xs) field) = prefix ++ field
  where
    prefix
      | null xs   = ""
      | otherwise = (intercalate "." (map show xs)) ++ "."

assignmentSpecToTrigger :: Definition -> String ->  (FilePath, BS.ByteString)
assignmentSpecToTrigger definition@(constraints, assignment) templateContents = (filename, filecontents)
  where
    relationships = definitionToRelationshipSet definition
    (Relationship sobjects _) = S.findMin relationships
    mainObject = head sobjects
    filename = show mainObject ++ ".trigger"
    fileContext = context [ ("mainObject", T.pack . show $ mainObject)
                          , ("queryFieldString", T.pack queryFieldString)
                          , ("conditionString", T.pack conditionString)
                          , ("assignmentString", T.pack assignmentString)
                          ]
    filecontents = E.encodeUtf8 $ substitute (T.pack templateContents) fileContext
    relationshipList = S.toList relationships
    queryFields = map relationshipToStringQuery relationshipList
    queryFieldString
      | null queryFields = ""
      | otherwise        =  ", " ++ (intercalate ", " queryFields)
    conditionString = "(" ++ (intercalate ") && (" constraintStrings) ++ ")"
    constraintStrings = (map (constraintToConditionString mainObject) constraints)
    (lrelationship := rrelationship) = assignment
    assignmentString = (relationshipToMainString mainObject lrelationship) ++ " = " ++ (relationshipToQueryString mainObject rrelationship) ++ ";"

otherSpecToTrigger :: ([SObjectType], Fields) -> String -> (FilePath, BS.ByteString)
otherSpecToTrigger spec@(sobjects, fields) templateContents = (filename, filecontents)
  where
    mainObject = last sobjects
    assignmentObject = head sobjects
    filename = show mainObject ++ ".trigger"
    fileContext = context [ ("mainObject", T.pack . show $ mainObject)
                          , ("fieldChangeConditional", T.pack fieldChangeConditional)
                          , ("mainObjectFieldOnTriggerObject", T.pack mainObjectFieldOnTriggerObject)
                          , ("assignmentObject", T.pack . show $ assignmentObject)
                          ]
    filecontents = E.encodeUtf8 $ substitute (T.pack templateContents) fileContext
    fieldList = S.toList fields
    fieldChangeConditional = intercalate " || " $ map (\x -> "oldTriggerObject." ++ x ++ " != triggerObject." ++ x) fieldList
    mainObjectFieldOnTriggerObject = intercalate "." $ (map show middlePieces) ++ [(show mainObject ++ "Id")]
    middlePieces = if length sobjects > 2 then (tail . init) sobjects else []

definitionToTriggers :: Definition -> IO [(FilePath, BS.ByteString)]
definitionToTriggers definition@(conditions, assignment) = mapM (specToTrigger definition) compiledList
  where
    compiledDefinition :: M.Map [SObjectType] Fields
    compiledDefinition = compileDefinition definition
    compiledList :: [([SObjectType], Fields)]
    compiledList = M.toList compiledDefinition

specToTrigger :: Definition -> ([SObjectType], Fields) -> IO (FilePath, BS.ByteString)
specToTrigger definition@(conditions, assignment) triggerMap@(sobjects, fields)
    | assignmentSobjects == sobjects = readFile "../templates/assignment.template" >>= (return . assignmentSpecToTrigger definition)
    | otherwise                      = readFile "../templates/observer.template" >>= (return . otherSpecToTrigger triggerMap)
  where
    ((Relationship assignmentSobjects _) := _) = assignment
    mainObject = head assignmentSobjects

constraintToConditionString :: SObjectType -> Constraint -> String
constraintToConditionString mainObject (Constraint relationship Equals expr) = (relationshipToQueryString mainObject relationship) ++ " == " ++ (expressionToApex expr)
constraintToConditionString mainObject (Constraint relationship NotEquals expr) = (relationshipToQueryString mainObject relationship) ++ " != " ++ (expressionToApex expr)

expressionToApex :: Expr -> String
expressionToApex (Var name) = name
expressionToApex (Con bool) = map toLower $ show bool
expressionToApex Null = "null"
expressionToApex (Duo exp1 duop exp2) = undefined
expressionToApex (Rel relationship) = undefined
expressionToApex (StringLiteral s) = "'" ++ s ++ "'"

relationshipToQueryString :: SObjectType -> Relationship -> String
relationshipToQueryString mainObject (Relationship sobjects field) = (intercalate "." sobjectStrings) ++ "." ++ field
  where
    sobjectStrings = map (showObject mainObject "queriedObject") sobjects

relationshipToMainString :: SObjectType -> Relationship -> String
relationshipToMainString mainObject (Relationship sobjects field) = (intercalate "." sobjectStrings) ++ "." ++ field
  where
    sobjectStrings = map (showObject mainObject "mainObject") sobjects

showObject :: SObjectType -> String -> SObjectType -> String
showObject mainObject mainObjectString object
  | mainObject == object = mainObjectString
  | otherwise            = show object

--main = createDirectoryIfMissing True "generated" >> setCurrentDirectory "generated" >> (writeFiles . definitionToTriggers) dealreg
main = do
    createDirectoryIfMissing True "generated"
    setCurrentDirectory "generated"
    filesToWrite <- definitionToTriggers oliDiscountFromOpptyAccount
    writeFiles filesToWrite
  where
    writeFiles = mapM_ (uncurry BS.writeFile)

