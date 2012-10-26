module Types where

data SObjectType = Opportunity
                 | OpportunityLineItem
                 | Campaign
                 | Campaign_Influence__c
                 | Account
                 | Discount_Partner_Level__c
                 | PriceBookEntry
                 | Product2
                   deriving(Show, Eq, Ord)

data Relationship = Relationship [SObjectType] String
                    deriving(Show, Eq, Ord)

data Expr = Var String
          | Con Bool
          | Null
          | Duo Expr Duop Expr
          | Rel Relationship
          | StringLiteral String
          deriving(Show)

data Duop = Equals
          | NotEquals
            deriving(Show)

data Assignment = Relationship := Relationship
                  deriving(Show)

data Constraint = Constraint Relationship Duop Expr
                  deriving(Show)

type Definition = ([Constraint], Assignment)
