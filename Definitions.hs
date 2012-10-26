module Definitions where

import Types

accnt :: Definition
accnt = (constraints, assignment)
      where
        constraints = [ Constraint (Relationship [OpportunityLineItem, Opportunity] "isActive") Equals (Con True)
                      , Constraint (Relationship [Discount_Partner_Level__c] "Product__c") Equals (Rel (Relationship [OpportunityLineItem, PriceBookEntry] "Product2Id"))
                      , Constraint (Relationship [Discount_Partner_Level__c] "Partner_Participation_Level__c") Equals (Rel (Relationship [OpportunityLineItem, Opportunity, Account] "Partner_Participation_Level__c"))
                      , Constraint (Relationship [Discount_Partner_Level__c] "Partner_Participation_Level__c") NotEquals Null
                      , Constraint (Relationship [Discount_Partner_Level__c] "Partner_Participation_Level__c") NotEquals (StringLiteral "")
                      ]
        assignment = (Relationship [OpportunityLineItem] "Account_Discount__c") := (Relationship [Discount_Partner_Level__c] "Discount__c")

dealreg :: Definition
dealreg = (constraints, assignment)
  where
    constraints = [ Constraint (Relationship [OpportunityLineItem, Opportunity] "isClosed") Equals (Con False)
                  , Constraint (Relationship [OpportunityLineItem, Opportunity] "RecordTypeId") Equals (StringLiteral "RecordTypeId")
                  ]
    assignment = (Relationship [OpportunityLineItem] "Deal_Reg_Discount__c") := (Relationship [OpportunityLineItem, PriceBookEntry, Product2] "Deal_Reg_Discount__c")

oliDiscountFromOpptyAccount :: Definition
oliDiscountFromOpptyAccount = (constraints, assignment)
  where
    constraints = [ Constraint (Relationship [OpportunityLineItem, Opportunity] "isClosed") Equals (Con False)
                  ]
    assignment = (Relationship [OpportunityLineItem] "Discount__c") := (Relationship [OpportunityLineItem, Opportunity, Account] "Discount__c")
