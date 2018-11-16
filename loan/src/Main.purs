module Main where

import Prelude
import Effect (Effect)
import Effect.Console (log)
import Data.Tuple
import Data.List (filter)
import Data.Maybe

main :: Effect Unit
main = do
  log "Hello sailor!"

add x y = x + y


--https://github.com/alfredherr/Demo/blob/master/Loaner/BoundedContexts/MaintenanceBilling/BusinessRules/CommandToBusinessRuleMap.txt
-- CommandToBusinessRuleMap
--BillingAssessment
-- |AssessTaxAsPercentageOfDuesDuringBilling
-- |TaxPercentageRate=[Double]
-- |Assess a tax concept to billing based on the amount of Dues.

type BusinessRuleMap = List BusinessRule
type BusinessRule =
  { command     :: Command
  , rule        :: Rule
  , parameters  :: Parameters
  , description :: String
  }

parseBusinessRuleMap :: String -> Maybe BusinessRuleMap
parseBusinessRuleMap s =
  Just (
  { command     : ""
  , rule        : ""
  , parameters  : ({name: "", value: ""} : Nil)
  , description : ""
  } : Nil)

--Read and parse a business rules file
--https://github.com/alfredherr/Demo/blob/master/Loaner/BoundedContexts/MaintenanceBilling/BusinessRules/Handler/AccountBusinessRulesMapper.cs#L249

--AccountBusinessRuleMapModel
--ClientRaintree-VILLADELMAR-*
--  |BillingAssessment
--  |AssessTaxAsPercentageOfDuesDuringBilling
--  |TaxPercentageRate=8.9

-- Client-Portfolio-Account
-- Command
-- Rule to map
-- Parameters(comma separated key value pairs)
type AccountRuleMap = List AccountRule
type AccountRule = 
  { client     :: Client
  , portfolio  :: Portfolio
  , account    :: Account
  , command    :: Command
  , rule       :: Rule
  , parameters :: Parameters  
  }

type Client = String
type Portfolio = String
type Account = String
type Command = String
type Rule = String
type Parameters = List Parameter
type Parameter = 
  { name  :: String
  , value :: String --type this
  }
type ParameterValue =
  { type  :: ParameterType
  , value :: String --This is not good
  }
data ParameterType = 
    String
  | Double
  -- | StateAbbreviation
  -- | OnePerState


parseRuleMap :: String -> Maybe AccountRuleMap
parseRuleMap s =
  Just (
  { client     : ""
  , portfolio  : ""
  , account    : ""
  , command    : ""
  , rule       : ""
  , parameters : ({name: "", value: ""} : Nil)
  } : Nil)



--https://github.com/alfredherr/Demo/blob/master/Loaner/BoundedContexts/MaintenanceBilling/BusinessRules/Rules/ClientSpecificRuleForCalculatingTax.cs#L45
-- RunRule(BillingAssessment com)









-- ProcessPayment(PayAccount cmd) :: PaymentAppliedToObligation
--Takes command `PayAccount` which contans an `AmountToPay`
--Finds the `AccountAdjustments` obligation
  
-- Creates a `PaymentAppliedToObligation`
  --  which takes: 
  --  `ObligationNumber`,
  --  `CreditCardPayment` which takes the `AmountToPay`
  --  `AmountToPay`

data Obligation = 
    AccountAdjustments

type AccountNumber = Number
--What if an Account is paid off? Hence no obligations
data Account =
    AccountWithObligations
      { accountNumber :: AccountNumber
      , obligations :: List Obligation
      }
type PayAccount =
  { account       :: Account
  , amountToPay   :: AmountToPay
  }
type AmountToPay = Number
data Payment =
    CreditCardPayment AmountToPay
type PaymentAppliedToObligation =
  { obligation       :: Obligation
  , payment          :: Payment
  , amountToPay      :: AmountToPay
  , description      :: String
  }

processPayment :: PayAccount -> PaymentAppliedToObligation
processPayment { account: obligations, amountToPay } =
  let
    hasAccountAdjustment =
      filter (\t -> t == AccountAdjustments) obligations
    payment =
      { obligation       : AccountAdjustment
      , payment          : CreditCardPayment amountToPay
      , amountToPay      : amountToPay
      , description      : "CreditCard Payment Applied To Dues"
      }
  in
    hasAccountAdjustment <??> payment


-- ProcessBilling(BillingAssessment command)
-- AddObligation(AddObligationToAccount command)



