import qualified Data.Map as Map
import qualified Data.List as List
import Test.QuickCheck

  
-- Some background to the problem:

-- An insurance cover is a contractual obligation to insure some thing
-- or person or service ("the risk", or "the insured") against some
-- event

  -- there is a cost ("cover premium") associated with providing this cover
  -- the cost depends on material facts of the risk & event
  -- material facts are also known as "risk variables"

-- Cover premium is calculated based on some or all of the risk
-- variables, and rating criteria set by the insurer.  Classic rating
-- model design is in two steps:

-- First we choose the "exposure base".  This is the "basic unit of
-- risk" to which the insurance rate applies: it should be some very
-- good proxy for expected losses (e.g. turnover, or number of
-- employees, or value of property). Note that this is often
-- constrained by historical precedent so that we can meaningfully
-- compare expected losses of an entire book of business with other
-- insurers in the same market

-- So, to get the base premium we choose a rate from a table according
-- to the number of units of the exposure base, then multiply by the
-- said number of units.  The table may be flat/regressive/progressive
-- depending on the relationship between the "size" of the insured &
-- expected loss

-- THe second step, once we have the base premium, is to add "loads"
-- and discounts, by applying other rates using other tables based on
-- other risk variables.  For example, postcode loadings or discounts
-- for years without claim.

-- A note on rate calculation:

-- With any model that has {pro,re}gressive rates it is worth
-- checking that the premium is a continuous function of the number of
-- units.  Otherwise there are weird discontinuities in the premium
-- payable as a business size increases.  One approach which
-- guarantees this, is to use "marginal" rates: if there is an
-- inflection at, say, £40000 turnover, then charge x% on the first
-- £40k and x-y% on (remaining turnover - 40000).  HMRC do this for
-- income tax.  This may be combined with a flat fee for the first
-- £x000 before unit rates kick in

-- The result of this two-step calculation is known as the "technical
-- premium".  There are a bunch of other things we will probably want
-- to do subsequently, like add commissions, or enforce a minimum
-- premium, or add tax.  We may also want to offer multiple covers in
-- the same transaction, but I'm deferring the decision of how to do
-- that for the moment

-- -- -- -- --

-- OK, here goes

-- -- -- -- --       

-- It would be good to be able to use money.  We introduce a Money
-- type which wraps a Double (yes, I know that floating point money is
-- a bad idea, this is temporary)

data Money = Money Double deriving (Show)

instance Eq Money where (Money m1) == (Money m2) = m1 == m2
instance Ord Money where (Money m1) `compare` (Money m2) = m1 `compare` m2

-- We can't define (*) for Money unless we make it a member of the
-- Num, typeclass, but that would mean defining all the numeric
-- operations for it.  Which I think would be wrong because you can't
-- multiply it by itself (or divide anything by it).  But it is nice
-- to be able to multiply a sum of money by a scalar.

multiply rate (Money m) = Money (m * rate)

-- A Cover contains some attributes of some kind (don't yet know what,
-- String is used here as a placeholder) and a premium.

data Cover = Cover (String, Money) deriving (Show)

-- But this is a quoting service, not an insurance purchase service,
-- so we can't actually form contracts - only offers.  Our output is
-- going to be an offer of a cover, which the purchaser chooses to
-- accept (or not)

data Decision = Offer Cover |  Rejection String deriving (Show)

-- Rate is basically an alias for Double

type Rate = Double

-- Right now I think there should be a unified interface to applying a
-- rate from any kind of rating table, but may change my mind.

data RateTable a = Bands [(a, Rate)]
                 | MarginalBands [(a, Rate)]
                 | Match [(a, Rate)]
                 deriving (Show)

exampleEmployeesTable = Bands [(0, 0.2),
                               (4, 0.1),
                               (10, 0.05),
                               (25, 0.04),
                               (100, 0.02)]


findEntry (Bands table) index = 
  let f (upperLimit, _) = (upperLimit <= index) in
    last (filter f table)

-- Discovering properties is hard.  Here are some I thought of.

-- (1) the found entry e should be a member of the table

prop_findEntry_ret_in_table :: NonNegative Int -> Bool
prop_findEntry_ret_in_table (NonNegative index) =
  let rows = [(0, 0.2),
              (4, 0.1),
              (10, 0.05),
              (25, 0.04),
              (100, 0.02)]
      e = findEntry (Bands rows) index in
    elem e rows

-- (2) the threshold of e is <= the search index

prop_findEntry_ret_smaller_than_index :: NonNegative Int -> Bool
prop_findEntry_ret_smaller_than_index (NonNegative index) =
  let rows = [(0, 0.2),
              (4, 0.1),
              (10, 0.05),
              (25, 0.04),
              (100, 0.02)]
      (th, r) = findEntry (Bands rows) index in
    th <= index

-- (3) no entry in table has a threshold higher than e and lower than index

-- This is kind of complicated, and I don't much like it for that
-- reason.  I think usually tests should be less complicated than the
-- code they're testing, because I like it to be obvious which of them is
-- wrong when they disagree.

prop_findEntry_no_better_row :: NonNegative Int -> Bool
prop_findEntry_no_better_row (NonNegative index) =
  let rows = [(0, 0.2),
              (4, 0.1),
              (10, 0.05),
              (25, 0.04),
              (100, 0.02)]
      (th, r) = findEntry (Bands rows) index in
    null (filter (\ (r_th, _) -> (r_th > th) && (r_th < index)) rows)


          
applyRate (Match table) index val = 
  let (Just rate) = lookup index table in
       multiply rate val

applyRate (Bands table) index val =
  let (threshold, rate) = findEntry (Bands table) index in
      multiply rate val


exampleTurnoverTable = Bands [(Money 10000.0, 0.1),
                              (Money 20000.0, 0.05),
                              (Money 50000.0, 0.04),
                              (Money 100000.0, 0.02)]

data Postcode = Postcode String
instance Eq Postcode where (Postcode p1) == (Postcode p2) = p1 == p2

-- presently applyRate requires that the index argument implements
-- Ord, because its definition on Bands uses (<).  This is not
-- the way I'd like things to work, but I don't know enough Haskell
-- to be able to fix it
instance Ord Postcode where (Postcode p1) `compare` (Postcode p2) =
                              p1 `compare` p2


exampleFloodTable = Match [(Postcode "SE1", 1),
                           (Postcode "NN1", 1.2)]

exampleTheftTable = Match [(Postcode "SE1", 1),
                           (Postcode "NN1", 1.2)]


-- this is syntactic sugar so we can write the rating stages in
-- "natural" order

pipeline functions = foldl (.) id (reverse functions) 


-- here is our made-up premium calculator

costOfEL turnover postcode =
  (pipeline [(applyRate exampleTurnoverTable turnover),
             (applyRate exampleFloodTable postcode),
             (applyRate exampleTheftTable postcode)])
    turnover
