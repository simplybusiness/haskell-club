import qualified Data.Map as Map 

isZero x = (if x == 0 then True else False)

data Money = Money Double deriving (Show)

instance Eq Money where (Money m1) == (Money m2) = m1 == m2
instance Ord Money where (Money m1) `compare` (Money m2) =
                           m1 `compare` m2

-- a cover is a contractual obligation to insure some risk against some event
  -- there is a cost ("cover premium") associated with providing this cover
  -- the cost depends on material facts of the risk & event
  -- material facts are also known as "risk variables"

-- cover premium is calculated based on some or all of the risk variables,  and rating criteria set by the insurer
 -- table lookups
 -- multipliers ("load factors"?)

-- exposure base is the "basic unit of risk" to which the insurance
-- rate applies: some very good proxy for expected losses.  Classic
-- rating model design is to (1) apply a rate to the number of units
-- of the exposure base, which may be flat/regressive/progressive depending
-- on relationship btwn insured "size" & expected loss, (2) fiddle with it
-- based on other factors.

-- with any model that has {pro,re}gressive rates it is worth checking
-- that the premium is a continuous function of the number of units

-- one approach to applying rating discounts, which guarantees this,
-- is to use "marginal" rates: if there is an inflection at, say,
-- £40000 turnover, then charge x% on the first £40k and x-y% on
-- (remaining turnover - 40000) - like HMRC do for income tax.  This
-- may be combined with a flat fee for the first £x000 before unit
-- rates kick in

-- once we've done the basic rate and the fiddling, we may also want
-- to apply a minimum premium (account for fixed-cost overheads of
-- selling & managing the policy)


-- I have a vague idea that a Cover should include details of the
-- thing covered: relevant risk variables or similar.  String is
-- obviously not the right type for this, but I haven't decided what
-- is.

data Cover = Cover (String, Money) deriving (Show)

-- a contract has offer and acceptance: our quoting service is only offering

data Decision = Offer Cover |  Rejection String deriving (Show)

type Rate = Double

data RateData a = FixedFee a
                | FlatRate Rate
                | Bands [(a, Rate)]
                | MarginalBands [(a, Rate)]
                deriving (Show)

applyRate (Money m) r = Money(m * r)

-- computePrice :: (RateData a) -> a -> a

computePrice (FixedFee m) val = m

computePrice (FlatRate m) val = applyRate val m 

computePrice (Bands table) val =
  let f (upperLimit, _) = (upperLimit > val)
      (threshold, rate) = head (filter f table) in
      computePrice (FlatRate rate) val


myTables = [("turnover-bands",
             Bands [(Money 10000.0, 0.1),
                    (Money 20000.0, 0.05),
                    (Money 50000.0, 0.04),
                    (Money 100000.0, 0.02)])]

project m fields = Map.intersection m fieldsMap
  where fieldsMap = (Map.fromList (Prelude.map (\l -> (l,l)) fields))


maybeOfferCover tables facts = 
  let materialFacts = project (Map.fromList facts) ["turnover"]
      trn = materialFacts Map.! "turnover"
      (Just ttable) = (lookup "turnover-bands" tables)
      price = computePrice ttable trn in
    Offer (Cover ("thanks", price))

-- maybeOfferCover myTables  [("turnover", (Money 15000.0))]

  
commissionRate = 0.1


applyCommission :: Money -> Money

applyCommission (Money premium) = Money ((1 + commissionRate) * premium)

  
-- a quotation is a collection of covers for the same customer (usually for
-- different events on the same risk).
--   . (net) premium : sum of the cover premiums
--   . basis of quotation: union of all the material facts
--   . commission, gross, tech, taxed premiums to be introduced later



-- requestQuote :: Rates -> Rfq -> Quotation

