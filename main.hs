
isZero x = (if x == 0 then True else False)

data Money = Money Double deriving (Show)

-- a cover is a contractual obligation to insure some risk against some event
  -- there is a cost ("cover premium") associated with providing this cover
  -- the cost depends on material facts of the risk & event
-- a cover premium is calculated based on one or more data fields (statements) in the Rfq,
-- and some rating criteria set by the insurer
 -- table lookups
 -- multipliers ("load factors"?)

data Cover = Cover (String, Money) deriving (Show)

-- a contract has offer and acceptance: our quoting service is only offering

data Decision = Offer Cover |  Rejection String deriving (Show)

-- respondToRfq :: [RatingTable] -> [Statement] -> Decision


-- find largest el in table which is smaller than val
lookupBand (Just table) val =
  let f (upperLimit, _) = (fromIntegral(upperLimit) > val) in
    head (filter f table)

myTables = [("turnover-bands",
             [(10000, 0.1),
               (20000, 0.05),
               (50000, 0.04),
               (100000, 0.02)])]
           
  
maybeOfferCover tables facts = 
  let (Just trn) = (lookup "turnover" facts)
      ttable = (lookup "turnover-bands" tables)
      (threshold, rate) = (lookupBand ttable trn) in
    Offer (Cover ("thanks", Money (trn * rate)))

-- maybeOfferCover myTables  [("turnover", 15000.0)]
  
commissionRate = 0.1


applyCommission :: Money -> Money

applyCommission (Money premium) = Money ((1 + commissionRate) * premium)

  
-- a quotation is a collection of covers for the same customer (usually for
-- different events on the same risk).
--   . (net) premium : sum of the cover premiums
--   . basis of quotation: union of all the material facts
--   . commission, gross, tech, taxed premiums to be introduced later



-- requestQuote :: Rates -> Rfq -> Quotation

