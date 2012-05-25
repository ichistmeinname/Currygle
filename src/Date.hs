-- ----------------------------------------------------------------------------

{- |
  Module     : Date

  Maintainer : Thorben Guelck, Tobias Lueders, Mathias Leonhardt, Uwe Schmidt
  Stability  : experimental
  Portability: portable

  Parsers for Strings containing date-expressions and functions to normalize them
  or convert normalized dates back to human-readable dates.
  This module also contains some helper functions for date operations.

-}

-- ----------------------------------------------------------------------------

module Date
  ( extractDateRepM
  , extractDateRep
  , normalizeDate
  , dateRep2NormalizedDates
  , dateRep2DatesContext
  , prepareNormDateForCompare
  , dateRep2stringWithTransformedDates
  , unNormalizeDate
  , cmpDate
  )
where

import Control.Arrow                    ( second )
import Control.Monad

import Data.Char                        ( toLower
                                        , toUpper
                                        )
import Data.List                        ( isPrefixOf )
import Data.Maybe                       ( fromMaybe )

import Text.Parsec
import Text.Regex.XMLSchema.String

import Data.Time.Clock
import Data.Time.Calendar
import Data.Time.Calendar.WeekDate

import Helpers

-- ------------------------------------------------------------

-- some little helpers for building r.e.s

star :: String -> String
star
    = (++ "*") . pars

plus :: String -> String
plus
    = (++ "+") . pars

opt :: String -> String
opt
    = (++ "?") . pars

dot :: String -> String
dot
    = (++ "\\.")

optDot :: String -> String
optDot
    = (++ alt [ws,"\\."])

pars :: String -> String
pars
    = ("(" ++) . (++ ")")

orr :: String -> String -> String
orr x y
    = pars $ pars x ++ "|" ++ pars y

xor :: String -> String -> String
xor x y
    = pars $ pars x ++ "{|}" ++ pars y

nocase :: String -> String
nocase []
    = []
nocase (x:xs)
    = '[' : toUpper x : toLower x : ']' : xs

suffix :: String -> String
suffix
    = foldr (\ x xs -> "(" ++ x : xs ++ ")?") ""

alt :: [String] -> String
alt
    = pars . foldr1 orr

altNC :: [String] -> String
altNC
    = pars . alt . map nocase

subex :: String -> String -> String
subex n e
    = pars $ "{" ++ n ++ "}" ++ pars e

ws :: String
ws = "\\s"

ws0 :: String
ws0 = star ws

ws1 :: String
ws1 = plus ws

s0 :: String -> String -> String
s0 x y
    = x ++ ws0 ++ y

-- the date and time r.e.s

day :: String
day
    = "(0?[1-9]|[12][0-9]|3[01])"

month :: String
month
    = "(0?[1-9]|1[0-2])"

year2 :: String
year2
    = "[0-5][0-9]"

year4 :: String
year4
    = alt ["19\\d{2}", "20" ++ year2]

year :: String
year
    = year4 `orr` year2 -- ! orr year' ?

year' :: String
year'
    = "'" ++ year2

dayD :: String
dayD
    = optDot day

monthD :: String
monthD
    = dot month

dayMonthYear :: String
dayMonthYear
    = dayD `s0` monthD `s0` year

dayMonth :: String
dayMonth
    = dayD `s0` monthD

dayOfWeekL :: String
dayOfWeekL
    = altNC
      [ "montag"
      , "dienstag"
      , "mittwoch"
      , "donnerstag"
      , "freitag"
      , "samstag"
      , "sonnabend"
      , "sonntag"
      ]

dayOfWeekA :: String
dayOfWeekA
    = alt . map dot $
      [ "Mo", "Di", "Mi", "Do", "Fr", "Sa", "So"]

dayOfWeekU :: String
dayOfWeekU
    = altNC
      [ "mon", "tue", "wed", "thu", "fri", "sat", "son"]

dayOfWeek :: String
dayOfWeek
    = dayOfWeekL `orr` dayOfWeekA `orr` dayOfWeekU

monthNames :: [String]
monthNames
    = [ "Januar"
      , "Februar"
      , "M\228rz"
      , "April"
      , "Mai"
      , "Juni"
      , "Juli"
      , "August"
      , "September"
      , "Oktober"
      , "November"
      , "Dezember"
      ]

monthL :: String
monthL
    = altNC monthNames

monthA :: String
monthA
    = altNC . map optDot $ map snd monthAbr

monthAbr :: [(Int, String)]
monthAbr
    = (3, "mar") : (5, "may") : (9, "sept") : (10, "oct") : (12, "dec") :
      ( zip [1..12]
        [ "jan", "feb", "m\228r", "apr", "mai", "jun", "jul", "aug", "sep", "okt", "nov", "dez"]
      )

monthN :: String
monthN
    = pars $ monthL `orr` monthA

hour :: String
hour
    = pars "([0-1]?[0-9])|(2[0-4])"

minute :: String
minute
    = pars "(0?[0-9])|([1-5][0-9])"

uhr :: String
uhr
    = ws0 ++ nocase "uhr"

hourMin :: String
hourMin
    = hour ++ ":" ++ minute ++ opt uhr

wsyear :: String
wsyear
    = year ++ "/[0-9]{2}"

wsem :: String
wsem
    = ("Wi?Se?" `orr` nocase "Wintersemester") ++ ws0 ++ wsyear

ssem :: String
ssem
    = ("So?Se?" `orr` nocase "Sommersemester") ++ ws0 ++ year

num :: String
num
    = "\\d+"

dateAlias :: String
dateAlias
    = alt $ map fst dateAliasFunc

-- -----------------------------------------------------------------
-- | r.e.s for date aliases are expanded to Lists of Dates by "scanDateAlias" (see below).
-- | The second value of a tuple is a function that is applied to the actual date ("today").
-- | The Result of this function is the List of dates corresponding to the date-alias String.

dateAliasFunc :: [(String, Day -> [Day])]
dateAliasFunc
    = [ ("heute",           box)
      , ("morgen",          box . addDays 1)
      , ("Morgen",          box . addDays 1)
      , ("diese Woche",     extractWeek)
      , ("Diese Woche",     extractWeek)
      , ("n\228chste Woche",   extractWeek . addDays 7)
      , ("N\228chste Woche",   extractWeek . addDays 7)
      , ("dieser Monat",    extractMonth)
      , ("Dieser Monat",    extractMonth)
      , ("n\228chster Monat",  extractMonth . addMonth)
      , ("N\228chster Monat",  extractMonth . addMonth)
      -- , ("\252bern\228chster Monat", extractMonth . addMonth . addMonth)
      -- , ("\220bern\228chster Monat", extractMonth . addMonth . addMonth)
      ]

dateAliasMatch :: [(String, String -> Bool)]
dateAliasMatch
    = map (second match) $
      [ ( "heute"
        , nocase "heu" ++
          suffix "te"
        )
      , ( "morgen"
        , nocase "mor" ++
          suffix "gen"
        )
      , ( "diese Woche"
        , nocase "di" ++
          suffix "ese" ++
          ws0 ++
          nocase "wo" ++
          suffix "che"
        )
      , ( "n\228chste Woche"
        , nocase "n(ae?|\228)" ++
          suffix "chste" ++
          ws0 ++
          nocase "wo" ++
          suffix "che"
        )
      , ( "dieser Monat"
        , nocase "di" ++
          suffix "eser" ++
          ws0 ++
          nocase "mo" ++
          suffix "nat"
        )
      , ( "n\228chster Monat"
        , nocase "n(ae?|\228)" ++
          suffix "chste" ++
          ws0 ++
          nocase "mo" ++
          suffix "nat"
        )
      ]

matchDateAlias :: String -> Maybe String
matchDateAlias s
    = foldr (\ x xs -> match' s x `mplus` xs) mzero dateAliasMatch
    where
      match' s' (res, mf)
          | mf s'
              = Just res
          | otherwise
              = Nothing

monthAliasMatch :: [(Int, String -> Bool)]
monthAliasMatch
    = zip [1..12] . map toFct $ monthNames
      where
        toFct
            = (match $) . uncurry mkRE . splitAt 3
            where
              mkRE x y
                  = nocase x ++ suffix y

matchMonthAlias :: Int -> Int -> String -> Maybe String
matchMonthAlias thisYear thisMonth s
    = fmap format $ month'
    where
      format m
          = (monthNames !! (m - 1)) ++ " " ++ show (thatYear m)
      thatYear m
          | m >= thisMonth = thisYear
          | otherwise      = thisYear + 1
      month'
          = foldr (\x xs -> match' s x `mplus` xs) mzero monthAliasMatch
          where
            match' s' (i, mf)
                | mf s'
                    = Just i
                | otherwise
                    = Nothing

-- the token types
tokenRE :: String
tokenRE
    = foldr1 xor $
      map (uncurry subex) $
              [ ( "ddmmyyyy",     dayMonthYear )
              , ( "ddMonthyyyy",  dayD `s0` monthN `s0` (year `orr` year') )
              , ( "monthyyyy",    monthN `s0` (year `orr` year') )
              , ( "ddmm",         dayMonth)
              , ( "ddMonth",      dayD `s0` monthN )
              , ( "yyyymmdd",     year ++ "[-/]" ++ month ++ "[-/]" ++ day )
              , ( "yyyy",         year4 `orr` ("'" ++ year2) )
              , ( "month",        monthN )
              , ( "weekday",      dayOfWeek )
              , ( "HHMM",         hourMin ++ opt uhr )
              , ( "HH",           hour    ++ uhr )
              , ( "wsem",         wsem)
              , ( "ssem",         ssem)
              , ( "dateAlias",    dateAlias)
              , ( "href",         "href:[^\\s]+")
              , ( "word",         "[\\w\\d]+")
              , ( "del",          "[^\\w\\d]+")
              ]

type Token         = (String, String)
type TokenStream   = [Token]

type DateParser a  = Parsec TokenStream () a

type TextFunc          = String -> String   -- represent Strings as functions for fast concatenation

-- representation of a parsed date
data DateVal
    = DT { _year   :: ! Int -- "!": strictness flag
         , _month  :: ! Int
         , _day    :: ! Int
         , _hour   :: ! Int
         , _min    :: ! Int
         }
      deriving (Eq, Show)

-- | parse-result of text containing a date.
data DateParse
    = DP { _pre    ::   TextFunc     -- This is the text that precedes the date.
         , _rep    ::   TextFunc     -- This is the text that was recognized as a date.
         , _dat    :: ! DateVal      -- This is the representation of the parsed date.
         }

-- | just a helper for result output of DateParse.
data DateRep
    = DR { _p ::   String
         , _r ::   String
         , _d :: ! DateVal
         }
      deriving (Eq, Show)

-- ------------------------------------------------------------

-- return a function representing an empty string

emptyText       :: TextFunc
emptyText       = id

-- return a function representing a string

mkText          :: String -> TextFunc
mkText          = (++)

-- return a function representing a concatenation of two strings

concText        :: TextFunc -> TextFunc -> TextFunc
concText        = (.)

-- evaluate a function representing a string to the represented string

textToString    :: TextFunc -> String
textToString    = ($ [])

-- initialize a date representation

emptyDateVal :: DateVal
emptyDateVal
    = DT { _year   = -1
         , _month  = -1
         , _day    = -1
         , _hour   = -1
         , _min    = -1
         }

-- initialize the parse-result of a string                     

emptyDateParse  :: DateParse
emptyDateParse
    = DP { _pre = emptyText
         , _rep = emptyText
         , _dat = emptyDateVal
         }

-- append a string to the _pre-part of a DateParse

appPre :: String -> DateParse -> DateParse
appPre s d
    = d { _pre = (_pre d) `concText` (mkText s) }

-- append a string to the _rep-part of a DateParse

appRep :: String -> DateParse -> DateParse
appRep s d
    = d { _rep = (_rep d) `concText` (mkText s) }

-- assign values to a DateVal.
-- year-values like e.g. "7" are expanded to "2007"

setDateVal      :: Int -> Int -> Int -> Int -> Int -> DateVal -> DateVal
setDateVal j m t s i (DT j' m' t' s' i' )
                = DT j'' m'' t'' s'' i''
    where
      j'' | j < 0     = j'              -- year not there
          | j < 100   = j + 2000        -- 2 digit year
          | otherwise = j               -- 4 digit year
      m''             = m `max` m'
      t''             = t `max` t'
      s''             = s `max` s'
      i''             = i `max` i'

-- set the day in the _dat-part of a DateParse

setDay          :: Int -> Int -> Int -> (DateParse -> DateParse)
setDay j m t d  = d { _dat = setDateVal j m t (-1) (-1) (_dat d) }

-- set the hour in the _dat-part of a DateParse

setHour         :: Int -> Int -> DateParse -> DateParse
setHour h m d   = d { _dat = setDateVal (-1) (-1) (-1) h m (_dat d) }

-- evaluate the TextFuncs to strings in a DateParse

datePToDateRep  :: DateParse -> DateRep
datePToDateRep dp
                = DR { _p = textToString $ _pre dp
                     , _r = textToString $ _rep dp
                     , _d =                _dat dp
                     }

-- ------------------------------------------------------------
-- all date parsers thread a state to the subparsers to accumulate
-- the parts of a date, the context, the external representation and
-- the pure data, year, month, day, ...

dateParser      :: DateParse -> DateParser DateParse
dateParser d    = ( do
                    s <- fillTok                -- delTok <|> wordTok
                    dateParser0 (appPre s d)    -- append Token to _pre
                  )
                  <|>
                  parseDate d                   -- here is the hook for the real date parser
                  <|>
                  ( do
                    s <- textTok                -- the default case: if parseDate fails
                    dateParser0 (appPre s d)    -- the token is handled like a normal word
                  )

dateParser0     :: DateParse -> DateParser DateParse
dateParser0 d   = dateParser d <|> return d


-- a simple helper for showing the results
dateSearch'     :: TokenStream -> [DateRep]
dateSearch'     = map datePToDateRep .
                  dateSearch

-- look for a sequence of date specs, the last entry in the list
-- does not contain a valid date, but just the context behind the last real date

dateSearch      :: TokenStream -> [DateParse]
dateSearch ts   = either (const []) id .
                  parse (many (dateParser emptyDateParse)) "" $
                  ts

parseDate       :: DateParse -> DateParser DateParse
parseDate d     = parseDate0 d
                  <|>
                  try
                  ( do
                    d1 <- parseWeekDay d
                    lookAheadN 3 parseDate0 d1  -- Freitag, den 13.
                  )

-- parse a date optionally followed by a time
parseDate0      :: DateParse -> DateParser DateParse
parseDate0 d    = ( do
                    d1 <- parseDay d
                    option d1 (parseFollowingHour d1)
                  )

-- parse a simple token for a day
parseDay        :: DateParse -> DateParser DateParse
parseDay d      = ( do
                    (s, d') <- parseDateTok "ddmmyyyy" d
                    let [t, m, j] = tokenize num s
                    return $ setDay (read j) (read m) (read t) d'
                  )
                  <|>
                  ( do
                    (s, d') <- parseDateTok "ddMonthyyyy" d
                    let s' = sed ((++ ".") . monthToM) monthN s
                    let [t, m, j] = tokenize num s'
                    return $ setDay (read j) (read m) (read t) d'
                  )
                  <|>
                  ( do
                    (s, d') <- parseDateTok "ddmm" d
                    let [t, m] = tokenize num s
                    return $ setDay (-1) (read m) (read t) d'
                  )
                  <|>
                  ( do
                    (s, d') <- parseDateTok "ddMonth" d
                    let s'     = sed ((++ ".") . monthToM) monthN s
                    let [t, m] = tokenize num s'
                    return $ setDay (-1) (read m) (read t) d'
                  )
                  <|>
                  ( do
                    (s, d') <- parseDateTok "yyyymmdd" d
                    let [j, m, t] = tokenize num s
                    return $ setDay (read j) (read m) (read t) d'
                  )
                  <|>
                  ( do
                    (s, d') <- parseDateTok "monthyyyy" d
                    let s' = sed ((++ ".") . monthToM) monthN s
                    let [m, j] = tokenize num s'
                    return $ setDay (read j) (read m) (-1) d'
                  )
                  <|>
                  ( do
                    (_, d') <- parseDateTok "dateAlias" d
                    return $ setDay (-1) (-1) (-1) d'
                  )

-- parseYear       :: DateParse -> DateParser DateParse
-- parseYear d     = ( do
--                     (s, d') <- parseDateTok "yyyy" d
--                     let [j] = tokenize num s
--                     return $ setDay (read j) (-1) (-1) d'
--                   )

-- parse a weekday and add it to the external rep.

parseWeekDay    :: DateParse -> DateParser DateParse
parseWeekDay d  = ( do
                    (_s, d') <- parseDateTok "weekday" d
                    return d'
                  )

-- parse a following hour spec, 5 fill tokens, words or delimiters are possible

parseFollowingHour      :: DateParse -> DateParser DateParse
parseFollowingHour
                = try .                         -- backtracking becomes neccessary
                  lookAheadN 5 parseHour        -- max 2 words and 3 delimiters

-- parse the simple time formats
parseHour       :: DateParse -> DateParser DateParse
parseHour d     = ( do
                    (s, d') <- parseDateTok "HHMM" d
                    let [h, m] = tokenize num s
                    return $ setHour (read h) (read m) d'
                  )
                  <|>
                  ( do
                    (s, d') <- parseDateTok "HH" d
                    let [h] = tokenize num s
                    return $ setHour (read h) 0 d'
                  )

-- ------------------------------------------------------------
--
-- auxiliary parser combinators

-- parse a token of a given type and add the text to the external rep.

parseDateTok    :: String -> DateParse -> DateParser (String, DateParse)
parseDateTok tt d
                = dateTok (isTokType (== tt)) d

dateTok         :: DateParser String -> DateParse -> DateParser (String, DateParse)
dateTok t d     = ( do
                    s <- t
                    return (s, appRep s d)
                  )

-- try to apply a parser, but first skip a given # of fill tokens

lookAheadN      :: Int -> (DateParse -> DateParser DateParse) -> DateParse -> DateParser DateParse
lookAheadN n p d
    | n <= 0    = p d
    | otherwise = do
                  (_, d1) <- dateTok fillTok d
                  ( lookAheadN (n - 1) p d1 <|> p d1 )

-- ------------------------------------------------------------
--
-- basic token parsers

-- the interface to the primitive parsec token parser
tok             :: (Token -> Bool) -> DateParser Token
tok _pred       = tokenPrim showTok nextPos testTok
    where
      showTok               = show . fst
      nextPos pos _tok _ts  = incSourceColumn pos 1
      testTok _tok          = if _pred _tok then Just _tok else Nothing

-- check for specific token type and in case of success return the text value
isTokType       :: (String -> Bool) -> DateParser String
isTokType isT   = tok (isT . fst) >>= return . snd

-- parse an arbitrary token and return the text value
textTok         :: DateParser String
textTok         = isTokType (const True)

-- a word
wordTok         :: DateParser String
wordTok         = isTokType (== "word") <|> hrefTok

-- a href token
--
-- this will be transformed into a phrase searched in the "uri" context
hrefTok         :: DateParser String
hrefTok         = isTokType (== "href")
                  >>=
                  return . href2uri
    where
      href2uri  = ("uri:\"" ++) . (++ "\"") . unwords . tokenize "[^/]+" . drop 5

-- a delimiter, whitespace is normalized, sequences are reduced to a single space char
delTok          :: DateParser String
delTok          = isTokType (== "del")
                  >>=
                  return . sed (const " ") ws1

-- tokens that don't contain date info

fillTok         :: DateParser String
fillTok         = delTok <|> wordTok

-- semester tokens, not yet interpreted
-- semTok'         :: String -> DateParser (String, Int, Bool)
-- semTok' sem     = do v <- isTokType (== sem)
--                   return (v, read . head . tokenizeExt year $ v, sem == "ssem")

-- semTok          :: DateParser (String, Int, Bool)
-- semTok          = semTok' "ssem" <|> semTok' "wsem"

-- ------------------------------------------------------------

-- conversion from month names to 1..12
monthToM        :: String -> String
monthToM m
    = show .
      (\ l -> if null l then 99 else head l) .
      map fst .
      filter ((== True) . snd) .
      map (second (`isPrefixOf` map toLower m)) $
      monthAbr

-- ----------------------------------------------------------------------------
-- | Converts a Number into a String of a certain length, filled with "*".
-- | e.g. digits 4 11 -> "**11", digits 4 2011 -> "2011", digits 4 -1 -> "****", ...
-- | Input:  number of digits in result, number to be converted
-- | Output: String containing the converted number
digits :: Int -> Int -> String
digits numDigits number = if (number < 0)
                          then (numDigits `times` "*")
                          else ((numDigits - (length numberAsString)) `times` "0") ++ numberAsString
                          where
                            numberAsString = show number
                            times n s = if (n > 0)
                                    then (times (n-1) s) ++ s
                                    else ""

-- ----------------------------------------------------------------------------
-- | take the first n words of a string

takeFirstNWords :: Int -> String -> String
takeFirstNWords n str = unwords . (take n) . words $ str

-- ----------------------------------------------------------------------------
-- | take the last n words of a string

takeLastNWords :: Int -> String -> String
takeLastNWords n str = unwords . reverse . (take n) . reverse . words $ str

-- ----------------------------------------------------------------------------
-- | Extracts a List of Date-Parse-Results of a String.
-- | e.g. "Heute ist der 10.7.2003 und am Freitag, den 11.7.2003 um 13:00 Uhr gibts Fisch" ->
-- | [DR {_p = "", _r = "Heute", _d = DT {_year = -1, _month = -1, _day = -1, _hour = -1, _min = -1}},
-- |  DR {_p = " ist der ", _r = "10.7.2003", _d = DT {_year = 2003, _month = 7, _day = 10, _hour = -1, _min = -1}},
-- |  DR {_p = " und am ", _r = "Freitag, den 11.7.2003 um 13:00 Uhr", _d = DT {_year = 2003, _month = 7, _day = 11, _hour = 13, _min = 0}},
-- |  DR {_p = " gibts Fisch", _r = "", _d = DT {_year = -1, _month = -1, _day = -1, _hour = -1, _min = -1}}]
-- | Words recognized as Dates are contained in the _r-fileds. Their interpretation is contained in th _d-fields. "Heute" is a special keyword
-- | ("date-alias") that can be expanded to the actual date by scanDateAlias.

extractDateRep :: String -> [DateRep]
extractDateRep s
    = extractDateRep' .                        -- extract the date reps
      fromMaybe s .                            -- expand query into an OR expr
      fmap mkOr .
      matchDateAlias $ s
    where
      mkOr s'
          = s' ++ " OR " ++ quote s
      quote s'
          | ' ' `elem` s'
              = "\"" ++ s' ++ "\""
          | otherwise
              = s'

extractDateRep' :: String -> [DateRep]
extractDateRep'
    = dateSearch' . tokenizeSubex tokenRE

-- | adds the abriviations for the current months

extractDateRepM :: String -> IO [DateRep]
extractDateRepM s
    = do (y, m, _) <- fmap (toGregorian . utctDay) getCurrentTime
         return .
             extractDateRep .
             fromMaybe s .
             matchMonthAlias (fromInteger y) m $ s      -- match an abr. date spec, e.g "di wo" for "diese Woche"

-- ----------------------------------------------------------------------------
-- | a DateRep-Item is transformed into a normalized Date (i.e. "Juni 2010 um 12:00" -> "****-06-02-12-00")
-- | Furthermore, the words describing the recognized date are returned to be further processed by "prepareNormDateForCompare"
-- | where dateAliases like "heute" or "diese Woche" are transformed into dates.
-- | Input:  Representation of a parsed date
-- | Output: (Normalized Version of the Date, Words recognized as the date)
-- | Examples:
-- | 1) DR {_p = " ist der ", _r = "10.7.2003", _d = DT {_year = 2003, _month = 7, _day = 10, _hour = -1, _min = -1}} -> ("2003-07-10-**-**", "10.7.2003")
-- | 2) DR {_p = "", _r = "Heute", _d = DT {_year = -1, _month = -1, _day = -1, _hour = -1, _min = -1}} -> ("****-**-**-**-**", "Heute")

normalizeDate :: DateRep -> (String, String)
normalizeDate d
    = (normalizedDateString, _dateAlias)
    where
      _dateAlias = _r d
      normalizedDateString
          = (digits 4 $ _year  $ _d d) ++ "-" ++
            (digits 2 $ _month $ _d d) ++ "-" ++
            (digits 2 $ _day   $ _d d) ++ "-" ++
            (digits 2 $ _hour  $ _d d) ++ "-" ++
            (digits 2 $ _min   $ _d d)

-- ----------------------------------------------------------------------------
-- | The list of Date-Parse.Results is transformed into a list of normalized dates
-- | e.g.
-- | [DR {_p = "", _r = "Heute", _d = DT {_year = -1, _month = -1, _day = -1, _hour = -1, _min = -1}},
-- |  DR {_p = " ist der ", _r = "10.7.2003", _d = DT {_year = 2003, _month = 7, _day = 10, _hour = -1, _min = -1}}]
-- | ->
-- | ["****-**-**-**-**", "2003-07-10-**-**"]

dateRep2NormalizedDates :: [DateRep] -> [String]
dateRep2NormalizedDates = map (fst . normalizeDate) . filter (\ x -> (_r x) /= "")

-- ----------------------------------------------------------------------------
-- | The list of Date-Parse.Results is transformed into a list of the dates (not normalized!), each surrounded by max. 5 words
-- | to the left and to the right of the date.
-- | e.g.
-- | [DR {_p = "", _r = "Heute", _d = DT {_year = -1, _month = -1, _day = -1, _hour = -1, _min = -1}},
-- |  DR {_p = " ist der ", _r = "10.7.2003", _d = DT {_year = 2003, _month = 7, _day = 10, _hour = -1, _min = -1}}
-- |  DR {_p = " und am ", _r = "Freitag, den 11.7.2003 um 13:00 Uhr", _d = DT {_year = 2003, _month = 7, _day = 11, _hour = 13, _min = 0}}
-- |  DR {_p = " gibts Fisch", _r = "", _d = DT {_year = -1, _month = -1, _day = -1, _hour = -1, _min = -1}}]
-- | ->
-- | [["","Heute","ist der"],["ist der","10.7.2003","und am"],["und am","Freitag, den 11.7.2003 um 13:00 Uhr","gibts Fisch"]]
-- |
-- | Result: One list item of the Result is formed like [leftContext, theDate, rightContext]

dateRep2DatesContext          :: [DateRep] -> [[String]] 
dateRep2DatesContext []       = []
dateRep2DatesContext (x:[])   = if not . null $ _r x then [[takeLastNWords 5 $ _p x , _r x, ""]] else []
dateRep2DatesContext (x:y:xs) = [[takeLastNWords 5 $ _p x , _r x , takeFirstNWords 5 $ _p y]] ++ (dateRep2DatesContext (y:xs))

-- ----------------------------------------------------------------------------
-- | Prepare a normalized Date-String (i.e. "****-**-03-12-**") for comparison with indexed normalized dates.
-- | The leading "****-**-" will be replaced with the actual date.
-- | Since the comparison is prefix-based, the trailing "-**" are simply truncated.
-- | Since the result depends on the actual (world) date, it is wrapped in the IO Monad.
-- |
-- | The second input value is perhaps a dateAlias like "Heute", "Morgen", ...
-- | In this case the result will be the expansion of the dateAlias, i.e. the list of prepared dates, concatenated by "OR"
-- |
-- | Input: (Normalized Date-String, dateAlias)
-- | Examples:
-- | 1) ("****-**-**-**-**", "diese Woche") ->
-- |    "2011-10-31 OR 2011-11-01 OR 2011-11-02 OR 2011-11-03 OR 2011-11-04 OR 2011-11-05 OR 2011-11-06 OR (\"diese Woche\")"
-- | 2) ("****-03-04-**-**", "3. April") ->
-- |    "2011-03-04"

prepareNormDateForCompare :: (String, String) -> IO String
prepareNormDateForCompare (normDate, aliasDate)
    = do dateAliasResult <- scanDateAlias $ aliasDate
         case dateAliasResult of
           Just res
             -> return $ res ++ " OR (\"" ++ aliasDate ++ "\")"
           Nothing
             -> fmap truncNormDate (fillNormDate normDate)
    where
      truncNormDate = reverse . truncNormDate' . reverse

      truncNormDate' []
          = []
      truncNormDate' _normDate@(x:xs)
          | (x == '*' || x == '-')
              = truncNormDate' xs
          | otherwise = _normDate

      fillNormDate d
          = do curr <- currentTimeStr
               return $ fillNormDate' d curr

      fillNormDate' [] _ = []
      fillNormDate' _ [] = []
      fillNormDate' _normDate@(x:xs) (y:ys)
          | (x == '*' || x == '-')
              = y:(fillNormDate' xs ys)
          | otherwise
              = _normDate

currentTimeStr :: IO String
currentTimeStr
    = do today <- fmap utctDay getCurrentTime
         return (showGregorian today)

-- ----------------------------------------------------------------------------
-- | Check if input string is a date alias (heute, diese Woche, ...).
-- | If it is, return Just the list of normalized days representing the date-alias,
-- | concatenated by "OR" and the fully expanded date alias
-- | If it is not, return Nothing.
-- | Helper function for prepareNormDateForCompare.

scanDateAlias :: String -> IO (Maybe String)
scanDateAlias s
    = runIfDefined $ lookup s dateAliasFunc
    where
      runIfDefined Nothing
          = return Nothing
      runIfDefined (Just f)
          = fmap (Just . toDays . f  . utctDay) getCurrentTime

      toDays [x] = showGregorian x
      toDays xs  = foldl1 (\ a b -> a ++ " OR " ++ b) $ map showGregorian xs

-- ----------------------------------------------------------------------------
-- | Input: start-day and end-day of an interval of days
-- | Output: the list containing all days from start-day til end-day

mkListOfDays :: Day -> Day -> [Day]
mkListOfDays dFrom dTo
    = map ModifiedJulianDay [dayFrom..dayTo]
    where
      dayFrom = toModifiedJulianDay dFrom
      dayTo   = toModifiedJulianDay dTo

-- ----------------------------------------------------------------------------
-- | Input: a day
-- | Output: list of all days of the week containing the input-day

extractWeek :: Day -> [Day]
extractWeek d
    = mkListOfDays (setToMonday weekDate) (setToSunday weekDate)
    where
      weekDate
          = toWeekDate d
      setToMonday (y,w,_)
          = fromWeekDate y w 1
      setToSunday (y,w,_)
          = fromWeekDate y w 7

-- ----------------------------------------------------------------------------
-- | Input: a day
-- | Output: list of all days of the month containing the input-day

extractMonth :: Day -> [Day]
extractMonth d
    = mkListOfDays (setToFirst date) (setToLast date)
    where
      date
          = toGregorian d
      setToFirst (y,m,_)
          = fromGregorian y m 1
      setToLast (y,m,_)
          = addDays (-1) $ if (m == 12) then fromGregorian (y+1) 1 1 else fromGregorian y (m+1) 1
    
-- ----------------------------------------------------------------------------
-- | Input: a day
-- | Output: the day after a month, where the length of the month is dependent of the input-day

addMonth :: Day -> Day
addMonth d
    = addDays (fromIntegral $ gregorianMonthLength y m) d
    where
      (y, m, _) = toGregorian d

-- ----------------------------------------------------------------------------
-- | Input: parse result of a String that may contain multiple dates
-- | Result: IO (String with transformed dates, number of transformations made)

dateRep2stringWithTransformedDates :: [DateRep] -> IO (String, Int)
dateRep2stringWithTransformedDates dateRep = do
  listOfStrWithNum <- mapM (\ dr -> conc (_p dr) (transformDate dr)) $ dateRep
  return $ concatStrWithNumber listOfStrWithNum
  where
    transformDate dr = if (_r dr == "")
                          then return ("", 0)
                          else do
                            dateInfo <- prepareNormDateForCompare $ normalizeDate dr
                            return (dateInfo, 1)
    conc str ioStrWithNumber = do
      strWithNumber <- ioStrWithNumber
      return (str ++ (fst strWithNumber), snd strWithNumber)
    concatStrWithNumber xs = (concat $ map fst xs, foldl (+) 0 $ map snd xs)



-- ----------------------------------------------------------------------------
-- | takes normalized Date-String (e.g. "****-**-03-12-**") and returns human readable
-- | date representation (e.g. "M\228rz, 12 Uhr")

unNormalizeDate :: String -> String
unNormalizeDate
    = unNormalizeDate' . (split' '-')
    where
      split' _ [] = [""]
      split' delim (c:cs)
          | c == delim = "" : rest
          | otherwise = (c : head rest) : tail rest
          where
            rest = split' delim cs
      unNormalizeDate' parts
          = ("" `maybeDay` ". ") ++
            month' ++
            (" " `maybeYear` "") ++
            (", " `maybeHours`  ((":" `maybeMins` "") ++ " Uhr"))
          where
            maybeYear
                = getIfNotEmpty 0 parts
            month'
                = monthNames' !! ((strToInt 13 (parts !! 1)) - 1)
                  -- month should always be part of a date expression
            maybeDay
                = getIfNotEmpty 2 parts
            maybeHours
                = getIfNotEmpty 3 parts
            maybeMins
                = getIfNotEmpty 4 parts
            getIfNotEmpty index array pred' succ'
                | (head elem' /= '*')
                    = pred' ++ elem' ++ succ'
                | otherwise = ""
                where
                  elem' = array !! index
            monthNames'
                = monthNames ++ ["???"]

-- ------------------------------------------------------------

cmpDate :: (String -> String -> Bool) -> Integer -> String -> IO Bool
cmpDate (.>.) ageInDays date'
    = fmap cmp getCurrentTime
    where
      cmp ct
          = match "\\d{4}-\\d{2}-\\d{2}.*" ndate
            &&
            (ndate .>. deadline)
          where
            deadline
                = showGregorian .
                  utctDay .
                  addUTCTime (fromInteger (-60*60*24*ageInDays)) $ ct
            ndate
                = head .
                  (++ [""]) .
                  dateRep2NormalizedDates .
                  extractDateRep $ date'

-- ----------------------------------------------------------------------------
