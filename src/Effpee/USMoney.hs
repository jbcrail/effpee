{-# OPTIONS -Wno-unused-matches #-}

module Effpee.USMoney
  ( evalCoins
  , evalBills
  , getPortrait
  , isPresident
  , Portrait (..)
  , USCoin (..)
  , USBill (..)
  , Many (..)
  ) where

import Data.Int
import Effpee
import Effpee.ADT
import GHC.Num    ((+))

evalCoin :: USCoin -> Int
evalCoin Penny         = 1
evalCoin Nickel        = 5
evalCoin Dime          = 10
evalCoin OneDollarCoin = 100

evalCoins
  :: Many USCoin
  -> Int
evalCoins Empty     = 0
evalCoins (x :. xs) = evalCoin x + evalCoins xs

evalBill :: USBill -> Int
evalBill = todo "Effpee.USMoney.evalBill"

-- Use @evalBill@ in this definition
evalBills
  :: Many USBill
  -> Int
evalBills = todo "Effpee.USMoney.evalBills"

-- Given a US bill/note produce the presient whose portrait appears on it.
-- * $1   => Washington
-- * $2   => Jefferson
-- * $5   => Lincoln
-- * $10  => Hamilton
-- * $20  => Jackson
-- * $50  => Grant
-- * $100 => Franklin
getPortrait
  :: USBill
  -> Portrait
getPortrait OneDollar         = Washington
getPortrait TwoDollar         = Jefferson
getPortrait FiveDollar        = Lincoln
getPortrait TenDollar         = Hamilton
getPortrait TwentyDollar      = Jackson
getPortrait FiftyDollar       = Grant
getPortrait OneHundredDollar  = Franklin

isPresident
  :: Portrait
  -> Boolean     -- ^ this is the @Boolean@ from the ADT module NOT the builtin @Bool@ type
isPresident Washington = Yeah
isPresident Jefferson  = Yeah
isPresident Lincoln    = Yeah
isPresident Hamilton   = Nah
isPresident Jackson    = Yeah
isPresident Grant      = Yeah
isPresident Franklin   = Nah
