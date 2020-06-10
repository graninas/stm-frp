module Morse where

import qualified Data.Map as Map

morseCode :: Map.Map Char String
morseCode = Map.fromList $
  [ ('A', ".-")
  , ('B', "-...")
  , ('C', "-.-.")
  , ('D', "-..")
  , ('E', ".")
  , ('F', "..-.")
  , ('G', "--.")
  , ('H', "....")
  , ('I', "..")
  , ('J', ".---")
  , ('K', "-.-")
  , ('L', ".-..")
  , ('M', "--")
  , ('N', "-.")
  , ('O', "---")
  , ('P', ".--.")
  , ('Q', "--.- ")
  , ('R', ".-.")
  , ('S', "...")
  , ('T', "-")
  , ('U', "..-")
  , ('V', "...-")
  , ('W', ".-- ")
  , ('X', "-..-")
  , ('Y', "-.-- ")
  , ('Z', "--..")
  , ('0', "-----")
  , ('1', ".----")
  , ('2', "..--- ")
  , ('3', "...--")
  , ('4', "....-")
  , ('5', ".....")
  , ('6', "-.... ")
  , ('7', "--...")
  , ('8', "---..")
  , ('9', "----.")
  , (' ', "#")
  ]
