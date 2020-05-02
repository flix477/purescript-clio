module Clio.Config.Help
  ( help
  , usage
  , formatArgumentName
  , title
  , option
  , options
  , argument
  ) where

import Prelude

import Data.List (List(..), (:), singleton, catMaybes)
import Data.Tuple (Tuple(..), uncurry)
import Data.Map (isEmpty, toUnfoldable) as M
import Data.Foldable (class Foldable, intercalate, foldl)
import Data.FunctorWithIndex (mapWithIndex)
import Data.Maybe (fromMaybe, Maybe(..), isJust)
import Data.String (length)
import Data.String.CodeUnits (singleton) as C

import Clio.Config (Config)
import Clio.Config.Argument (ArgumentType(..), ArgumentEntry)
import Clio.Config.Arguments (Arguments)
import Clio.Config.Option (Name(..), Option)

margin :: Int
margin = 2

align :: String -> String
align = append $ replicateChar ' ' margin

help :: Config -> String
help c = lines
  $ apply (title:usage:optionsAndArgs)
  $ singleton c
  where optionsAndArgs = apply (options:arguments:Nil) (singleton $ getPadding c)

title :: Config -> String
title c = c.title <> strFromMaybe (append ": " <$> c.description)

usage :: Config -> String
usage c = intercalateMaybe " "
  $ (Just "Usage:")
  : (Just c.title)
  : (if M.isEmpty c.options then Nothing else Just "[options]")
  : ((Just) <$> helpArguments c.arguments)

helpArguments :: Arguments -> List String
helpArguments Nil = Nil
helpArguments x = map formatArgumentName x

options :: Int -> Config -> String
options padding = append "Options:\n"
  <<< lines
  <<< map align
  <<< (mapWithIndex $ option padding)
  <<< _.options

arguments :: Int -> Config -> String
arguments padding = append "Arguments:\n"
  <<< lines
  <<< map align
  <<< (map $ argument padding)
  <<< _.arguments

option :: Int -> Name -> Option -> String
option p n o = left
  <> (strFromMaybe $ formatDescription o)
  where left = if isJust o.description
    then (padEnd ' ' p $ optionLeft n o)
    else optionLeft n o

argument :: Int -> ArgumentEntry -> String
argument p e@(Tuple _ a) = left
  <> (strFromMaybe $ formatDescription a)
  where left = if isJust a.description
    then padEnd ' ' p (formatArgumentName e)
    else formatArgumentName e

optionLeft :: Name -> Option -> String
optionLeft n o = formatName n
  <> (strFromMaybe $ append " " <$> optionPlaceholder o)

optionPlaceholder :: Option -> Maybe String
optionPlaceholder = map _.placeholder <<< _.value

formatDescription
  :: forall r. { description :: Maybe String | r }
  -> Maybe String
formatDescription = map align <<< _.description

formatName :: Name -> String
formatName (Short c) = "-" <> C.singleton c
formatName (Long l) = "--" <> l
formatName (Full c l) =
  formatName (Short c) <> ", " <> formatName (Long l)

formatArgumentName :: ArgumentEntry -> String
formatArgumentName (Tuple n { argumentType: a }) =
  formatArgumentName' n a

formatArgumentName' :: String -> ArgumentType -> String
formatArgumentName' n Required = n
formatArgumentName' n Optional = "[" <> n <> "]"
formatArgumentName' n Variadic = "[..." <> n <> "]"

getPadding :: Config -> Int
getPadding = foldl max 0
  <<< join
  <<< apply (getOptionsPadding:getArgsPadding:Nil)
  <<< singleton

getOptionsPadding :: Config -> List Int
getOptionsPadding = map l
  <<< M.toUnfoldable
  <<< _.options
  where l = length <<< uncurry optionLeft

getArgsPadding :: Config -> List Int
getArgsPadding = map l <<< _.arguments
  where l = length <<< formatArgumentName 

intercalateMaybe :: String -> List (Maybe String) -> String
intercalateMaybe c = intercalate c <<< catMaybes

strFromMaybe :: Maybe String -> String
strFromMaybe = fromMaybe ""

lines :: forall f. Foldable f => f String -> String
lines = intercalate "\n"

padEnd :: Char -> Int -> String -> String
padEnd c i s = append s $ replicateChar c amount
  where amount = max 0 $ i - length s

replicateChar :: Char -> Int -> String
replicateChar _ 0 = ""
replicateChar c n = replicateChar c (n - 1) <> C.singleton c
