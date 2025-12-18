module Syntax where

data Align
  = AlignLeft
  | AlignCenter
  | AlignRight
  | UnknowAlign
  deriving (Show, Eq)

data BarCmd
  = SetFG String
  | SetBG String
  | SetAlign Align
  | DrawText String
  deriving (Show, Eq)
