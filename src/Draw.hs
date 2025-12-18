module Draw
  ( toLayout
  , Layout
  , Canvas(..)
  , Drawing(drawFG, drawBG, drawAlign, drawStr)
  , drawLayout
  , parseLayout
  ) where

import Control.Monad (foldM, foldM_)
import Data.Functor ((<&>))

import Data.Maybe (fromMaybe)
import Parser
  ( ParseError(PE)
  , ParseResult(ParsedResult)
  , Parser(Parsed, UnParsed)
  , parseLine
  )
import Syntax
  ( Align(AlignCenter, AlignLeft, AlignRight)
  , BarCmd(DrawText, SetAlign, SetBG, SetFG)
  )

data Drawing = Drawing
  { drawFG :: String
  , drawBG :: String
  , drawAlign :: Align
  , drawStr :: String
  } deriving (Show)

data DrawOpts = DrawOpts
  { optFG :: Maybe String
  , optBG :: Maybe String
  , optAlign :: Maybe Align
  } deriving (Show)

type Layout = ([Drawing], [Drawing], [Drawing])

parseLayout :: String -> Layout
parseLayout str =
  case parseLine str of
    Parsed (ParsedResult cmds _) -> toLayout cmds
    UnParsed (PE err) -> ([], [Drawing "red" "" AlignCenter err], [])

toLayout :: [BarCmd] -> Layout
toLayout = layout . fromCmd

toDrawing :: DrawOpts -> String -> Drawing
toDrawing (DrawOpts fg bg a) =
  Drawing (fromMaybe "" fg) (fromMaybe "" bg) (fromMaybe AlignCenter a)

fromCmdWithOpts :: DrawOpts -> [BarCmd] -> [Drawing]
fromCmdWithOpts _ [] = []
fromCmdWithOpts opts (b:cmd) =
  case b of
    DrawText t -> toDrawing opts t : fromCmdWithOpts opts cmd
    SetFG fg -> fromCmdWithOpts (opts {optFG = Just fg}) cmd
    SetBG bg -> fromCmdWithOpts (opts {optBG = Just bg}) cmd
    SetAlign a -> fromCmdWithOpts (opts {optAlign = Just a}) cmd

fromCmd :: [BarCmd] -> [Drawing]
fromCmd = fromCmdWithOpts (DrawOpts Nothing Nothing Nothing)

layout :: [Drawing] -> Layout
layout xs = (left, center, right)
  where
    center = filter ((== AlignCenter) . drawAlign) xs
    right = filter ((== AlignRight) . drawAlign) xs
    left = filter ((== AlignLeft) . drawAlign) xs

draw :: Canvas a => Drawing -> a -> Int -> Int -> IO ()
draw (Drawing "" bg _ str) canvas x w =
  fillLine canvas bg x w >> drawLine canvas (defFG canvas) str x
draw (Drawing fg "" _ str) canvas x _ = drawLine canvas fg str x
draw (Drawing fg bg _ str) canvas x w =
  fillLine canvas bg x w >> drawLine canvas fg str x

drawingStringLen :: Canvas a => a -> [Drawing] -> IO Int
drawingStringLen canvas =
  foldM (\len d -> glyphWidth canvas (drawStr d) <&> (+ len)) 0

drawT :: Canvas a => a -> Int -> [Drawing] -> IO ()
drawT c = foldM_ drawM
  where
    drawM x d =
      glyphXoff c (drawStr d)
        >>= draw d c x
        >> glyphXoff c (drawStr d) <&> (x +)

drawLayout :: Canvas a => a -> Layout -> IO ()
drawLayout canvas (left, center, right) =
  drawWithOffset left 0
    >> drawingStringLen canvas center
    >>= drawWithOffset center . (half . (w -))
    >> drawingStringLen canvas right
    >>= drawWithOffset right . (w -)
  where
    drawWithOffset ds off = drawT canvas off ds
    half n = div n 2
    w = width canvas

class Canvas a where
  defFG :: a -> String
  defBG :: a -> String
  width :: a -> Int
  height :: a -> Int
  glyphWidth :: a -> String -> IO Int
  glyphXoff :: a -> String -> IO Int
  drawLine :: a -> String -> String -> Int -> IO ()
  fillLine :: a -> String -> Int -> Int -> IO ()
