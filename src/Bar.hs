module Bar
  ( runBar
  ) where

import Control.Monad (join, liftM2, when)
import Data.Functor ((<&>))
import Draw (Canvas(..), Layout, drawLayout, parseLayout)
import Foreign.C.String (peekCString)
import GHC.Bits ((.|.))
import Graphics.X11.Types
  ( Drawable
  , EventMask
  , Window
  , exposureMask
  , propertyChangeMask
  )
import Graphics.X11.Xft
  ( XftFont
  , withXftColorName
  , withXftDraw
  , xftDrawRect
  , xftDrawString
  , xftFontOpen
  , xftTextExtents
  , xftfont_ascent
  , xftfont_height
  )
import Graphics.X11.Xlib.Event (allocaXEvent, flush, nextEvent, selectInput)

import Graphics.X11.Xlib.Window
  ( clearWindow
  , createSimpleWindow
  , setWindowBackground
  )

import Graphics.X11.Xlib.Extras
  ( Event(ExposeEvent, PropertyEvent)
  , TextProperty(tp_value)
  , changeProperty32
  , getEvent
  , getTextProperty
  , mapRaised
  , propModeReplace
  )
import Graphics.X11.Xrender (xglyphinfo_width, xglyphinfo_xOff)

import Control.Exception
import Graphics.X11.Xlib.Atom (aTOM, internAtom, wM_NAME)
import Graphics.X11.Xlib.Color (allocNamedColor)
import Graphics.X11.Xlib.Display
  ( blackPixel
  , defaultColormap
  , defaultRootWindow
  , defaultScreen
  , defaultVisual
  , displayWidth
  , openDisplay
  , screenOfDisplay
  , whitePixel
  )
import Graphics.X11.Xlib.Types (Color(color_pixel), Display)

data FontBar = XftBar
  { dpy :: Display
  , fn :: XftFont
  }

data Bar = Bar
  { fontBar :: FontBar
  , colors :: (String, String)
  , heightBar :: Int
  , win :: Drawable
  }

instance Canvas Bar where
  defFG = fst . colors
  defBG = snd . colors
  width bar = fromIntegral $ displayWidth dpyBar scrN
    where
      dpyBar = dpy $ fontBar bar
      scrN = defaultScreen dpyBar
  height = heightBar
  glyphWidth bar str =
    xftTextExtents (dpy fnBar) (fn fnBar) str <&> xglyphinfo_width
    where
      fnBar = fontBar bar
  glyphXoff bar str =
    xftTextExtents (dpy fnBar) (fn fnBar) str <&> xglyphinfo_xOff
    where
      fnBar = fontBar bar
  drawLine bar cN str p = withXftColorName (dpy fnBar) v c cN drawWithColor
    where
      fnBar = fontBar bar
      v = defaultVisual (dpy fnBar) scrN
      c = defaultColormap (dpy fnBar) scrN
      scrN = defaultScreen (dpy fnBar)
      drawWithColor xftColor =
        withXftDraw (dpy fnBar) (win bar) v c (drawIn xftColor (fn fnBar))
      drawIn xftColor xftFont xftDraw =
        xftfont_ascent xftFont >>= f xftColor xftFont xftDraw . (+ (-1))
      f xftColor xftFont xftDraw yOff =
        xftDrawString xftDraw xftColor xftFont p yOff str
  fillLine bar cN p w = withXftColorName (dpy fnBar) v c cN f
    where
      fnBar = fontBar bar
      v = defaultVisual (dpy fnBar) scrN
      c = defaultColormap (dpy fnBar) scrN
      scrN = defaultScreen (dpy fnBar)
      f xftColor = withXftDraw (dpy fnBar) (win bar) v c (g xftColor)
      g xftColor xftDraw =
        xftfont_height (fn fnBar)
          >>= xftDrawRect xftDraw xftColor p (0 :: Int) w

createBar :: (String, String) -> FontBar -> IO Bar
createBar colorsT fbar = xftfont_height (fn fbar) >>= createWindow
  where
    createWindow h =
      createSimpleWindow
        dpyBar
        root
        0
        0
        (fromIntegral w)
        (fromIntegral h)
        0
        bpx
        wpx
        <&> Bar fbar colorsT h
    dpyBar = dpy fbar
    w = displayWidth dpyBar scrN
    root = defaultRootWindow dpyBar
    bpx = blackPixel dpyBar scrN
    wpx = whitePixel dpyBar scrN
    scrN = defaultScreen dpyBar

initXft :: String -> Display -> IO FontBar
initXft fontName d = font <&> XftBar d
  where
    font = xftFontOpen d scr fontName
    scrN = defaultScreen d
    scr = screenOfDisplay d scrN

listen :: [EventMask] -> Bar -> IO Bar
listen events bar =
  selectInput dpyBar (drwBar :: Window) evMasks
    >> selectInput dpyBar root propertyChangeMask
    >> return bar
  where
    dpyBar = dpy $ fontBar bar
    drwBar = win bar
    root = defaultRootWindow dpyBar
    evMasks = foldr1 (.|.) events

open :: Bar -> IO Bar
open bar = dock bar >> mapRaised (dpy $ fontBar bar) (win bar) >> return bar

tryGetWMName :: Bar -> IO String
tryGetWMName bar =
  catch (getTextProperty dpyBar root wM_NAME >>= peekCString . tp_value) onError
  where
    dpyBar = dpy $ fontBar bar
    root = defaultRootWindow dpyBar
    onError :: IOException -> IO String
    onError _ = putStrLn "Cannot get WM_NAME" >> return ""

tryParseLayout :: String -> IO Layout
tryParseLayout str = return $ parseLayout str

drawStatus :: Bar -> IO ()
drawStatus bar =
  clearWindow (dpy $ fontBar bar) (win bar)
    >> tryGetWMName bar
    >>= tryParseLayout
    >>= drawLayout bar

dock :: Bar -> IO ()
dock bar =
  join
    $ liftM2
        changeP
        (internAtom dpyBar "_NET_WM_WINDOW_TYPE" False)
        (internAtom dpyBar "_NET_WM_WINDOW_TYPE_DOCK" False)
  where
    dpyBar = dpy $ fontBar bar
    drwBar = win bar
    changeP windowType dockType =
      changeProperty32
        dpyBar
        (drwBar :: Window)
        windowType
        aTOM
        propModeReplace
        [fromIntegral dockType]

run :: Bar -> IO ()
run bar =
  allocaXEvent popEvent
    >>= getEvent
    >>= eventHandler bar
    >> flush dpyBar
    >> run bar
  where
    popEvent ptr = nextEvent dpyBar ptr >> return ptr
    dpyBar = dpy $ fontBar bar

bgBar :: Bar -> IO Bar
bgBar bar =
  allocNamedColor dpyBar cmap (defBG bar)
    >>= setWindowBackground dpyBar (win bar) . (color_pixel . fst)
    >> return bar
  where
    dpyBar = dpy $ fontBar bar
    cmap = defaultColormap dpyBar scrN
    scrN = defaultScreen dpyBar

eventHandler :: Bar -> Event -> IO ()
eventHandler bar (PropertyEvent _ _ _ _ _ atom _ _) =
  when (atom == wM_NAME) $ drawStatus bar
eventHandler bar (ExposeEvent {}) = drawStatus bar
eventHandler _ _ = return ()

runBar :: IO ()
runBar =
  openDisplay ":0"
    >>= initXft "monospace"
    >>= createBar ("#ffffff", "#000000")
    >>= bgBar
    >>= listen [exposureMask]
    >>= open
    >>= run
