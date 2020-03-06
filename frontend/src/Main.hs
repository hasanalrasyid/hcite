-- Requires installation of SDL2 https://github.com/haskell-game/sdl2
-- and possibly GLUT https://hackage.haskell.org/package/GLUT

-- Using graphics code from https://wiki.haskell.org/OpenGLTutorial1
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Control.Monad
import Data.IORef
import Data.Foldable (for_)
import Foreign.C.Types
import Linear
import Linear.Affine
import SDL (($=))
import qualified SDL

import Graphics.UI.GLUT -- still compiles if you include Graphics.Rendering.OpenGL instead

myPoints :: [(GLfloat,GLfloat,GLfloat)]
myPoints = [ (sin (2*pi*k/12), cos (2*pi*k/12), 0) | k <- [1..12] ]

screenWidth, screenHeight :: CInt
(screenWidth, screenHeight) = (640, 480)

main :: IO ()
main = do
  SDL.initialize [SDL.InitVideo]
  SDL.HintRenderScaleQuality $= SDL.ScaleLinear
  do renderQuality <- SDL.get SDL.HintRenderScaleQuality
     when (renderQuality /= SDL.ScaleLinear) $
       putStrLn "Warning: Linear texture filtering not enabled!"

  window <-
    SDL.createWindow
      "SDL / OpenGL Example"
      SDL.defaultWindow {SDL.windowInitialSize = V2 screenWidth screenHeight,
                         SDL.windowOpenGL = Just SDL.defaultOpenGL}
  SDL.showWindow window

  SDL.glCreateContext(window)

  let loop = do
        let collectEvents = do
              e <- SDL.pollEvent
              case e of
                Nothing -> return []
                Just e' -> (e' :) <$> collectEvents
        events <- collectEvents
        let quit = any (== SDL.QuitEvent) $ map SDL.eventPayload events

        clear [ColorBuffer]
        renderPrimitive Points $
           mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) myPoints
        flush
        SDL.glSwapWindow window

        unless quit (loop)

  loop

  SDL.destroyWindow window
  SDL.quit

