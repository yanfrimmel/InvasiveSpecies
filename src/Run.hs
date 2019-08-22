{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
module Run
    ( run
    )
where

import           Import
import qualified SDL
import qualified SDLUtils
import           Control.Monad.Loops            ( iterateUntilM )
import           Data.Foldable                  ( foldl' )

run :: RIO App ()
run = do
    logInfo "We're inside run!"
    liftIO $ SDLUtils.withSDL $ SDLUtils.withSDLImage $ do
        SDLUtils.setHintQuality
        SDLUtils.withWindow "InvasiveSpecies" (800, 600) $ \world ->
            SDLUtils.withRenderer world $ \renderer -> do
                textureAndInfo <- SDLUtils.loadTextureWithInfo
                    renderer
                    "./assets/mouse_states.png"
                let doRender = renderWorld renderer textureAndInfo
                _ <- iterateUntilM
                    exiting
                    (\currentWorld ->
                        updateWorld currentWorld
                            <$> SDL.pollEvents
                            >>= \currentWorld' ->
                                    currentWorld' <$ doRender currentWorld'
                    )
                    initialWorld
                SDL.destroyTexture (fst textureAndInfo)



selectQuadrant :: (Num a, Ord a) => a -> a -> Quadrant
selectQuadrant x y | x < 320 && y < 240   = TopLeft
                   | x >= 320 && y < 240  = TopRight
                   | x < 320 && y >= 240  = BottomLeft
                   | x >= 320 && y >= 240 = BottomRight
                   | otherwise            = undefined


applyIntent :: Intent -> World -> World
applyIntent (Press   q) = pressWorld q
applyIntent (Release q) = releaseWorld q
applyIntent (Hover   q) = hoverWorld q
applyIntent (Leave   q) = leaveWorld q
applyIntent Idle        = idleWorld
applyIntent Quit        = quitWorld


updateTile
    :: (Pane -> Pane) -> (Pane -> Pane) -> Quadrant -> Tile -> Tile
updateTile f g TopLeft (Tile tl tr bl br) =
    Tile (f tl) (g tr) (g bl) (g br)
updateTile f g TopRight (Tile tl tr bl br) =
    Tile (g tl) (f tr) (g bl) (g br)
updateTile f g BottomLeft (Tile tl tr bl br) =
    Tile (g tl) (g tr) (f bl) (g br)
updateTile f g BottomRight (Tile tl tr bl br) =
    Tile (g tl) (g tr) (g bl) (f br)


pressWorld :: Quadrant -> World -> World
pressWorld q w = w { tiles = tiles' }
    where tiles' = updateTile setDown id q (tiles w)


releaseWorld :: Quadrant -> World -> World
releaseWorld q w = w { tiles = tiles' }
    where tiles' = updateTile setUp id q (tiles w)


hoverWorld :: Quadrant -> World -> World
hoverWorld q w = w { tiles = tiles' }
    where tiles' = updateTile setOver setOut q (tiles w)


leaveWorld :: Quadrant -> World -> World
leaveWorld q w = w { tiles = tiles' }
    where tiles' = updateTile setOut setOver q (tiles w)


setOut :: Pane -> Pane
setOut Down = Down
setOut _    = Out


setOver :: Pane -> Pane
setOver Down = Down
setOver Up   = Up
setOver _    = Over


setDown :: Pane -> Pane
setDown _ = Down


setUp :: Pane -> Pane
setUp Down = Up
setUp p    = p


idleWorld :: World -> World
idleWorld = id


quitWorld :: World -> World
quitWorld w = w { exiting = True }


renderWorld :: SDL.Renderer -> (SDL.Texture, SDL.TextureInfo) -> World -> IO ()
renderWorld r t w = do
    SDL.clear r
    drawWorld r t w
    SDL.present r


drawWorld :: SDL.Renderer -> (SDL.Texture, SDL.TextureInfo) -> World -> IO ()
drawWorld renderer (texture, textureInfo) world = do
    renderPane (topLeft $ tiles world)     TopLeft
    renderPane (topRight $ tiles world)    TopRight
    renderPane (bottomLeft $ tiles world)  BottomLeft
    renderPane (bottomRight $ tiles world) BottomRight
  where
    textureWidth :: Double
    textureWidth = fromIntegral $ SDL.textureWidth textureInfo
    textureHeight = fromIntegral $ SDL.textureHeight textureInfo

    s  = SDLUtils.mkRect 0 0 (textureWidth / 2) (textureHeight / 2)

    mFor c = s `moveTo` getMask c
    pFor c = s `moveTo` getPosition c

    renderPane pane quadrant =
        SDL.copy renderer texture (Just $ floor <$> mFor pane) (Just $ floor <$> pFor quadrant)


getMask :: (Num a) => Pane -> (a, a)
getMask Out  = (0, 0)
getMask Over = (320, 0)
getMask Down = (0, 240)
getMask Up   = (320, 240)


getPosition :: (Num a) => Quadrant -> (a, a)
getPosition TopLeft     = (0, 0)
getPosition TopRight    = (320, 0)
getPosition BottomLeft  = (0, 240)
getPosition BottomRight = (320, 240)


moveTo :: SDL.Rectangle a -> (a, a) -> SDL.Rectangle a
moveTo (SDL.Rectangle _ d) (x, y) = SDL.Rectangle (SDLUtils.mkPoint x y) d

initialWorld :: World
initialWorld = World { exiting = False, tiles = initialtiles }

initialtiles :: Tile
initialtiles = Tile { topLeft     = Out
                       , topRight    = Out
                       , bottomLeft  = Out
                       , bottomRight = Out
                       }

updateWorld :: World -> [SDL.Event] -> World
updateWorld w =
    foldl' (flip applyIntent) w . fmap (payloadToIntent . SDL.eventPayload)

payloadToIntent :: SDL.EventPayload -> Intent
payloadToIntent SDL.QuitEvent            = Quit
payloadToIntent (SDL.MouseMotionEvent e) = motionIntent e
payloadToIntent (SDL.MouseButtonEvent e) = buttonIntent e
payloadToIntent _                        = Idle

motionIntent :: SDL.MouseMotionEventData -> Intent
motionIntent e = Hover q
  where
    q                    = selectQuadrant x y
    (SDL.P (SDL.V2 x y)) = SDL.mouseMotionEventPos e

-- | SDL.mouseButtonEventMotion e == SDL.Pressed -> Down
--
buttonIntent :: SDL.MouseButtonEventData -> Intent
buttonIntent e = getIntentFromQuadrant quadrant
  where
    quadrant = selectQuadrant x y
    (SDL.P (SDL.V2 x y)) = SDL.mouseButtonEventPos e
    getIntentFromQuadrant = if SDL.mouseButtonEventMotion e == SDL.Pressed then Press else Release