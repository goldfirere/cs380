module Pong where

import Graphics.Gloss.Interface.Pure.Game
import Data.Monoid

paddleWidth, paddleHeight :: Float
paddleWidth = 10
paddleHeight = 40

paddlePath :: Path
paddlePath = [ (-paddleWidth / 2, paddleHeight / 2)
             , (-paddleWidth / 2, -paddleHeight / 2)
             , (paddleWidth / 2, -paddleHeight / 2)
             , (paddleWidth / 2, paddleHeight / 2) ]

ballRadius :: Float
ballRadius = 5

ballSpeed :: Float
ballSpeed = 1

stageWidth, stageHeight :: Int
stageWidth = 300
stageHeight = 200

stageWidthF, stageHeightF :: Float
stageWidthF = fromIntegral stageWidth
stageHeightF = fromIntegral stageHeight

data World = World { w_lpaddle :: Float
                   , w_ball    :: (Float, Float)
                   , w_ball_motion :: (Float, Float)
                   , w_paddle_motion  :: Float
                   , w_playing :: Bool }

initialWorld = World { w_lpaddle = stageHeightF / 2
                     , w_ball    = (stageWidthF / 2, stageHeightF / 2)
                     , w_ball_motion = (ballSpeed, ballSpeed)
                     , w_paddle_motion  = 0
                     , w_playing = False }

render :: World -> Picture
render (World { w_lpaddle = lpaddle
              , w_ball    = (ball_x, ball_y) })
  = translate (-stageWidthF / 2) (-stageHeightF / 2) $
    (translate (paddleWidth / 2) lpaddle $
     color blue $
     polygon paddlePath) <>
    (translate (stageWidthF - paddleWidth / 2) (clampPaddle ball_y) $
     color blue $
     polygon paddlePath) <>
    (translate ball_x ball_y $
     color red $
     circleSolid ballRadius)

clampPaddle :: Float -> Float
clampPaddle pad = min (stageHeightF - paddleHeight / 2) $
                  max (paddleHeight / 2) $
                  pad

inRange :: Ord a => a -> (a, a) -> Bool
x `inRange` (a, b) = x >= a && x <= b

step :: Float -> World -> World
step _ w@(World { w_playing = False }) = w
step _ w@(World { w_lpaddle = lpaddle
                , w_ball    = (ball_x, ball_y)
                , w_ball_motion = (ball_dx, ball_dy)
                , w_paddle_motion = paddle_motion })
  = let lpaddle' = clampPaddle (lpaddle + paddle_motion)
        ball_x'  = ball_x + ball_dx
        ball_y'  = ball_y + ball_dy

        ball_dx'
          | ball_x' < paddleWidth + ballRadius
          , ball_y' `inRange` ( lpaddle' - paddleHeight / 2
                              , lpaddle' + paddleHeight / 2)
          = ballSpeed

          | ball_x' > stageWidthF - paddleWidth - ballRadius
          = -ballSpeed

          | otherwise
          = ball_dx

        ball_dy'
          | ball_y' < ballRadius
          = ballSpeed

          | ball_y' > stageHeightF - ballRadius
          = -ballSpeed

          | otherwise
          = ball_dy

        playing' = not (ball_x' < paddleWidth / 2)
    in
    w { w_lpaddle = lpaddle'
      , w_ball    = (ball_x', ball_y')
      , w_ball_motion = (ball_dx', ball_dy')
      , w_playing = playing' }

react :: Event -> World -> World
react (EventKey (MouseButton LeftButton) Down _ _)
      w@(World { w_playing = False })
  = w { w_playing = True
      , w_ball    = (stageWidthF / 2, stageHeightF / 2)
      , w_ball_motion = (ballSpeed, ballSpeed)
      , w_paddle_motion = 0 }
react ev w@(World { w_playing = True })
  | EventKey (SpecialKey KeyDown) Down _ _ <- ev
  = w { w_paddle_motion = -ballSpeed }
  | EventKey (SpecialKey KeyUp) Down _ _ <- ev
  = w { w_paddle_motion = ballSpeed }
  | EventKey _ Up _ _ <- ev
  = w { w_paddle_motion = 0 }
react _ w = w

main :: IO ()
main = play (InWindow "Pong" (stageWidth, stageHeight) (200, 200))
            white
            50
            initialWorld
            render
            react
            step
