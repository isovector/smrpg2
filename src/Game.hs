
module Game where

import Control.Monad
import Data.Monoid
import Engine.Types
import Engine.Drawing
import Engine.Utils
import Data.Foldable


handleClose :: SF RawFrameInfo (Event BattleMenu)
handleClose = proc rfi -> do
  att <- edge -< c_ok     $ fi_controls rfi
  def <- edge -< c_cancel $ fi_controls rfi
  returnA -< asum
    [ AttackMenu <$ att
    , DefendMenu <$ def
    ]

menu :: Display a => [a] -> BattleMenu -> SF RawFrameInfo (Renderable, Event (Either BattleMenu a))
menu opts me = loopPre 0 $ proc (rfi, ix) -> do
  close <- handleClose -< rfi
  let ev = do
        e <- close
        guard $ me == e
        pure $ Right $ opts !! ix


  up    <- edge -< c_up $ fi_controls rfi
  down  <- edge -< c_down $ fi_controls rfi

  let update_ix :: Event (Int -> Int)
      update_ix = fmap appEndo $ mconcat
        [ Endo (clamp 0 (len - 1) . subtract 1) <$ up
        , Endo (clamp 0 (len - 1) . (+ 1))      <$ down
        ]

  let out :: Renderable
      out = mconcat
        [ drawFilledRect (V4 0 50 50 255) $ Rectangle (P $ V2 50 50) (V2 100 200)
        , mconcat $ do
           (opt, i) <- zip opts [0..]
           let y = 50 + fromIntegral i * 16
           pure $ mconcat
            [ ifA (i == ix) $
                drawFilledRect (V4 0 100 100 255) $ Rectangle (P $ V2 50 y) (V2 100 12)
            , drawText 12 (V3 255 255 255) (display opt) $ V2 50 y
            ]

        ]

  returnA -< ((out, asum [ ev, fmap Left close ] ), fromEvent id update_ix ix)
 where
  len = length opts


data BattleMenu = AttackMenu | DefendMenu | ItemMenu | SpellMenu
  deriving (Eq, Ord, Show, Enum, Bounded)

data BattleAction = Attack | Defend | RunAway
  deriving (Eq, Ord, Show, Enum, Bounded)

instance Display BattleAction where
  display = show

instance Display BattleMenu where
  display = show

battleMenu :: Maybe BattleMenu -> Swont RawFrameInfo Renderable BattleAction
battleMenu m = do
  e <- swont $ case m of
    Nothing -> fmap ((mempty, ) . fmap Left) handleClose
    Just AttackMenu -> menu [Attack] AttackMenu
    Just DefendMenu -> menu [Defend, RunAway] DefendMenu
    _ -> error "bad"
  case e of
    Right a -> pure a
    Left m' -> battleMenu $ Just m'

game :: SF RawFrameInfo Renderable
game = runSwont (error . show) $ battleMenu Nothing


--   proc (rfi, which) -> do
--   (att, _) <- pause mempty _ $ menu ["attack"] (arr $ c_ok . fi_controls) -< rfi
--   (def, _) <- pause mempty _ $ menu ["defend", "run away"] (arr $ c_cancel . fi_controls) -< rfi
--   returnA -< (, which) $ mconcat
--     [ att
--     , def
--     ]

