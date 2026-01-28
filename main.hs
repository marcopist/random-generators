module Lib (unfairCoinToss, fairCoinFromUnfair, monteCarloPi) where

binaryOfFloat :: (Ord f, Fractional f) => f -> [Bool]
binaryOfFloat x
  | x <= 0 = [False]
  | x >= 1 = [True]
  | x >= 1 / 2 = True : binaryOfFloat (2 * x - 1)
  | otherwise = False : binaryOfFloat (2 * x)

unfairCoinToss :: (Fractional p, Ord p) => p -> IO Bool -> IO Bool
unfairCoinToss p fairCoinToss =
  let binexp_p = binaryOfFloat p
      go bin = do
        firstToss <- fairCoinToss
        case bin of
          [] -> pure False
          firstBit : rest ->
            case (firstToss, firstBit) of
              (False, True) -> pure True
              (True, False) -> pure False
              _ -> go rest
  in go binexp_p

fairCoinFromUnfair :: IO Bool -> IO Bool
fairCoinFromUnfair unfairCoin = do
  first_draw <- unfairCoin
  second_draw <- unfairCoin
  case (first_draw, second_draw) of
    (True, False) -> pure True
    (False, True) -> pure False
    _ -> fairCoinFromUnfair unfairCoin

monteCarloPi :: IO Double -> IO Double
monteCarloPi uniformDistGenerator =
  pure $ aux 0.0 0
  where
    inCircle x y = (x ^ 2) + (y ^ 2) <= 1
    aux acc n =
      if n >= 100
        then (acc / n * 4)
        else do
          x <- uniformDistGenerator
          y <- uniformDistGenerator
          if inCircle x y
            then aux (acc + 1) (n + 1)
            else aux acc (n + 1)
