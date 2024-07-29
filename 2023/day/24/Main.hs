import Control.Monad
import Data.Char
import Data.Functor
import Data.List
import Data.Maybe
import Text.ParserCombinators.ReadP
import Z3.Monad qualified as Z3

data V a = V !a !a !a deriving (Eq, Foldable, Functor, Ord, Traversable, Show)

V x1 y1 z1 .+ V x2 y2 z2 = V (x1 + x2) (y1 + y2) (z1 + z2)

parseVector = V <$> (skipSpaces *> num) <*> (sep *> num) <*> (sep *> num)
  where
    sep = char ',' *> skipSpaces
    num = do
      sign <- option 1 (char '-' $> (-1))
      digits <- many1 (satisfy isDigit)
      return (fromIntegral (sign * read digits))

data Hailstone a = Hailstone (V a) (V a) deriving (Eq, Foldable, Functor, Ord, Show, Traversable)

Hailstone (V px py pz) (V vx vy vz) .@ t = V (px + vx * t) (py + vy * t) (pz + vz * t)

parseHailstone :: ReadP (Hailstone Integer)
parseHailstone = Hailstone <$> parseVector <*> (string " @ " *> parseVector)

intersection :: Hailstone Double -> Hailstone Double -> Maybe (Double, Double)
intersection (Hailstone p1@(V px1 py1 pz1) v1@(V vx1 vy1 vz1)) (Hailstone p3@(V px3 py3 pz3) v3@(V vx3 vy3 vz3))
  | denominator == 0 = Nothing
  | otherwise = Just (t, u)
  where
    p2@(V px2 py2 pz3) = p1 .+ v1
    p4@(V px4 py4 pz4) = p3 .+ v3
    t = ((px1 - px3) * (py3 - py4) - (py1 - py3) * (px3 - px4)) / denominator
    u = -((px1 - px2) * (py1 - py3) - (py1 - py2) * (px1 - px3)) / denominator
    denominator = (px1 - px2) * (py3 - py4) - (py1 - py2) * (px3 - px4)

main = do
  input <- readFile "input"
  let [(hailstones, "")] = readP_to_S (endBy parseHailstone (char '\n') <* eof) input

  -- Part 1
  let (lo, hi) = (200000000000000, 400000000000000)
  let inside (V x y z) = and [x >= lo, x <= hi, y >= lo, y <= hi]

  print $
    length
      [ (x, y)
      | (x : ys) <- tails (map (fmap fromIntegral) hailstones),
        y <- ys,
        -- Check intersection
        (tx, ty) <- maybeToList (intersection x y),
        -- Time need to be in the future
        tx > 0,
        ty > 0,
        -- Check that collision happens inside test area
        inside (x .@ tx),
        inside (y .@ ty)
      ]

  -- Part 1 in Z3 (might not work, takes forever)
  {-
  let pairs = [(x, y) | (x : ys) <- tails hailstones, y <- ys]
  ans1 <- length . filter (== Z3.Sat) <$> (forM pairs $ \(Hailstone p1 v1, Hailstone p2 v2) -> Z3.evalZ3 $ do
        _0 <- Z3.mkInteger 0
        lo' <- Z3.mkInteger lo
        hi' <- Z3.mkInteger hi

        V px1 py1 _pz1 <- traverse Z3.mkInteger p1
        V vx1 vy1 _vz1 <- traverse Z3.mkInteger v1
        V px2 py2 _pz2 <- traverse Z3.mkInteger p2
        V vx2 vy2 _vz2 <- traverse Z3.mkInteger v2

        -- Clamp
        let clamp n = Z3.assert =<< Z3.mkAnd =<< sequence [Z3.mkGe n lo', Z3.mkLe n hi']
        mapM_ clamp [px1, py1, px2, py2]

        t <- Z3.mkFreshRealVar "t"
        Z3.assert =<< Z3.mkGt t _0

        u <- Z3.mkFreshRealVar "u"
        Z3.assert =<< Z3.mkGt u _0

        -- ap + av * t == bp + bv * u
        let collide (ap, av) (bp, bv) = do
              avt <- Z3.mkMul [av, t]
              apavt <- Z3.mkAdd [ap, avt]
              bvu <- Z3.mkMul [bv, u]
              bpbvu <- Z3.mkAdd [bp, bvu]
              Z3.assert =<< Z3.mkEq apavt bpbvu

        zipWithM_ collide [(px1, vx1), (py1, vy1)] [(px2, vx2), (py2, vy2)]
        Z3.check
    )
  print ans1
  -}

  -- Part 2
  (Z3.Sat, Just ans2) <- Z3.evalZ3 $ do
    _0 <- Z3.mkInteger 0
    px <- Z3.mkFreshIntVar "px"
    py <- Z3.mkFreshIntVar "py"
    pz <- Z3.mkFreshIntVar "pz"
    vx <- Z3.mkFreshIntVar "vx"
    vy <- Z3.mkFreshIntVar "vy"
    vz <- Z3.mkFreshIntVar "vz"

    -- Let Z3 sum x, y, z
    ans <- Z3.mkAdd [px, py, pz]

    forM_ hailstones $ \(Hailstone (V px' py' pz') (V vx' vy' vz')) -> do
      t <- Z3.mkFreshIntVar "t"
      Z3.assert =<< Z3.mkGe t _0 -- t >= 0

      -- Hailstone parameters in Z3
      hpx <- Z3.mkInteger px'
      hpy <- Z3.mkInteger py'
      hpz <- Z3.mkInteger pz'

      hvx <- Z3.mkInteger vx'
      hvy <- Z3.mkInteger vy'
      hvz <- Z3.mkInteger vz'

      -- hpx + hvx * t == px + vx * t
      l <- Z3.mkAdd =<< (: [hpx]) <$> Z3.mkMul [hvx, t]
      r <- Z3.mkAdd =<< (: [px]) <$> Z3.mkMul [vx, t]
      Z3.assert =<< Z3.mkEq l r

      -- hpy + hvy * t == py + vy * t
      l <- Z3.mkAdd =<< (: [hpy]) <$> Z3.mkMul [hvy, t]
      r <- Z3.mkAdd =<< (: [py]) <$> Z3.mkMul [vy, t]
      Z3.assert =<< Z3.mkEq l r

      -- hpz + hvz * t == pz + vz * t
      l <- Z3.mkAdd =<< (: [hpz]) <$> Z3.mkMul [hvz, t]
      r <- Z3.mkAdd =<< (: [pz]) <$> Z3.mkMul [vz, t]
      Z3.assert =<< Z3.mkEq l r

    Z3.withModel $ \m -> fromJust <$> Z3.evalInt m ans

  print ans2
