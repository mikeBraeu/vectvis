import Codec.Picture
import Codec.Picture.Types
import Data.Word
import Data.Vector as V
import Data.Complex



{-                            -}



compfunc :: Complex Double -> Complex Double
compfunc x = x

adjust :: Double -> Int -> Int -> Double
adjust s xr x = (fromIntegral x - (fromIntegral xr)/2)*s

cmpmake :: Double -> Int -> Int -> Int -> Int -> Complex Double
cmpmake s xr yr x y = adjust s xr x :+ adjust s yr y

cmptohsl :: Complex Double -> (Double, Double)
cmptohsl z = let  (mag, ph) = polar z
                  ph'       = if ph >= 0 then ph else 2*pi + ph
                  ph''       = ph'*360/(2*pi)
                  mag'      = (1 - (0.9**mag))
                    in (mag', ph'')

hsltorgb :: Double -> Double -> Double -> (Double, Double, Double)
hsltorgb h s l = let    c = (1 - abs(2*l - 1))*s
                        h' = h/60
                        h'mod2 = 2*(h'/2 - fromIntegral (floor $ h'/2))
                        x  = c*(1-abs(h'mod2 -1))
                        m = l - c/2
                        (r', g', b') =
                            case floor h' of
                                0 -> (c, x, 0)
                                1 -> (x, c, 0)
                                2 -> (0, c, x)
                                3 -> (0, x, c)
                                4 -> (x, 0, c)
                                5 -> (c, 0, x)
                                6 -> (c, 0, x)
                                h'' -> error $ "fuckup: h:" Prelude.++ (show h'') Prelude.++ " c: " Prelude.++ (show c) Prelude.++ " x: " Prelude.++ (show x)
                                    in (r' + m, g' + m, b' + m)

sample :: (Complex Double -> Complex Double) -> Double -> Int -> Int -> Int -> Int -> PixelRGB16
sample f s xr yr x y = let  z         = cmpmake s xr yr x y
                            w         = f z
                            (mag', ph') = cmptohsl w
                            (r, g, b) = hsltorgb ph' 1.0 mag'
                            to256 a = round $ a*65536
                              in PixelRGB16 (to256 r) (to256 g) (to256 b)

bitmap :: Image PixelRGB16
bitmap = generateImage f w h where
  w = 1000
  h = 1000
  f x y = sample compfunc 0.01 1000 1000 x y

makeSampleSpace :: Int -> Int -> Double -> [[(Double, Double)]]
makeSampleSpace w h s = [ [(s*(fromIntegral x),s*(fromIntegral $ -y)) | x <- [(-w)..w]] |  y <- [(-h)..h]]

inmap :: (a -> b) -> [[a]] -> [[b]]
inmap f l = Prelude.map (Prelude.map f) l

urx = 1000
ury = 1000
iscale = 0.003

f z = z ^ 5 - 1

newimage :: [[PixelRGB16]]
newimage = let  a = makeSampleSpace urx ury iscale
                b = inmap (\(x,y) -> cmptohsl $ f $ x :+ y) a
                c = inmap (\(mag, ph) -> hsltorgb ph 1.0 mag) b
                d = inmap (\(r, g, b) -> PixelRGB16 (round $ r*65530) (round $ g*65530) (round $ b*65530)) c
                  in d

stage1 = makeSampleSpace urx ury iscale
stage2 = inmap (\(x,y) -> cmptohsl $ f $ x :+ y) stage1
stage3 = inmap (\(mag, ph) -> hsltorgb ph 1.0 mag) stage2
stage4 = inmap (\(r, g, b) -> PixelRGB16 (round $ r*65536) (round $ g*65536) (round $ b*65536)) stage3

wubly :: (Show a) => [[a]] -> IO ()
wubly i = putStr $ Prelude.concat [ Prelude.concat ["\t" Prelude.++ (show e) Prelude.++ "\n" | e <- l]  Prelude.++ "\n\n\n\n\n" | l <- i]


--work with lists first
main :: IO ()
main = writePng "./blah.png" $ generateImage (\x y -> newimage !! y !! x) (2*urx + 1) (2*ury + 1)
