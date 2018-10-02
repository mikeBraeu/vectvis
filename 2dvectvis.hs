import Codec.Picture
import Codec.Picture.Types
import Data.Word
import Data.Vector as V
import Data.Complex

adjust :: Double -> Int -> Int -> Double
adjust s xr x = (fromIntegral x - (fromIntegral xr)/2)*s

cmpmake :: Double -> Int -> Int -> Int -> Int -> Complex Double
cmpmake s xr yr x y = adjust s xr x :+ adjust s yr y


polartolch :: (Double, Double) -> Double -> (Double, Double, Double)
polartolch (mag, ph) high = (linearInter high 60 mag, 90, ph)


lchtolab :: (Double, Double, Double) -> (Double, Double, Double)
lchtolab (l, c, h) = (l, c * (cos h), c * (sin h))

labtoxyz :: (Double, Double, Double) -> (Double, Double, Double)
labtoxyz (l, a, b) = let  eps = 216/24389
                          kap = 24389/27
                          fy  = (l + 16)/116
                          fz  = fy - (b/200)
                          fx  = (a/500) + fy
                          x   = if fx**3 > eps then fx**3 else (116*fx - 16)/kap
                          y   = if l > kap*eps then ((l + 16)/116)**3 else l/kap
                          z   = if fz**3 > eps then fz**3 else (116*fz - 16)/kap
                            in (x, y, z)

--https://www.mathworks.com/help/images/ref/lab2xyz.html

a =   (1.0985,  1,          0.3558)
c =   (0.9807,  1,          1.1822)
e =   (1,       1,          1)
d50 = (0.9642,  1.0,        0.8251)
d55 = (0.9568,  1.0,        0.9214)
d65 = (0.95047, 1.00,       1.08883)
icc = (0.9642,  1,          0.8249)

data RGBgamut = RGBgamut {row1 :: (Double, Double, Double),
                          row2 :: (Double, Double, Double),
                          row3 :: (Double, Double, Double),
                          white :: (Double, Double, Double),
                          gamma :: Double}

sRGBgamut = RGBgamut (3.2404542, -1.5371385, -0.4985314)
                      (-0.9692660, 1.8760108, 0.0415560)
                      (0.0556434, -0.2040259, 1.0572252)
                      d65
                      2.4
adobeRGBgamut = RGBgamut (2.0413690, -0.5649464, -0.3446944)
                          (-0.9692660, 1.8760108, 0.0415560)
                          (0.0134474, -0.1183897, 1.0154096)
                          d65
                          (563/256)

adobeWideRGBgamut = RGBgamut (1.4628067, -0.1840623, -0.2743606)
                              (-0.5217933, 1.4472381, 0.0677227)
                              (0.0349342, -0.0968930, 1.2884099)
                              d50
                              (563/256)

proPhotoRGBgamut = RGBgamut (1.3459433, -0.2556075, -0.0511118)
                              (-0.5445989, 1.5081673, 0.0205351)
                              (0,       0,            1.2118128)
                              d50
                              1.8

pointwise :: (Double, Double, Double) -> (Double, Double, Double) -> (Double, Double, Double)
pointwise (a, b, c) (x, y, z) = (a*x, b*y, c*z)

matrixMult :: (Double, Double, Double) -> (Double, Double, Double) -> (Double, Double, Double) -> (Double, Double, Double) -> (Double, Double, Double)
matrixMult (a, b, c) (d, e, f) (g, h, i) (x, y, z) = (a*x + b*y + c*z, d*z + e*y + f*z, g*x + h*y + i*z)

xyztorgb :: RGBgamut -> (Double, Double, Double) -> (Double, Double, Double)
xyztorgb gam (x, y, z) =  let (xl, yl, zl)  = matrixMult (row1 gam) (row2 gam) (row3 gam) $ pointwise (x, y, z) (white gam)
                              compand c     = c**(1/(gamma gam))
                                in (compand xl, compand yl, compand zl)


linearInter :: Double -> Double -> Double -> Double
linearInter highin highout x = highout/highin * x

expInter :: Double -> Double -> Double -> Double
expInter low high x = 1 - base ** (x) where
  base = 0.5

makeSampleSpace :: Int -> Int -> Double -> Vector (Vector (Double, Double))
makeSampleSpace w h s = generate (2*h + 1) (\y -> generate (2*w + 1) (\x -> (s*(fromIntegral x - fromIntegral w), -s*(fromIntegral y - fromIntegral h))))

inmap :: (a -> b) -> Vector (Vector a) -> Vector (Vector b)
inmap f l = V.map (V.map f) l

findextrema :: Vector (Vector (Double, Double)) -> Double
findextrema lst =
  let lst' = V.concat $ toList lst
      lst'' = V.map (\(x, y) -> x) lst'
          in V.maximum lst''

highest :: Vector Double -> Double
highest vec = if V.length vec == 1 then V.head vec else
            (if V.head vec > (highest $ slice 1 (V.length vec - 1) vec) then V.head vec else (highest $ slice 1 (V.length vec - 1) vec))

urx = 1000
ury = 1000
iscale = 0.003

f :: Complex Double -> Complex Double
f z =  cexp $ z

cexp :: Complex Double -> Complex Double
cexp (a :+ b) = (exp a :+ 0) * (cis b)

newimage :: Vector (Vector PixelRGB16)
newimage = let  sampling = makeSampleSpace urx ury iscale
                polars = inmap (\(x,y) -> polar $ f $ x :+ y) sampling
                high = findextrema polars
                lch = inmap (\(x,y) -> polartolch (x,y) high) polars
                lab = inmap lchtolab lch
                xyz = inmap labtoxyz lab
                rgb = inmap (xyztorgb proPhotoRGBgamut) xyz
                rounded = inmap (\(r, g, b) -> PixelRGB16 (round $ r*65530) (round $ g*65530) (round $ b*65530)) rgb
                  in rounded



main :: IO ()
main = writePng "./blah.png" $ generateImage (\x y -> newimage ! y ! x) (2*urx + 1) (2*ury + 1)
