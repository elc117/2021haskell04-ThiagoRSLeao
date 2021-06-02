-- Prática 04 de Haskell
-- Nome: Thiago Rodrigues Silva

faixaIdoso :: Int -> String
faixaIdoso a 
  |(a >= 60 && a <= 64) = "IDO64"
  |(a >= 65 && a <= 69) = "IDO69"
  |(a >= 70 && a <= 74) = "IDO74"
  |(a >= 75 && a <= 79) = "IDO79"
  |(a >= 80) = "IDO80"
  |otherwise = "ND"

classifIdosos :: [(String,Int)] -> [(String,Int,String)]
classifIdosos idosos = [(fst idoso, snd idoso, faixaIdoso (snd idoso)) | idoso <- idosos]

classifIdosos' :: [(String,Int)] -> [(String,Int,String)]
classifIdosos' idosos = map (\idoso -> (fst idoso, snd idoso, faixaIdoso (snd idoso))) idosos

f (a,b,c) = a
s (a,b,c) = b
t (a,b,c) = c

strColor :: (Int,Int,Int) -> String
strColor cor = "rgb(" ++ show (f cor)  ++ ", " ++ show (s cor) ++ ", " ++ show (t cor) ++ ")"

genCircs :: Int -> (Int,Int) -> Int -> [(Int,Int,Int)]
genCircs n p r = [ ((fst p) + 20 * d, snd p, r) | d  <- [1..n]]

genReds :: Int -> [(Int,Int,Int)]
genReds n = [((mod v 255)+1, 0 , 0) | v <- [0..n]]
