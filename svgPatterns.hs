import Text.Printf

type Point     = (Float,Float)
type Rect      = (Point,Float,Float)
type Circle    = (Point,Float)


-------------------------------------------------------------------------------
-- Paletas
-------------------------------------------------------------------------------

-- Paleta (R, G, B) só com tons de verde "hard-coded" 
-- (pode ser melhorado substituindo os valores literais por parâmetros)
-- Além disso, o que acontecerá se n for muito grande ou negativo?
greenPalette :: Int -> [(Int,Int,Int)]
greenPalette n = [(0, 80+i*10, 0) | i <- [0..n] ]

-- Paleta com n valores retirados de uma lista com sequências de R, G e B 
-- O '$' é uma facilidade sintática que substitui parênteses
-- O cycle é uma função bacana -- procure saber mais sobre ela :-)
rgbPalette :: Int -> [(Int,Int,Int)]
rgbPalette n = take n $ cycle [(255,0,0),(0,255,0),(0,0,255)]



-------------------------------------------------------------------------------
-- Geração de retângulos em suas posições
-------------------------------------------------------------------------------

genRectsInLine :: Int -> [Rect]
genRectsInLine n  = [((m*(w+gap), 0.0), w, h) | m <- [0..fromIntegral (n-1)]]
  where (w,h) = (50,50)
        gap = 10


-------------------------------------------------------------------------------
-- Strings SVG
-------------------------------------------------------------------------------

-- Gera string representando retângulo SVG 
-- dadas coordenadas e dimensões do retângulo e uma string com atributos de estilo
svgRect :: Rect -> String -> String 
svgRect ((x,y),w,h) style = 
  printf "<rect x='%.3f' y='%.3f' width='%.2f' height='%.2f' style='%s' />\n" x y w h style

svgCirc :: Circle -> String -> String 
svgCirc ((x,y),r) style = 
  printf "<circle cx='%.3f' cy='%.3f' r='%.2f' style='%s' />\n" x y r style

svgPath :: String -> String -> String 
svgPath d style = 
  printf "<path d='%s' style='%s' />\n" d style
  
-- String inicial do SVG
svgBegin :: Float -> Float -> String
svgBegin w h = printf "<svg width='%.2f' height='%.2f' style='background: white;transform: rotate(30deg);' xmlns='http://www.w3.org/2000/svg'>\n" w h 

-- String final do SVG
svgEnd :: String
svgEnd = "</svg>"

-- Gera string com atributos de estilo para uma dada cor
-- Atributo mix-blend-mode permite misturar cores
svgStyle :: (Int,Int,Int) -> String
svgStyle (r,g,b) = printf "fill:rgb(%d,%d,%d); mix-blend-mode: screen;" r g b

-- Gera strings SVG para uma dada lista de figuras e seus atributos de estilo
-- Recebe uma função geradora de strings SVG, uma lista de círculos/retângulos e strings de estilo
svgElements :: (a -> String -> String) -> [a] -> [String] -> String
svgElements func elements styles = concat $ zipWith func elements styles


cx ((x,y),r) = x;
cy ((x,y),r) = y;
cr ((x,y),r) = r;

-------------------------------------------------------------------------------
-- Função principal que gera arquivo com imagem SVG
-------------------------------------------------------------------------------


main :: IO ()
main = do
  writeFile "figs.svg" $ svgstrs
  where svgstrs = svgBegin w h ++ svgfigs ++ svgEnd
        svgfigs =  svgPath "M 225 200 C 200 150 275 100 250 10" "stroke: rgb(255, 102, 36); stroke-width: 6; fill: none;" ++ svgPath "M 249 10 C 325 150 200 100 275 200" "stroke: rgb(255, 102, 36); stroke-width: 6; fill: none;" ++ svgPath "M 275 250 C 250 200 325 150 300 60" "stroke: rgb(255, 102, 36); stroke-width: 6; fill: none;" ++ svgPath "M 299 60 C 375 200 250 150 325 250" "stroke: rgb(255, 102, 36); stroke-width: 6; fill: none;" ++ svgPath "M 225 250 C 225 150 150 150 200 60" "stroke: rgb(255, 102, 36); stroke-width: 6; fill: none;" ++ svgPath "M 199 60 C 100 150 225 150 175 250" "stroke: rgb(255, 102, 36); stroke-width: 6; fill: none;" ++ svgCirc ((250,250), 75) "fill:rgb(255, 47, 36);stroke: rgb(235, 16, 5);stroke-width: 6;"
        (w,h) = (500,500) -- width,height da imagem SVG



