import Data.Array
import Data.Ratio

type Radio = Float
type Lado = Float
type Perimetro = Float
type Apotema = Float
type Ancho = Float
type Altura = Float
type Base = Float
type BaseMayor = Float
type BaseMenor = Float
type Alto = Float
type Pi = Float

data Figura = Circulo Radio Pi
			| Cuadrado Lado
			| Rectangulo Lado Lado
            | Punto
            | Poligono Apotema Perimetro
            | Triangulo Ancho Alto
            | Rombo Ancho Alto
            | Paralelogramo Base Altura
            | Trapecio BaseMayor BaseMenor Altura
            | PoligonoRegular Perimetro Apotema

			  deriving Show

--a
perimetro :: Figura -> Float
perimetro (Circulo radio pi) = 2 * pi * radio
perimetro (Cuadrado lado) = 4 * lado
perimetro (Rectangulo ancho alto) = 2 * ancho + 2 * alto
perimetro (Punto) = error "no se puede calcular el perimetro del punto"

area :: Figura -> Float
area (Cuadrado lado) = lado * lado
area (Rectangulo ancho alto) = ancho * alto
area (Triangulo ancho alto) = (ancho * alto)/2
area (Rombo dMayor dMenor) = (dMayor * dMenor)/2
area (Paralelogramo base altura) = (base * altura)
area (Trapecio baseMayor baseMenor altura) = ((baseMenor*baseMayor)/2)*altura
area (PoligonoRegular perimetro apotema) = (perimetro*apotema)/2
area (Circulo pi radio) = pi * (radio^2)

--b
triangulo::Int->Int->Int->String
triangulo a b c | a+b 
     | a==b&&b==c&&c==b="triangulo equilatero"
     | a==b||a==c||b==c="triangulo isoceles"
     | (a^2 + b^2)==c^2||a^2 + c^2==b^2||c^2 + b^2==a^2="triangulo rectangulo"
     |otherwise="triangulo escaleno"

--c
fahrenheitAcelsius :: Float -> Float
fahrenheitAcelsius f = (celsius)
         where 
             celsius = (f-32)*(5/9)
          
             --d
base7 :: Int -> Int
base7 b10 = (b7)
where
    b7 = (b10/50)+(b10*2)

    --e
numSig :: Float -> Float
numSignificativo n|n<10 = n
|otherwise = 
    significativo(div n 10)

    --f
cantNum :: Float -> FLoat
x | x==0 =0
  | x<10 && even x=1
  | x<10 && odd x=0
  | otherwise = cantidadDigitos(div x 10)+ cantidadDigitos(mod x 10)

 --g
caracter :: Int -> Int
caracter n = (caractere)
where
    caractere = 
--h
impar :: Int -> Int

--i
enteroParte :: Float -> Char
enteroParte n, n1 = (confirmacion)
if ((n-n1)>-1) then confirmacion="Si es parte" else confirmacion="No es parte"

--j 


inverso :: Float -> Float
inverso num | num<10
            | otherwise = invertirNumero(mod num 10) && invertirNumero (num/10)*10

retornar :: Float -> Float
retornar x y
        | y < 10 = 10*x*y
        | otherwise=10 * retornarDos x (y div 10) + (y rem 10)

--m
retorno n=(div n 1000, mod n 1000)

--n
unir  a b = n1 + b
where
    dig c | c < 10 = 10
          | otherwise = 10 * dig(div c 10)
    primer = a * dig b

--o
base5 :: Float -> Float
base5 n = conv (dig n) n
where
    conv d n | n<10=n*(5^(d))
             | otherwise = (div n (10^d)) * (5^d) + conv (d-1) (mod n (10^d))
    digitos m | m < 10 = 0
              | otherwise = 1 + digitos (div m 10)
              