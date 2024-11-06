{-# LANGUAGE DeriveGeneric #-}

module Main where

import Text.Printf (printf)
import Data.Complex (Complex(..), cis)
import Data.List (intercalate)


data EquationType = Quadratic | Cubic deriving (Show, Eq)

-- Main
main :: IO ()
main = do
    putStrLn "Seleccione el tipo de procedimiento"
    putStrLn "1. Ecuación Cuadrática (ax^2 + bx + c = 0)"
    putStrLn "2. Ecuación Cúbica (ax^3 + bx^2 + cx + d = 0)"
    option <- getLine
    case option of
        "1" -> solveQuadratic
        "2" -> solveCubic
        _   -> putStrLn "Seleccione una opción válida"

-- =====================================     FORMULA CUADRATICA     ====================================================
solveQuadratic :: IO ()
solveQuadratic = do
    putStrLn "Resolviendo ax^2 + bx + c = 0"
    a <- prompt "Ingrese el valor de a: "
    b <- prompt "Ingrese el valor de b: "
    c <- prompt "Ingrese el valor de c: "
    let (x1, x2) = quadratic (read a) (read b) (read c)
    putStrLn $ "Las soluciones son:\n x1 = " ++ show x1 ++ ", x2 = " ++ show x2 ++ "\n\n"
    main

-- Resolución de la Ecuación Cuadrática
quadratic :: Double -> Double -> Double -> (Double, Double)
quadratic a b c = 
    let discriminant = b^2 - 4*a*c
    in if discriminant < 0 
       then error "No tiene soluciones reales"
       else ((-b + sqrt discriminant) / (2*a), (-b - sqrt discriminant) / (2*a))

-- Función para pedir datos al usuario
prompt :: String -> IO String
prompt msg = do
    putStrLn msg
    getLine

-- =====================================     CUBICA POR VIETA    ====================================================
solveCubic :: IO ()
solveCubic = do
    putStrLn "Resolviendo ax^3 + bx^2 + cx + d = 0"
    a <- prompt "Ingrese el valor de a: "
    b <- prompt "Ingrese el valor de b: "
    c <- prompt "Ingrese el valor de c: "
    d <- prompt "Ingrese el valor de d: "
    let (r1, r2, r3) = cubic (read a) (read b) (read c) (read d)
    putStrLn $ "Las soluciones son:\n" ++ showRoots [r1, r2, r3]
    putStrLn "\n \n"
    main

-- Resolucion por Vieta
-- pag 
-- https://ru.wikipedia.org/wiki/%D0%A2%D1%80%D0%B8%D0%B3%D0%BE%D0%BD%D0%BE%D0%BC%D0%B5%D1%82%D1%80%D0%B8%D1%87%D0%B5%D1%81%D0%BA%D0%B0%D1%8F_%D1%84%D0%BE%D1%80%D0%BC%D1%83%D0%BB%D0%B0_%D0%92%D0%B8%D0%B5%D1%82%D0%B0

-- para la funciones trigonometricas
-- https://downloads.haskell.org/~ghc/5.04/docs/html/base/Prelude.html#!!

-- Double -> Double -> Double -> Double -> 
cubic :: Double -> Double -> Double -> Double -> (Complex Double, Complex Double, Complex Double)
cubic a b c d = 
    let 
        -- Ajuste de los numeros segun a para tener el formato Original de Vieta
        b' = b / a
        c' = c / a
        d' = d / a

        -- Calculos de Q,R,S
        q = (b'^2 - 3 * c') / 9
        r = ((2 * b'^3) - (9 * b' * c') + (27 * d')) / 54
        s = q^3 - r^2


        -- Calcular phi basado en s y q
        -- Cuando S> 0 Phi sera con arcocoseno 
        -- cuando S < 0  las funciones trigonometricas se reemplazan con hiperbolicas
            -- si q > 0   sera un arcocosenoHiperbolico Modulo del numero solo en R
            -- si q < 0 sera un arcosenohiperbolico Modulo del Numero en R y en Q 
        -- Cuando s = 0  o tambien cuando q = 0 no se usa phi 
        phi 
            | s > 0     = acos (r / sqrt (q^3)) / 3
            | s < 0 && q > 0 = acosh (abs r / sqrt (q^3)) / 3
            | s < 0 && q < 0 = asinh (abs r / sqrt ((abs q)^3)) / 3
            | otherwise = 0

        -- Calculo de las Raizes segun el caso
        (x1, x2, x3)
            | s > 0 = 
                let
                    rootQ = sqrt q
                    -- Para este caso las 3 raices son Reales
                    x'  = -2 * rootQ * cos (phi) - (b' / 3)
                    x'' = -2 * rootQ * cos (phi + (2 * pi / 3)) - (b' / 3)
                    x'''= -2 * rootQ * cos (phi - (2 * pi / 3)) - (b' / 3)
                    --Formato de salida (Parte real + parte imaginaria) 
                in (x' :+ 0, x'' :+ 0, x''' :+ 0)
            
            | s < 0 && q > 0 = 
                let
                    rootQ = sqrt q
                    sgnR = sgn r
                    -- 1 Raiz real y 2 raizes con imaginarios
                    -- la parte real es igual a la primera raiz pero sin la multiplicacion de -2
                    -- podemos tomar x' igual a la parte real * -2 y listo
                    realPart = sgnR * rootQ * cosh phi - (b' / 3)
                    x'  = -2 * sgnR * rootQ * cosh phi - (b' / 3)
                    -- para estos casos la parte imaginaria una es positiva la otra negativa 
                    -- podemos simplificar con un signo
                    imagPart = sqrt 3 * rootQ * sinh phi
                in (x' :+ 0, realPart :+ imagPart, realPart :+ (-imagPart))
            
            | s < 0 && q < 0 = 
                let
                    rootAbsQ = sqrt (abs q)
                    sgnR = sgn r
                    -- Igual que el anterior 2 reices complejas y una real
                    -- aqui es con senoshiperbolicos
                    realPart = sgn r * rootAbsQ * sinh phi - (b' / 3)
                    x'  = -2 * sgn r * rootAbsQ * sinh phi - (b' / 3)
                    imagPart = sqrt 3 * rootAbsQ * cosh phi
                in (x' :+ 0, realPart :+ imagPart, realPart :+ (-imagPart))
            
            | q == 0 = 
                let
                    x' = - (cubicRoot (d' - (b'^3 / 27))) - (b' / 3)
                    realPart = (-b' + x') / 2
                    imagPart = (sqrt (abs ((b' - 3 * x') * (b' + x') - 4 * c')))/2
                in (x' :+ 0, realPart :+ imagPart, realPart :+ (-imagPart))

            | otherwise = 
                -- Caso cuando s = 0 y una raíz doble
                let
                    
                    x'' = sgn r * sqrt q - (b' / 3)
                    x'  = -2 * x''
                in (x' :+ 0, x'' :+ 0, x'' :+ 0)

    in (x1, x2, x3)

-- Funcion Signo del Numero 
sgn :: Double -> Double
sgn x
    | x > 0     = 1
    | x < 0     = -1
    | otherwise = 0

-- Calculo de una Cubica 
-- se calcula el singo de x numero y se multiplica por el la raiz cubica del valor absoluto del numero
cubicRoot :: Double -> Double
cubicRoot x = sgn x * (abs x**(1/3))

showRoots :: [Complex Double] -> String
showRoots roots = unlines $ map (\x -> show x ++ "i") roots
