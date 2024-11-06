{-# LANGUAGE DeriveGeneric #-}
module Main where

import Data.Aeson (FromJSON, ToJSON, decode, encode)
import qualified Data.ByteString.Lazy as B
import Data.Char (isUpper)
import Data.List (isInfixOf, isSuffixOf)
import Control.Monad (forM_)
import GHC.Generics (Generic)
import Control.Concurrent (threadDelay)

-- Definición de la estructura del correo
data Email = Email
    { body :: String
    , email :: String
    } deriving (Show, Generic)

instance FromJSON Email
instance ToJSON Email

-- Lista de palabras y dominios sospechosos
spamWords :: [String]
spamWords = ["win", "prize", "free", "urgent", "click here", "limited", "offer"]

suspiciousDomains :: [String]
suspiciousDomains = ["@freemail.com", "@discountmail.com", "@spamdomain.org"]

-- Función para detectar palabras de spam en el correo
hasSpamWords :: String -> Int
hasSpamWords email = length (filter (`isInfixOf` email) spamWords)

-- Función para detectar si el correo proviene de un dominio sospechoso
hasSuspiciousDomain :: String -> Int
hasSuspiciousDomain email = length (filter (`isSuffixOf` email) suspiciousDomains)

-- Regla heurística para detectar mayúsculas en el asunto
tooManyCaps :: String -> Int
tooManyCaps subject = if (length (filter isUpper subject) > 10) then 1 else 0 -- si la cantida de letras mayusculas son mayores a 10, retorna 1

-- Clasifica el correo en función de su puntuación heurística
isSpam :: Email -> Bool
isSpam (Email subject email) =
    let score = hasSpamWords email
              + hasSuspiciousDomain email
              + tooManyCaps subject
    in score > 1  -- umbral de spam (ajustable)

-- Clasificación de lista de correos
classifyEmails :: [Email] -> ([Email], [Email])
classifyEmails emails = (filter isSpam emails, filter (not . isSpam) emails)

-- Función principal para cargar y guardar correos
--module Main where
main :: IO ()
main = do
    -- Leer correos desde un archivo JSON
    jsonData <- B.readFile "data/received.json"
    let emails = decode jsonData :: Maybe [Email]
    putStrLn "\nClasificando . . ."
    threadDelay (5 * 1000000)
    
    case emails of
        Nothing -> putStrLn "Error al leer el archivo JSON."
        Just emailList -> do
            let (spamEmails, nonSpamEmails) = classifyEmails emailList
            
            -- Guardar correos spam en un nuevo archivo JSON
            B.writeFile "data/spam/spam_emails.json" (encode spamEmails)
            B.writeFile "data/principal/non_spam_emails.json" (encode nonSpamEmails)
            
            putStrLn "\n-*-*-*-*- Clasificación completada y archivos guardados -*-*-*-*-\n"
