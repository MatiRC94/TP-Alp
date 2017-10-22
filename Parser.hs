{-# LANGUAGE FlexibleContexts #-}
module Parser where
-- I import qualified so that it's clear which
-- functions are from the parsec library:
import qualified Text.Parsec as P

-- I am the choice and optional error message infix operator, used later:
import Text.Parsec ((<?>))

-- Imported so we can play with applicative things later.
-- not qualified as mostly infix operators we'll be using.
import Control.Applicative

-- Get the Identity monad from here:
import Control.Monad.Identity (Identity)

import Data.Char

--Mis Datos
import Data as D


-- alias parseTest for more concise usage in my examples:
parse rule text = P.parse rule "tester" text

parse2 rule text = case P.parse rule "tester" text of
                        Left err -> do putStrLn "Error parsing input:"
                                       print err
                                       return err
                        Right a -> a

--Parser de Config, parte de Fondo
fond :: P.ParsecT [Char] u Identity Config
fond = do
         P.string "ondo"
         P.spaces
         c <- P.digit
         P.spaces
         i <- P.digit
         return $ Fondo (digitToInt c) (digitToInt i)

--Parser de Config, parte de Fuente
font :: P.ParsecT [Char] u Identity Config
font = do
         P.string "uente"
         P.spaces
         c <- P.digit
         P.spaces
         i <- P.digit
         return $ Fuente (digitToInt c) (digitToInt i)

com1 ::  P.ParsecT [Char] u Identity [Char]
com1 =   P.many (P.noneOf ['\"'])

--Parser de Prior
urlP :: P.ParsecT [Char] u Identity Prior
urlP =  do
          P.char 'P'
          P.spaces
          P.char '['
          alta <- algo
          P.char ']'
          P.spaces
          P.char '['
          media <- algo
          P.char ']'
          P.spaces
          P.char '['
          baja <- algo
          P.char ']' 
 --         P.char '\n'
 --         P.eof
          return $ D.P alta media baja

--Parsea la lista de Urls, teniendo en cuenta comillas y separacion por comas
algo :: P.ParsecT [Char] u Identity [Url]
algo = (do
          P.char '\"'
          xs <- com1
          P.char '\"'
          ((do
              P.char ','
              x <- algo
              return ([xs]++x))
            <|> return ([xs]) )
        <|> 
          return [])


--Parser general para configuraciones y prior
p1 :: [Config] -> Prior -> P.ParsecT [Char] u Identity ([Config],Prior)
p1 config (D.P a m b)  =P.try(do
                                commando
                                P.try (do
                                           prior <- urlP
                                           p1 config prior)
                                           <|> do
                                                 P.char 'F'
                                                 P.try(do
                                                         fondo <- fond
                                                         p1 (reemp fondo config) (D.P a m b))
                                                         <|> do
                                                               fuente <- font
                                                               p1 (reemp fuente config) (D.P a m b) )
                            <|> P.try ( do 
                                           return (config,(D.P a m b)) )
                                        <|> do 
                                              P.spaces
                                              com1
                                              P.spaces
                                              p1 config (D.P a m b)



--Reemplazar en caso de encontrar una configuracon vieja
reemp :: Config -> [Config] -> [Config]
reemp c [] = [c]
reemp (Fondo a b) (x:xs) = case x of
                                Fondo p t -> (Fondo a b):xs
                                _         -> x : (reemp (Fondo a b) xs)
reemp (Fuente a b) (x:xs) = case x of
                                Fuente p t -> (Fuente a b):xs
                                _          -> x : (reemp (Fuente a b) xs)


prueba :: String -> Either P.ParseError Char
prueba x = do 
             d <- parse (P.digit) x
             return d

test = (readFile "Config/Config.cfg") >>= \x -> return $ parse (p1 [] (D.P [] [] [])) x

commando :: P.ParsecT [Char] u Identity ()
commando = P.spaces >>  P.char '#' >>  P.spaces



{-
    --  GRAMATICA A GROSSO MODO  
 num# -> # n | comment
 n -> (NA | NM | NB ) parseoT
 parseoT -> "(" tuplas ")"
 tuplas -> "[" tuplas' "]" "," Int  
 tuplas' -> "(" "\"" auxN "\"" "," "\"" auxU "\"" ")" ( "," tupla' | E ) | E
 comment -> alphanumsYspaces num#
-}



--Parser general para las News

parseNews :: News -> P.ParsecT [Char] u Identity News
parseNews (N a m b)= P.try
                    ((do
                        commando
                        P.char 'N'
                        P.try (do
                                 altanew <- altaNew
                                 parseNews (N altanew m b))
                               <|> ( P.try(do
                                             medianew <- mediaNew
                                             parseNews (N a medianew b))
                               <|>( do
                                       bajanew <- bajaNew
                                       parseNews (N a m bajanew) ) ) )
                               <|> do 
                                     com1
                                     P.space
                                     parseNews (N a m b) )
                        <|> do
                              return (N a m b) 






--Parser por prioridades
altaNew :: P.ParsecT [Char] u Identity ([(String,Url)],Int)
altaNew = do
            P.string "A"
            P.spaces
            alta <- parseoT
            return alta

mediaNew :: P.ParsecT [Char] u Identity ([(String,Url)],Int)
mediaNew = do
             P.string "M"
             P.spaces
             media <- parseoT
             return media

bajaNew :: P.ParsecT [Char] u Identity ([(String,Url)],Int)
bajaNew = do
            P.string "B"
            P.spaces
            baja <- parseoT
            return baja

--Parser de tuplas
parseoT  :: P.ParsecT [Char] u Identity ([(String,Url)],Int)
parseoT = do
            P.char '('
            t <- tuplas
            P.char ')'
            return t


tuplas :: P.ParsecT [Char] u Identity ([(String,Url)],Int)
tuplas = do
            P.char '['
            t <- tuplas'
            P.char ']'
            P.spaces
            P.char ','
            P.spaces
            n <- numeros
            return (t,n)


tuplas' :: P.ParsecT [Char] u Identity [(String,Url)]
tuplas' = do
            xs <- auxN
            url <- com1
            P.string "\")"
--            P.char '\"'
--            P.char ')'
            ((do
                 P.char ','
                 t2 <- tuplas'
                 return ( [(xs,url)]++t2 ) )
              <|> return ([(xs,url)]) )
          <|> return []

numeros :: P.ParsecT [Char] u Identity Int
numeros = P.many P.digit >>= \a -> return $ (read a :: Int)


--auxN :: P.ParsecT [Char] u Identity [Char]
--auxN = P.manyTill P.anyChar (P.string "\",\"")

--Parsear los titulos de las noticias 
-- Agrego caracteres (como las comillas a la izq o a der) por distintas representaciones y encoding en distintas paginas 
auxN :: P.ParsecT [Char] u Identity [Char]
auxN =  do
           P.string "(\""
           P.manyTill P.anyChar (P.try (P.string "{}\",\""))
test2 :: IO (Either P.ParseError News)
test2 = (readFile "tester") >>= \x -> return $ parse (parseNews  (N ([],0) ([],0) ([],0))) x
--test3 = (readFile "tester") >>= \x -> return $ parse2 (parseNews  (N ([],0) ([],0) ([],0))) x


