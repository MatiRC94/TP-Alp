{-# LANGUAGE FlexibleContexts #-}
module Configure where

import Text.Show.Unicode(ushow)
import System.Console.ANSI as A (setSGR, SGR(..), ColorIntensity(..), ColorIntensity(..),ConsoleLayer (..), Color, clearScreen )
import System.IO (putStrLn, getChar, readFile, writeFile)
import System.Directory (createDirectoryIfMissing,doesFileExist,renameFile,removeFile)
import Data.Char (digitToInt)
import System.IO (hSetBuffering, stdin, BufferMode(NoBuffering))

import Parsing 
import Data as D
import Parser as PA
import Scraper

import qualified Text.Parsec as PP

initialNews = "# NA ([],0) \n# NM ([],0) \n# NB ([],0)"
initialConfig = "#Fondo 4 0 \n#Fuente 3 1 \n#P [] [] []"
emptyData = D.P [] [] []
emptyNews = (N ([],0) ([],0) ([],0))
colores =["0- Negro","1- Rojo","2- Verde","3- Amarillo","4- Azul","5- Magenta","6- Cyan","7- Blanco"]
intensidad = ["0- Opaco","1- Vivido"]
config="Config"
cfg = "Config/Config.cfg"
notis="Config/Noticias.cfg"
cfgtemp = "Config/ConfigTemp.cfg"
notistemp="Config/NoticiasTemps.cfg"




--TODO : hacer que ws pueda devolver Errores, quizas con Maybe y cambiar todo o Either
--TODO : SI hay un error en Noticias.cfg no devuelve nada
-- TODO : ver cuando cambias de color que podes poner cualquier fruta ver arrreglo de parsercito2 del main


--Evalua la Configuracion y ejecuta el cambio de estilo
evalConf :: Config -> IO ()
evalConf (Fondo c i)  = setSGR [ SetColor Background (toColorI i) (toColor c) ]
evalConf (Fuente c i) = setSGR [ SetColor Foreground (toColorI i) (toColor c) ]

--Parsea las noticias en el archivo de noticias
findNews :: IO News
findNews  = do
              checkNews
              contN <- readFile notis
            --  return $ PA.parse (parseNews emptyNews) contN
              case (PA.parse (parseNews emptyNews) contN) of
                    Left e -> do putStrLn "Error parsing input:"
                                 print e
                                 return emptyNews
                    Right n -> return n
--              return $ fst $ (Parser.parse (parseNews emptyNews) contN) !! 0

--Checkea la existencia del directorio Config
checkConfig :: IO ()
checkConfig = createDirectoryIfMissing False config


--Checkea existencia del archivo de noticias
checkNews :: IO ()
checkNews = do 
              bool <- doesFileExist notis
              if bool then return () else defaultNews

--Checkea existencia del archivo de configuracion
checkCfg :: IO ()
checkCfg = do 
             bool <- doesFileExist cfg
             if bool then return () else defaultConfig

-- Remueve si existe el archivo, la string es FilePath
removingFiles :: String -> IO ()
removingFiles s = do 
                    bool <- doesFileExist notis
                    if bool then removeFile s else return ()

--Para una escritura segura de las config, evita locks
secCfg :: IO ()
secCfg = do
             removingFiles cfg
             renameFile cfgtemp cfg

--Para una escritura segura de las noticias, evita locks
secNotis :: IO ()
secNotis = do 
             removingFiles notis
             renameFile notistemp notis

--Crea el archivo de noticias por Default
defaultNews :: IO ()
defaultNews = do
                 writeFile notistemp initialNews
                 secNotis

--Crea el archivo de configuracion por Default
defaultConfig :: IO ()
defaultConfig = do 
                 writeFile cfgtemp initialConfig
                 secCfg

--Pone los archivos de configuracion y noticias por default
restoreDefault :: IO ()
restoreDefault = do
                    defaultConfig
                    defaultNews
                    procesarConf
                    clearScreen 
                    return ()                    

--Ejecuta la configuracion grafica
procesarConf :: IO ([Config],Prior)
procesarConf = do
                  cont <- readFile cfg
                  case PA.parse (p1 [] emptyData ) cont of
                       Left err -> do putStrLn (show err) >> return ([],emptyData)
                       Right n -> mapM_ evalConf ( fst n) >> return n
     --                 in do mapM_ evalConf $ fst c
       --                     return (fst c ,snd c)       

--no aanda ole porque es https
--procesarConf = return ([Fondo 1 2 ,Fuente 1 2],D.P ["http://contenidos.lanacion.com.ar/herramientas/rss-origen=2"]["https://www.ole.com.ar/rss/ultimas-noticias/"]["http://www.laizquierdadiario.com/spip.php?page=backend_portada"])
--Funcion para cambiar el Estilo del programa

elegirColor :: Prior -> IO ()
elegirColor p = do
                     putStrLn "\nElija su estilo "
                     putStrLn "Color de Fondo:"
                     c1  <- listarOpc colores
                     clearScreen
                     putStrLn "Intensidad del color:"
                     i1 <- listarOpc intensidad
                     clearScreen
                     putStrLn "Color de Fuente:"
                     c2  <- listarOpc colores
                     clearScreen
                     putStrLn "Intensidad del color:"
                     i2 <- listarOpc intensidad
                     clearScreen
                     changeConfigCol p [digitToInt(c1),digitToInt(i1),digitToInt(c2),digitToInt(i2)] 

--Listar el argumento en forma de opciones
listarOpc :: [String] -> IO Char
listarOpc x = do 
                mapM_ putStrLn x
                putStr  "\n"
                noBuffering
                getChar

--Funcion para no tener buffer de stdin y no tener que presionar entrer con getLine
noBuffering :: IO ()
noBuffering = hSetBuffering stdin NoBuffering

--Evita errores de tipos
toColor:: Int -> Color
toColor = toEnum

--Evita errores de tipos
toColorI:: Int -> ColorIntensity
toColorI = toEnum


--Cambia el archivo de configuracion, utiliza un archivo temporal para evitar problemas de LOCK
changeConfigCol :: Prior -> [Int] -> IO ()
changeConfigCol (D.P a m b) c = do
                                  writeFile cfgtemp $ "#Fondo "++ show (c!!0) ++" "++ show (c!!1) ++"\n"++"#Fuente "++ show (c!!2) ++" "++ show (c!!3) ++"\n"++"#P "++show a  ++ show m ++show b
                                  secCfg
                                  procesarConf
                                  return ()

-- Agrega una Url al archivo de config
agregarUrlConf :: Url -> Priority -> ([Config],Prior) -> IO () 
agregarUrlConf u pr (xs,p) = agregarUrlConf' u pr p xs


agregarUrlConf' :: Url -> Priority -> Prior -> [Config] -> IO () 
agregarUrlConf' url p pr conf = do
                                  newUrl <- addUrl url p pr
                                  writeFile cfgtemp $ "#"++show (conf!!0) ++"\n"++"#"++ show (conf!!1) ++"\n"++"#P "++show(a newUrl) ++ show (m newUrl)++show (b newUrl)
                                  secCfg
                                  procesarConf
                                  return ()

--Remueve una URL del archivo de config
removerUrlConf :: Url -> ([Config],Prior) -> IO () 
removerUrlConf u (xs,p) = removerUrlConf' u p xs

removerUrlConf' ::Url -> Prior -> [Config] -> IO () 
removerUrlConf' url pr conf = do
                                  newUrl <- removeUrl url pr
                                  writeFile cfgtemp $ "#"++show (conf!!0) ++"\n"++"#"++ show (conf!!1) ++"\n"++"#P "++show(a newUrl) ++ show (m newUrl)++show (b newUrl)
                                  secCfg
                                  procesarConf
                                  return ()

--Tomando una prioridad, Muestra las noticias que esten en el archivo de noticias sin actualizarlas
showNews :: Priority -> IO Int
showNews Alta  = findNews >>= \x -> let tupla = na x
                                    in if snd(tupla)==0 then putStrLn "No hay noticias" >> return 1 else mapM_ auxPrint (zip [1..] (fst tupla)) >> return 0                                    
showNews Media = findNews >>= \x -> let tupla = nm x
                                    in if snd(tupla)==0 then putStrLn "No hay noticias" >> return 1 else mapM_ auxPrint (zip [1..] (fst tupla)) >> return 0
showNews Baja  = findNews >>= \x -> let tupla = nb x
                                    in if snd(tupla)==0 then putStrLn "No hay noticias" >> return 1 else mapM_ auxPrint (zip [1..] (fst tupla)) >> return 0

--Para imprimir de forma mas elegante y con numeros
auxPrint :: (Int,(String,Url)) -> IO ()
auxPrint (a,b) = putStrLn $ (ushow a++"- " ++ fst b)

-- A partir de una Prioridad y una lista, Actualizo las noticias en el archivo de noticias  
updateNews :: Priority -> Prior -> News -> IO Int
updateNews Alta p n = let newslist = (a p)                          
                          in do 
                               if (length newslist == 0) then putStrLn "No hay ningun diario en la lista." >> return 1 else do {scrapeo <- auxParse newslist ; writeNews Alta scrapeo p n ; return 0}
updateNews Media p n = let newslist = (m p)                          
                           in do 
                                if (length newslist == 0) then putStrLn "No hay ningun diario en la lista." >> return 1 else do {scrapeo <- auxParse newslist ; writeNews Media scrapeo p n ; return 0}
updateNews Baja p n = let newslist = (b p)                          
                          in do 
                               if (length newslist == 0) then putStrLn "No hay ningun diario en la lista." >> return 1 else do {scrapeo <- auxParse newslist ; writeNews Baja scrapeo p n ; return 0}


--Escribe las Noticias junto con su Link en el archivo de noticias
auxParse :: [Url] -> IO [(String, Url)]
auxParse []   = return []  
auxParse (x:xs) =  do 
                     scr  <- scrap x
                     scr2 <- auxParse xs
                     --return $ scr++scr2
                     return $ [(x++"{}",x)]++scr++scr2

-- Escribe en el archivo de noticias las nuevas noticias
writeNews :: Priority -> [(String,Url)] -> Prior -> News -> IO ()
writeNews Alta l p (N na1 nm1 nb1) = do
                                        let tam = length l
                                        writeFile notistemp ( "# NA ("++ ushow l ++","++ ushow tam ++")"++"\n# NM "++ ushow nm1 ++"\n# NB "++ ushow nb1) >> secNotis
writeNews Media l p (N na1 nm1 nb1)= do
                                        let tam = length l
                                        writeFile notistemp ( "# NA "++ ushow na1 ++ "\n# NM ("++ ushow l ++","++ ushow tam ++")"++"\n# NB "++ ushow nb1) >> secNotis
writeNews Baja l p (N na1 nm1 nb1) = do
                                        let tam = length l
                                        writeFile notistemp ( "# NA "++ ushow na1 ++ "\n# NM "++ ushow nm1 ++"\n# NB ("++ ushow l ++","++ ushow tam ++")") >> secNotis


indices = PA.parse numeros


