module Main where

import System.Console.ANSI as A 
import System.IO (hSetBuffering, stdin, BufferMode(LineBuffering))
import System.Exit (exitSuccess) 
import Data.Char (digitToInt)
import System.Process (runCommand, ProcessHandle)

import Parsing (parse, integer)
import Data
import Configure

opciones = ["1- Noticias", "2- Agregar Links RSS", "3- Info RSS", "4- Opciones Graficas","5- Restaurar Valores Predeterminados", "q- Salir","\n\nCualquier Otra tecla es Erronea"]
opRss = ["1- Ver Status Rss", "2- Eliminar RSS", "3- Agregar Links RSS","v- Volver Menu","q- Salir"]
opGraph = ["1- Cambiar Estilo","2- Default Config","v- Volver Menu","q- Salir"]
opNews = ["1- Ver Noticias", "2- Actualizar Noticias","v- Volver Menu","q- Salir"]
prioridad = ["1- Alta", "2- Media", "3- Baja","v- Volver Menu", "q- Salir"]
titulo = "Resumidor de Noticias"



--Funcion para cargar el buffer y poder aceptar backspaces con getLine
buffering :: IO ()
buffering = hSetBuffering stdin LineBuffering



--Menu principal
menu :: ([Config],Prior) -> IO ()           
menu tup = do
             putStrLn "Elija alguna Accion\n"
             c <- listarOpc opciones
             case c of
                  '1' -> findNews >>= menuNoticias (snd tup)    
                  '2' -> agregarLinks tup
                  '3' -> infoRss (snd tup) (fst tup)
                  '4' -> graphOptions (snd tup)
                  '5' -> restoreDefault >> putStrLn "\n Se volvio a la configuracion Default"
                  'q' -> cursorStart >> exitSuccess
                  _   -> cursorStart >> putStrLn "\nTecla incorrecta" >> menu tup
             tup2 <- procesarConf
             cursorStart
             menu tup2

--Menu para volver al menu principal
volverMenu :: IO ()
volverMenu = do
                        putStrLn "\n"
                        putStrLn "\n\nPresione Cualquier tecla para volver al menu principal\nq- Salir "
                        noBuffering
                        c <- getChar
                        case c of
                            'q' -> exitSuccess
                            _   -> return ()

-- Menu para agregar links
agregarLinks :: ([Config],Prior) -> IO ()
agregarLinks tup = do 
                     buffering
                     cursorStart
                     putStrLn "Ingrese Link RSS o escriba 'v':"
                     url <- getLine 
                     if url=="v" then return () else (do
                        cursorStart
                        putStrLn "Prioridad ?"                    
                        c <- listarOpc prioridad
                        putStr "\n"                     
                        case c of
                            '1' -> agregarUrlConf url Alta tup >> putStrLn  (url++" Agregado") >> return () --volverMenu 
                            '2' -> agregarUrlConf url Media tup >> putStrLn (url++" Agregado") >> return () --volverMenu 
                            '3' -> agregarUrlConf url Baja tup >> putStrLn (url++" Agregado") >> return () --volverMenu 
                            'v' -> return () --volverMenu 
                            'q' -> exitSuccess
                            _   -> putStrLn "Tecla incorrecta" >> agregarLinks tup)

-- Menu de opciones de noticias
menuNoticias :: Prior-> News -> IO ()
menuNoticias p n = do 
                     cursorStart
                     putStrLn "Que desea hacer ?"                    
                     c <- listarOpc opNews
                     cursorStart
                     case c of 
                         '1' -> verNoticias n
                         '2' -> actNoticias p n
                         'v' -> return () --volverMenu 
                         'q' -> exitSuccess
                         _   -> putStrLn "Tecla incorrecta" >> menuNoticias p n



-- Muestra las noticias, sin necesidad de hacer el scraping de los rss
verNoticias :: News -> IO ()
verNoticias news = do 
                     cursorStart
                     putStrLn "Noticias de que prioridad desea Ver?"                    
                     c <- listarOpc prioridad
                     cursorStart
                     case c of 
                         '1' -> showNews Alta >>= \x -> case x of
                                                             1 -> volverMenu 
                                                             0 ->  irUrl Alta news -- >> volverMenu
                         '2' -> showNews Media >>= \x -> case x of
                                                              1 -> volverMenu 
                                                              0 ->  irUrl Media news -- >> volverMenu
                         '3' -> showNews Baja >>= \x -> case x of
                                                             1 -> volverMenu 
                                                             0 -> irUrl Baja news -- >> volverMenu
                         'v' -> return () --volverMenu 
                         'q' -> exitSuccess
                         _   -> putStrLn "Tecla incorrecta" >> verNoticias news


-- Muestra las noticias actualizadas, realiza el scraping 
actNoticias :: Prior-> News -> IO ()
actNoticias p n = do 
                     cursorStart
                     putStrLn "Noticias de que prioridad desea Actualizar?"                    
                     c <- listarOpc prioridad
                     cursorStart
                     case c of
                         '1' ->  putStrLn "Cargando\n" >> updateNews Alta p n >>= \x -> case x of
                                                                                             1 -> volverMenu 
                                                                                             0 -> findNews >>= \news -> showNews Alta >>= \x -> case x of
                                                                                                                                                     1 -> return () --volverMenu 
                                                                                                                                                     0 ->  irUrl Alta news -- >> volverMenu
                         '2' ->  putStrLn "Cargando\n" >> updateNews Media p n >>= \x -> case x of
                                                                    1 -> volverMenu 
                                                                    0 -> findNews >>= \news -> showNews Media >>= \x -> case x of
                                                                                                                             1 -> return () --volverMenu 
                                                                                                                             0 ->  irUrl Media news -- >> volverMenu
                         '3' ->  putStrLn "Cargando\n" >> updateNews Baja p n >>= \x -> case x of
                                                                                             1 -> volverMenu 
                                                                                             0 -> findNews >>= \news -> showNews Baja >>= \x -> case x of
                                                                                                                                                     1 -> return () --volverMenu 
                                                                                                                                                     0 ->  irUrl Baja news -- >> volverMenu
                         'v' -> return () --volverMenu 
                         'q' -> exitSuccess
                         _   -> cursorStart >> putStrLn "Tecla incorrecta" >> actNoticias p n

-- Abre el Link elegido si el indice es correcto
--irUrl2 :: Priority -> News -> IO ()
--irUrl2 p n = do
--                 putStrLn "\n"
--                 putStrLn "\n\nElija una Noticia para Abrir en Firefox o presione 0 para volver"
--                 buffering
--                 c <- getLine
--                 case parsercito c of
--                      Left x  -> irUrl p n
--                      Right i -> case str of
--                                      Left t    -> putStrLn "Error de Indice"
--                                      Right url -> (runCommand $ "firefox "++url) >> irUrl p n
--                                 where str = getUrlNews p n i

-- Abre el Link elegido si el indice es correcto
irUrl :: Priority -> News -> IO ()
irUrl p n = do
                 putStrLn "\n"
                 putStrLn "\n\nElija una Noticia para Abrir en Firefox o presione 0 para volver"
                 buffering
                 c <- getLine
                 parsercito2 c p n
                
--Parser para corroborar que lo ingresado sea un indice 
parsercito2 :: String -> Priority -> News -> IO () 
parsercito2 s p n = case indices s of
                         Right 0 -> putStrLn "Elegiste el 0" >> return ()
                         Right x -> case str of
                                         Left inderr -> cursorStart >> putStrLn ("El indice "++show x++" es erroneo") >> irUrl p n
                                         Right url   -> (runCommand $ "firefox "++url) >> irUrl p n
                                    where str = getUrlNews p n x
                         Left err  -> cursorStart >> putStrLn (show err) >> irUrl p n


--Parser para corroborar que lo ingresado sea un indice 
--parsercito :: String -> Either (IO ()) Int
--parsercito s = case (parse integer s) of
--                    [] -> Left $ putStrLn (s++"No es una opcion valida")
--                    x  -> case (x!!0) of
--                               (a,"") -> Right a
--                               _      -> Left $ putStrLn (s++"No es una opcion valida")

--Menu sobre RSS
infoRss :: Prior -> [Config] -> IO ()
infoRss pr conf = do
                    cursorStart
                    putStrLn "Que desea hacer ?"
                    c <- listarOpc opRss
                    case c of
                         '1' -> cursorStart >> showUrls pr >> checkAll pr >> volverMenu 
                         '2' -> cursorStart >> showUrls pr >> eliminarRss pr conf >> volverMenu 
                         '3' -> agregarLinks (conf,pr) -- >> volverMenu 
                         'v' -> return () --volverMenu 
                         'q' -> exitSuccess
                         _   -> cursorStart >> putStrLn "Tecla incorrecta" >> infoRss pr conf


--Eliminar un Link RSS si el link se encuentra en la lista
eliminarRss :: Prior -> [Config] -> IO ()
eliminarRss pr conf = putStrLn "Ingrese Link RSS o escriba 'v':" >> 
                      buffering >> 
                      getLine >>= \url -> if elem url (a pr) || elem url (m pr) || elem url (b pr) then removerUrlConf url (conf,pr) >>
                      putStrLn ( url++"\n Removido de la lista ") else 
                      case url of 
                           "v" -> return ()
                           _ -> cursorStart >> putStrLn (url++"  no es una Url valida, debe escribirla exactamente como esta guardada") >> eliminarRss pr conf

                      


--Menu de opciones graficas
graphOptions :: Prior -> IO ()
graphOptions p =  do
                    cursorStart
                    putStrLn "Que desea hacer ?"
                    c <- listarOpc opGraph
                    case c of
                         '1' -> cursorStart >> verificaFColor p >> return () --volverMenu --elegirColor p cambiado por verificarColor p
                         '2' -> defaultConfig >> return () --volverMenu 
                         'q' -> exitSuccess
                         'v' -> return () --volverMenu 
                         _   -> cursorStart >> putStrLn "Tecla incorrecta" >> graphOptions p


-- Main principal              
main :: IO ()
main = do
        noBuffering
        checkConfig
        checkCfg
        info <- procesarConf
        clearScreen
        setTitle titulo
        t <- cursorCol
        setCursorPosition 0 t
        putStr bienvenida
        setCursorPosition 5 0
        menu info

         
         


