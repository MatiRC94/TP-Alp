module Scraper where


import Codec.Binary.UTF8.String       (decodeString)
import Network.HTTP (getResponseBody,getRequest,simpleHTTP,defaultGETRequest_)
import Text.Feed.Import (parseFeedString)
import Text.Feed.Query (feedItems,getItemTitle,getItemLink,getFeedTitle)
import Text.Feed.Types (Item,Feed)
import Data.Maybe (Maybe)
import Network.URI (parseURI, uriToString)
import Text.Show.Unicode (ushow)
import Control.Exception
import Network.Stream
import Network.HTTP.Base
import Network.TCP

getTitleAndUrl :: Item -> (Maybe String, Maybe String)
getTitleAndUrl item = (getItemTitle item, getItemLink item)



--Obtener las tuplas con la informacion del titulo y la Url
getTuples :: String -> IO (Maybe [(Maybe String, Maybe String)])
getTuples s = fmap (map getTitleAndUrl) <$> fmap (feedItems) <$> ( parseFeedString <$> getResponseRss2 s )

--Extraer los datos tomados de getTuples
extractData :: Maybe [(Maybe String, Maybe String)] -> [(String,String)]
extractData (Just feedlist ) = map extract feedlist
extractData _                = error "error en el parseo de feed"

--Extraer los datos tomados de getTuples
extractData2 :: Maybe [(Maybe String, Maybe String)] -> IO [(String,String)]
extractData2 (Just feedlist ) = return $map extract feedlist
extractData2 _                = putStrLn "Error en el Screapeo, url erronea" >> return []

--Para ver errores y sacar el Just
extract :: (Maybe String, Maybe String) -> (String,String)
extract (Just title,Just link) = (title++"{}",link)
extract _ = error "Error en el Screapeo, puede que la url tenga errores"

--A partir de una Url, obtengo la informacion decodificada
getResponseRss :: String -> IO String
getResponseRss s = do 
                     s1 <- simpleHTTP (getRequest s) -- :: IO (Either SomeException (IO (Network.Stream.Result (Network.HTTP.Base.Response ty))) )
                     getResponseBody s1 >>= \x -> return $ decodeString x

--A partir de una Url, obtengo la informacion decodificada
getResponseRss2 :: String -> IO String
getResponseRss2 s = do 
                     result <- try (simpleHTTP (getRequest s)) :: IO (Either SomeException (Result (Response String)))
                     case result of 
                          Left ex  -> putStrLn ("Caught exception: " ++ show ex) >> return "Hubo un Error"
                          Right val -> getResponseBody val >>= \x -> return $ decodeString x


--Imprime las tuplas
printTuples :: [(String, String)] -> IO ()
printTuples s = mapM_ (putStrLn.ushow) s                 

--Funcion para probar el scraping
probando :: String -> IO ()
probando rss = getTuples rss >>= \x -> printTuples $ extractData x

--Funcion de scraping
scrap :: String -> IO [(String,String)]
scrap rss = getTuples rss >>= \x -> extractData2 x

