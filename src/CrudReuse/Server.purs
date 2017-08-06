module CrudReuse.Server where
  
import Prelude
import Control.Monad.Aff (attempt)
import Control.Monad.Error.Class (throwError)
import CrudReuse.Common (AjaxM)
import CrudReuse.Model (KeyT(..), Entity(..))
import Data.Argonaut (class DecodeJson, class EncodeJson, Json, decodeJson, encodeJson)
import Data.Either (Either(..), either)
import Data.HTTP.Method (Method(..))
import Data.Maybe (Maybe(Just))
import Data.Unit (unit)
import Network.HTTP.Affjax (defaultRequest, affjax, AffjaxResponse)
--import Network.HTTP.Affjax.Response (ResponseType(..))

{-
 TODO error handling may need some thinking?
-}
 
type EntityURI = String

-- TODO this should be configurable
baseURL :: String
baseURL = "http://localhost:3000/"

getList :: forall e a. DecodeJson a => EntityURI -> AjaxM e (Array a)
getList uri = 
  do 
     let reqUrl = baseURL <> uri
     let affReq =  defaultRequest {url = reqUrl }
     resp <- attempt $ affjax affReq
     let decode = (\x -> decodeJson x.response) :: AffjaxResponse Json -> Either String (Array a)
     either throwError (pure <<< decode) resp

getSingle :: forall e a. DecodeJson a => EntityURI -> KeyT a -> AjaxM e a  
getSingle uri (KeyT i) = 
  do 
     let reqUrl = baseURL <> uri <> "/" <> (show i)
     let affReq =  defaultRequest {url = reqUrl }
     resp <- attempt $ affjax affReq
     let decode = (\x -> decodeJson x.response) :: AffjaxResponse Json -> Either String a
     either throwError (pure <<< decode) resp

postSingle :: forall e a. DecodeJson a => EncodeJson a => EntityURI -> a -> AjaxM e (Entity (KeyT a) a)
postSingle uri elem = 
  do 
     let reqUrl = baseURL <> uri
     let affReq =  defaultRequest {method = Left POST, url = reqUrl, content = Just (encodeJson elem) }
     resp <- attempt $ affjax affReq
     let decode = (\x -> decodeJson x.response) :: AffjaxResponse Json -> Either String (Entity (KeyT a) a)
     either throwError (pure <<< decode) resp

putSingle :: forall e a. DecodeJson a => EncodeJson a => EntityURI -> KeyT a -> a -> AjaxM e a  
putSingle uri (KeyT i) elem = 
  do 
     let reqUrl = baseURL <> uri <> "/" <> (show i)
     let affReq =  defaultRequest {method = Left PUT, url = reqUrl, content = Just (encodeJson elem) }
     resp <- attempt $ affjax affReq
     let decode = (\x -> decodeJson x.response) :: AffjaxResponse Json -> Either String a
     either throwError (pure <<< decode) resp
  
deleteSingle :: forall e a. EntityURI -> KeyT a -> AjaxM e Unit  
deleteSingle uri (KeyT i) = 
  do 
     let reqUrl = baseURL <> uri <> "/" <> (show i)
     let affReq =  defaultRequest {method = Left DELETE, url = reqUrl }
     resp <- attempt $ affjax affReq
     let decode = (const $ Right unit) :: AffjaxResponse Json -> Either String Unit
     either throwError (pure <<< decode) resp
