module CrudReuse.Server where
  
import Prelude

import Control.Monad.Aff (attempt)
import Control.Monad.Error.Class (throwError)
import CrudReuse.Model (KeyT(..))
import CrudReuse.Common (AjaxM)
import Data.Argonaut (class DecodeJson, Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Either (Either, either)
import Network.HTTP.Affjax (defaultRequest, affjax, AffjaxResponse)
--import Network.HTTP.Affjax.Response (ResponseType(..))
 
type EntityURI = String

-- TODO this should be configurable
baseURL :: String
baseURL = "http://localhost:3000/"

getElements :: forall e a. DecodeJson a => EntityURI -> AjaxM e (Array a)
getElements uri = -- do
  do 
     let reqUrl = baseURL <> uri
     let affReq =  defaultRequest {url = reqUrl }
     resp <- attempt $ affjax affReq
     let decode = (\x -> decodeJson x.response) :: AffjaxResponse Json -> Either String (Array a)
     either throwError (pure <<< decode) resp

getElement :: forall e a. DecodeJson a => EntityURI -> KeyT a -> AjaxM e a  
getElement uri (KeyT i) = 
  do 
     let reqUrl = baseURL <> uri <> "/" <> (show i)
     let affReq =  defaultRequest {url = reqUrl }
     resp <- attempt $ affjax affReq
     let decode = (\x -> decodeJson x.response) :: AffjaxResponse Json -> Either String a
     either throwError (pure <<< decode) resp
