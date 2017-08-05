module CrudReuse.Server where
  
import Prelude
--import CrudEx.Model
import Control.Monad.Aff (attempt)
import Control.Monad.Error.Class (throwError)
import CrudReuse.Common (AjaxM)
import Data.Argonaut (class DecodeJson, Json)
import Data.Argonaut.Decode (decodeJson)
import Data.Either (Either, either)
import Network.HTTP.Affjax (defaultRequest, affjax, AffjaxResponse)
 

getElements :: forall e a. DecodeJson a => String -> AjaxM e (Array a)
getElements uri = -- do
  do 
     let reqUrl = "http://localhost:3000/" <> uri
     let affReq =  defaultRequest {url = reqUrl }
     resp <- attempt $ affjax affReq
     let decode = (\x -> decodeJson x.response) :: AffjaxResponse Json -> Either String (Array a)
     either throwError (pure <<< decode) resp
  