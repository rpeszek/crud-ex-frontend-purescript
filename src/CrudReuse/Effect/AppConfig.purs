module CrudReuse.Effect.AppConfig (
  APPCONFIG
  , getAppConfigBaseUrl
) where

import Control.Monad.Eff (Eff, kind Effect)
-- import Prelude
-- import CrudReuse.Debug (debugShow)

foreign import data APPCONFIG :: Effect

-- | getAppConfigItem key defVal
foreign import getAppConfigItem :: forall e. String -> String -> Eff (appconf :: APPCONFIG | e) String

defaultBaseUrl :: String
defaultBaseUrl = "http://localhost:3000/"

getAppConfigBaseUrl :: forall e . Eff (appconf :: APPCONFIG | e) String
getAppConfigBaseUrl =  {- (debugShow "getAppConfigBaseUrl") <$> -} getAppConfigItem "baseUrl" defaultBaseUrl
    
