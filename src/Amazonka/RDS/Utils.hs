{-# LANGUAGE TypeFamilies           #-}  -- required for 'Rs'
{-# LANGUAGE FlexibleInstances      #-}  -- required for ToQuery String
{-# OPTIONS_GHC -fno-warn-orphans   #-}
module  Amazonka.RDS.Utils
    (   generateDbAuthToken
    ,   Endpoint
    ,   Port
    ,   DBUsername
    ,   Region
    ,   regionFromText
    )
where

import           Amazonka                   ( runResourceT
                                            , presignURL
                                            )
import qualified Amazonka.RDS               as RDS
import           Amazonka.Endpoint          ( setEndpoint )
import qualified Amazonka.Env               as Env
import qualified Amazonka.Request           as AWSReq
import qualified Amazonka.Response          as AWSResp
import           Amazonka.Data.Text         ( fromText )
import           Amazonka.Data.Path         ( ToPath (..)
                                            )
import           Amazonka.Data.Query        ( ToQuery (..)
                                            , QueryString ( QList )
                                            )
import           Amazonka.Data.Headers      ( ToHeaders (..)
                                            )
import           Amazonka.Types             ( Seconds (..)
                                            , AWSRequest (..)
                                            , AWSResponse
                                            , Service (..)
                                            , Region
                                            )
import           Prelude                    hiding ( drop, length )
import           Control.Lens               ( (&), set )
import           Data.ByteString            ( ByteString, drop, length )
import           Data.ByteString.Char8      ( pack )
import qualified Data.Text                  as T
import qualified Data.Time.Clock            as Clock
import           Data.Generics.Product      ( field )

type Endpoint   = String
type Port       = Int
type DBUsername = String
type Token      = ByteString

tokenExpiration :: Seconds
tokenExpiration = Seconds 900  -- 15 minutes

serviceSigningName :: ByteString
serviceSigningName = "rds-db"

thisService :: Service
thisService = RDS.defaultService { _serviceSigningName = serviceSigningName }

dropPrefix :: ByteString -> ByteString
dropPrefix = drop $ length "https://"

-- Amazon docs:             https://docs.aws.amazon.com/AmazonRDS/latest/UserGuide/UsingWithRDS.IAMDBAuth.html
-- Python implementation:   https://github.com/boto/botocore/blob/77527250093fc97cbf078adab04bdd74b1fd3c03/botocore/signers.py#L409
-- Go implementation:       https://github.com/aws/aws-sdk-go/blob/e2d6cb448883e4f4fcc5246650f89bde349041ec/service/rds/rdsutils/connect.go#L36-L67
-- | Generates RDS auth token that can be used as a temporary password for Postgres connections.
generateDbAuthToken :: Env.Env
                    -> Endpoint
                    -> Port
                    -> DBUsername
                    -> Region
                    -> IO Token
generateDbAuthToken env endp prt username region = do
    -- it has some overhead, but we're just making sure we're composing a correct URL
    let action = GetDBAuthToken $ PresignParams
                                    { endpoint   = endp
                                    , port       = prt
                                    , dbUsername = username
                                    }
        regionalEnv = env & set (field @"envRegion") region

    signingTime <- Clock.getCurrentTime

    runResourceT $ do
        val <-  presignURL
                regionalEnv
                signingTime
                tokenExpiration
                action
        pure $ dropPrefix val


data PresignParams = PresignParams
    { endpoint   :: Endpoint
    , port       :: Port
    , dbUsername :: DBUsername
    }


newtype GetDBAuthTokenResponse = GetDBAuthTokenResponse ByteString

newtype GetDBAuthToken = GetDBAuthToken PresignParams

instance AWSRequest GetDBAuthToken where
    type AWSResponse GetDBAuthToken = GetDBAuthTokenResponse

    service _proxy = thisService

    request serv (GetDBAuthToken params) =
        AWSReq.defaultRequest svc (GetDBAuthToken params) where
            svc = setEndpoint useHTTPS (pack . endpoint $ params) (port params) serv
            useHTTPS = True

    response = AWSResp.receiveBytes $ \_s _h x -> pure $ GetDBAuthTokenResponse x


instance ToPath GetDBAuthToken where
    toPath _ = ""


instance ToQuery String where
    toQuery = toQuery . pack


instance ToQuery GetDBAuthToken where
    toQuery (GetDBAuthToken params) = QList (toQuery <$> xs) where
        xs :: [(String, String)]
        xs = [("Action", "connect"), ("DBUser", dbUsername params)]


instance ToHeaders GetDBAuthToken where
    toHeaders _ = []


regionFromText :: T.Text -> Either String Region
regionFromText = fromText
