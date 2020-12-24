module Main where

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Lazy.Char8 as LC
import Network.HTTP.Simple
import Data.Aeson
import Data.Text as T
import GHC.Generics
import Lib

dogHost :: BC.ByteString
dogHost = "dog.ceo"

apiPath :: BC.ByteString
apiPath = "/api/breeds/image/random"

buildRequest :: BC.ByteString -> BC.ByteString -> BC.ByteString -> Request
buildRequest host method path = setRequestMethod method
                                    $ setRequestHost host
                                    $ setRequestPath path
                                    $ setRequestSecure True
                                    $ setRequestPort 443
                                    $ defaultRequest

request :: Request
request = buildRequest dogHost "GET" apiPath

main :: IO ()
main = do
  response <- httpLBS request
  let status = getResponseStatusCode response
  if status == 200
  then do
    print "saving request to file"
    let jsonBody = getResponseBody response
    L.writeFile "data.json" jsonBody
  else print "request failed with error"


data Person = Person {
      name :: Text
    , age  :: Int
    } deriving (Generic, Show)

instance FromJSON Person
instance ToJSON Person

