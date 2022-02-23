{-# LANGUAGE OverloadedStrings #-}
module Repository.Client where
import Network.HTTP.Client
import Network.HTTP.Types.Status (statusCode)
import Network.HTTP.Types
import qualified Data.ByteString.Lazy as LBS

data HTTPResult = HTTPResult Int LBS.ByteString deriving Show

getRequest :: String -> IO HTTPResult
getRequest url = do
  manager <- newManager defaultManagerSettings

  request <- parseRequest url
  response <- httpLbs request manager

  let status = statusCode $ responseStatus response
      body = responseBody response
  return $ HTTPResult status body

putRequest :: String -> RequestHeaders -> RequestBody -> IO HTTPResult
putRequest url header body = do
  manager <- newManager defaultManagerSettings

  initialRequest <- parseRequest url
  let request = initialRequest 
            { method = "PUT"
            , requestHeaders = header
            , requestBody = body
            }
  response <- httpLbs request manager
  let status = statusCode $ responseStatus response
      body = responseBody response
  return $ HTTPResult status body

putCourseInformation :: [(String, LBS.ByteString)] -> IO [HTTPResult]
putCourseInformation [] = return []
putCourseInformation (x:xs) = do
  result <- case x of 
              (_id, json) -> putRequest ("http://localhost:8000/v1/documents/"++_id) 
                                         [("Content-Type","application/json")] 
                                         $ RequestBodyLBS json
  rest_part <- putCourseInformation xs
  return $ result:rest_part









