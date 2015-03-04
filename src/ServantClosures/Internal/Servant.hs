{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}
module ServantClosures.Internal.Servant where
import Control.Applicative

import Data.Aeson
import Data.Proxy
import Data.Text (Text)
import GHC.Generics
import Network.Wai.Handler.Warp (run)
import Servant
import Servant.Client
data Book = Book
  { title  :: Text
  , author :: Text
  } deriving Generic

sampleBook :: Book
sampleBook = Book "Infinite Jest" "David Foster Wallace"

           -- JSON instances
instance FromJSON Book
instance ToJSON Book

             -- we explicitly say we expect a request body,
             -- of type Book
type BookApi = "books" :> ReqBody Book :> Post Book  -- POST /books
          :<|> "books" :> Get [Book]                 -- GET /books

server :: () -> Server BookApi
server conn = postBook
         :<|> getBooks

  where -- the aforementioned 'ReqBody' automatically makes this handler
        -- receive a Book argument
        postBook book =  return sampleBook
        getBooks      =  return [sampleBook]

bookApi :: Proxy BookApi
bookApi = Proxy


(getAllBooks :<|> postNewBook) = client bookApi
