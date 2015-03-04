{-# LANGUAGE OverloadedStrings #-} -- For the JSON Keys
{-# LANGUAGE DeriveDataTypeable #-} -- To enable Typeable on the example type
{-# LANGUAGE DeriveGeneric #-} -- For the Binary instances on the example
{-# LANGUAGE GeneralizedNewtypeDeriving #-} -- To make Closures Web exportable
module ServantClosures.Internal.Closure ( sendBookClosure
                                        , comicRemoteTable
                                        , JSONClosure(..)
                                        , Book(..)) where

import Control.Distributed.Static ( Static
                                  , staticLabel
                                  , RemoteTable
                                  , Closure 
                                  , staticCompose
                                  , registerStatic
                                  , closure
                                  , initRemoteTable)
import qualified Data.ByteString.Base64 as B64 
       
-- import Data.Text (Text)       
-- import qualified Data.Text as T
import qualified Data.Text.Encoding as T       
import Data.Binary (Binary
                   ,encode
                   ,decode)      
import Control.Applicative ((<$>))       
import GHC.Generics (Generic)      
import Data.Aeson.Types (Parser)       
import Data.Aeson (ToJSON
                  ,FromJSON
                  ,fromJSON
                  ,Result(..)
                  ,Value(..)
                  ,object
                  ,toJSON
                  ,parseJSON
                  ,(.=)
                  ,(.:))  

import qualified Data.ByteString.Lazy as Lazy 
import Data.Rank1Dynamic (toDynamic)
import Data.Rank1Typeable
  ( Typeable)   
  

data Book = Book { title:: String
                   , author :: String }
 deriving (Typeable , Generic)

instance Binary Book where 
         
instance FromJSON Book where
instance ToJSON Book where 
-- | so for some function that operates on a Book
-- to be serialized it first has to be declared static

sendBook :: Book ->  
             String -> 
             IO Book
sendBook c newTitle = return $ c{title=newTitle}


-- |Declare it static 
   
sendBookStatic :: Static (Book ->
                           String -> IO Book)
sendBookStatic = staticLabel "$sendBook"                           

-- |Define a decoder for the serialized free variables
decodeBookStatic :: Static (Lazy.ByteString -> Book)
decodeBookStatic = staticLabel "$decodeBook"


-- | Define the remote table

comicRemoteTable :: RemoteTable
comicRemoteTable = registerStatic "$sendBook" (toDynamic sendBook)
                   . registerStatic "$decodeBook" (toDynamic (decode :: Lazy.ByteString -> Book))
                   $ initRemoteTable               


-- | define the closure
-- note... the first argument to the sendBookstatic is decoded by the decodeBookStatic
-- so it can be furnished by the Serialization   
sendBookClosure :: Book -> Closure (String -> IO Book)
sendBookClosure c = closure decoder (encode c)
   where 
      decoder :: Static (Lazy.ByteString -> String -> IO Book)
      decoder = sendBookStatic  `staticCompose` decodeBookStatic


-- | The web versions


-- | JSONClosure just adds the most important, to and from json stuff
newtype JSONClosure a = JSONClosure (Closure a)
 deriving (Eq,Ord,Show,Typeable)



instance (Typeable a) => ToJSON (JSONClosure a) where 
  toJSON (JSONClosure c) = object ["jsonClosure" .= internalClosure]
    where internalClosure = toJSON 
                          . T.decodeUtf8 
                          . B64.encode 
                          . Lazy.toStrict
                          . encode $ c


instance (Typeable a) => FromJSON (JSONClosure a) where
   parseJSON (Object jclosure) = decodeInternalClosure =<<
                                 (jclosure .: "jsonClosure" )

     where
       decodeInternalClosure :: (Typeable a)=> Value -> Parser (JSONClosure a)
       decodeInternalClosure v = JSONClosure <$> 
                                 decodeTextAndB64 v
       decodeTextAndB64 :: (Typeable a) => Value -> Parser (Closure a)
       decodeTextAndB64 i = case fromJSON i of
                            (Error e) -> fail e
                            (Success r) ->   fmap (decode . Lazy.fromStrict )
                                           . either fail return
                                           . B64.decode . T.encodeUtf8 $ r

   parseJSON _ = fail "JSON Closure requires an object, recieved other"


