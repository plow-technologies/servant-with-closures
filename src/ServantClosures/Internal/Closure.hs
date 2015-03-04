{-# LANGUAGE DeriveDataTypeable #-} -- To enable Typeable on the example type
{-# LANGUAGE DeriveGeneric #-} -- For the Binary instances on the example
module ServantClosures.Internal.Closure ( sendComicClosure
                                        , comicRemoteTable
                                        , Comic(..)) where

import Control.Distributed.Static ( Static
                                  , staticLabel
                                  , RemoteTable
                                  , Closure 
                                  , staticCompose
                                  , registerStatic
                                  , closure
                                  , initRemoteTable)
import Data.Binary (Binary
                   ,encode
                   ,decode)      
import GHC.Generics (Generic)      
import qualified Data.ByteString.Lazy as Lazy 
import Data.Rank1Dynamic (toDynamic)
import Data.Rank1Typeable
  ( Typeable)   
  

data Comic = Comic { title:: String
                   , author :: String }
 deriving (Typeable , Generic)

instance Binary Comic where 
-- | so for some function that operates on a Comic
-- to be serialized it first has to be declared static

sendComic :: Comic ->  
             String -> 
             IO Comic
sendComic c newTitle = return $ c{title=newTitle}


-- |Declare it static 
   
sendComicStatic :: Static (Comic ->
                           String -> IO Comic)
sendComicStatic = staticLabel "$sendComic"                           

-- |Define a decoder for the serialized free variables
decodeComicStatic :: Static (Lazy.ByteString -> Comic)
decodeComicStatic = staticLabel "$decodeComic"


-- | Define the remote table

comicRemoteTable :: RemoteTable
comicRemoteTable = registerStatic "$sendComic" (toDynamic sendComic)
                   . registerStatic "$decodeComic" (toDynamic (decode :: Lazy.ByteString -> Comic))
                   $ initRemoteTable               


-- | define the closure
-- note... the first argument to the sendComicstatic is decoded by the decodeComicStatic
-- so it can be furnished by the Serialization   
sendComicClosure :: Comic -> Closure (String -> IO Comic)
sendComicClosure c = closure decoder (encode c)
   where 
      decoder :: Static (Lazy.ByteString -> String -> IO Comic)
      decoder = sendComicStatic  `staticCompose` decodeComicStatic

