module ServantClosures.Internal.Closure where

import Data.Text
import Control.Distributed.Static    
import Data.Binary       
import Data.ByteString       
import Data.Rank1Dynamic (Dynamic, toDynamic, fromDynamic, dynApply)
import Data.Rank1Typeable
  ( Typeable
  , typeOf
  , ANY1
  , ANY2
  , ANY3
  , ANY4
  , isInstanceOf
  )   
  
  
data Comic = Comic { title:: Text
                   , author :: Text }



-- | so for some function that operates on a Comic
-- to be serialized it first has to be declared static

sendComic :: Comic ->  
             Text -> 
             IO Comic
sendComic c newTitle = return $ c{title=newTitle}


-- |Declare it static 
   
sendComicStatic :: Static (Comic ->
                           Text -> IO Comic)
sendComicStatic = staticLabel "$sendComic"                           


decodeComicStatic :: Static (ByteString -> Int)
decodeComicStatic = staticLabel "$decodeComic"

                  
                  
rtable :: RemoteTable
rtable = registerStatic "$sendComic" (toDynamic sendComic)
       . registerStatic "$decodeComic" (toDynamic (decode :: ByteString -> Comic))
       $ initRemoteTable               
