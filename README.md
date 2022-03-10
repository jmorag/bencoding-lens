# bencoding-lens

Law abiding lenses and traversals for
[bencoding](https://hackage.haskell.org/package/bencoding), ported from
[lens-aeson](https://hackage.haskell.org/package/lens-aeson-1.1.1). Works with
arbitrary bencoded data, although most examples in the wild are torrent files.


``` haskell
{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import Data.BEncode
import Data.BEncode.Lens

import Network.HTTP.Simple -- from http-conduit
import Crypto.Hash -- from cryptonite

main = do
  -- Get the ubuntu 20.04 live server torrent file with http-conduit
  torrent <- getResponseBody <$> httpBS "https://releases.ubuntu.com/20.04/ubuntu-20.04.4-live-server-amd64.iso.torrent"
  -- Print some interesting bits of the torrent file

  -- Announce link
  print (torrent ^? key "announce") 
  -- Just (BString "https://torrent.ubuntu.com/announce")

  -- Keys of the top level dict
  print (fmap fst (torrent ^@.. members))
  -- ["announce","announce-list","comment","created by","creation date","info"]

  -- Keys of the info dict
  print (fmap fst (torrent ^@.. key "info" . members))
  -- ["length","name","piece length","pieces"]

  -- Piece length
  print (torrent ^? key "info" . key "piece length" . _BInteger)
  -- Just 262144

  -- Compute the info hash (see definition below)
  print (torrent ^?! infoHash)
  -- b44a0e20fa5b7cecb77156333b4268dfd7c30afb
  
infoHash :: (AsBValue t) => Fold t (Digest SHA1)
infoHash = key "info" . to (hashlazy . encode)
```
