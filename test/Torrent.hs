{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Lens
import Crypto.Hash
import Data.BEncode
import Data.BEncode.Lens
import Network.HTTP.Simple
import Test.Hspec

main = do
  -- Get the ubuntu 20.04 live server torrent file with http-conduit
  resp <- httpBS "https://releases.ubuntu.com/20.04/ubuntu-20.04.4-live-server-amd64.iso.torrent"
  let torrent = getResponseBody resp
  hspec do
    it "should get the corrent ubuntu announce string" do
      (torrent ^?! key "announce" . _BString) `shouldBe` "https://torrent.ubuntu.com/announce"
    it "should get the keys of the top level dict" do
      fmap fst (torrent ^@.. members) `shouldBe` ["announce", "announce-list", "comment", "created by", "creation date", "info"]

    it "should get the keys of the info dict" do
      fmap fst (torrent ^@.. key "info" . members) `shouldBe` ["length", "name", "piece length", "pieces"]

    it "should find the piece length" do
      (torrent ^?! key "info" . key "piece length" . _BInteger) `shouldBe` 262144

    it "should compute the info hash (see definition below)" do
      show (torrent ^?! infoHash) `shouldBe` "b44a0e20fa5b7cecb77156333b4268dfd7c30afb"

infoHash :: (AsBValue t) => Fold t (Digest SHA1)
infoHash = key "info" . to (hashlazy . encode)
