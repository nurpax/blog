---
title: Reading TCX in Haskell
author: Janne Hellsten
date: December 27, 2012
public: true
---

I use a Garmin GPS/heart-rate monitor watch to track my running.  I
also upload this GPS data to a service called
[RunKeeper](http://www.runkeeper.com) to keep a history of my runs.
While I've generally been happy with RunKeeper, my experience
uploading Garmin GPS data to RunKeeper has been less than stellar.  My
biggest complaint is that usually the original GPS data changes
significantly when uploaded to RunKeeper.  For example, a 10.0 km run
(according to Garmin) can become 10.2 km in RunKeeper.

I decided to do a bit of data mining on Garmin GPS files to figure out
how RunKeeper interprets it differently.  Garmin's tools can export
GPS data as both GPX and
[TCX](http://en.wikipedia.org/wiki/Training_Center_XML).  The latter
format is developed by Garmin and quite likely the closest match to
the their native format.  Both GPX and TCX are XML.

I didn't get very far with actual GPS track analysis but I did write a
TCX file reader in Haskell using the [Haskell XML Toolbox
(hxt)](http://hackage.haskell.org/package/hxt) library.  As there
seems to be a bit of a lack of Haskell XML parsing examples on the
Internet, I decided to post my TCX reader here as an example of
parsing Real World XML data in Haskell.

Here's a short sample of what a TCX file looks like (some elements
have been omitted and xmlns URLs truncated for brevity):


~~~~~{.xml}
<?xml version="1.0" encoding="UTF-8"?>
<!-- Some elements omitted for brevity -->
<TrainingCenterDatabase
  xsi:schemaLocation="http://www.garmin.com/xmlschemas/...d"
  xmlns:ns5="http://www.garmin.com/xmlschemas/ActivityGoals/v1"
  xmlns:ns3="http://www.garmin.com/xmlschemas/ActivityExtension/v2"
  xmlns:ns2="http://www.garmin.com/xmlschemas/UserProfile/v2"
  xmlns="http://www.garmin.com/xmlschemas/TrainingCenterDatabase/v2"
  xmlns:xsi="http://www.w3.org/2001/XMLSchema-instance" xmlns:ns4="...">
  <Activities>
    <Activity Sport="Biking">
      <Id>2012-12-22T13:47:50.000Z</Id>
      <Lap StartTime="2012-12-22T13:47:50.000Z">
        <DistanceMeters>1000.0</DistanceMeters>
        <Track>
          <Trackpoint>
            <Time>2012-12-22T13:47:49.000Z</Time>
            <AltitudeMeters>-2.799999952316284</AltitudeMeters>
            <DistanceMeters>0.0</DistanceMeters>
            <HeartRateBpm>
              <Value>134</Value>
            </HeartRateBpm>
~~~~~

The Haskell code for TCX file reading can be found below (also up on
[github](https://github.com/nurpax/hs-tcx) with a .cabal file).
Here's an outline of what it does:

* The input XML document is read from a file called `test-act.tcx`
* The XML document is massaged into Haskell objects using HXT combinators
* The resulting Haskell objects (`Activity`, `Lap` and `Trackpoint`)
  are traversed and output to stdout

Note: As is probably obvious, this code is not meant to be
comprehensive library for accessing TCX data -- it's really just an
example of how to get started with HXT and TCX.

~~~~~{.haskell}
{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}

import Text.XML.HXT.Core
import Data.Time (UTCTime, readTime)
import System.Locale (defaultTimeLocale)

data Activity = Activity [Lap]
  deriving (Show)

data Lap = Lap {
    lapDistance :: Float
  , lapTrackpoints :: [Trackpoint]
  } deriving (Show)

data Trackpoint = Trackpoint {
    tpTime :: UTCTime
  , tpBpm :: String
  } deriving (Show)

atTag :: ArrowXml a => String -> a XmlTree XmlTree
atTag tag = deep (isElem >>> hasName tag)

text :: ArrowXml a => a XmlTree String
text = getChildren >>> getText

-- Note: the hardcoded .000 part is kludge but for my inputs this was
-- an easy way to get timestamps to parse.
readt :: String -> UTCTime
readt = readTime defaultTimeLocale "%FT%T.000%Z"

getTrackpoint :: ArrowXml a => a XmlTree Trackpoint
getTrackpoint = atTag "Trackpoint" >>>
  proc x -> do
    time <- text <<< atTag "Time" -< x
    bpm <- text <<< atTag "Value" <<< atTag "HeartRateBpm" -< x
    returnA -< Trackpoint (readt time) bpm

getLap :: ArrowXml a => a XmlTree Lap
getLap = getChildren >>> isElem >>> hasName "Lap" >>>
  proc x -> do
    pts <- listA getTrackpoint <<< atTag "Track" -< x
    dist <- getChildren >>> isElem >>> hasName "DistanceMeters" >>> text -< x
    returnA -< Lap (read dist) pts

getActivity :: ArrowXml a => a XmlTree Activity
getActivity = atTag "Activity" >>>
  proc x -> do
    laps <- listA getLap -< x
    returnA -< Activity laps

getActivities :: ArrowXml a => a XmlTree [Activity]
getActivities = deep (isElem >>> hasName "TrainingCenterDatabase" /> hasName "Activities") >>>
  proc x -> do
    activities <- listA getActivity -< x
    returnA -< activities

main :: IO ()
main = do
  activities <- runX (readDocument [withValidate no] "test-act.tcx" >>> getActivities)
  mapM_ printActivity (head activities)
  where
    printActivity (Activity laps) = do
      putStrLn "Activity:"
      mapM_ printLaps laps

    printLaps (Lap distance trackpts) = do
      putStrLn "  Lap:"
      putStrLn ("    Distance: " ++ show distance)
      mapM_ printTrackpoint trackpts

    printTrackpoint (Trackpoint time bpm) =
      putStrLn ("    time: " ++ show time ++ " bpm: " ++ show bpm)
~~~~~
