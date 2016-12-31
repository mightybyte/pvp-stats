{-

Based on code by Taylor Fausak, original published at:

https://gist.github.com/tfausak/98726c8255c5bc5c940e0f4939978a43/9a6049d80e421f737072f05a5a583203762cc432#file-output-txt-L4

-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Function ((&))

import           Control.Arrow
import qualified Codec.Archive.Tar as Tar
import qualified Codec.Compression.GZip as GZip
import           Control.Monad
import qualified Data.ByteString.Lazy as B
import           Data.Either
import qualified Data.List as List
import qualified Data.Map as Map
import qualified Data.Maybe as Maybe
import qualified Data.Ord as Ord
import qualified Data.Set as Set
import qualified Data.Time as Time
import qualified Data.Tuple as Tuple
import qualified Data.Version as Version
import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.TLS as TLS
import           System.Directory
import qualified System.FilePath as FilePath
import qualified Text.ParserCombinators.ReadP as ReadP
import           Text.Printf
import HackageDiff

outFile :: FilePath
outFile = "pvp.csv"

main :: IO ()
main = do
    packages <- getPackages
    let num = Map.size packages
    putStrLn ("Found " ++ pluralize num "package" ++ ".")
    exists <- doesFileExist outFile
    unless exists $ writeFile outFile "package, guess, num_versions, num_parsed_versions, breaking_versions\n"
    let names = Map.keys packages
    mapM_ (writeGuess packages) names

writeGuess ps p = do
    putStrLn p
    let Just vset = Map.lookup p ps
    ds <- allDiffs ps p
    let breakingVersions = filter snd $ map (second hasAnyBreaking) ds
        brokeAt = length . dropWhile (==0) . reverse . Version.versionBranch
        inds = map (brokeAt . fst) breakingVersions
        guess = guessPVP inds
    appendFile "pvp.csv" $ printf "%s, %s, %d, %d, %s\n" p (show guess) (Set.size vset) (length ds)
      (List.intercalate ", " (map (Version.showVersion . fst) breakingVersions))
  

indexFile :: String
indexFile = "index.tar.gz"

type Packages = Map.Map Package Versions

data BrokenType = Symbol | Module
  deriving (Eq,Ord,Show)

data BrokenAction = Removed | Modified
  deriving (Eq,Ord,Show)

data Broken = Broken
    { brokenType :: BrokenType
    , brokenAction :: BrokenAction
    , brokenItem :: String
    } deriving (Eq,Ord,Show)

-- False positive
-- "snapletConfig :: Lens' (Snaplet s_au1z) SnapletConfig"
-- "snapletConfig :: Lens' (Snaplet s_aug6) SnapletConfig")

exportBreaking EAdded = False
exportBreaking ERemoved = True
exportBreaking (EModified _) = True
exportBreaking EUnmodified = False

moduleBreaking :: ModuleCmp -> Bool
moduleBreaking (MAdded _) = False
moduleBreaking MAddedParseError = False
moduleBreaking (MRemoved _) = True
moduleBreaking MRemovedParseError = True
moduleBreaking MNotSureIfModifiedParseError = False
moduleBreaking (MModified es) = any (exportBreaking . fst) es
moduleBreaking MUnmodifed = False

pairBreaking (EAdded, _) = []
pairBreaking (ERemoved, s) = [Broken Symbol Removed s]
pairBreaking (EModified sOld, sNew) = [Broken Symbol Modified (show (sOld, sNew))]
pairBreaking (EUnmodified, _) = []

brokenList (MAdded _) = []
brokenList MAddedParseError = []
brokenList m@(MRemoved s) = [Broken Module Removed (show s)]
brokenList m@MRemovedParseError = [Broken Module Removed "parse error"]
brokenList MNotSureIfModifiedParseError = []
brokenList (MModified es) = concatMap pairBreaking es
brokenList MUnmodifed = []

data VersionScheme = PVP | SemVer | Unknown
  deriving (Eq,Ord,Show,Read)

guessPVP bbs =
    case mode $ filter (<3) bbs of
      Just 2 -> PVP
      Just 1 -> SemVer
      _ -> Unknown
    
topBreakingBump ps p = do
    ds <- allDiffs ps p
    let breakingVersions = filter snd $ map (second hasAnyBreaking) ds
        brokeAt = length . dropWhile (==0) . reverse . Version.versionBranch
    return $ map (brokeAt . fst) breakingVersions

hasAnyBreaking :: Diff -> Bool
hasAnyBreaking = any moduleBreaking . map fst

fromRight (Right a) = a
fromRight _ = error "fromRight error: should never happen"

allDiffs :: Packages -> Package -> IO [(Version.Version, Diff)]
allDiffs ps p = do
  let Just vset = Map.lookup p ps
      vs = Set.toList vset
  diffs <- zipWithM (breakingChanges p) vs (tail vs)
  return $  map (second fromRight) $ filter (isRight . snd) $ zip (tail vs) diffs

breakingChanges :: Package -> Version.Version -> Version.Version -> IO (Either String Diff)
breakingChanges p v1 v2 = do
  let dc = DiffConfig ModeDownloadDB True
  packageDiff dc $ PackageToDiff p (Version.showVersion v1) (Version.showVersion v2)

getPackages :: IO Packages
getPackages = do
  exists <- doesFileExist indexFile
  body <- if exists
            then B.readFile indexFile
            else downloadPackages

  let archive = GZip.decompress body
  let entries = Tar.read archive
  return $ Tar.foldEntries update Map.empty (const Map.empty) entries

downloadPackages :: IO B.ByteString
downloadPackages = do
  now <- Time.getCurrentTime
  print now
  putStrLn "Getting package index ..."
  let url = "https://hackage.haskell.org/packages/" ++ indexFile
  request <- Client.parseUrlThrow url
  manager <- Client.newManager TLS.tlsManagerSettings
  response <- Client.httpLbs request manager
  putStrLn "Got package index."
  let body = Client.responseBody response
  B.writeFile indexFile body
  return body

type Package = String

type Versions = Set.Set Version.Version

update :: Tar.Entry -> Map.Map Package Versions -> Map.Map Package Versions
update entry x =
  case getMetadata entry of
    Nothing -> x
    Just (package, version) ->
      Map.insertWith Set.union package (Set.singleton version) x

getMetadata :: Tar.Entry -> Maybe (Package, Version.Version)
getMetadata entry = do
  guard (isNormalFile entry)
  let path = Tar.entryPath entry
  guard (isCabalFile path)
  (package, rawVersion) <-
    case FilePath.splitPath path of
      [package, version, _] -> Just (safeInit package, safeInit version)
      _ -> Nothing
  version <- readVersion rawVersion
  Just (package, version)

isNormalFile :: Tar.Entry -> Bool
isNormalFile entry =
  case Tar.entryContent entry of
    Tar.NormalFile _ _ -> True
    _ -> False

isCabalFile :: FilePath -> Bool
isCabalFile path =
  case FilePath.takeExtension path of
    ".cabal" -> True
    _ -> False

safeInit :: [a] -> [a]
safeInit x =
  case x of
    [] -> []
    _ -> init x

readVersion :: String -> Maybe Version.Version
readVersion x = do
  let parses = ReadP.readP_to_S Version.parseVersion x
  parse <- safeLast parses
  case parse of
    (version, "") -> Just version
    _ -> Nothing

safeLast :: [a] -> Maybe a
safeLast x =
  case x of
    [] -> Nothing
    _ -> Just (last x)

pluralize :: Int -> String -> String
pluralize count singular =
  let plural = singular ++ "s"
      word =
        case count of
          1 -> singular
          _ -> plural
  in unwords [show count, word]

mode
  :: Ord a
  => [a] -> Maybe a
mode xs =
  xs & List.sort & List.group & map (\x -> (length x, x)) &
  List.sortBy (Ord.comparing Ord.Down) &
  map snd &
  Maybe.mapMaybe Maybe.listToMaybe &
  Maybe.listToMaybe

toMapOfSets
  :: (Ord k, Ord v)
  => Map.Map k v -> Map.Map v (Set.Set k)
toMapOfSets x =
  x & Map.toAscList & map Tuple.swap & map (\(k, v) -> (k, Set.singleton v)) &
  Map.fromListWith Set.union

showMap
  :: (Show k, Show v)
  => Map.Map k v -> String
showMap x =
  x & Map.toAscList & map (\(k, v) -> show k ++ " => " ++ show v) & unlines
