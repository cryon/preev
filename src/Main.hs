{-# LANGUAGE OverloadedStrings #-}

{-  Author: John Olsson <john@cryon.se>

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <http://www.gnu.org/licenses/>.
-}

import Data.Char (toUpper)
import Text.Printf (printf)
import Options.Applicative
import Control.Monad (mzero)
import Data.List (intercalate)
import Network.HTTP.Conduit (simpleHttp)
import Data.Aeson (decode, FromJSON(..), Value(..), (.:))
import qualified Data.ByteString.Lazy.Char8 as B
import qualified Data.Map as M

-- Parse Arguments -------------------------------------------------------------

defaultCurrency :: String
defaultCurrency = "USD"

defaultSources :: [String]
defaultSources = ["bitstamp", "btce", "localbitcoins"]

type Amount   = Double
type Currency = String
type Source   = String
type Verbose  = Bool

data Arguments = Arguments Amount Currency Source Verbose

cmdParser :: Parser Arguments
cmdParser = Arguments <$>
            -- amount in btc
            argument auto (metavar "AMOUNT") <*>

            -- target currency
            strOption (long    "to"
                    <> short   't'
                    <> metavar "CURRENCY"
                    <> help    "Target currency (e.g. USD, EUR, SEK, NOK) defaults to USD"
                    <> value   defaultCurrency) <*>

            -- exchanges to use
            strOption (long   "source"
                    <> short  's'
                    <> metavar "EXCHANGES"
                    <> help    "Comma separated (no whitespaces) list of sources (e.g. bitstamp,btce)"
                    <> value (intercalate "," defaultSources)) <*>

            -- be annoyingly noisy please
            switch (long  "verbose"
                 <> short 'v'
                 <> help  "Enable verbose mode")

-- Make request ----------------------------------------------------------------

baseUrl :: String
baseUrl = "http://preev.com/pulse"

buildUrl :: Source -> Currency -> String
buildUrl source to = baseUrl ++ sourcesParam ++ unitParam
  where sourcesParam = "/source:"   ++ source
        unitParam    = "/unit:btc," ++ to

getData :: String -> IO B.ByteString
getData = simpleHttp

-- Parse response --------------------------------------------------------------

type Volume = Double
type Price  = Double

data Market = Market { price  :: Price
                     , volume :: Volume
                     } deriving (Show)

-- TODO: 'readMaybe' instead of the partial 'read'
instance FromJSON Market where
  parseJSON (Object o) = Market <$>
                         (read <$> o .: "price") <*>
                         (read <$> o .: "vol")
  parseJSON _ = mzero

type MarketsObj = M.Map String Market
type Version    = String
type Slot       = Int

data Response = Response MarketsObj Version Slot deriving (Show)

instance FromJSON Response where
  parseJSON (Object o) = Response <$>
                         (o .: "markets" >>= parseJSON) <*>
                         (o .: "ver") <*>
                         (o .: "slot")
  parseJSON _ = mzero

getMarkets :: Response -> [Market]
getMarkets (Response m _ _) = map snd $ M.toList m

-- Verbose stuff ---------------------------------------------------------------

marketString :: String -> String -> Market -> String
marketString n c m = printf "1 BTC = %.2f %s on %s" (price m) c n

marketsString :: Response -> String -> String
marketsString (Response mo _ _) curr = intercalate "\n" $ map marketStrAcc pairs
  where pairs = M.toList mo
        marketStrAcc (n, m) = marketString n curr m

resultString :: Double -> Double -> String -> String
resultString =
  printf "\n-------\n%.2f BTC are on average (weighted by volume) worth %.2f %s"

-- Get things done! ------------------------------------------------------------

totalVolume :: [Market] -> Double
totalVolume = foldl (\acc m -> acc + volume m) 0

avgPrice :: [Market] -> Double
avgPrice ms = foldl (\acc m -> acc + (price m) * (volume m) / totalVol) 0 ms
  where totalVol = totalVolume ms

execute :: Arguments -> IO ()
execute (Arguments amount to source verbose) = do
    json <- getData $ buildUrl source to
    printResult $ decode json
    where
      printResult Nothing = putStrLn "Error while parsing result. Call the cops!"
      printResult (Just r)
        | verbose   = putStrLn $ (marketsString r curr) ++ (resultString amount result curr)
        | otherwise = printf "%.2f\n" result
        where result = amount * (avgPrice $ getMarkets r)
              curr = map toUpper to

main :: IO ()
main = execParser opts >>= execute
  where opts = info (helper <*> cmdParser)
          (fullDesc
           <> header "Gets the average market price for any amount of BTC from several exchanges weighted by volume"
           <> progDesc "Average market price for AMOUNT bitcoins")
