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
import Data.Text (unpack)
import Text.Printf (printf)
import Options.Applicative
import Control.Monad (mzero)
import Data.List (intercalate)
import Data.HashMap.Strict (toList)
import Network.HTTP.Conduit (simpleHttp)
import Data.Aeson (decode, FromJSON(..), Value(..), (.:))
import qualified Data.Aeson.Types as AT
import qualified Data.ByteString.Lazy.Char8 as B

-- Parse Arguments -------------------------------------------------------------

defaultCurrency :: String
defaultCurrency = "USD"

defaultSources :: [String]
defaultSources = ["bitstamp", "btce", "localbitcoins"]

type Amount   = Double
type Currency = String
type Source   = String
type Verbose  = Bool

data Arguments = Arguments { amount   :: Amount
                           , currency :: Currency
                           , source   :: Source
                           , verbose  :: Verbose
                           }

cmdParser :: Parser Arguments
cmdParser = Arguments
            -- amount in btc
            <$> argument auto (metavar "AMOUNT")

            -- target currency
            <*> strOption (long    "to"
                        <> short   't'
                        <> metavar "CURRENCY"
                        <> help    "Target currency (e.g. USD, EUR, SEK, NOK) \
                                   \defaults to USD"
                        <> value   defaultCurrency)

             -- exchanges to use
             <*> strOption (long   "source"
                        <> short  's'
                        <> metavar "EXCHANGES"
                        <> help    "Comma separated (no whitespaces) \
                                   \list of sources (e.g. bitstamp,btce)"
                        <> value (intercalate "," defaultSources))

             -- be annoyingly noisy please
             <*> switch (long  "verbose"
                        <> short 'v'
                        <> help  "Enable verbose mode")

-- Make request ----------------------------------------------------------------

baseUrl :: String
baseUrl = "http://preev.com/pulse"

buildUrl :: Source -> Currency -> String
buildUrl s c = baseUrl ++ sourcesParam ++ unitParam
  where sourcesParam = "/source:"   ++ s
        unitParam    = "/unit:btc," ++ c

getData :: String -> IO B.ByteString
getData = simpleHttp

-- Parse response --------------------------------------------------------------

type Volume = Double
type Price  = Double
type Name   = String

data Market = Market { name   :: Name
                     , volume :: Volume
                     , price  :: Price
                     } deriving (Show)

type Version = String
type Slot    = Int

data Response = Response [Market] Version Slot deriving (Show)

markets :: Response -> [Market]
markets (Response ms _ _) = ms

instance FromJSON Response where
  parseJSON (Object o) =
    Response
    <$> (o .: "markets" >>= parseMarkets)
    <*> (o .: "ver")
    <*> (o .: "slot")
  parseJSON _ = mzero

parseMarkets :: Value -> AT.Parser [Market]
parseMarkets (Object o) = do
  sequence $ map parseMarketObj $ toList o
  where parseMarketObj (k, (Object v)) =
  -- TODO: 'readMaybe' instead of the partial 'read'
          Market <$> (pure $ unpack k)
                 <*> (read <$> v .: "vol")
                 <*> (read <$> v .: "price")

-- Verbose stuff ---------------------------------------------------------------

marketString :: Market -> Currency -> String
marketString m c = printf "1 BTC = %.2f %s on %s" (price m) c (name m)

marketsString :: [Market] -> Currency -> String
marketsString ms c = intercalate "\n" $ map (flip marketString c) ms

resultString :: Amount -> Double -> Currency -> String
resultString = printf "\n-----\n%.2f BTC are on average (weighted by volume) \
                       \worth %.2f %s"

responseString :: [Market] -> Arguments -> Price -> String
responseString ms a res
  | verbose a = (marketsString ms curr) ++ (resultString (amount a) res curr)
  | otherwise = printf "%.2f\n" res
  where curr = map toUpper $ currency a

-- Get things done! ------------------------------------------------------------

totalVolume :: [Market] -> Volume
totalVolume = foldl (\acc m -> acc + volume m) 0

avgPrice :: [Market] -> Price
avgPrice ms = foldl (\acc m -> acc + (price m) * (volume m) / totalVol) 0 ms
  where totalVol = totalVolume ms

totalValue :: [Market] -> Amount -> Price
totalValue ms a = a * avgPrice ms

execute :: Arguments -> IO ()
execute a = do
    json   <- getData $ buildUrl (source a) (currency a)
    putStrLn $ case decode json of
      Nothing -> "Error while parsing result. Call the cops!"
      Just r  -> responseString (markets r) a $ totalValue (markets r) (amount a)

main :: IO ()
main = execParser opts >>= execute
  where opts = info (helper <*> cmdParser)
          (fullDesc
           <> header "Gets the average market price for any amount of BTC \
                     \from several exchanges weighted by volume"
           <> progDesc "Average market price for AMOUNT bitcoins")
