{- | App to calculate gas costs using the MapQuest API -}

-- From: https://stackoverflow.com/questions/17844223/json-parsing-in-haskell
{-# LANGUAGE DeriveDataTypeable #-}

import Network.HTTP
import System.IO
import Data.Maybe
import Data.List (stripPrefix, intercalate)
import Data.Text (stripStart, pack, unpack)
import Network.URI
import Control.Monad (liftM, liftM2)
import Text.JSON.Generic

main = do
        putStrLn $ "Enter your vehicle's fuel efficiency. Kilometers per litre ("
                    ++ show KPL
                    ++ ") is the default. Add a suffix of "
                    ++ show MPG
                    ++ " for miles per gallon."
        kpl <- liftM toMetric (readLn :: IO UnitValue)
        putStrLn $ "Enter the expected gas price. [Currency] per litre ("
                    ++ show LI
                    ++ ") is the default. Add a suffix of "
                    ++ show GA
                    ++ " for [currency] per gallon."
        dpl <- liftM toMetric (readLn :: IO UnitValue)
        fromAddr <- getAddress "Enter the start address" False
        toAddrs <- getToAddresses
        route <- getRoute fromAddr toAddrs
        let cost = calcTripCost (distance route) kpl dpl 
        putStrLn $ "The gas cost of your trip with " ++ (show route) ++ " is estimated to cost: [currency] " ++ (show cost)

calcTripCost :: Double -> UnitValue -> UnitValue -> Double
calcTripCost distance UnitValue{value=kpl} UnitValue{value=dpl} = dpl * (distance / kpl)

getAddress :: String -> Bool -> IO String
getAddress prompt allowEmpty =
    do
    putStrLn prompt
    addr <- getLine
    if addr == "" && not allowEmpty
        then 
            do
            putStrLn "Address must not be empty."
            getAddress prompt allowEmpty
        else return addr

getToAddresses :: IO [String]
getToAddresses =
    do
        putStrLn "Enter the stops of the trip in order\n"
        as <- addresses
        case as of
            [] ->
                do
                putStrLn "At least one stop is required.\n"
                getToAddresses
            _ -> return as
    where
        addresses :: IO [String]
        addresses =
                do 
                address <- getAddress "Enter a stop address or leave empty when finished:" True
                case address of
                    "" -> return []
                    a -> liftM2 (:) (return a) addresses

data Unit = MPG | KPL | LI | GA | Unknown

type UnitVal = Double
data UnitValue = UnitValue {value :: UnitVal, unit :: Unit}

instance Show Unit where
    show MPG = "mpg"
    show KPL = "kpl"
    show LI = "li"
    show GA = "ga"
    show Unknown = ""

instance Read Unit where
    readsPrec _ value = tryParse [MPG, KPL, LI, GA]
        where
            tryParse :: [Unit] -> [(Unit, String)]
            tryParse (u:us) = case stripPrefix (show u) $ unpack $ stripStart $ pack value of
                                Just rest -> [(u, rest)]
                                Nothing -> tryParse us
            tryParse _ = []

instance Show UnitValue where
    show UnitValue{value=v, unit=u} = show v ++ " " ++ show u

instance Read UnitValue where
    readsPrec _ value = 
        case getVal of
            Just (val, rest) -> [(UnitValue{value=val,unit=unit}, unitRest)]
                where (unit,unitRest) = case (reads rest :: [(Unit,String)]) of
                                (x:_) -> x
                                _ -> (Unknown,rest)
            Nothing -> []
        where 
            getVal :: Maybe (UnitVal, String)
            getVal =
                case (reads value :: [(UnitVal, String)]) of
                    (x:_) -> Just x
                    _ -> Nothing

oneUSGalInLi = 3.785412
oneMiInKm = 1.609344
toMetric :: UnitValue -> UnitValue
toMetric u@UnitValue{value=_,unit=KPL} = u
toMetric u@UnitValue{value=_,unit=LI} = u
toMetric u@UnitValue{value=_,unit=Unknown} = u
toMetric UnitValue{value=v,unit=MPG} = UnitValue{value=(v * oneMiInKm) / oneUSGalInLi, unit=KPL}
toMetric UnitValue{value=v,unit=GA} = UnitValue{value=v * oneUSGalInLi, unit=LI}

data Location = Location {
                    adminArea1 :: String,
                    adminArea3 :: String,
                    adminArea5 :: String,
                    street :: String
                } deriving (Typeable, Data)
data Route = Route {distance :: Double, locations :: [Location]} deriving (Typeable, Data)
data DirectionsRes = DirectionsRes {route :: Route} deriving (Show, Typeable, Data)

instance Show Location where
    show Location
        {
            adminArea1=country,
            adminArea3=province,
            adminArea5=city,
            street=street} = intercalate ", " [street, city, province, country]

instance Show Route where
    show Route {distance=distance, locations=locations} =
        "route of:" ++ (intercalate "\n-> " $ "" : map show locations)
        ++ "\nwith distance: " ++ (show distance) ++ "km"

getRoute :: String -> [String] -> IO Route
getRoute fromAddr toAddrs =
    do
    result <- downloadURL $ getEndpointURI fromAddr toAddrs 
    -- Uncomment below and comment above to use saved response
    -- result <- liftM Right $ readFile "response.json"
    case result of
        (Right res) ->
            do
            let decoded = (decodeJSON res :: DirectionsRes)
            return $ route decoded
        (Left err) -> error err

getEndpointURI :: String -> [String] -> String
getEndpointURI fromAddr toAddrs =
    joinURIParams [base, fromParam, toParams, unitParam]
    where
        fromParam = "from=" ++ (escapeForURI fromAddr)
        toParams = joinURIParams $ map (("to=" ++) . escapeForURI) toAddrs
        unitParam = "unit=k"
        base = "http://www.mapquestapi.com/directions/v2/route?key=SyKXK8zuuTZUrUsQKLNwGEVHqWzTthQB"

escapeForURI :: String -> String
escapeForURI = escapeURIString (\c -> isAllowedInURI c)

joinURIParams :: [String] -> String
joinURIParams = intercalate "&"

{- | Download a URL.  (Left errorMessage) if an error,
(Right doc) if success.
Based off http://book.realworldhaskell.org/read/extended-example-web-client-programming.html-}
downloadURL :: String -> IO (Either String String)
downloadURL url =
    do resp <- simpleHTTP $ getRequest $ show $ fromJust $ parseURI url
       case resp of
         Left x -> return $ Left ("Error connecting: " ++ show x)
         Right r -> 
             case rspCode r of
               (2,_,_) -> return $ Right (rspBody r)
               _ -> return $ Left (show r)

