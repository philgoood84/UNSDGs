module UNAPI exposing (SearchCmd(..), Msg(..), Return(..), queryDB, Country, Goal, Target, Indicator, Serie, Dimension, DimensionCode, dataDecoder, DataUN, CsvParsing(..))
import Http
import Json.Decode as JD
import Json.Decode.Extra as JDE
import Dict exposing (Dict)
import Csv.Decode as CSV

type SearchCmd
    = AllCountries
    | AllGoals
    | TargetsFrom Goal
    | IndicatorsFrom Target
    | SeriesFrom Indicator
    | DimensionsFrom Serie
    | DataFrom String (Dict String String) Bool Bool Int String

type Msg 
    = Description (Result Http.Error Return)
    | Data String (Dict String String) Bool Bool Int String (Result Http.Error String)
    

type Return
    = Countries (List Country)
    | Goals (List Goal)
    | Targets (List Target)
    | Indicators (List Indicator)
    | Series (List Serie)
    | Dimensions (List Dimension)

-- Data Getter
queryDB : SearchCmd -> Cmd Msg
queryDB searchCmd =
    let url = makeUrl searchCmd
    in
        case searchCmd of
            AllCountries -> Http.get 
                        { url = url
                        , expect = Http.expectJson Description countriesDecoder
                        }
            AllGoals -> Http.get 
                        { url = url
                        , expect = Http.expectJson Description goalsDecoder
                        } 
            TargetsFrom goal -> Http.get    
                        { url = url
                        , expect = Http.expectJson Description targetsDecoder
                        }
            IndicatorsFrom target -> Http.get
                        { url = url
                        , expect = Http.expectJson Description indicatorsDecoder
                        }
            SeriesFrom indicator -> Http.get
                        { url = url
                        , expect = Http.expectJson Description seriesDecoder
                        }
            DimensionsFrom serie -> Http.get
                        { url = url
                        , expect = Http.expectJson Description dimensionsDecoder
                        }
            DataFrom serie dimensions direction lastOnly factor description -> Http.get
                        { url = url
                        , expect = Http.expectString (Data serie dimensions direction lastOnly factor description)
                        }


checkDimensionEqualT : String -> String -> Bool
checkDimensionEqualT _ value = value /= "_T"


makeUrl : SearchCmd -> String
makeUrl searchCmd =
    case searchCmd of
        AllCountries -> "https://restcountries.com/v3.1/all"
        AllGoals -> "https://unstats.un.org/SDGAPI/v1/sdg/Goal/List?includechildren=false"
        TargetsFrom goal -> "https://unstats.un.org/SDGAPI/v1/sdg/Goal/" ++ goal.code ++ "/Target/List?includechildren=true"
        IndicatorsFrom target -> "https://unstats.un.org/SDGAPI/v1/sdg/Target/" ++ target.code ++ "/Indicator/List?includechildren=true"
        SeriesFrom indicator -> "https://unstats.un.org/SDGAPI/v1/sdg/Indicator/" ++ indicator.code ++ "/Series/List"
        DimensionsFrom serie -> "https://unstats.un.org/SDGAPI/v1/sdg/Series/" ++ serie.code ++ "/Dimensions"
        DataFrom serie dimensions _ _ _ _-> makeDataUrl serie dimensions

makeDataUrl : String -> (Dict String String) -> String
makeDataUrl serie dimensions =
    let 
        baseUrl = "https://data.un.org/ws/rest/data/IAEG-SDGs,DF_SDG_GLH,1.6/"
        dimensions_cleaned = Dict.filter checkDimensionEqualT dimensions
        structureDimensions = dimensionOptions serie dimensions_cleaned
        periodStart = "/ALL/?startPeriod=2015"
        formatOption = "&format=csv"
    in 
        baseUrl ++ structureDimensions ++ periodStart ++ formatOption

dimensionOptions : String -> (Dict String String) -> String
dimensionOptions serie dimensions =
    ""
    |> (++) (addDimensionToPath "PRODUCT" dimensions)
    |> (++) (addDimensionToPath "ACTIVITY" dimensions)
    |> (++) (addDimensionToPath "DISABILITY_STATUS" dimensions)
    |> (++) (addDimensionToPath "COMPOSITE_BREAKDOWN" dimensions)
    |> (++) (addDimensionToPath "CUST_BREAKDOWN" dimensions)
    |> (++) (addDimensionToPath "OCCUPATION" dimensions)
    |> (++) (addDimensionToPath "EDUCATION_LEV" dimensions)
    |> (++) (addDimensionToPath "INCOME_WEALTH_QUANTILE" dimensions)
    |> (++) (addDimensionToPath "URBANISATION" dimensions)
    |> (++) (addDimensionToPath "AGE" dimensions)
    |> (++) (addDimensionToPath "SEX" dimensions)
    |> (++) (addDimensionToPath "REF_AREA" dimensions)
    |> (++) (String.toUpper serie)
    |> (++) (addDimensionToPath "REPORTING_TYPE" dimensions)
    |> (++) (addDimensionToPath "FREQ" dimensions)
    
    

addDimensionToPath : String -> Dict String String -> String
addDimensionToPath id dimensions =
    case Dict.get (String.toUpper id) dimensions of
        Nothing -> "."
        Just dimension -> (String.toUpper dimension)



-- JSON Decoders
--Countries
type alias Country =
    { name: String
    , alpha: String
    , code: Maybe Int
    }

countryDecoder : JD.Decoder Country
countryDecoder =
    JD.map3 Country 
        (JD.field "name" (JD.field "common" JD.string))
        (JD.field "cca2" JD.string)
        (JD.maybe (JD.field "ccn3" JDE.parseInt))
        

countriesDecoder : JD.Decoder Return
countriesDecoder =
    JD.list countryDecoder
    |> JD.andThen countriesToReturns

countriesToReturns : (List Country) -> JD.Decoder Return
countriesToReturns countries = JD.succeed (Countries countries)

-- Goals
type alias Goal =
    { code: String
    , title: String
    , description: String
    , uri: String
    }

goalDecoder : JD.Decoder Goal
goalDecoder = 
    JD.map4 Goal
         (JD.field "code" JD.string) 
         (JD.field "title" JD.string) 
         (JD.field "description" JD.string) 
         (JD.field "uri" JD.string)
    

goalsDecoder : JD.Decoder Return
goalsDecoder =
    JD.list goalDecoder
    |> JD.andThen goalsToReturns

goalsToReturns : (List Goal) -> JD.Decoder Return
goalsToReturns goals = JD.succeed (Goals goals)


-- Targets
type alias Target =
    { code: String
    , title: String
    , description: String
    , uri: String
    }

targetDecoder : JD.Decoder Target
targetDecoder =
    JD.map4 Target
        (JD.field "code" JD.string)
        (JD.field "title" JD.string)
        (JD.field "description" JD.string)
        (JD.field "uri" JD.string)
    
targetsDecoder : JD.Decoder Return
targetsDecoder =
    JD.index 0 (JD.field "targets" (JD.list targetDecoder))
    |> JD.andThen targetsToReturns
        
targetsToReturns : (List Target) -> JD.Decoder Return
targetsToReturns targets = JD.succeed (Targets targets)


--Indicators

type alias Indicator =
    { code: String
    , description: String
    , tier: String
    , uri: String
    }

indicatorDecoder : JD.Decoder Indicator
indicatorDecoder =
    JD.map4 Indicator
        (JD.field "code" JD.string)
        (JD.field "description" JD.string)
        (JD.field "tier" JD.string)
        (JD.field "uri" JD.string)

indicatorsDecoder : JD.Decoder Return
indicatorsDecoder =
    JD.index 0 (JD.field "indicators" (JD.list indicatorDecoder))
    |> JD.andThen indicatorsToReturn

indicatorsToReturn : (List Indicator) -> JD.Decoder Return
indicatorsToReturn indicators = JD.succeed (Indicators indicators)


--Series

type alias Serie =
    { release: String
    , code: String
    , description: String
    , uri: String
    }

serieDecoder : JD.Decoder Serie
serieDecoder =
    JD.map4 Serie
        (JD.field "release" JD.string)
        (JD.field "code" JD.string)
        (JD.field "description" JD.string)
        (JD.field "uri" JD.string)

seriesDecoder : JD.Decoder Return
seriesDecoder =
    JD.index 0 (JD.field "series" ( JD.list serieDecoder ))
    |> JD.andThen seriesToReturns


seriesToReturns : (List Serie) -> JD.Decoder Return
seriesToReturns series = JD.succeed (Series series)


--Dimensions
type alias Dimension = 
    { id: String
    , codes: List DimensionCode
    }
type alias DimensionCode =
    { code: String
    , description: String
    , sdmx: String
    }

dimensionCodeDecoder : JD.Decoder DimensionCode
dimensionCodeDecoder =
    JD.map3 DimensionCode
        (JD.field "code" JD.string)
        (JD.field "description" JD.string)
        (JD.field "sdmx" JD.string)

dimensionDecoder : JD.Decoder Dimension
dimensionDecoder =
    JD.map2 Dimension
        (JD.field "id" JD.string)
        (JD.field "codes" (JD.list dimensionCodeDecoder))

dimensionsDecoder : JD.Decoder Return
dimensionsDecoder = 
    JD.list dimensionDecoder
    |> JD.andThen dimensionsToReturns

dimensionsToReturns : (List Dimension) -> JD.Decoder Return
dimensionsToReturns dimensions = JD.succeed (Dimensions dimensions)



--Data
type alias DataUN =
    { ref_aera: Int
    , time_period: Int
    , obs_value: Maybe Float
    }


dataDecoderHelper : CSV.Decoder DataUN
dataDecoderHelper =
    CSV.into DataUN
        |> CSV.pipeline (CSV.field "REF_AREA" CSV.int)
        |> CSV.pipeline (CSV.field "TIME_PERIOD" CSV.int)
        |> CSV.pipeline (CSV.field "OBS_VALUE" (CSV.blank CSV.float))

type CsvParsing
    = CsvParsed (List DataUN)
    | ErrParsingCsv String

dataDecoder : String -> CsvParsing
dataDecoder csv =
    case CSV.decodeCsv CSV.FieldNamesFromFirstRow dataDecoderHelper csv of
        Ok l -> CsvParsed l
        Err err -> ErrParsingCsv (CSV.errorToString err)



