module ApiUN exposing (..)
import Json.Decode as JD exposing (Decoder, field, int , string, list, map4)
-- API

type SearchCmd 
    = NoneSearched 
    | GoalSearched String
    | TargetSearched String
    | IndicatorSearched String
    | SerieSearched String


type APIResultsWrapper
    = APIGoal Goal
    | APITarget Target
    | APIIndicator Indicator
    | APISerie Serie
    | APIDimension Dimension



resultsToSearchCmd : APIResultsWrapper -> SearchCmd
resultsToSearchCmd wrapper =
    case wrapper of
        APIGoal goal -> GoalSearched goal.code
        APITarget target -> TargetSearched target.code
        APIIndicator indicator -> IndicatorSearched indicator.code
        APISerie serie -> SerieSearched serie.code
        APIDimension dimension -> SerieSearched dimension.id

-- Goals

type alias Goal =
    { code: String
    , title: String
    , description: String
    , uri: String
    }

goalDecoder : Decoder Goal
goalDecoder = 
    JD.map4 Goal
         (field "code" string) 
         (field "title" string) 
         (field "description" string) 
         (field "uri" string)
    

goalsDecoder : Decoder (List Goal)
goalsDecoder =
    JD.list goalDecoder
  


-- Targets
type alias Target =
    { code: String
    , title: String
    , description: String
    , uri: String
    }

targetDecoder : Decoder Target
targetDecoder =
    JD.map4 Target
        (field "code" string)
        (field "title" string)
        (field "description" string)
        (field "uri" string)
    
targetsDecoder : Decoder (List Target)
targetsDecoder =
    JD.index 0 (field "targets" (JD.list targetDecoder))
        

--Indicators

type alias Indicator =
    { code: String
    , description: String
    , tier: String
    , uri: String
    }

indicatorDecoder : Decoder Indicator
indicatorDecoder =
    JD.map4 Indicator
        (field "code" string)
        (field "description" string)
        (field "tier" string)
        (field "uri" string)

indicatorsDecoder : Decoder (List Indicator)
indicatorsDecoder =
    JD.index 0 (field "indicators" (JD.list indicatorDecoder))


--Series

type alias Serie =
    { release: String
    , code: String
    , description: String
    , uri: String
    }

serieDecoder : Decoder Serie
serieDecoder =
    JD.map4 Serie
        (field "release" string)
        (field "code" string)
        (field "description" string)
        (field "uri" string)

seriesDecoder : Decoder (List Serie)
seriesDecoder =
    JD.index 0 (field "series" ( JD.list serieDecoder ))


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
dimensionCodeDecoder : Decoder DimensionCode
dimensionCodeDecoder =
    JD.map3 DimensionCode
        (field "code" string)
        (field "description" string)
        (field "sdmx" string)

dimensionDecoder : Decoder Dimension
dimensionDecoder =
    JD.map2 Dimension
        (field "id" string)
        (field "codes" (JD.list dimensionCodeDecoder))

dimensionsDecoder : Decoder (List Dimension)
dimensionsDecoder = 
    JD.list dimensionDecoder

type alias DimensionCodeAssociated =
    { dimension: Dimension
    , code: DimensionCode
    }

updateDimensions : DimensionCode -> (List DimensionCodeAssociated) -> (List APIResultsWrapper) -> (List DimensionCodeAssociated)
updateDimensions code codes wrappers =
    let 
        dimension = findDimensionAssociatedWith code wrappers
    in 
        case dimension of
            Nothing -> codes
            Just dim -> {dimension: dim, code: code} :: List.filter (/x -> x.dimension != dim) codes


findDimensionAssociatedWith : DimensionCode -> (List APIResultsWrapper) -> Maybe Dimension
findDimensionAssociatedWith code apis =
    List.head <| List.filter (/x -> (dimensionHasCode code x)) dimensions
  
filterHelper : Dimensioncode -> APIResultsWrapper -> Bool
filterHelper code wrapper = 
    case wrapper of
        APIDimension dimension -> (dimensionHasCode code dimension)
        _ -> False

dimensionHasCode : DimensionCode -> Dimension -> Bool
dimensionHasCode code dimension = List.any (/x -> x.sdmx == code.sdmx) dimension.codes

--PATH
managePath : SearchCmd -> List SearchCmd -> List SearchCmd
managePath new_path list =
    -- Remove all paths smalller or equal
    let onlyGreater searchCmd = pathIsBigger new_path searchCmd
    in 
        new_path :: (List.filter onlyGreater list)

pathIsBigger : SearchCmd -> SearchCmd -> Bool
pathIsBigger lhs rhs = 
    case lhs of
        NoneSearched -> False
        GoalSearched _ -> 
            case rhs of
                NoneSearched -> True
                _ -> False
        TargetSearched _ -> 
            case rhs of 
                NoneSearched -> True
                GoalSearched _ -> True
                _ -> False
        IndicatorSearched _ ->
            case rhs of
                IndicatorSearched _ -> False
                SerieSearched _ -> False
                _ -> True
        SerieSearched _ -> True

makeAPIUrl : SearchCmd -> String
makeAPIUrl searchCmd =
    case searchCmd of
        NoneSearched -> "https://unstats.un.org/SDGAPI/v1/sdg/Goal/List?includechildren=false"
        GoalSearched goal -> "https://unstats.un.org/SDGAPI/v1/sdg/Goal/" ++ goal ++ "/Target/List?includechildren=true"
        TargetSearched target -> "https://unstats.un.org/SDGAPI/v1/sdg/Target/" ++ target ++ "/Indicator/List?includechildren=true"
        IndicatorSearched indicator -> "https://unstats.un.org/SDGAPI/v1/sdg/Indicator/" ++ indicator ++ "/Series/List"
        SerieSearched serie -> "https://unstats.un.org/SDGAPI/v1/sdg/Series/" ++ serie ++ "/Dimensions"
