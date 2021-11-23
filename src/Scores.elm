module Scores exposing (Score(..), empty, update, Msg(..), ScoreMember, toArrow, toIcon, toCross, toMap, equals, zscores, ChartItem, encode, decode, BuildingMember, toCsv)
import UNAPI as API exposing (DataUN)
import Dict exposing (Dict)
import RunningStat exposing (..)
import Json.Encode as JE
import Json.Decode as JD
import Csv.Encode as CSV

-- Scores
type Score = Score (List ScoreMember)

type alias ChartItem =
    { id: String
    , score: Float
    }

type Msg 
    = Factor Int ScoreMember
    | Direction ScoreMember
    | LastOnly ScoreMember
    | Delete ScoreMember
    | AddSerie BuildingMember (List DataUN)

empty : Score
empty = Score []

update : Msg -> Score -> Score
update msg (Score list) =
    case msg of
        Factor n target -> updateMembers target (changeFactor n) list
        Direction target -> updateMembers target changeDirection list
        LastOnly target -> updateMembers target changeLastOnly list
        Delete target -> Score <| List.filter (\s -> s /= target) list
        AddSerie builder values -> 
            Score <| ((member builder values) :: list)



updateMembers : ScoreMember -> (ScoreMember -> ScoreMember) -> (List ScoreMember) -> Score
updateMembers target fun list =
    Score <| List.map (\s -> (checkEqualityAndChange s target fun)) list

checkEqualityAndChange : ScoreMember -> ScoreMember -> (ScoreMember -> ScoreMember) -> ScoreMember
checkEqualityAndChange serie target fun =
    if equals serie target then
        fun serie
    else 
        serie

equals : ScoreMember -> ScoreMember -> Bool
equals lhs rhs = lhs.id == rhs.id

-- SelectedSerie

type alias ScoreMember =
    { id: String
    , config: BuildingMember
    , values: (List DataUN)
    }



member : BuildingMember -> (List DataUN) -> ScoreMember
member builder values =
    let
        id = makeUniqueId builder.serie builder.dimensions
    in 
        ScoreMember id builder values

makeUniqueId : String -> Dict String String -> String
makeUniqueId serie dimensions =
    let dimension = Dict.foldl (\k v s -> s ++ "." ++ v) "" dimensions
    in serie ++ dimension


changeDirection : ScoreMember -> ScoreMember
changeDirection ({config} as scoreMember) = 
    { scoreMember 
    | config = { config | direction = not config.direction }}

changeLastOnly : ScoreMember -> ScoreMember
changeLastOnly ({config} as scoreMember) = 
    { scoreMember 
    | config = {config | lastOnly = not config.lastOnly }}

toArrow : ScoreMember -> String 
toArrow serie =
    case serie.config.direction of
        True -> String.fromChar (Char.fromCode 0x1F4C8)
        False -> String.fromChar (Char.fromCode 0x1F4C9)

toIcon : ScoreMember -> String
toIcon serie =
    case serie.config.lastOnly of
        True -> String.fromChar (Char.fromCode 0x1F3C1)
        False -> String.fromChar (Char.fromCode 0x1F680)

toCross : String
toCross = String.fromChar (Char.fromCode 0x274C)


toMap : String
toMap = String.fromChar (Char.fromCode 0x1F5FA)

changeFactor : Int -> ScoreMember -> ScoreMember
changeFactor change ({config} as scoreMember) = 
    { scoreMember 
    | config = {config | factor = (max 1 (config.factor + change)) }}


--Compute Zscores
zscores : Dict Int String -> Score -> List ChartItem
zscores countries (Score list)  =
    let
        factor = toFloat <| List.foldl (\x s -> s + x.config.factor) 0 list 
        results = 
            List.foldl insertIntoDict Dict.empty list
            |> Dict.map (\_ value -> value / factor)
        wellFormated = 
            Dict.merge onlyValue bothValueCountries onlyCountries results countries Dict.empty 
    in 
        wellFormated 
        |> Dict.toList
        |> List.map (\(first, second) -> ChartItem first second)

 
onlyValue : Int -> Float -> (Dict String Float) -> (Dict String Float)
onlyValue code value dict = dict

onlyCountries : Int -> String -> (Dict String Float) -> (Dict String Float)
onlyCountries code name dict = dict

bothValueCountries : Int -> Float -> String -> (Dict String Float) -> (Dict String Float)
bothValueCountries code value name dict = Dict.insert name value dict

insertIntoDict : ScoreMember -> (Dict Int Float) -> (Dict Int Float)
insertIntoDict serie dict =
    Dict.foldl addValueIfFound dict <| (memberZscore serie)

addValueIfFound : Int -> Float -> (Dict Int Float) -> (Dict Int Float)
addValueIfFound key value dict =
    case Dict.get key dict of
        Nothing -> Dict.insert key value dict
        Just old -> Dict.insert key (value + old) dict



memberZscore : ScoreMember -> Dict Int Float
memberZscore serie =
    let
        lasts = getLastByCountry serie.values
        stat = case serie.config.lastOnly of
            True -> computeStat lasts
            False -> computeStat serie.values
        sens = case serie.config.direction of 
            True -> 1
            False -> -1
        factor = serie.config.factor * sens
    in
        normalized lasts stat factor

computeStat : (List DataUN) -> RunningStat
computeStat list = List.foldl testMaybeFloat RunningStat.empty list

testMaybeFloat : DataUN -> RunningStat -> RunningStat
testMaybeFloat data stat =
    case data.obs_value of
        Nothing -> stat
        Just x -> RunningStat.push x stat

getLastByCountry : (List DataUN) -> (List DataUN)
getLastByCountry list =
    Dict.values <| List.foldl addIfMoreRecent Dict.empty list


addIfMoreRecent : DataUN -> (Dict Int DataUN) -> (Dict Int DataUN)
addIfMoreRecent data dict =
    case Dict.get data.ref_aera dict of
        Nothing -> Dict.insert data.ref_aera data dict
        Just previous -> 
            case data.obs_value of
                Nothing -> dict
                Just new -> if previous.time_period > data.time_period then
                                dict
                            else
                                Dict.insert data.ref_aera data dict


normalized : (List DataUN) -> RunningStat -> Int -> Dict Int Float
normalized datas stat factor =
    datas
    |> List.filterMap (normalizeOnlyJust stat factor)
    |> Dict.fromList  

normalizeOnlyJust : RunningStat -> Int -> DataUN -> Maybe (Int, Float)
normalizeOnlyJust stat factor data =
    case data.obs_value of
        Nothing -> Nothing
        Just x -> Just (data.ref_aera, (RunningStat.normalize x stat) * (toFloat factor))



-- JSON Encoding
encode : Score -> String
encode (Score list) =
    list
    |> List.map (\x -> x.config)
    |> JE.list encodeMember
    |> JE.encode 2 


encodeMember : BuildingMember -> JE.Value
encodeMember serie = 
    JE.object 
        [ ( "serie", JE.string serie.serie)
        , ( "dimensions", (JE.dict identity JE.string serie.dimensions))
        , ( "direction", JE.bool serie.direction)
        , ( "lastOnly", JE.bool serie.lastOnly)
        , ( "factor", JE.int serie.factor)
        , ( "description", JE.string serie.description)
        ]

-- JSON Decoding
decode : String -> Result JD.Error (List BuildingMember)
decode string =
    JD.decodeString (JD.list decodeMember) string

type alias BuildingMember =
    { serie: String
    , dimensions: Dict String String
    , direction: Bool
    , lastOnly: Bool
    , factor: Int
    , description: String
    }

decodeMember : JD.Decoder BuildingMember
decodeMember =
    JD.map6 BuildingMember
        (JD.field "serie" JD.string)
        (JD.field "dimensions" (JD.dict JD.string))
        (JD.field "direction" JD.bool)
        (JD.field "lastOnly" JD.bool)
        (JD.field "factor" JD.int)
        (JD.field "description" JD.string)


-- CSV Encoding
toCsv : Dict Int String -> Score -> String
toCsv countries score =
    zscores countries score
    |> List.map (\x -> (x.id, x.score))
    |> CSV.encode
        { encoder =
            CSV.withFieldNames
                (\(id, value) ->
                    [ ("Pays", id)
                    , ("Score", String.fromFloat value)
                    ]
                )
        , fieldSeparator = ','
        }