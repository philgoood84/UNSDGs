module Scores exposing (Score(..), empty, update, Msg(..), ScoreMember, toArrow, toIcon, toCross, toMap, equals, zscores, ChartItem)
import UNAPI as API exposing (DataUN, Serie)
import Dict exposing (Dict)
import RunningStat exposing (..)

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
    | AddSerie (Serie, Dict String String) (List DataUN)



empty : Score
empty = Score []



update : Msg -> Score -> Score
update msg (Score list) =
    case msg of
        Factor n target -> updateMembers target (changeFactor n) list
        Direction target -> updateMembers target changeDirection list
        LastOnly target -> updateMembers target changeLastOnly list
        Delete target -> Score <| List.filter (\s -> s /= target) list
        AddSerie (serie, dimensions) values -> 
            Score <| ((member serie dimensions values) :: list)



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
    , serie: String
    , dimensions: List (String, String)
    , direction: Bool
    , lastOnly: Bool
    , factor: Int
    , values: (List DataUN)
    }

member : Serie -> Dict String String -> (List DataUN) -> ScoreMember
member serie dimensions values =
    let
        id = makeUniqueId serie dimensions
    in 
        ScoreMember id serie.description (Dict.toList dimensions) True True 1 values

makeUniqueId : Serie -> Dict String String -> String
makeUniqueId serie dimensions =
    let dimension = Dict.foldl (\k v s -> s ++ "." ++ v) "" dimensions
    in serie.code ++ dimension

changeDirection : ScoreMember -> ScoreMember
changeDirection serie = { serie | direction = not serie.direction }

changeLastOnly : ScoreMember -> ScoreMember
changeLastOnly serie = { serie | lastOnly = not serie.lastOnly }

toArrow : ScoreMember -> String 
toArrow serie =
    case serie.direction of
        True -> String.fromChar (Char.fromCode 0x1F4C8)
        False -> String.fromChar (Char.fromCode 0x1F4C9)

toIcon : ScoreMember -> String
toIcon serie =
    case serie.lastOnly of
        True -> String.fromChar (Char.fromCode 0x1F3C1)
        False -> String.fromChar (Char.fromCode 0x1F680)

toCross : String
toCross = String.fromChar (Char.fromCode 0x274C)


toMap : String
toMap = String.fromChar (Char.fromCode 0x1F5FA)

changeFactor : Int -> ScoreMember -> ScoreMember
changeFactor change serie = { serie | factor = (max 1 (serie.factor + change)) }


--Compute Zscores
zscores : Score -> Dict Int String -> List ChartItem
zscores (Score list) countries =
    let
        factor = toFloat <| List.foldl (\x s -> s + x.factor) 0 list
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
        stat = case serie.lastOnly of
            True -> computeStat lasts
            False -> computeStat serie.values
        sens = case serie.direction of 
            True -> 1
            False -> -1
        factor = serie.factor * sens
    in
        normalized lasts stat factor

computeStat : (List DataUN) -> RunningStat
computeStat list = List.foldl (\x stat -> (RunningStat.push x.obs_value stat)) RunningStat.empty list


getLastByCountry : (List DataUN) -> (List DataUN)
getLastByCountry list =
    Dict.values <| List.foldl addIfMoreRecent Dict.empty list


addIfMoreRecent : DataUN -> (Dict Int DataUN) -> (Dict Int DataUN)
addIfMoreRecent data dict =
    case Dict.get data.ref_aera dict of
        Nothing -> Dict.insert data.ref_aera data dict
        Just previous -> 
            if previous.time_period > data.time_period then
                dict
            else
                Dict.insert data.ref_aera data dict


normalized : (List DataUN) -> RunningStat -> Int -> Dict Int Float
normalized datas stat factor =
    datas
    |> List.map (\data -> (data.ref_aera, (RunningStat.normalize data.obs_value stat) * (toFloat factor)))
    |> Dict.fromList  

