port module Main exposing (..)
import Browser
import Http
import Html exposing (..)
import Html.Attributes exposing (id, class, src, hidden, style, value, placeholder, disabled, selected)
import Html.Events exposing (..)
import UNAPI as API exposing (Country, Goal, Target, Indicator, Serie, Dimension, DimensionCode, Return(..))
import Scores as SS exposing (ScoreMember, Score)
import Dict exposing (Dict)
import Tuple exposing (..)
import File exposing (File)
import File.Download as FD
import File.Select as FS
import Task
import Json.Decode as JD

main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }

-- PORTS
port updateChartData : List SS.ChartItem -> Cmd msg


type alias Model =
    { data: DataFromAPI
    , countries: Dict Int String  
    , path: List API.SearchCmd
    , isLoading: Bool
    , dimensions: Dict String String
    , score: Score
    , graphed: Maybe ScoreMember
    }

type DataFromAPI
    = FetchedDescription API.Return
    | ErrorDescription String
    | ErrorParsingCSV String
    | ErrorParsingJson String
    | ErrorInPath 

type SavingChoice 
    = SaveResults
    | SaveScores

init : () -> (Model, Cmd Msg)
init _ = 
    let 
        model = Model (ErrorParsingCSV "") Dict.empty [] True Dict.empty SS.empty Nothing
    in 
        update (ClickPath API.AllCountries) model

-- UPDATE

type Msg
    = ClickPath API.SearchCmd
    | Fetching API.SearchCmd
    | GotDataFromAPI API.Msg
    | AddDimensions Dimension DimensionCode
    | AddSerie SS.BuildingMember
    | ChangeDirection ScoreMember
    | ChangeMomentum ScoreMember
    | DeleteSelected ScoreMember
    | ChangeFactor Int ScoreMember
    | ShowMap (Maybe ScoreMember)
    | Saving String
    | LoadScore
    | JsonLoaded File
    | JsonRead String



update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ClickPath searchCmd -> update (Fetching searchCmd) { model | path = managePath searchCmd model.path, dimensions = Dict.empty }
        Fetching searchCmd -> ( { model | isLoading = True }, Cmd.map GotDataFromAPI (API.queryDB searchCmd))
        GotDataFromAPI apiMsg -> 
            case apiMsg of
                API.Description result ->
                    case result of
                        Ok return -> 
                            case return of
                                API.Countries countries -> update (ClickPath API.AllGoals) { model | countries = makeCountryDict countries }
                                _ -> update (ShowMap Nothing) {model | data = FetchedDescription return, isLoading= False}
                        Err err -> ({model | data = ErrorDescription (errorToString err), isLoading = False }, Cmd.none)
                API.Data serie dimensions direction lastOnly factor description result -> 
                    case result of
                        Ok csv -> 
                            let builder = SS.BuildingMember serie dimensions direction lastOnly factor description
                            in manageCSV builder model csv
                        Err err -> ({model | data = ErrorDescription (errorToString err), isLoading = False }, Cmd.none)
                
        AddDimensions dimension code ->
            ({ model | dimensions = manageDimensions dimension.id code.sdmx model.dimensions }, Cmd.none)
        AddSerie builder ->
            update (Fetching (fromBMtoSC builder)) model
        ChangeDirection selectedSerie ->
            update (ShowMap model.graphed) { model | score = SS.update (SS.Direction selectedSerie) model.score }
        ChangeMomentum selectedSerie ->
            update (ShowMap model.graphed) { model | score = SS.update (SS.LastOnly selectedSerie) model.score }
        DeleteSelected selectedSerie ->
            update (ShowMap model.graphed) { model | score = SS.update (SS.Delete selectedSerie) model.score }
        ChangeFactor n selectedSerie ->
            update (ShowMap model.graphed) { model | score = SS.update (SS.Factor n selectedSerie) model.score }
        ShowMap mScore -> 
            let
                score =
                    case mScore of
                        Nothing -> model.score
                        Just serie -> (SS.Score <| [serie])
            in 
                ({ model | graphed = mScore } , updateChartData (SS.zscores model.countries score))
        Saving choice ->
            case choice of
                "Results" -> (model, downloadResults model.countries model.score)
                "Scores" -> (model, downloadScore model.score)
                _ -> (model, Cmd.none)
        LoadScore -> ( {model | score = SS.empty }, uploadScore)
        JsonLoaded file ->
            (model, Task.perform JsonRead (File.toString file))
        JsonRead file ->
            case SS.decode file of
                Err err -> ({ model | data = ErrorParsingJson (JD.errorToString err)}, Cmd.none)
                Ok list -> 
                    let 
                        list_cmd = 
                            list
                            |> List.map fromBMtoSC 
                            |> List.map (\sc -> Cmd.map GotDataFromAPI (API.queryDB sc))
                    in (model, Cmd.batch list_cmd)


fromBMtoSC : SS.BuildingMember -> API.SearchCmd
fromBMtoSC builder = 
    API.DataFrom builder.serie builder.dimensions builder.direction builder.lastOnly builder.factor builder.description


downloadScore : SS.Score -> Cmd Msg
downloadScore score =
    FD.string "score.json" "application/json" (SS.encode score)

downloadResults :  Dict Int String -> SS.Score -> Cmd Msg
downloadResults countries score =
    FD.string "results.csv" "text/csv" (SS.toCsv countries score)

uploadScore : Cmd Msg
uploadScore =
    FS.file ["application/json"] JsonLoaded


addMember : Model -> (List SS.BuildingMember) -> Model
addMember model list =
    case list of
        [] -> model
        x :: xs -> 
            let 
                (new_model, _) = update (AddSerie x) model
            in
                addMember new_model xs


makeCountryDict : (List Country) -> Dict Int String
makeCountryDict countries =
    countries
        |> List.filterMap (\country -> 
            case country.code of
                Nothing -> Nothing
                Just code -> Just (code, country.alpha)
            )
        |> Dict.fromList



manageCSV : SS.BuildingMember -> Model -> String -> (Model, Cmd Msg)
manageCSV builder model csv=
    case API.dataDecoder csv of
        API.CsvParsed list ->
            update (ShowMap model.graphed) { model | score = SS.update (SS.AddSerie builder list) model.score, isLoading = False}
        API.ErrParsingCsv err -> ({model | data = ErrorParsingCSV err, isLoading = False }, Cmd.none)


retrieveSerieFromPath : Model -> Maybe Serie 
retrieveSerieFromPath model =
    case model.path of
        [] -> Nothing
        x :: _ -> 
            case x of
                API.DimensionsFrom serie -> Just serie
                _ -> Nothing



manageDimensions : String -> String -> Dict String String -> Dict String String
manageDimensions dimension code dict =
    case Dict.get dimension dict of
        Nothing -> Dict.insert dimension code dict
        Just s -> 
            if s == code then
                Dict.remove dimension dict
            else
                Dict.insert dimension code dict

      

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW

view : Model -> Browser.Document Msg
view model =
    { title = "SDG Scores"
    , body = 
        [ div [ id "chartdiv" ] []
        , div [ class "shared-params", id "choices" ] 
            [ div [ class "shared-params", class "side-panel" ] 
                [ showTopPath model
                , showLeftPanel model
                ]
            , div [ class "shared-params", class "side-panel" ] 
                [ div [ class "shared-params", class "top-header"] makeRightPanelHeader
                , showRightPanel model.score
                ]
            ]
        ]
    }


showLeftPanel : Model -> Html Msg
showLeftPanel model =
    case model.isLoading of
        True -> showState "Chargement en cours..."
        False -> 
            case model.data of
                FetchedDescription return ->
                    case return of
                        Countries _ -> div [] [ text "Erreur Affichage des pays"]
                        Goals goals -> div [] <| List.map showGoal goals
                        Targets targets -> div [] <| List.map showTarget targets
                        Indicators indicators -> div [] <| List.map showIndicator indicators
                        Series series -> div [] <| List.map showSerie series
                        Dimensions dimensions -> div [] <| showDimensions dimensions model.dimensions
                ErrorDescription s -> showState s
                ErrorParsingCSV s -> showState s
                ErrorInPath -> showState "Le chemin a changé, le csv ne peut être attribué"
                ErrorParsingJson s -> showState s


showState : String -> Html Msg
showState s =
    div [ class "shared-params", class "elements", class "api-node" ] [ text s ]   

showGoal : Goal -> Html Msg
showGoal goal = div [ class "shared-params", class "elements", class "api-node" ] [ span [] [text goal.title]
                                        , button [ class "shared-params", class "elements", class "api-button", onClick (ClickPath (API.TargetsFrom goal))] makeSearchButton]

showTarget : Target -> Html Msg
showTarget target =div [ class "shared-params", class "elements", class "api-node"] [ span [] [text target.title]
                                        , button [ class "shared-params", class "elements", class "api-button", onClick (ClickPath (API.IndicatorsFrom target))] makeSearchButton]

showIndicator : Indicator -> Html Msg
showIndicator indicator = div [class "shared-params", class "elements", class "api-node"] [ span [] [text indicator.description]
                                        , button [ class "shared-params", class "elements", class "api-button", onClick (ClickPath (API.SeriesFrom indicator))] makeSearchButton]

showSerie : Serie -> Html Msg
showSerie serie = div [class "shared-params", class "elements", class "api-node"] [ span [] [text serie.description]
                                        , button [ class "shared-params", class "elements", class "api-button", onClick (ClickPath (API.DimensionsFrom serie))] makeSearchButton]

makeSearchButton : List (Html Msg)
makeSearchButton = [ text (String.fromChar (Char.fromCode 0x1f50e))]

showDimensions : List Dimension -> Dict String String -> List (Html Msg)
showDimensions dimensions dict =
    let 
        alreadySelected = List.map (\dim -> (pair dim (Dict.get dim.id dict))) dimensions
    in 
        List.map (\t -> showOneDimension t) alreadySelected

showOneDimension : (Dimension, Maybe String) -> Html Msg
showOneDimension (dim, ms) = div [] [ span [class "shared-params", class "elements", class "api-node"] [text dim.id]
                                    , div [class "shared-params", class "elements", class "dimension-choices"] <| List.map (\code -> (showOneDimensionCode dim ms code)) dim.codes ]
    
showOneDimensionCode : Dimension -> Maybe String -> DimensionCode -> Html Msg
showOneDimensionCode dim ms code =
    let 
        buttonClass =
            case ms of
                Nothing -> "dimension"
                Just s ->
                    if code.sdmx == s then
                        "selected-dimension"
                    else
                        "dimension"
    in 
        button  [ class "shared-params", class "elements", class buttonClass
                , onClick (AddDimensions dim code)] [text code.description ]

-- Right Panel

showRightPanel : Score -> Html Msg
showRightPanel (SS.Score list) =
    div [] <| List.map (\serie -> showOneSelectedSerie serie) list

showOneSelectedSerie : ScoreMember -> Html Msg
showOneSelectedSerie serie =
    div [ class "shared-params", class "elements", class "api-node" ]
        [ div [ class "shared-params", class "elements", class "serie-preview" ] 
                [ text serie.config.description
                , div [ class "shared-params", class "elements", class "dimensions-selected"] <| List.map showDimensionOfSelectedSerie (Dict.toList serie.config.dimensions)
                ]
        , showButtonsSelected serie
        ]

showButtonsSelected : ScoreMember -> Html Msg
showButtonsSelected serie =
    div [ class "shared-params", class "elements", class "factors" ] 
        [ showFactorButton serie
        , showUpAndDownButton serie
        , showLevelSlopeButton serie
        , showMapButton serie
        , showCrossButton serie]

showFactorButton : ScoreMember -> Html Msg
showFactorButton serie =
    div [ class "shared-params", class "elements", class "factors" ] 
        [ button [ class "shared-params", class "elements", class "dimension", onClick (ChangeFactor -1 serie) ] [ text "-"]
        , text (String.fromInt serie.config.factor)
        , button [ class "shared-params", class "elements", class "dimension", onClick (ChangeFactor 1 serie) ] [ text "+"]
        ]


showUpAndDownButton : ScoreMember -> Html Msg
showUpAndDownButton serie =
    button [ class "shared-params", class "elements", class "dimension", onClick (ChangeDirection serie) ] [ text (SS.toArrow serie) ]

showLevelSlopeButton : ScoreMember -> Html Msg
showLevelSlopeButton serie =
    button [ class "shared-params", class "elements", class "dimension", onClick (ChangeMomentum serie) ] [ text (SS.toIcon serie) ]

showMapButton : ScoreMember -> Html Msg
showMapButton serie =
    button [ class "shared-params", class "elements", class "dimension", onMouseOver (ShowMap (Just serie)), onMouseLeave (ShowMap Nothing) ] [ text SS.toMap ]

showCrossButton : ScoreMember -> Html Msg
showCrossButton serie =
    button [ class "shared-params", class "elements", class "dimension", onClick (DeleteSelected serie) ] [ text SS.toCross ]




showDimensionOfSelectedSerie : (String, String) -> Html Msg
showDimensionOfSelectedSerie (first, second) = div [ class "shared-params", class "elements", class "dimension" ] [ text (first ++ ": " ++ second) ]

makeOneDimensionSelected : String -> String -> Html Msg
makeOneDimensionSelected dimension code = div [ class "shared-params", class "elements", class "dimension" ] [ text code ]


makeRightPanelHeader : List (Html Msg)
makeRightPanelHeader =
    [ button [ class "shared-params", class "elements", class "api-node", (onClick (LoadScore))] [text (String.fromChar (Char.fromCode 0x1F4C2))]
    , div [ class "shared-params", class "elements", class "titleScore" ] [ text "Score" ]
    , select [ class "shared-params", class "elements", class "api-node", placeholder "Save", onInput Saving] 
        [ option [value "", disabled True, selected True, hidden True] [ text (String.fromChar (Char.fromCode 0x1F4BE))] 
        , option [ value "Scores" ] [ text "Scores"]
        , option [ value "Results" ] [ text "Results"]
        ]
    ]


 --PATH
showTopPath : Model -> Html Msg
showTopPath model =
    let 
        backButtons = List.map pathButton model.path
        addSerieButton =
            case model.path of
                head :: _ ->
                    case head of
                        API.DimensionsFrom serie ->
                            let builder = SS.BuildingMember serie.code model.dimensions True True 1 serie.description
                            in
                            button [ class "shared-params", class "elements", class "api-node", onClick (AddSerie builder)] [ text "Add Serie ->" ]
                        _ -> div [] []
                [] -> div [] []
    in  
        div [ class "shared-params", class "top-header"] 
            [ div [ class "shared-params", id "path" ] <| List.reverse <| backButtons
            , addSerieButton
            ]

pathButton : API.SearchCmd -> Html Msg
pathButton searchCmd = 
    let pathText = case searchCmd of
                        API.AllGoals -> "..."
                        API.TargetsFrom g -> "Goal " ++ g.code
                        API.IndicatorsFrom t -> "Target " ++ t.code
                        API.SeriesFrom i -> "Indicator " ++ i.code
                        API.DimensionsFrom s -> "Serie " ++ s.code
                        _ -> "Erreur Data"
    in 
        button [ class "shared-params", class "elements", class "api-node", onClick (ClickPath searchCmd) ] [ text pathText ]


managePath : API.SearchCmd -> List API.SearchCmd -> List API.SearchCmd
managePath new_path list =
    -- Remove all paths smalller or equal
    case new_path of
        API.AllCountries -> list
        API.DataFrom _ _ _ _ _ _-> list
        _ -> 
            let onlyGreater searchCmd = pathIsBigger new_path searchCmd
            in 
                new_path :: (List.filter onlyGreater list)

pathIsBigger : API.SearchCmd -> API.SearchCmd -> Bool
pathIsBigger lhs rhs = 
    case lhs of
        API.AllGoals -> False
        API.TargetsFrom _ -> 
            case rhs of
                API.AllGoals -> True
                _ -> False
        API.IndicatorsFrom _ -> 
            case rhs of 
                API.AllGoals -> True
                API.TargetsFrom _ -> True
                _ -> False
        API.SeriesFrom _ ->
            case rhs of
                API.SeriesFrom _ -> False
                API.DimensionsFrom _ -> False
                _ -> True
        API.DimensionsFrom _ -> 
            case rhs of 
                API.DimensionsFrom _ -> False
                _ -> True
        API.DataFrom _ _ _ _ _ _ -> True
        API.AllCountries -> True



errorToString : Http.Error -> String
errorToString error =
    case error of
        Http.BadUrl url ->
            "The URL " ++ url ++ " was invalid"
        Http.Timeout ->
            "Unable to reach the server, try again"
        Http.NetworkError ->
            "Unable to reach the server, check your network connection"
        Http.BadStatus 500 ->
            "The server had a problem, try again later"
        Http.BadStatus 400 ->
            "Verify your information and try again"
        Http.BadStatus _ ->
            "Unknown error"
        Http.BadBody errorMessage ->
            errorMessage
 

