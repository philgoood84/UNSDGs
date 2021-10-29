module Main exposing (..)
import Browser
import Http
import Html exposing (..)
import Html.Attributes exposing (id, class, src, hidden, style)
import Html.Events exposing (..)
import ApiUN exposing (..)

main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }


type alias Model =
    { listAsked: List APIResultsWrapper
    , listDimension: List DimensionCodeAssociated       
    , path: List SearchCmd
    , isLoading: Bool
    , error: Maybe Http.Error
    }


init : () -> (Model, Cmd Msg)
init _ = 
    update (ClickPath NoneSearched) { listAsked = [], listDimension = [], path = [], isLoading = False, error = Nothing}

-- UPDATE

type Msg
    = ClickPath SearchCmd
    | Fetching SearchCmd
    | GotGoals (Result Http.Error (List Goal))
    | GotTargets (Result Http.Error (List Target))
    | GotIndicators (Result Http.Error (List Indicator))
    | GotSeries (Result Http.Error (List Serie))
    | GotDimensions (Result Http.Error (List Dimension))
    | ClickDimension DimensionCode


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        ClickPath searchCmd -> update (Fetching searchCmd) { model | path = managePath searchCmd model.path, listDimension = [] }
        Fetching searchCmd -> ( { model | isLoading = True, listDimension = [] }, getApiJson searchCmd )
        GotGoals result ->
            case result of
                Ok goals ->
                    ({model | listAsked = List.map (\x -> APIGoal x) goals, isLoading= False, error = Nothing }, Cmd.none)
                Err err -> 
                    ({model | listAsked = [], isLoading = False, error = Just err }, Cmd.none)
        GotTargets result ->
            case result of
                Ok targets ->
                    ({model | listAsked = List.map (\x -> APITarget x) targets, isLoading= False, error = Nothing }, Cmd.none)
                Err err -> 
                    ({model | listAsked = [], isLoading = False, error = Just err }, Cmd.none)
        GotIndicators result ->
            case result of
                Ok indicators ->
                    ({model | listAsked = List.map (\x -> APIIndicator x) indicators, isLoading = False, error = Nothing }, Cmd.none)
                Err err -> 
                    ({model | listAsked = [], isLoading = False, error = Just err }, Cmd.none)
        GotSeries result ->
            case result of
                Ok series ->
                    ({model | listAsked = List.map (\x -> APISerie x) series, isLoading = False, error = Nothing }, Cmd.none)
                Err err -> 
                    ({model | listAsked = [], isLoading = False, error = Just err }, Cmd.none)
        GotDimensions result ->
            case result of
                Ok dimensions ->
                    ({model | listAsked = List.map (\x -> APIDimension x) dimensions, isLoading = False, error = Nothing }, Cmd.none)
                Err err ->
                    ({model | listAsked = [], isLoading = False, error = Just err }, Cmd.none)
        ClickDimension dimensionCode ->
            ({ model | listDimension = updateListDimensions DimensionCode model.listDimension model.listSelected})

-- SUBSCRIPTIONS

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none



-- VIEW

view : Model -> Browser.Document Msg
view model =
    { title = "SDG Scores"
    , body = 
        [ div [ id "graphic" ] []
        , showingErrorAPI model
        , div [ id "choices" ] 
            [ div [ class "side-panel"] 
            [ showPath model.path
            , showResults model.listAsked
            ]
            , div [ class "side-panel" ] 
            [ div [ class "api-node", class "side-panel-headers"] [ text "Selected Series" ] 
            , showSelectedSerie model.listSelected ]
            ]
        ]
    }
    
showingErrorAPI : Model -> Html Msg
showingErrorAPI model =
    case model.error of
        Just err -> div [ id "Error-panel", hidden False] [text (errorToString err)]
        Nothing ->  div [ id "Error-panel", hidden True] []


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
 


showPath : (List SearchCmd) -> Html Msg
showPath path = div [id "path"] <| List.map pathButton <| List.reverse path


pathButton : SearchCmd -> Html Msg
pathButton searchCmd = 
    let pathText = case searchCmd of
                        NoneSearched -> "..."
                        GoalSearched g -> "Goal " ++ g
                        TargetSearched t -> "Target " ++ t
                        IndicatorSearched i -> "Indicator " ++ i
                        SerieSearched s -> "Serie " ++ s
    in 
        button [ class "api-node", onClick (ClickPath searchCmd) ] [ text pathText ]




showResults : (List APIResultsWrapper) -> Html Msg
showResults response = div [] <| List.map showOneResult response

showOneResult : APIResultsWrapper -> Html Msg
showOneResult wrapper = adjustResultsPresentation wrapper


adjustResultsPresentation : APIResultsWrapper -> Html Msg
adjustResultsPresentation wrapper =
    case wrapper of
        APIDimension dimension -> showDimensionResults dimension
        _ -> div [class "api-node"] [ span [] [ text (wrapperToString wrapper)]
                                    , adjustClickable wrapper ]

showDimensionResults : Dimension -> Html Msg
showDimensionResults dimension =
    div [] 
    [ span [class "api-node"] [text dimension.id]
    , div [class "dimension-choices"] <| List.map showOneDimensionCode dimension.codes  
    ]
    

showOneDimensionCode : DimensionCode -> Html Msg
showOneDimensionCode code =
    div [class "dimension"] [text code.description] 


adjustClickable : APIResultsWrapper -> Html Msg 
adjustClickable wrapper = button [ class "api-button", onClick (ClickPath (resultsToSearchCmd wrapper))] [ img [ src "./static/img/loupe.png" ] []]

wrapperToString : APIResultsWrapper -> String
wrapperToString c =
    case c of
        APIGoal g -> g.title
        APITarget t -> t.title
        APIIndicator i -> i.description
        APISerie s -> s.description
        APIDimension d -> d.id


showSelectedSerie : (List Serie) -> Html Msg
showSelectedSerie series = div [ class "results" ] <| List.map showOneSerie series

showOneSerie : Serie -> Html msg
showOneSerie serie = div [ class "description" ] [ text serie.description ]

--API
getApiJson : SearchCmd -> Cmd Msg
getApiJson searchCmd = 
    let apiUrl = makeAPIUrl searchCmd
    in 
        case searchCmd of
            NoneSearched -> 
                Http.get
                { url = apiUrl
                , expect = Http.expectJson GotGoals goalsDecoder
                }
            GoalSearched goal ->
                Http.get
                { url = apiUrl
                , expect = Http.expectJson GotTargets targetsDecoder
                }
            TargetSearched target ->
                Http.get
                { url = apiUrl
                , expect = Http.expectJson GotIndicators indicatorsDecoder
                }
            IndicatorSearched indicator -> 
                Http.get
                { url = apiUrl
                , expect = Http.expectJson GotSeries seriesDecoder
                }
            SerieSearched serie ->
                Http.get
                { url = apiUrl
                , expect = Http.expectJson GotDimensions dimensionsDecoder
                }
