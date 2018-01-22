module App exposing (..)

import Html exposing (..)
import Html.Attributes exposing (..)
import Navigation exposing (Location)
import UrlParser exposing (..)
import Html.Events exposing (onClick, onInput)


-- APP


main : Program Never Model Msg
main =
    Navigation.program OnLocationChange { init = init, view = view, update = update, subscriptions = subscriptions }



-- MODEL


type alias Model =
    { route : Route, userName : String }


type Msg
    = OnLocationChange Location
    | ShowUser String
    | UpdateUserName String


initialModel : Route -> Model
initialModel route =
    { route = route, userName = "" }


init : Location -> ( Model, Cmd Msg )
init location =
    let
        currentRoute =
            parseLocation location
    in
        ( initialModel currentRoute, Cmd.none )


view : Model -> Html Msg
view model =
    div []
        [ page model ]


page : Model -> Html Msg
page model =
    case model.route of
        Home ->
            homeView model

        Admin ->
            homeView model

        NotFound ->
            notFoundView


homeView : Model -> Html Msg
homeView model =
    div [ class "home-page" ]
        [ input [ type_ "text", placeholder "User name", onInput UpdateUserName ] []
        , button [ onClick (ShowUser model.userName) ] [ text "go" ]
        ]


notFoundView : Html Msg
notFoundView =
    div [] [ text "Not Found" ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLocationChange location ->
            let
                newRoute =
                    parseLocation location
            in
                ( { model | route = newRoute }, Cmd.none )

        UpdateUserName userName ->
            ( { model | userName = userName }, Cmd.none )

        -- TODO redirect to new url
        ShowUser userName ->
            ( { model | route = NotFound }, Cmd.none )


subscriptions : model -> Sub msg
subscriptions =
    \_ -> Sub.none



-- Routing


type Route
    = Home
    | Admin
    | NotFound


matcher : Parser (Route -> a) a
matcher =
    oneOf
        [ UrlParser.map Home top
        , UrlParser.map Admin (UrlParser.s "admin")
        ]


parseLocation : Location -> Route
parseLocation location =
    case (parsePath matcher location) of
        Just route ->
            route

        Nothing ->
            NotFound
