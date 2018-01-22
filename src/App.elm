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


type alias Session =
    { from : String
    , to : String
    , topic : String
    , location : String
    }


type alias Model =
    { route : Route
    , userName : String
    , currentSession : Session
    , sessions : List Session
    }


type Msg
    = OnLocationChange Location
    | ShowUser
    | UpdateUserName String
    | UpdateSession (Session -> Session)
    | CreateSession


initialModel : Route -> Model
initialModel route =
    { route = route
    , userName = ""
    , currentSession = { from = "", to = "", location = "", topic = "" }
    , sessions = []
    }


init : Location -> ( Model, Cmd Msg )
init location =
    let
        currentRoute =
            parseLocation location
    in
        ( initialModel currentRoute, Cmd.none )


adminView : Model -> Html Msg
adminView model =
    let
        updateFrom =
            onInput (\s -> UpdateSession (\session -> { session | from = s }))

        updateTo =
            onInput (\s -> UpdateSession (\session -> { session | to = s }))

        updateTopic =
            onInput (\s -> UpdateSession (\session -> { session | topic = s }))

        updateLocation =
            onInput (\s -> UpdateSession (\session -> { session | location = s }))

        sessionLi session =
            li []
                [ span [] [ text session.from ]
                , span [] [ text session.to ]
                , span [] [ text session.topic ]
                , span [] [ text session.location ]
                , button [] [ text "-" ]
                ]
    in
        div []
            [ span [] [ text "Admin view" ]
            , div []
                [ ul []
                    ([ li []
                        [ input [ type_ "text", placeholder "From", updateFrom ] []
                        , input [ type_ "text", placeholder "To", updateTo ] []
                        , input [ type_ "text", placeholder "Topic", updateTopic ] []
                        , input [ type_ "text", placeholder "Location", updateLocation ] []
                        , button [ onClick CreateSession ] [ text "+" ]
                        ]
                     ]
                        ++ (model.sessions |> List.map sessionLi)
                    )
                ]
            ]


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
            adminView model

        User userName ->
            homeView model

        NotFound ->
            notFoundView


homeView : Model -> Html Msg
homeView model =
    div [ class "home-page" ]
        [ input [ type_ "text", placeholder "User name", onInput UpdateUserName ] []
        , button [ onClick ShowUser ] [ text "go" ]
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

        UpdateSession update ->
            let
                updatedSession =
                    update model.currentSession
            in
                ( { model | currentSession = updatedSession }, Cmd.none )

        CreateSession ->
            ( { model | sessions = model.currentSession :: model.sessions }, Cmd.none )

        ShowUser ->
            ( model, Navigation.newUrl model.userName )


subscriptions : model -> Sub msg
subscriptions =
    \_ -> Sub.none



-- Routing


type Route
    = Home
    | Admin
    | User String
    | NotFound


matcher : Parser (Route -> a) a
matcher =
    oneOf
        [ UrlParser.map Home top
        , UrlParser.map Admin (UrlParser.s "admin")
        , UrlParser.map User (top </> string)
        ]


parseLocation : Location -> Route
parseLocation location =
    case (parsePath matcher location) of
        Just route ->
            route

        Nothing ->
            NotFound
