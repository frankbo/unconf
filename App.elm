import Html exposing (..)

import Navigation exposing (Location)
import UrlParser exposing (..)


-- APP
main : Program Never Model Msg
main =
  Navigation.program OnLocationChange { init = init, view = view, update = update, subscriptions = subscriptions }

-- MODEL
type alias Model = { route : Route }

type Msg = OnLocationChange Location

initialModel : Route -> Model
initialModel route = { route = route }

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
            homeView
       NotFound ->
           notFoundView


homeView : Html Msg
homeView =
    div [] [ text "Hello Woop" ]

notFoundView : Html Msg
notFoundView =
    div [] [ text "Hello Not Found" ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        OnLocationChange location ->
           let
               newRoute =
                   parseLocation location
           in
               ( { model | route = newRoute }, Cmd.none )

subscriptions : model -> Sub msg
subscriptions = \_ -> Sub.none

-- Routing
type Route =
    Home
    | NotFound



matchers : Parser (Route -> a) a
matchers =
    oneOf
        [ UrlParser.map Home top ]


parseLocation : Location -> Route
parseLocation location =
    case (parseHash matchers location) of
        Just route ->
            route

        Nothing ->
            NotFound