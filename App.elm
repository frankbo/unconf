import Html exposing (..)


-- APP
main : Program Never Model Msg
main =
  Html.program { init = (model, Cmd.none), view = view, update = update, subscriptions = subscriptions }

-- MODEL
type alias Model = Int

type Msg
    = SomeThing

model : Model
model = 0

view : Model -> Html Msg
view model =
    div [] [ text "Hello Woop" ]

update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    (model, Cmd.none)

subscriptions : model -> Sub msg
subscriptions = \_ -> Sub.none