module ReverseOrder exposing (main)

import Html
import Html.Attributes
import Html.Events


main =
    Html.text "compiles"


hayMain =
    state 0 <|
        state "" <|
            render <|
                \message count ->
                    Html.div []
                        [ Html.button [ Html.Events.onClick <| count.set <| count.get + 1 ] [ Html.text "+" ]
                        , Html.p [] [ Html.text <| String.fromInt <| count.get ]
                        , Html.input [ Html.Attributes.value message.get, Html.Events.onInput message.set ] []
                        ]


type alias Updater data msg =
    { get : data, set : data -> msg }


state : a -> (Updater a msg -> b) -> b
state initialState withStateUpdater =
    withStateUpdater (updaterForState initialState)


updaterForState : data -> Updater data msg
updaterForState data =
    { get = data, set = \futureData -> Debug.todo "" }


render : withUpdater -> withUpdater
render withUpdater =
    withUpdater
