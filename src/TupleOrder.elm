module TupleOrder exposing (main)

import Html
import Html.Attributes
import Html.Events


main =
    Html.text "compiles"


hayMain : () -> Html.Html msg
hayMain =
    state 0 <|
        state "" <|
            \( ( message, setMessage ), ( ( count, setCount ), () ) ) ->
                Html.div []
                    [ Html.button [ Html.Events.onClick <| setCount <| count + 1 ] [ Html.text "+" ]
                    , Html.p [] [ Html.text <| String.fromInt <| count ]
                    , Html.input [ Html.Attributes.value message, Html.Events.onInput setMessage ] []
                    ]


state : a -> (( Updater a msg, other ) -> b) -> (other -> b)
state initialState withStateUpdater =
    \other -> withStateUpdater ( updaterForState initialState, other )


updaterForState : data -> Updater data msg
updaterForState data =
    ( data, \futureData -> Debug.todo "whatever magic you imagine would be here" )


type alias Updater data msg =
    ( data, data -> msg )
