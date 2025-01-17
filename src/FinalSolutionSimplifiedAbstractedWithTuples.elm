module FinalSolutionSimplifiedAbstractedWithTuples exposing (main)

import Browser
import Html
import Html.Attributes
import Html.Events


counterLeigh : LeighProgram State
counterLeigh =
    state 0 <|
        \( count, setCount ) ->
            state "" <|
                \( message, setMessage ) ->
                    render <|
                        \globalState ->
                            Html.div []
                                [ Html.button [ Html.Events.onClick <| (globalState |> setCount ((globalState |> count) + 1)) ] [ Html.text "+" ]
                                , Html.p [] [ Html.text <| String.fromInt <| (globalState |> count) ]
                                , Html.input [ Html.Attributes.value (globalState |> message), Html.Events.onInput (\value -> globalState |> setMessage value) ] []
                                ]


main : Program () State State
main =
    counterLeigh |> leighToProgram


type alias State =
    ( Int, ( String, () ) )



--


type alias LeighProgram state =
    ModelTranslation state state
    -> { initialState : state, interface : state -> Html.Html state }


leighToProgram : LeighProgram state -> Program () state state
leighToProgram =
    \leigh ->
        let
            initialStateAndInterface : { initialState : state, interface : state -> Html.Html state }
            initialStateAndInterface =
                leigh modelTranslationIdentity
        in
        Browser.element
            { init = \() -> ( initialStateAndInterface.initialState, Cmd.none )
            , view = initialStateAndInterface.interface
            , update = \newState _ -> ( newState, Cmd.none )
            , subscriptions = \_ -> Sub.none
            }


modelTranslationIdentity : ModelTranslation a a
modelTranslationIdentity =
    { alterSub = \f -> f
    , toSub = identity
    }


state :
    a
    ->
        (Updater a model
         -> ModelTranslation otherFields model
         -> { initialState : otherFields, interface : interface }
        )
    -> ModelTranslation ( a, otherFields ) model
    -> { initialState : ( a, otherFields ), interface : interface }
state initialState withStateUpdater translateAOtherFieldsToModel =
    let
        updaterForAAndOtherFields : Updater a ( a, otherFields )
        updaterForAAndOtherFields =
            updaterTupleFirst

        modelTranslation : ModelTranslation otherFields model
        modelTranslation =
            modelTranslationCompose tupleSecondModelTranslation translateAOtherFieldsToModel

        translatedUpdater : Updater a model
        translatedUpdater =
            updaterForAAndOtherFields |> translateUpdater translateAOtherFieldsToModel

        interfaceAndInitialOtherFieldsState : { initialState : otherFields, interface : interface }
        interfaceAndInitialOtherFieldsState =
            withStateUpdater translatedUpdater modelTranslation
    in
    { initialState = ( initialState, interfaceAndInitialOtherFieldsState.initialState )
    , interface = interfaceAndInitialOtherFieldsState.interface
    }


tupleSecondModelTranslation : ModelTranslation second ( first, second )
tupleSecondModelTranslation =
    { alterSub = Tuple.mapSecond
    , toSub = Tuple.second
    }


modelTranslationCompose : ModelTranslation subSub sub -> ModelTranslation sub whole -> ModelTranslation subSub whole
modelTranslationCompose subSubToSub subToWhole =
    { alterSub = \alterInner -> subToWhole.alterSub (subSubToSub.alterSub alterInner)
    , toSub = \whole -> whole |> subToWhole.toSub |> subSubToSub.toSub
    }


translateUpdater : ModelTranslation sub model -> (Updater a sub -> Updater a model)
translateUpdater modelTranslation subUpdater =
    let
        ( subUpdaterGet, subUpdaterSet ) =
            subUpdater

        modelGet : model -> a
        modelGet =
            \model ->
                model |> modelTranslation.toSub |> subUpdaterGet

        modelSet : a -> (model -> model)
        modelSet =
            \newValue ->
                \model ->
                    model |> modelTranslation.alterSub (subUpdaterSet newValue)
    in
    ( modelGet
    , modelSet
    )


type alias ModelTranslation sub whole =
    { alterSub : (sub -> sub) -> (whole -> whole)
    , toSub : whole -> sub
    }


render :
    b
    -> ModelTranslation () model
    -> { initialState : (), interface : b }
render interface _ =
    { initialState = (), interface = interface }


updaterTupleFirst : Updater data ( data, otherFields )
updaterTupleFirst =
    ( \whole -> whole |> Tuple.first
    , \newData -> Tuple.mapFirst (\_ -> newData)
    )


type alias Updater focus model =
    ( model -> focus
    , focus -> model -> model
    )
