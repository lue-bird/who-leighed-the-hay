module FinalSolutionIdenticalToRequest exposing (main)

import Browser
import Html
import Html.Attributes
import Html.Events


counterLeigh : LeighProgram State
counterLeigh =
    state 0 <|
        \count setCount ->
            state "" <|
                \message setMessage ->
                    render <|
                        \model ->
                            Html.div []
                                [ Html.button [ Html.Events.onClick <| setCount <| count model + 1 ] [ Html.text "+" ]
                                , Html.p [] [ Html.text <| String.fromInt <| count model ]
                                , Html.input [ Html.Attributes.value (message model), Html.Events.onInput setMessage ] []
                                ]


main : Program () State State
main =
    counterLeigh |> leighToProgram


type alias LeighProgram state =
    ModelTranslation state state
    -> { initialState : state, interface : state -> Html.Html (state -> state) }


leighToProgram : LeighProgram state -> Program () state state
leighToProgram =
    \leigh ->
        let
            initialStateAndInterface : { initialState : state, interface : state -> Html.Html (state -> state) }
            initialStateAndInterface =
                leigh modelTransitionIdentity
        in
        Browser.element
            { init = \() -> ( initialStateAndInterface.initialState, Cmd.none )
            , view = \model -> initialStateAndInterface.interface model |> Html.map (\f -> f model)
            , update = \newState _ -> ( newState, Cmd.none )
            , subscriptions = \_ -> Sub.none
            }


type alias State =
    ( Int, ( String, () ) )


modelTransitionIdentity : ModelTranslation a a
modelTransitionIdentity =
    { alterSub = \f -> f
    , toSub = identity
    }


state :
    a
    ->
        ((model -> a)
         -> (a -> (model -> model))
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
            modelTransitionCompose tupleSecondModelTransition translateAOtherFieldsToModel

        translatedUpdater : Updater a model
        translatedUpdater =
            updaterForAAndOtherFields |> translateUpdater translateAOtherFieldsToModel

        interfaceAndInitialOtherFieldsState =
            withStateUpdater translatedUpdater.get translatedUpdater.set modelTranslation
    in
    { initialState = ( initialState, interfaceAndInitialOtherFieldsState.initialState )
    , interface = interfaceAndInitialOtherFieldsState.interface
    }


tupleSecondModelTransition : ModelTranslation second ( first, second )
tupleSecondModelTransition =
    { alterSub = Tuple.mapSecond
    , toSub = Tuple.second
    }


modelTransitionCompose : ModelTranslation subSub sub -> ModelTranslation sub whole -> ModelTranslation subSub whole
modelTransitionCompose subSubToSub subToWhole =
    { alterSub = \alterInner -> subToWhole.alterSub (subSubToSub.alterSub alterInner)
    , toSub = \whole -> whole |> subToWhole.toSub |> subSubToSub.toSub
    }


translateUpdater : ModelTranslation sub model -> (Updater a sub -> Updater a model)
translateUpdater modelTranslation subUpdater =
    let
        modelGet : model -> a
        modelGet =
            \model ->
                model |> modelTranslation.toSub |> subUpdater.get

        modelSet : a -> (model -> model)
        modelSet =
            \newValue ->
                \model ->
                    model |> modelTranslation.alterSub (subUpdater.set newValue)
    in
    { get = modelGet
    , set = modelSet
    }


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
    { get = \whole -> whole |> Tuple.first
    , set = \newData -> Tuple.mapFirst (\_ -> newData)
    }


type alias Updater focus model =
    { get : model -> focus
    , set : focus -> model -> model
    }
