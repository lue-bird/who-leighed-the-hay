module DirectApplyFinallyModelAndInitialState exposing (main)

import Browser
import Html
import Html.Attributes
import Html.Events


main : Program () State State
main =
    let
        leigh : ModelTranslation State State -> { initialState : State, interface : State -> Html.Html State }
        leigh =
            state 0 <|
                \count ->
                    state "" <|
                        \message ->
                            render <|
                                { initialState = ( count.initialValue, ( message.initialValue, () ) )
                                , interface =
                                    \globalState ->
                                        Html.div []
                                            [ Html.button [ Html.Events.onClick <| (globalState |> set count ((globalState |> get count) + 1)) ] [ Html.text "+" ]
                                            , Html.p [] [ Html.text <| String.fromInt <| (globalState |> get count) ]
                                            , Html.input [ Html.Attributes.value (globalState |> get message), Html.Events.onInput (\value -> globalState |> set message value) ] []
                                            ]
                                }

        initialStateAndInterface : { initialState : State, interface : State -> Html.Html State }
        initialStateAndInterface =
            leigh modelTranslationIdentity
    in
    Browser.element
        { init = \() -> ( initialStateAndInterface.initialState, Cmd.none )
        , view = initialStateAndInterface.interface
        , update = \newState _ -> ( newState, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


type alias State =
    ( Int, ( String, () ) )


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
         -> final
        )
    -> ModelTranslation ( a, otherFields ) model
    -> final
state initialState withStateUpdater translateAOtherFieldsToModel =
    let
        updaterForAAndOtherFields : Updater a ( a, otherFields )
        updaterForAAndOtherFields =
            updaterTupleFirst initialState

        modelTranslation : ModelTranslation otherFields model
        modelTranslation =
            modelTranslationCompose tupleSecondModelTranslation translateAOtherFieldsToModel

        translatedUpdater =
            updaterForAAndOtherFields |> translateUpdater translateAOtherFieldsToModel
    in
    withStateUpdater translatedUpdater modelTranslation


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
    { initialValue = subUpdater.initialValue
    , get = modelGet
    , set = modelSet
    }


type alias ModelTranslation sub whole =
    { alterSub : (sub -> sub) -> (whole -> whole)
    , toSub : whole -> sub
    }


render :
    b
    -> ModelTranslation () model
    -> b
render fullThing _ =
    fullThing


updaterTupleFirst : data -> Updater data ( data, otherFields )
updaterTupleFirst initialData =
    { initialValue = initialData
    , get = \whole -> whole |> Tuple.first
    , set = \newData -> Tuple.mapFirst (\_ -> newData)
    }


type alias Updater focus model =
    { get : model -> focus
    , initialValue : focus
    , set : focus -> model -> model
    }


get : Updater focus model -> (model -> focus)
get updater model =
    model |> updater.get


set : Updater focus model -> focus -> (model -> model)
set updater newValue model =
    model |> updater.set newValue
