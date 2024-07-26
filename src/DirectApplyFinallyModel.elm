module DirectApplyFinallyModel exposing (main)

import Html
import Html.Attributes
import Html.Events


main =
    Html.text "compiles"


type alias State =
    ( Int, ( String, () ) )


hayMain : State -> Html.Html State
hayMain globalState =
    let
        leigh : ModelTranslation State State -> Html.Html State
        leigh =
            state 0 <|
                \( getCount, setCount ) ->
                    state "" <|
                        \( getMessage, setMessage ) ->
                            render <|
                                Html.div []
                                    [ Html.button [ Html.Events.onClick <| (globalState |> setCount ((globalState |> Just |> getCount) + 1)) ] [ Html.text "+" ]
                                    , Html.p [] [ Html.text <| String.fromInt <| (globalState |> Just |> getCount) ]
                                    , Html.input [ Html.Attributes.value (globalState |> Just |> getMessage), Html.Events.onInput (\value -> globalState |> setMessage value) ] []
                                    ]
    in
    leigh modelTransitionIdentity


modelTransitionIdentity : ModelTranslation a a
modelTransitionIdentity =
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
            updaterForState initialState

        modelTranslation : ModelTranslation otherFields model
        modelTranslation =
            modelTransitionCompose tupleSecondModelTransition translateAOtherFieldsToModel

        translatedUpdater =
            updaterForAAndOtherFields |> translateUpdater translateAOtherFieldsToModel
    in
    withStateUpdater translatedUpdater modelTranslation


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
        ( subGet, subSet ) =
            subUpdater

        modelGet : Maybe model -> a
        modelGet =
            \maybeModel ->
                case maybeModel of
                    Nothing ->
                        subGet Nothing

                    Just model ->
                        model |> modelTranslation.toSub |> Just |> subGet

        modelSet : a -> (model -> model)
        modelSet =
            \newValue ->
                \model ->
                    model |> modelTranslation.alterSub (subSet newValue)
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
    -> b
render fullThing _ =
    fullThing


updaterForState : data -> Updater data ( data, otherFields )
updaterForState initialData =
    ( \soFar ->
        case soFar of
            Nothing ->
                initialData

            Just fullTuple ->
                fullTuple |> Tuple.first
    , \newData -> Tuple.mapFirst (\_ -> newData)
    )


type alias Updater focus model =
    ( Maybe model -> focus, focus -> model -> model )
