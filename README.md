Goal is creating a typechecking
```elm
main =
  state 0 <| \count setCount ->
    state "" <| \message setMessage ->
      render <| \model ->
        Html.div []
          [ Html.button [ Html.Events.onClick <| setCount <| (count model) + 1 ] [ Html.text "+" ]
          , Html.p [] [ Html.text <| String.fromInt <| count model ]
          , Html.input [ Html.Attributes.value (message model), Html.Events.onInput setMessage ] []
          ]
```

I claim that I got it.
My original solution can be found in `src/FinalSolution.elm`.

A better, simplified attempt is located at `src/FinalSolutionSimplified.elm`
