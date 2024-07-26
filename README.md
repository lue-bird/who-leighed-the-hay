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

I claim that I got it. My solution can run snippets that look _exactly_ like above and can be found in `src/FinalSolutionIdenticalToRequest.elm`

To run:
```bash
elm reactor
```
then click on `src/FinalSolutionIdenticalToRequest.elm` or some other file:

  - my original solution: `src/FinalSolution.elm`
  - a better, simplified attempt: `src/FinalSolutionSimplified.elm`
  - a version that converts any of these functions into runnable elm programs: `src/FinalSolutionSimplifiedAbstracted.elm`
  - a version that additionally uses tuples instead of records for get,set: `src/FinalSolutionSimplifiedAbstractedWithTuples.elm`
  - a version that additionally uses 2 arguments instead of records for get,set: `src/FinalSolutionSimplifiedAbstractedWithArguments.elm`
