module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as Attributes



-- MAIN


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    { dividend : Dividend
    , divisor : Divisor
    }


type Dividend
    = Dividend Int


type Divisor
    = Divisor Int


type alias Step =
    { quotient : Int
    , remainder : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (Dividend 444) (Divisor 3)
    , Cmd.none
    )


equationSteps : Dividend -> Divisor -> List Step
equationSteps ((Dividend dividendInt) as dividend) ((Divisor divisorInt) as divisor) =
    let
        parts =
            dividendInt |> String.fromInt >> String.split "" >> List.map String.toInt >> List.map (Maybe.withDefault 0)

        _ =
            Debug.log "parts" parts

        calculateStep dividendPart previousRemainder =
            let
                adjustedDend =
                    dividendPart + (previousRemainder * 10)

                _ =
                    Debug.log "calculate step" { diviendPart = dividendPart, adjustedDend = adjustedDend }
            in
            { quotient = adjustedDend // divisorInt, remainder = remainderBy divisorInt adjustedDend }
    in
    parts
        |> List.foldl
            (\dend ( acc, prevRmd ) ->
                let
                    step =
                        calculateStep dend prevRmd
                in
                ( step :: acc, step.remainder )
            )
            ( [], 0 )
        |> Tuple.first



-- UPDATE


type Msg
    = Noop
    | NextStep


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        NextStep ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    Html.div
        [ Attributes.style "display" "flex"
        , Attributes.style "flex-direction" "column"
        , Attributes.style "align-items" "center"
        , Attributes.style "justify-content" "center"
        , Attributes.style "font-family" "monospace"
        , Attributes.style "font-size" "5vmin"
        ]
        [ Html.h1 [] [ Html.text "Elm Division" ]
        , Html.div []
            (viewEquation model.dividend model.divisor :: viewQuotient (equationSteps model.dividend model.divisor))
        ]


viewEquation : Dividend -> Divisor -> Html Msg
viewEquation (Dividend dividend) (Divisor divisor) =
    Html.text <| (dividend |> String.fromInt) ++ " : " ++ (divisor |> String.fromInt) ++ " = "


viewQuotient : List Step -> List (Html msg)
viewQuotient steps =
    case steps of
        [] ->
            []

        currentStep :: rest ->
            Html.text "this is a step, " :: viewQuotient rest
