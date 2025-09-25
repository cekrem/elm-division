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
    { dividend : EquationNumber Dividend
    , divisor : EquationNumber Divisor
    }


type EquationNumber a
    = EquationNumber Int


toInt : EquationNumber a -> Int
toInt (EquationNumber int) =
    int


type Dividend
    = Dividend


toDividend : Int -> EquationNumber Dividend
toDividend int =
    EquationNumber int


type Divisor
    = Divisor


toDivisor : Int -> EquationNumber Divisor
toDivisor int =
    EquationNumber int


type Quotient
    = Quotient


toQuotient : Int -> EquationNumber Quotient
toQuotient int =
    EquationNumber int


type Remainder
    = Remainder


toRemainder : Int -> EquationNumber Remainder
toRemainder int =
    EquationNumber int


type Product
    = Product


toProduct : Int -> EquationNumber Product
toProduct int =
    EquationNumber int


type alias Step =
    { dividendPart : EquationNumber Dividend
    , divisor : EquationNumber Divisor
    , quotient : EquationNumber Quotient
    , remainder : EquationNumber Remainder
    , product : EquationNumber Product
    }


step : EquationNumber Divisor -> EquationNumber Dividend -> EquationNumber Remainder -> Step
step ((EquationNumber divisor) as typedDivisor) (EquationNumber dividend) (EquationNumber prevRemainder) =
    let
        adjustedDividend =
            dividend + (prevRemainder * 10)

        quotient =
            adjustedDividend // divisor

        product =
            quotient * divisor

        remainder =
            adjustedDividend - product

        _ =
            Debug.log "calculate step"
                { divisorInt = divisor
                , adjustedDividend = adjustedDividend
                , product = product
                , remainder = remainder
                }
    in
    { dividendPart = adjustedDividend |> toDividend
    , divisor = typedDivisor
    , quotient = quotient |> toQuotient
    , product = product |> toProduct
    , remainder = remainder |> toRemainder
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (EquationNumber 444) (EquationNumber 3)
    , Cmd.none
    )


equationSteps : EquationNumber Dividend -> EquationNumber Divisor -> List Step
equationSteps (EquationNumber dividendInt) divisor =
    let
        parts =
            dividendInt |> String.fromInt >> String.split "" >> List.map String.toInt >> List.map (Maybe.withDefault 0) |> List.map toDividend

        _ =
            Debug.log "parts" parts

        stepWithDevisor =
            step divisor
    in
    parts
        |> List.foldl
            (\dend ( acc, prevRmd ) ->
                let
                    currentStep =
                        stepWithDevisor dend prevRmd
                in
                ( currentStep :: acc, currentStep.remainder )
            )
            ( [], 0 |> toRemainder )
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


viewEquation : EquationNumber Dividend -> EquationNumber Divisor -> Html Msg
viewEquation (EquationNumber dividendInt) (EquationNumber divisorInt) =
    Html.text <| (dividendInt |> String.fromInt) ++ " : " ++ (divisorInt |> String.fromInt) ++ " = "


viewQuotient : List Step -> List (Html msg)
viewQuotient steps =
    case steps of
        [] ->
            []

        currentStep :: rest ->
            Html.text "this is a step, " :: viewQuotient rest
