module Main exposing (main)

import Browser
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events



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
    , activeStep : Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( Model (EquationNumber 444) (EquationNumber 3) 0
    , Cmd.none
    )


type EquationNumber a
    = EquationNumber Int


toInt : EquationNumber a -> Int
toInt (EquationNumber int) =
    int


toString : EquationNumber a -> String
toString (EquationNumber int) =
    int |> String.fromInt


type Dividend
    = Dividend


toDividend : Int -> EquationNumber Dividend
toDividend int =
    EquationNumber int


type DividendPart
    = DividendPart


toDividendPart : Int -> EquationNumber DividendPart
toDividendPart int =
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
    { dividendPart : EquationNumber DividendPart
    , divisor : EquationNumber Divisor
    , quotient : EquationNumber Quotient
    , remainder : EquationNumber Remainder
    , product : EquationNumber Product
    }


step : EquationNumber Divisor -> EquationNumber DividendPart -> EquationNumber Remainder -> Step
step ((EquationNumber divisor) as typedDivisor) (EquationNumber dividendPart) (EquationNumber prevRemainder) =
    let
        adjustedDividend =
            dividendPart + (prevRemainder * 10)

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
    { dividendPart = adjustedDividend |> toDividendPart
    , divisor = typedDivisor
    , quotient = quotient |> toQuotient
    , product = product |> toProduct
    , remainder = remainder |> toRemainder
    }


equationSteps : { a | dividend : EquationNumber Dividend, divisor : EquationNumber Divisor } -> List Step
equationSteps { dividend, divisor } =
    let
        parts =
            dividend |> toInt |> String.fromInt >> String.split "" >> List.map String.toInt >> List.map (Maybe.withDefault 0) |> List.map toDividendPart

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
        |> List.reverse



-- UPDATE


type Msg
    = Noop
    | NextStep
    | PrevStep


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        NextStep ->
            ( { model | activeStep = model.activeStep + 1 }, Cmd.none )

        PrevStep ->
            ( { model | activeStep = max (model.activeStep - 1) 0 }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        activeSteps =
            model |> equationSteps |> List.take model.activeStep
    in
    Html.div
        [ Attributes.style "display" "flex"
        , Attributes.style "flex-direction" "column"
        , Attributes.style "align-items" "center"
        , Attributes.style "justify-content" "center"
        , Attributes.style "font-family" "monospace"
        , Attributes.style "font-size" "5vmin"
        ]
        [ Html.h1 [] [ Html.text "Elm Division" ]
        , Html.button [ Events.onClick NextStep ] [ Html.text ">" ]
        , Html.button [ Events.onClick PrevStep ] [ Html.text "<" ]
        , Html.div []
            (viewEquation model.dividend model.divisor :: viewQuotient activeSteps)
        ]


viewQuotient : List { a | quotient : EquationNumber Quotient } -> List (Html msg)
viewQuotient steps =
    steps
        |> List.map (\s -> s.quotient |> toString |> Html.text)


viewEquation : EquationNumber Dividend -> EquationNumber Divisor -> Html Msg
viewEquation (EquationNumber dividendInt) (EquationNumber divisorInt) =
    Html.text <| (dividendInt |> String.fromInt) ++ " : " ++ (divisorInt |> String.fromInt) ++ " = "
