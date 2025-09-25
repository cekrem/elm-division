module Main exposing (main)

import Array
import Browser
import Dict
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import HtmlHelpers



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
    ( Model (EquationNumber 292) (EquationNumber 3) 0
    , Cmd.none
    )


type EquationNumber a
    = EquationNumber Int


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


dividendParts : EquationNumber Divisor -> EquationNumber Dividend -> List (EquationNumber DividendPart)
dividendParts (EquationNumber divisorInt) (EquationNumber dividendInt) =
    let
        naiveParts =
            dividendInt |> String.fromInt |> String.split "" |> List.map String.toInt >> List.map (Maybe.withDefault 0)

        makeDivisable parts =
            case parts of
                first :: second :: rest ->
                    if first < divisorInt then
                        makeDivisable ((first * 10) + second :: rest)

                    else
                        parts |> List.map EquationNumber

                _ ->
                    parts |> List.map EquationNumber
    in
    makeDivisable naiveParts


equationSteps : { a | dividend : EquationNumber Dividend, divisor : EquationNumber Divisor } -> ( List Step, Int )
equationSteps { dividend, divisor } =
    let
        parts =
            dividend |> dividendParts divisor

        stepWithDevisor =
            step divisor
    in
    ( parts
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
    , List.length parts
    )



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
        ( steps, numberOfSteps ) =
            model |> equationSteps
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
        , Html.div []
            [ Html.button [ Events.onClick PrevStep, Attributes.disabled (model.activeStep == 0) ] [ Html.text "<" ]
            , Html.button [ Events.onClick NextStep, Attributes.disabled (model.activeStep == numberOfSteps) ] [ Html.text ">" ]
            ]
        , Html.div []
            (viewEquation model.dividend model.divisor :: viewQuotient steps)
        , viewEquationSteps steps numberOfSteps model.activeStep
        , viewStepExplanation steps
        ]


equationColors : Array.Array String
equationColors =
    [ "rgba(255, 99, 132, 0.8)"
    , "rgba(54, 162, 235, 0.8)"
    , "rgba(255, 205, 86, 0.8)"
    , "rgba(75, 192, 192, 0.8)"
    , "rgba(153, 102, 255, 0.8)"
    , "rgba(255, 159, 64, 0.8)"
    , "rgba(199, 199, 199, 0.8)"
    , "rgba(83, 102, 255, 0.8)"
    , "rgba(255, 99, 255, 0.8)"
    , "rgba(50, 205, 50, 0.8)"
    , "rgba(255, 20, 147, 0.8)"
    , "rgba(0, 191, 255, 0.8)"
    , "rgba(255, 140, 0, 0.8)"
    , "rgba(148, 0, 211, 0.8)"
    , "rgba(220, 20, 60, 0.8)"
    , "rgba(32, 178, 170, 0.8)"
    , "rgba(255, 215, 0, 0.8)"
    , "rgba(106, 90, 205, 0.8)"
    , "rgba(255, 105, 180, 0.8)"
    , "rgba(34, 139, 34, 0.8)"
    ]
        |> Array.fromList


color : Int -> String
color int =
    equationColors |> Array.get int |> Maybe.withDefault "unset"


viewEquationSteps : List Step -> Int -> Int -> Html msg
viewEquationSteps steps numberOfSteps activeStep =
    Html.div
        [ Attributes.style "text-align" "left"
        , Attributes.style "align-self" "center"
        ]
        (steps
            |> List.indexedMap
                (\index st ->
                    let
                        spacer =
                            "" |> String.padLeft index (Char.fromCode 160) |> Html.text
                    in
                    Html.div [ Attributes.style "color" (color index) ]
                        [ Html.div []
                            [ Html.div [] [ spacer, Html.text (st.dividendPart |> toString |> String.append "\u{200E} ") ]
                            , Html.div [] [ spacer, Html.text (st.product |> toString |> String.append "-") ]
                            , Html.div [] [ spacer, Html.text (st.remainder |> toString |> String.append "= ") ]
                            ]
                        ]
                )
        )


viewStepExplanation : List Step -> Html msg
viewStepExplanation steps =
    Html.div []
        (steps
            |> List.indexedMap
                (\index st ->
                    Html.div
                        [ Attributes.style "color" (color index)
                        , Attributes.style "text-align" "right"
                        ]
                        [ Html.div []
                            [ Html.text (st.dividendPart |> toString)
                            , Html.text " : "
                            , Html.text (st.divisor |> toString)
                            , Html.text " = "
                            , Html.text (st.quotient |> toString)
                            ]
                        , Html.div []
                            [ Html.text (st.quotient |> toString)
                            , Html.text " x "
                            , Html.text (st.divisor |> toString)
                            , Html.text " = "
                            , Html.text (st.product |> toString)
                            ]
                        , Html.div []
                            [ Html.text (st.dividendPart |> toString)
                            , Html.text " - "
                            , Html.text (st.product |> toString)
                            , Html.text " = "
                            , Html.text (st.remainder |> toString)
                            ]
                        , Html.div []
                            [ Html.text "Remainder = "
                            , Html.text (st.remainder |> toString)
                            ]
                        , Html.hr [] []
                        ]
                )
        )


viewQuotient : List { a | quotient : EquationNumber Quotient } -> List (Html msg)
viewQuotient steps =
    steps
        |> List.map (\s -> s.quotient |> toString |> Html.text)


viewEquation : EquationNumber Dividend -> EquationNumber Divisor -> Html Msg
viewEquation (EquationNumber dividendInt) (EquationNumber divisorInt) =
    Html.text <| (dividendInt |> String.fromInt) ++ " : " ++ (divisorInt |> String.fromInt) ++ " = "
