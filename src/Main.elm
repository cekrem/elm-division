module Main exposing (main)

import Array
import Browser
import Dict
import Html exposing (Html)
import Html.Attributes as Attributes
import Html.Events as Events
import Html.Lazy
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


parseDividend : String -> Maybe (EquationNumber Dividend)
parseDividend input =
    if input |> String.isEmpty then
        Just (EquationNumber 0)

    else
        input
            |> String.toInt
            |> Maybe.map EquationNumber


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


parseDivisor : String -> Maybe (EquationNumber Divisor)
parseDivisor input =
    if input |> String.isEmpty then
        Just (EquationNumber 0)

    else
        input
            |> String.toInt
            |> Maybe.map EquationNumber


type QuotientPart
    = QuotientPart


toQuotient : Int -> EquationNumber QuotientPart
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
    { dividend : EquationNumber DividendPart
    , dividendWithRemainder : EquationNumber DividendPart
    , divisor : EquationNumber Divisor
    , quotient : EquationNumber QuotientPart
    , remainder : EquationNumber Remainder
    , product : EquationNumber Product
    , nextNumber : Maybe (EquationNumber DividendPart)
    }


stepWithoutNextNumber : EquationNumber Divisor -> EquationNumber DividendPart -> EquationNumber Remainder -> Step
stepWithoutNextNumber ((EquationNumber divisor) as typedDivisor) ((EquationNumber dividendPart) as typedDividendPart) (EquationNumber prevRemainder) =
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
    { dividend = typedDividendPart
    , dividendWithRemainder = adjustedDividend |> toDividendPart
    , divisor = typedDivisor
    , quotient = quotient |> toQuotient
    , product = product |> toProduct
    , remainder = remainder |> toRemainder
    , nextNumber = Nothing
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
            stepWithoutNextNumber divisor
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
        -- reverse list and add "next number" (to draw down)
        |> List.foldl
            (\current acc ->
                case acc of
                    next :: rest ->
                        { current | nextNumber = Just next.dividend } :: next :: rest

                    rest ->
                        current :: rest
            )
            []
    , List.length parts
    )



-- UPDATE


type Msg
    = Noop
    | NextStep
    | PrevStep
    | SetDividend String
    | SetDivisor String


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Noop ->
            ( model, Cmd.none )

        NextStep ->
            ( { model | activeStep = model.activeStep + 1 }, Cmd.none )

        PrevStep ->
            ( { model | activeStep = max (model.activeStep - 1) 0 }, Cmd.none )

        SetDividend input ->
            case input |> parseDividend of
                Just d ->
                    ( { model | dividend = d }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )

        SetDivisor input ->
            case input |> parseDivisor of
                Just d ->
                    ( { model | divisor = d }, Cmd.none )

                Nothing ->
                    ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    Html.Lazy.lazy3
        (\dividend divisor activeStep ->
            let
                ( steps, numberOfSteps ) =
                    model |> equationSteps

                activeSteps =
                    steps |> List.take activeStep
            in
            Html.div
                [ Attributes.style "display" "flex"
                , Attributes.style "flex-direction" "column"
                , Attributes.style "height" "100vh"
                , Attributes.style "width" "100vw"
                , Attributes.style "box-sizing" "border-box"
                , Attributes.style "padding" "2em 0"
                , Attributes.style "overflow-y" "auto"
                , Attributes.style "align-items" "center"
                , Attributes.style "justify-content" "flex-start"
                , Attributes.style "font-family" "sans-serif"
                , Attributes.style "font-size" "6vmin"
                ]
                [ Html.h3 [ Attributes.style "margin" "0" ] [ Html.text "Elm Division" ]
                , Html.div []
                    [ Html.input [ Attributes.value (dividend |> toString), Events.onInput SetDividend ] []
                    , Html.input [ Attributes.value (divisor |> toString), Events.onInput SetDivisor ] []
                    ]
                , Html.div []
                    [ Html.button [ Events.onClick PrevStep, Attributes.disabled (model.activeStep == 0) ] [ Html.text "<" ]
                    , Html.button [ Events.onClick NextStep, Attributes.disabled (model.activeStep == numberOfSteps) ] [ Html.text ">" ]
                    ]
                , viewEquation { dividend = dividend, divisor = divisor, steps = activeSteps }
                , viewStepExplanation activeSteps
                ]
        )
        model.dividend
        model.divisor
        model.activeStep


mathGridWrapper : List (Html msg) -> Html msg
mathGridWrapper content =
    Html.div
        [ Attributes.style "background-image"
            (String.join ","
                [ "linear-gradient(to right, transparent calc(100% - 0.5px), grey 100%)"
                , "linear-gradient(to bottom, transparent calc(100% - 0.5px), grey 100%)"
                ]
            )
        , Attributes.style "background-size" "1ch calc(1em - 1px), 1ch calc(1em - 1px)"
        , Attributes.style "background-position" "0.5px 0.5px"
        , Attributes.style "background-origin" "content-box"
        , Attributes.style "background-clip" "content-box"
        , Attributes.style "background-repeat" "repeat, repeat"

        -- typography
        , Attributes.style "font-family"
            "ui-monospace, SFMono-Regular, Menlo, Monaco, Consolas, 'Liberation Mono', 'Courier New', monospace"
        , Attributes.style "font-variant-numeric" "tabular-nums"
        , Attributes.style "font-feature-settings" "\"tnum\" 1"
        , Attributes.style "line-height" "1"
        , Attributes.style "letter-spacing" "0"

        -- avoid shifting the grid
        , Attributes.style "padding" "0"
        , Attributes.style "margin" "1em"
        , Attributes.style "outline" "1px solid grey"
        ]
        content


viewEquation : { a | dividend : EquationNumber Dividend, divisor : EquationNumber Divisor, steps : List Step } -> Html msg
viewEquation { dividend, divisor, steps } =
    mathGridWrapper
        [ textSpacer 1
        , Html.text (equationText { dividend = dividend, divisor = divisor })
        , (steps
            |> mapToColoredSpans
                (\st ->
                    [ Html.text (st.quotient |> toString) ]
                )
                []
          )
            |> HtmlHelpers.wrapToSingleNode
        , (steps
            |> mapToColoredDivsIndexed
                (\index st ->
                    [ Html.div []
                        [ HtmlHelpers.when (index > 1) (Html.div [] [ textSpacer (index + 1), Html.text (st.dividendWithRemainder |> toString) ])
                        , Html.div []
                            [ textSpacer 0
                            , Html.text "-"
                            , Html.text (st.product |> toString)

                            -- "draw down" arrow:
                            , HtmlHelpers.maybeNode
                                (\_ ->
                                    Html.span
                                        [ Attributes.style "color" "rgba(127,127,127, 0.5)"
                                        ]
                                        [ Html.text "↓" ]
                                )
                                st.nextNumber
                            ]
                        , Html.div []
                            [ textSpacer 0
                            , Html.text "="
                            , Html.text (st.remainder |> toString)

                            -- "draw down" next number:
                            , HtmlHelpers.maybeNode
                                (\nextNumber ->
                                    Html.text (nextNumber |> toString)
                                )
                                st.nextNumber
                            ]
                        ]
                    ]
                )
                []
          )
            |> HtmlHelpers.wrapToSingleNode
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
    let
        index =
            remainderBy (Array.length equationColors) int
    in
    equationColors |> Array.get index |> Maybe.withDefault "unset"


mapToColoredSpans : (a -> List (Html msg)) -> List (Html.Attribute msg) -> List a -> List (Html msg)
mapToColoredSpans transform attributes =
    List.indexedMap (\index entry -> Html.span (Attributes.style "color" (color index) :: attributes) (transform entry))


mapToColoredDivsIndexed : (Int -> a -> List (Html msg)) -> List (Html.Attribute msg) -> List a -> List (Html msg)
mapToColoredDivsIndexed transform attributes =
    List.indexedMap (\index entry -> Html.div (Attributes.style "color" (color index) :: attributes) (transform index entry))


viewStepExplanation : List Step -> Html msg
viewStepExplanation steps =
    Html.div []
        (steps
            |> mapToColoredDivsIndexed
                (\_ st ->
                    [ mathGridWrapper
                        [ Html.div []
                            [ Html.text (st.dividendWithRemainder |> toString)
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
                            [ Html.text (st.dividendWithRemainder |> toString)
                            , Html.text " - "
                            , Html.text (st.product |> toString)
                            , Html.text " = "
                            , Html.text (st.remainder |> toString)
                            ]
                        , Html.div []
                            [ Html.text "Remainder = "
                            , Html.text (st.remainder |> toString)
                            ]
                        ]
                    ]
                )
                []
        )


textSpacer : Int -> Html msg
textSpacer int =
    "" |> String.padLeft int (Char.fromCode 160) |> Html.text


equationText :
    { a
        | dividend : EquationNumber Dividend
        , divisor : EquationNumber Divisor
    }
    -> String
equationText { dividend, divisor } =
    (dividend |> toString) ++ " : " ++ (divisor |> toString) ++ " = "
