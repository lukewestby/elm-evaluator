module Main exposing (..)

import Dict
import Html exposing (Html)
import Repl.Ast as Ast exposing (Expr(..))
import Repl.Evaluator as Evaluator
import Repl.Printer as Printer


test : Expr
test =
    ELet
        ( "add2"
        , ELambda "a"
            (ECall
                (EVarLocal "a")
                (ECall
                    (EInt 2)
                    (EVarImported "Basics" "add")
                )
            )
        )
        (EIf
            [ ( EBool False, ECall (EInt 1) (EVarLocal "add2") )
            , ( EBool False, ECall (EInt 4) (ECall (EInt 2) (EVarImported "Basics" "add")) )
            ]
            (ECall (EInt 5) (EVarLocal "add2"))
        )


type alias Model =
    {}


init : ( Model, Cmd Msg )
init =
    ( {}, Cmd.none )


type Msg
    = NoOp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    Html.pre []
        [ Html.text <| Printer.print test
        , Html.br [] []
        , Html.br [] []
        , Html.br [] []
        , Html.text <|
            case Evaluator.evaluate Evaluator.defaultContext test of
                Ok expr ->
                    Printer.print expr

                Err message ->
                    message
        ]


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program Never Model Msg
main =
    Html.program
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }
