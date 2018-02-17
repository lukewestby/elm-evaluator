module Repl.Evaluator.Prelude exposing (..)

import Dict exposing (Dict)
import Repl.Ast as Ast exposing (Expr(..), Name)


prelude : Dict Name (Dict Name Expr)
prelude =
    Dict.fromList
        [ ( "Basics", basics )
        ]


basics : Dict Name Expr
basics =
    Dict.fromList
        [ ( "add"
          , ELambda "l"
                (ELambda "r"
                    (EKernel
                        "_Basics_add"
                        [ EVarLocal "l", EVarLocal "r" ]
                    )
                )
          )
        , ( "eq"
          , ELambda "l"
                (ELambda "r"
                    (EKernel
                        "_Basics_eq"
                        [ EVarLocal "l", EVarLocal "r" ]
                    )
                )
          )
        , ( "not"
          , ELambda "a"
                (EIf
                    [ ( EVarLocal "a", EBool False ) ]
                    (EBool True)
                )
          )
        , ( "/="
          , ELambda "l"
                (ELambda "r"
                    (ECall
                        (EBinop "Basics" "==" (EVarLocal "l") (EVarLocal "r"))
                        (EVarImported "Basics" "not")
                    )
                )
          )
        , ( "=="
          , EVarImported "Basics" "eq"
          )
        , ( "GT"
          , ECtor "GT" []
          )
        , ( "EQ"
          , ECtor "EQ" []
          )
        , ( "LT"
          , ECtor "LT" []
          )
        ]
