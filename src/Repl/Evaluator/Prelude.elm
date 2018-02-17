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
                        (ETuple2 (EVarLocal "l") (EVarLocal "r"))
                    )
                )
          )
        ]
