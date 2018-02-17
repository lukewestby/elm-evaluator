module Repl.Evaluator.Kernel exposing (..)

import Repl.Ast as Ast exposing (Expr(..), Name)


evaluate : Name -> List Expr -> Result String Expr
evaluate name inputs =
    case name of
        "_Basics_add" ->
            case inputs of
                [ EInt l, EInt r ] ->
                    Ok <| EInt (l + r)

                [ EFloat l, EFloat r ] ->
                    Ok <| EFloat (l + r)

                _ ->
                    Err "Type mismatch in kernel call _Basics_add"

        "_Basics_eq" ->
            case inputs of
                [ l, r ] ->
                    Ok <| EBool (l == r)

                _ ->
                    Err "Type mismatch in kernel call _Basics_eq"

        "_Basics_cmp" ->
            case inputs of
                [ EInt l, EInt r ] ->
                    Ok <| cmp l r

                [ EFloat l, EFloat r ] ->
                    Ok <| cmp l r

                [ EChar l, EChar r ] ->
                    Ok <| cmp l r

                [ EString l, EString r ] ->
                    Ok <| cmp l r

                _ ->
                    Err "Type mismatch in kernel call _Basics_cmp"

        _ ->
            Err <| "Unknown kernel call " ++ name


cmp : comparable -> comparable -> Expr
cmp l r =
    case compare l r of
        LT ->
            EVarImported "Basics" "LT"

        EQ ->
            EVarImported "Basics" "EQ"

        GT ->
            EVarImported "Basics" "GT"
