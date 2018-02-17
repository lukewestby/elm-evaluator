module Repl.Evaluator exposing (..)

import Dict exposing (Dict)
import Repl.Ast as Ast exposing (Eq(..), Expr(..), Name, Ord(..))
import Repl.Evaluator.Kernel as Kernel
import Repl.Evaluator.Prelude as Prelude


type alias Context =
    { locals : Dict Name Expr
    , imports : Dict Name (Dict Name Expr)
    }


getLocal : Name -> Context -> Result String Expr
getLocal name context =
    context.locals
        |> Dict.get name
        |> Result.fromMaybe ("No variable defined with name " ++ name)


getImported : Name -> Name -> Context -> Result String Expr
getImported moduleName varName context =
    context.imports
        |> Dict.get moduleName
        |> Result.fromMaybe ("No module named " ++ moduleName ++ " exists")
        |> Result.andThen (Dict.get varName >> Result.fromMaybe ("Module " ++ moduleName ++ " does not expose " ++ varName))


defaultContext : Context
defaultContext =
    { locals = Dict.empty
    , imports = Prelude.prelude
    }


evaluate : Context -> Expr -> Result String Expr
evaluate context expr =
    case expr of
        EUnit ->
            Ok EUnit

        EVarLocal name ->
            getLocal name context
                |> Result.andThen (evaluate context)

        EVarImported moduleName varName ->
            getImported moduleName varName context
                |> Result.andThen (evaluate context)

        EChar char ->
            Ok <| EChar char

        EString string ->
            Ok <| EString string

        EInt int ->
            Ok <| EInt int

        EFloat float ->
            Ok <| EFloat float

        EBool bool ->
            Ok <| EBool bool

        ETuple2 left right ->
            Result.map2 ETuple2
                (evaluate context left)
                (evaluate context right)

        EList exprs ->
            exprs
                |> List.map (evaluate context)
                |> List.foldr (Result.map2 (::)) (Ok [])
                |> Result.map EList

        ENegate expr ->
            case evaluate context expr of
                Ok (EInt int) ->
                    Ok <| EInt -int

                Ok (EFloat float) ->
                    Ok <| EFloat -float

                Ok _ ->
                    Err "type mismatch in negate Expr"

                Err message ->
                    Err message

        ELambda argument body ->
            Ok <| EClosure context.locals argument body

        ECall argument callable ->
            case ( evaluate context argument, evaluate context callable ) of
                ( Ok argValue, Ok (EClosure env argName body) ) ->
                    evaluate
                        { context | locals = Dict.insert argName argValue env }
                        body

                ( Err message, _ ) ->
                    Err message

                ( _, Err message ) ->
                    Err message

                ( _, a ) ->
                    let
                        _ =
                            Debug.log "bad callable" a
                    in
                    Err "trying to call something that isn't callable"

        ELet ( binding, value ) inBlock ->
            case evaluate context value of
                Ok actualValue ->
                    evaluate
                        { context | locals = Dict.insert binding actualValue context.locals }
                        inBlock

                Err message ->
                    Err message

        EIf branches elseCase ->
            evaluateIf context branches elseCase

        EClosure env arg body ->
            Ok <| EClosure env arg body

        EKernel name inputs ->
            inputs
                |> evaluate context
                |> Result.andThen (Kernel.evaluate name)


evaluateIf : Context -> List ( Expr, Expr ) -> Expr -> Result String Expr
evaluateIf context branches elseCase =
    case branches of
        [] ->
            evaluate context elseCase

        ( pred, ret ) :: xs ->
            case evaluate context pred of
                Ok (EBool True) ->
                    evaluate context ret

                Ok (EBool False) ->
                    evaluateIf context xs elseCase

                Ok _ ->
                    Err "type mismatch in if pattern"

                Err message ->
                    Err message
