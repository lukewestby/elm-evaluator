module Repl.Evaluator exposing (..)

import Dict exposing (Dict)
import Repl.Ast as Ast exposing (Expr(..), Name)
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
        -- Variables: Look up from env
        EVarLocal name ->
            getLocal name context
                |> Result.andThen (evaluate context)

        EVarImported moduleName varName ->
            getImported moduleName varName context
                |> Result.andThen (evaluate context)

        -- Values: Already fully evaluated, just return
        EUnit ->
            Ok EUnit

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

        -- Containers: evaluate members
        ETuple2 left right ->
            Result.map2 ETuple2
                (evaluate context left)
                (evaluate context right)

        ERecord pairs ->
            pairs
                |> Dict.toList
                |> List.map (\( k, v ) -> Result.map ((,) k) (evaluate context v))
                |> List.foldr (Result.map2 (::)) (Ok [])
                |> Result.map (Dict.fromList >> ERecord)

        EList exprs ->
            exprs
                |> List.map (evaluate context)
                |> List.foldr (Result.map2 (::)) (Ok [])
                |> Result.map EList

        ECtor name values ->
            values
                |> List.map (evaluate context)
                |> List.foldr (Result.map2 (::)) (Ok [])
                |> Result.map (ECtor name)

        -- Lambdas: evauate a lambda as a closure over the current context's locals
        ELambda argument body ->
            Ok <| EClosure context.locals argument body

        EClosure env arg body ->
            Ok <| EClosure env arg body

        -- Operations: execute the semantics of the operation
        EUpdate { record, replacements } ->
            case evaluate context record of
                Ok (ERecord oldRecord) ->
                    Ok <| ERecord <| Dict.union replacements oldRecord

                Ok _ ->
                    Err "Cannot update something that isn't a record"

                Err message ->
                    Err message

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

        EBinop moduleName opName left right ->
            case evaluate context (EVarImported moduleName opName) of
                Ok callable ->
                    evaluate context <| ECall left <| ECall right callable

                Err message ->
                    Err message

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

        EKernel name inputs ->
            inputs
                |> traverseList (evaluate context)
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


traverseList : (a -> Result x b) -> List a -> Result x (List b)
traverseList f list =
    list
        |> List.map f
        |> List.foldr (Result.map2 (::)) (Ok [])
