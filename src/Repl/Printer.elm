module Repl.Printer exposing (..)

import Dict exposing (Dict)
import Repl.Ast as Ast exposing (Expr(..))


print : Expr -> String
print expr =
    case expr of
        EUnit ->
            "()"

        EVarLocal name ->
            name

        EVarImported moduleName varName ->
            moduleName ++ "." ++ varName

        EChar char ->
            "'" ++ String.fromChar char ++ "'"

        EString string ->
            "\"" ++ string ++ "\""

        EInt int ->
            toString int

        EFloat float ->
            toString float

        EBool bool ->
            toString bool

        ETuple2 left right ->
            "( " ++ print left ++ ", " ++ print right ++ " )"

        ERecord pairs ->
            if Dict.isEmpty pairs then
                "{}"
            else
                "{ "
                    ++ (pairs
                            |> Dict.toList
                            |> List.map (\( key, value ) -> key ++ " = " ++ print value)
                            |> String.join ", "
                       )
                    ++ " }"

        EList list ->
            case list of
                [] ->
                    "[]"

                _ ->
                    "[ " ++ String.join ", " (List.map print list) ++ " ]"

        ECtor name values ->
            case values of
                [] ->
                    name

                _ ->
                    name ++ " " ++ String.join " " (List.map print values)

        ELambda arg body ->
            "\\" ++ arg ++ " -> " ++ print body

        EClosure _ arg body ->
            print (ELambda arg body)

        ENegate expr ->
            "-" ++ print expr

        EBinop _ opName left right ->
            print left ++ " " ++ opName ++ " " ++ print right

        EUpdate { record, replacements } ->
            "{ "
                ++ print record
                ++ " | "
                ++ String.join ", " (List.map (\( key, value ) -> key ++ " = " ++ print value) (Dict.toList replacements))
                ++ " }"

        ECall arg callable ->
            print callable ++ " " ++ print arg

        ELet ( binding, value ) inBlock ->
            "let"
                ++ "\n    "
                ++ binding
                ++ " =\n        "
                ++ print value
                ++ "\nin\n"
                ++ print inBlock

        EIf branches elseCase ->
            (branches
                |> List.map printIf
                |> String.join "else "
            )
                ++ printElse elseCase

        EKernel _ _ ->
            "XXX"


printIf : ( Expr, Expr ) -> String
printIf ( predicate, return ) =
    "if "
        ++ print predicate
        ++ " then\n"
        ++ "    "
        ++ print return
        ++ "\n"


printElse : Expr -> String
printElse return =
    "else\n    " ++ print return ++ "\n"
