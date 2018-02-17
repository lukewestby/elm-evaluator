module Repl.Printer exposing (..)

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

        EList list ->
            case list of
                [] ->
                    "[]"

                _ ->
                    "[ " ++ String.join ", " (List.map print list) ++ " ]"

        ENegate expr ->
            "-" ++ print expr

        ELambda arg body ->
            "\\" ++ arg ++ " -> " ++ print body

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

        EClosure _ arg body ->
            print (ELambda arg body)

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
