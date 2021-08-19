module NoMissingDocumentation exposing (rule)

{-| Make sure that every top level declaration is documented.

# Rule

@docs rule
-}

import Elm.Syntax.Declaration exposing (Declaration(..))
import Elm.Syntax.Documentation exposing (Documentation)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Range)
import Review.Rule as Rule exposing (Direction(..), Error, Rule)


{-|


## Usage

After adding [elm-review](https://package.elm-lang.org/packages/jfmengels/elm-review/latest/) to your project, import this rule to
`ReviewConfig.elm` file and add it to the config.


## Example configuration

    import NoMissingDocumentation
    import Review.Rule exposing (Rule)

    config : List Rule
    config =
        [ NoMissingDocumentation.rule ]

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoMissingDocumentation" ()
        |> Rule.withSimpleDeclarationVisitor declarationVisitor
        |> Rule.fromModuleRuleSchema


{-| Is documentation present
-}
isWithDocumentation : Maybe (Node Documentation) -> Bool
isWithDocumentation maybeDocumentationNode =
    maybeDocumentationNode /= Nothing


{-| Checks if node contains documentation and throws an error if not
-}
throwRuleErrorIfNoDocumentation : Maybe (Node Documentation) -> Range -> List (Error {})
throwRuleErrorIfNoDocumentation maybeDocumentationNode range =
    if isWithDocumentation maybeDocumentationNode then
        []

    else
        [ Rule.error
            { message = "No undocumented function/type alias/custom type"
            , details = [ "Every top level function/type alias/custom type must be documented" ]
            }
            range
        ]


{-| Declaration visitor
-}
declarationVisitor : Node Declaration -> List (Error {})
declarationVisitor node =
    case Node.value node of
        AliasDeclaration { documentation, generics, name } ->
            List.map Node.range generics
                |> (::) (Node.range name)
                |> Range.combine
                |> throwRuleErrorIfNoDocumentation documentation

        FunctionDeclaration { declaration, documentation } ->
            Node.value declaration
                |> .name
                |> Node.range
                |> throwRuleErrorIfNoDocumentation documentation

        CustomTypeDeclaration { documentation, generics, name } ->
            List.map Node.range generics
                |> (::) (Node.range name)
                |> Range.combine
                |> throwRuleErrorIfNoDocumentation documentation

        _ ->
            []
