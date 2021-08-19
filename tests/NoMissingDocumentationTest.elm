module NoMissingDocumentationTest exposing (..)

import Review.Test exposing (ReviewResult)
import NoMissingDocumentation exposing (rule)
import Test exposing (Test)


{-| Rule to test
-}
testRule : String -> ReviewResult
testRule string =
    Review.Test.run rule string


{-| Test cases for NoUndocumentedTopLevelDeclarations rule
-}
tests : Test
tests =
    Test.describe "NoMissingDocumentation"
        [ Test.test "No errors are thrown in case function/type alias/custom type is documented" <|
            \() ->
                testRule """module A exposing (..)

{-| Documentation for custom type
-}
type A =
    String

{-| Documentation for type alias
-}
type alias ThisIsATypeAlias =
    A.B.OtherTypeConstructor SomeOtherTypes

{-| Documentation for function
-}
sum : Int -> Int -> Int
sum a b =
    a + b
"""
                    |> Review.Test.expectNoErrors
        , Test.test "Throws and error when documentation is missing for function/type alias/custom type" <|
            \() ->
                testRule """module A exposing (..)

type AString = String

type Either a b =
    Left a | Right b


type alias RecordType = { a : String, b : Int }

type alias ThisIsATypeAlias phantom =
    A.B.OtherTypeConstructor SomeOtherTypes

sum : Int -> Int -> Int
sum a b =
    a + b

difference a b =
    a - b
"""
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "No undocumented function/type alias/custom type"
                            , under = "AString"
                            , details = [ "Every top level function/type alias/custom type must be documented" ]
                            }
                        , Review.Test.error
                            { message = "No undocumented function/type alias/custom type"
                            , under = "Either a b"
                            , details = [ "Every top level function/type alias/custom type must be documented" ]
                            }
                        , Review.Test.error
                            { message = "No undocumented function/type alias/custom type"
                            , under = "RecordType"
                            , details = [ "Every top level function/type alias/custom type must be documented" ]
                            }
                        , Review.Test.error
                            { message = "No undocumented function/type alias/custom type"
                            , under = "ThisIsATypeAlias phantom"
                            , details = [ "Every top level function/type alias/custom type must be documented" ]
                            }
                        , Review.Test.error
                            { message = "No undocumented function/type alias/custom type"
                            , under = "sum"
                            , details = [ "Every top level function/type alias/custom type must be documented" ]
                            }
                            |> Review.Test.atExactly { start = { row = 15, column = 1 }, end = { row = 15, column = 4 } }
                        , Review.Test.error
                            { message = "No undocumented function/type alias/custom type"
                            , under = "difference"
                            , details = [ "Every top level function/type alias/custom type must be documented" ]
                            }
                        ]
        ]
