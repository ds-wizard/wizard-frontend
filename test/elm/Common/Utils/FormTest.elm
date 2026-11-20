module Common.Utils.FormTest exposing (suite)

{-| A field the server refuses says why: in our words when we know the code, in
the server's when we do not, and in general words when it sent none — never
an empty error.
-}

import Common.Api.ApiError as ApiError
import Common.Utils.Form as FormUtils
import Common.Utils.Form.FormError exposing (FormError)
import Expect
import Form exposing (Form)
import Form.Field as Field
import Form.Validate as V
import Gettext
import Json.Encode as E
import Test exposing (Test, describe, test)


appState : { locale : Gettext.Locale }
appState =
    { locale = Gettext.defaultLocale }


suite : Test
suite =
    describe "FormUtils.setFormErrors"
        [ test "a code we know is in our words" <|
            \_ ->
                errorFor "error.validation.km_id_uniqueness" "Something else" []
                    |> Expect.equal (Just "Knowledge model ID is already used.")
        , test "a code we do not know is in the server's words" <|
            \_ ->
                errorFor "error.validation.something_new" "%s '%s' is not right" [ "name", "x" ]
                    |> Expect.equal (Just "name 'x' is not right")
        , test "with no words at all, the value is invalid" <|
            \_ ->
                errorFor "error.validation.something_new" "" []
                    |> Expect.equal (Just "Invalid value.")
        ]


errorFor : String -> String -> List String -> Maybe String
errorFor code defaultMessage params =
    form
        |> FormUtils.setFormErrors appState (ApiError.BadStatus 400 (body code defaultMessage params))
        |> Form.getFieldAsString "name"
        |> .error
        |> Maybe.map (FormUtils.errorToString appState.locale "Name")


form : Form FormError String
form =
    Form.initial [ ( "name", Field.string "x" ) ] (V.field "name" V.string)


body : String -> String -> List String -> String
body code defaultMessage params =
    E.encode 0 <|
        E.object
            [ ( "type", E.string "UserFormError" )
            , ( "formErrors", E.list identity [] )
            , ( "fieldErrors"
              , E.object
                    [ ( "name"
                      , E.list identity
                            [ E.object
                                [ ( "code", E.string code )
                                , ( "defaultMessage", E.string defaultMessage )
                                , ( "params", E.list E.string params )
                                ]
                            ]
                      )
                    ]
              )
            ]
