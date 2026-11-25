module Common.Api.Models.Settings.ZenodoSettings exposing
    ( ZenodoRepositorySettings
    , ZenodoSettings
    , decoder
    , encode
    , initEmptyForm
    , initForm
    , section
    , validation
    )

{-| The OAuth applications registered in Zenodo, one per repository, that users
connect their Zenodo accounts with. The server never returns a client secret: a
secret that is set comes as `********`, and sending `********` back keeps it.
-}

import Common.Utils.Form.FormError exposing (FormError)
import Common.Utils.Form.Validate as V
import Form exposing (Form)
import Form.Field as Field exposing (Field)
import Form.Validate as V exposing (Validation)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Pipeline as D
import Json.Encode as E


type alias ZenodoSettings =
    { sandbox : ZenodoRepositorySettings
    , production : ZenodoRepositorySettings
    }


type alias ZenodoRepositorySettings =
    { enabled : Bool
    , clientId : String
    , clientSecret : String
    }


section : { name : String, decoder : Decoder ZenodoSettings, encode : ZenodoSettings -> E.Value }
section =
    { name = "zenodo", decoder = decoder, encode = encode }



-- JSON


decoder : Decoder ZenodoSettings
decoder =
    D.succeed ZenodoSettings
        |> D.required "sandbox" repositoryDecoder
        |> D.required "production" repositoryDecoder


repositoryDecoder : Decoder ZenodoRepositorySettings
repositoryDecoder =
    D.succeed ZenodoRepositorySettings
        |> D.required "enabled" D.bool
        |> D.required "clientId" D.string
        |> D.required "clientSecret" D.string


encode : ZenodoSettings -> E.Value
encode settings =
    E.object
        [ ( "sandbox", encodeRepository settings.sandbox )
        , ( "production", encodeRepository settings.production )
        ]


encodeRepository : ZenodoRepositorySettings -> E.Value
encodeRepository repository =
    E.object
        [ ( "enabled", E.bool repository.enabled )
        , ( "clientId", E.string repository.clientId )
        , ( "clientSecret", E.string repository.clientSecret )
        ]



-- Form


validation : Validation FormError ZenodoSettings
validation =
    V.succeed ZenodoSettings
        |> V.andMap (V.field "sandbox" validateRepository)
        |> V.andMap (V.field "production" validateRepository)


validateRepository : Validation FormError ZenodoRepositorySettings
validateRepository =
    V.succeed ZenodoRepositorySettings
        |> V.andMap (V.field "enabled" V.bool)
        |> V.andMap (V.field "enabled" V.bool |> V.ifElse "clientId" V.string V.optionalString)
        |> V.andMap (V.field "enabled" V.bool |> V.ifElse "clientSecret" V.string V.optionalString)


initEmptyForm : Form FormError ZenodoSettings
initEmptyForm =
    Form.initial [] validation


initForm : ZenodoSettings -> Form FormError ZenodoSettings
initForm settings =
    Form.initial
        [ ( "sandbox", Field.group (initRepository settings.sandbox) )
        , ( "production", Field.group (initRepository settings.production) )
        ]
        validation


initRepository : ZenodoRepositorySettings -> List ( String, Field )
initRepository repository =
    [ ( "enabled", Field.bool repository.enabled )
    , ( "clientId", Field.string repository.clientId )
    , ( "clientSecret", Field.string repository.clientSecret )
    ]
