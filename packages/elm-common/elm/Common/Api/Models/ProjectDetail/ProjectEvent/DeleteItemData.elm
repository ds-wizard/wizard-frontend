module Common.Api.Models.ProjectDetail.ProjectEvent.DeleteItemData exposing
    ( DeleteItemData
    , decoder
    , encode
    )

import Common.Api.Models.UserSuggestion as UserSuggestion exposing (UserSuggestion)
import Json.Decode as D exposing (Decoder)
import Json.Decode.Extra as D
import Json.Decode.Pipeline as D
import Json.Encode as E
import Json.Encode.Extra as E
import Time
import Uuid exposing (Uuid)


type alias DeleteItemData =
    { uuid : Uuid
    , path : String
    , itemUuid : String
    , itemTitle : Maybe String
    , createdAt : Time.Posix
    , createdBy : Maybe UserSuggestion
    }


encode : DeleteItemData -> E.Value
encode data =
    E.object
        [ ( "type", E.string "DeleteItemEvent" )
        , ( "uuid", Uuid.encode data.uuid )
        , ( "path", E.string data.path )
        , ( "itemUuid", E.string data.itemUuid )
        , ( "itemTitle", E.maybe E.string data.itemTitle )
        ]


decoder : Decoder DeleteItemData
decoder =
    D.succeed DeleteItemData
        |> D.required "uuid" Uuid.decoder
        |> D.required "path" D.string
        |> D.required "itemUuid" D.string
        |> D.optional "itemTitle" (D.maybe D.string) Nothing
        |> D.required "createdAt" D.datetime
        |> D.required "createdBy" (D.maybe UserSuggestion.decoder)
