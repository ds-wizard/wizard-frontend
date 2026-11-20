module Common.Api.Models.ProjectDetail.ProjectEventTest exposing (deleteItemTest)

import Common.Api.Models.ProjectDetail.ProjectEvent as ProjectEvent exposing (ProjectEvent)
import Expect
import Json.Decode as D
import Json.Encode as E
import Test exposing (Test, describe, test)
import Time
import Uuid


eventJson : String -> String
eventJson itemTitle =
    """
    {
        "type": "DeleteItemEvent",
        "uuid": "00000000-0000-4000-8000-000000000001",
        "path": "chapter.list",
        "itemUuid": "item",
        "itemTitle": ITEM_TITLE,
        "createdAt": "2026-11-19T10:00:00Z",
        "createdBy": null
    }
    """
        |> String.replace "ITEM_TITLE" itemTitle


deleteItem : Maybe String -> ProjectEvent
deleteItem itemTitle =
    ProjectEvent.DeleteItem
        { uuid = Uuid.fromUuidString "00000000-0000-4000-8000-000000000001"
        , path = "chapter.list"
        , itemUuid = "item"
        , itemTitle = itemTitle
        , createdAt = Time.millisToPosix 1795082400000
        , createdBy = Nothing
        }


deleteItemTest : Test
deleteItemTest =
    describe "ProjectEvent.DeleteItem"
        [ test "decodes with a title" <|
            \_ ->
                D.decodeString ProjectEvent.decoder (eventJson "\"First\"")
                    |> Expect.equal (Ok (deleteItem (Just "First")))
        , test "decodes without a title" <|
            \_ ->
                D.decodeString ProjectEvent.decoder (eventJson "null")
                    |> Expect.equal (Ok (deleteItem Nothing))
        , test "encodes as the server expects it" <|
            \_ ->
                ProjectEvent.encode (deleteItem (Just "First"))
                    |> E.encode 0
                    |> Expect.equal "{\"type\":\"DeleteItemEvent\",\"uuid\":\"00000000-0000-4000-8000-000000000001\",\"path\":\"chapter.list\",\"itemUuid\":\"item\",\"itemTitle\":\"First\"}"
        , test "encodes a missing title as null" <|
            \_ ->
                ProjectEvent.encode (deleteItem Nothing)
                    |> D.decodeValue (D.field "itemTitle" (D.null ()))
                    |> Expect.equal (Ok ())
        , test "is visible in the history" <|
            \_ ->
                ProjectEvent.isInvisible (deleteItem Nothing)
                    |> Expect.equal False
        , test "has the list's path" <|
            \_ ->
                ProjectEvent.getPath (deleteItem Nothing)
                    |> Expect.equal (Just "chapter.list")
        ]
