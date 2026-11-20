module Common.Api.Models.ProjectQuestionnaireTest exposing (deleteItemTest, isCurrentVersionTest)

import Common.Api.Models.KnowledgeModel as KnowledgeModel
import Common.Api.Models.ProjectDetail.ProjectEvent as ProjectEvent exposing (ProjectEvent)
import Common.Api.Models.ProjectDetail.Reply.ReplyValue exposing (ReplyValue(..))
import Common.Api.Models.ProjectQuestionnaire as ProjectQuestionnaire exposing (ProjectQuestionnaire)
import Common.Components.Questionnaire.Fixture as Fixture
import Dict
import Expect
import Test exposing (Test, describe, test)
import Time
import Uuid exposing (Uuid)


listPath : String
listPath =
    Fixture.chapter1 ++ "." ++ Fixture.list


itemQuestionPath : String -> String
itemQuestionPath itemUuid =
    listPath ++ "." ++ itemUuid ++ "." ++ Fixture.template


{-| Two items, the second's uuid starting with the first's.
-}
withItems : ProjectQuestionnaire
withItems =
    let
        questionnaire =
            Fixture.questionnaire
                (Dict.fromList
                    [ ( listPath, Fixture.reply (ItemListReply [ "item", "item2" ]) )
                    , ( itemQuestionPath "item", Fixture.reply (StringReply "First") )
                    , ( itemQuestionPath "item2", Fixture.reply (StringReply "Second") )
                    ]
                )
    in
    { questionnaire
        | labels =
            Dict.fromList
                [ ( itemQuestionPath "item", [ ProjectQuestionnaire.todoUuid ] )
                , ( itemQuestionPath "item2", [ ProjectQuestionnaire.todoUuid ] )
                ]
    }


deleteItemTest : Test
deleteItemTest =
    describe "ProjectQuestionnaire.deleteItem"
        [ test "takes the item out of the list" <|
            \_ ->
                ProjectQuestionnaire.deleteItem listPath "item" withItems
                    |> .replies
                    |> Dict.get listPath
                    |> Maybe.map .value
                    |> Expect.equal (Just (ItemListReply [ "item2" ]))
        , test "removes the item's replies and labels, and keeps the other item's" <|
            \_ ->
                ProjectQuestionnaire.deleteItem listPath "item" withItems
                    |> (\q -> ( Dict.keys q.replies, Dict.keys q.labels ))
                    |> Expect.equal ( [ listPath, itemQuestionPath "item2" ], [ itemQuestionPath "item2" ] )
        , test "does nothing for an item no longer there" <|
            \_ ->
                ProjectQuestionnaire.deleteItem listPath "gone" withItems
                    |> Expect.equal withItems
        , test "the item's title is found from the list question's template" <|
            \_ ->
                ProjectQuestionnaire.getItemTitle withItems
                    (String.split "." listPath ++ [ "item" ])
                    (KnowledgeModel.getQuestionItemTemplateQuestions Fixture.list withItems.knowledgeModel)
                    |> Expect.equal (Just "First")
        ]


uuid : Int -> Uuid
uuid n =
    Uuid.fromUuidString ("00000000-0000-4000-8000-" ++ String.padLeft 12 '0' (String.fromInt n))


isCurrentVersionTest : Test
isCurrentVersionTest =
    let
        setList n =
            ProjectEvent.SetReply { uuid = uuid n, path = listPath, value = ItemListReply [ "item" ], createdAt = Time.millisToPosix n, createdBy = Nothing }

        deleteItem : Int -> ProjectEvent
        deleteItem n =
            ProjectEvent.DeleteItem { uuid = uuid n, path = listPath, itemUuid = "item", itemTitle = Nothing, createdAt = Time.millisToPosix n, createdBy = Nothing }

        setValue n =
            ProjectEvent.SetReply { uuid = uuid n, path = itemQuestionPath "item", value = StringReply "a", createdAt = Time.millisToPosix n, createdBy = Nothing }
    in
    describe "ProjectQuestionnaire.isCurrentVersion"
        [ test "a deleted item last is the current version" <|
            \_ ->
                ProjectQuestionnaire.isCurrentVersion [ setList 1, setValue 2, deleteItem 3 ] (uuid 3)
                    |> Expect.equal True
        , test "a deleted item makes the change before it no longer current" <|
            \_ ->
                ProjectQuestionnaire.isCurrentVersion [ setList 1, setValue 2, deleteItem 3 ] (uuid 2)
                    |> Expect.equal False
        ]
