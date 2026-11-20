module Ui.BadgeTest exposing (suite)

import Html.Attributes as Attr
import Test exposing (Test, describe, test)
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, class, tag, text)
import Ui.Badge as Badge


type Msg
    = Pressed


suite : Test
suite =
    describe "Ui.Badge"
        [ test "a plain badge is a span" <|
            \_ ->
                Badge.warning "update available"
                    |> Badge.toHtml
                    |> Query.fromHtml
                    |> Query.has [ tag "span", class "ui-badge", class "ui-badge--warning", text "update available" ]
        , test "a badge with a URL is drawn by toHtml as a link" <|
            \_ ->
                Badge.warning "update available"
                    |> Badge.withUrl "/import"
                    |> Badge.toHtml
                    |> Query.fromHtml
                    |> Query.has [ tag "a", class "ui-badge--link", attribute (Attr.href "/import"), text "update available" ]
        , test "toLink draws the same link" <|
            \_ ->
                Badge.warning "Outdated KM"
                    |> Badge.toLink "/settings"
                    |> Query.fromHtml
                    |> Query.has [ tag "a", class "ui-badge--link", attribute (Attr.href "/settings"), text "Outdated KM" ]
        , test "an action wins over a URL" <|
            \_ ->
                Badge.warning "update available"
                    |> Badge.withUrl "/import"
                    |> Badge.withAction Pressed
                    |> Badge.toHtml
                    |> Query.fromHtml
                    |> Query.has [ tag "button", class "ui-badge--action" ]
        ]
