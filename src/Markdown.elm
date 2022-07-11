module Markdown exposing (..)

import Content exposing (fontSizeScaled)
import Element
    exposing
        ( Element
        , alignTop
        , column
        , el
        , fill
        , newTabLink
        , paragraph
        , row
        , shrink
        , spacing
        , text
        , width
        )
import Element.Background
import Element.Border
import Element.Font as Font
import Element.Input
import Html
import Html.Attributes
import Markdown.Block as Block exposing (ListItem(..), Task(..))
import Markdown.Html
import Markdown.Parser
import Markdown.Renderer


markdownView_ : String -> List (Element msg)
markdownView_ md =
    case markdownView md of
        Ok els ->
            els

        Err err ->
            [ text err ]


markdownView : String -> Result String (List (Element msg))
markdownView =
    Result.andThen (Markdown.Renderer.render elmUiRenderer)
        << Result.mapError (String.join "\n" << List.map Markdown.Parser.deadEndToString)
        << Markdown.Parser.parse


elmUiRenderer : Markdown.Renderer.Renderer (Element msg)
elmUiRenderer =
    { heading = heading
    , paragraph = paragraph []
    , thematicBreak = Element.none
    , text = text
    , strong = row [ Font.bold ]
    , emphasis = row [ Font.italic ]
    , strikethrough = row [ Font.strike ]
    , codeSpan = text
    , link =
        \{ destination } body ->
            newTabLink []
                { url = destination
                , label = paragraph [ Font.underline ] body
                }
    , hardLineBreak = Element.html <| Html.br [] []
    , image =
        \image ->
            Element.image [ Element.width Element.fill ]
                { src = image.src, description = image.alt }
    , blockQuote =
        column
            [ Element.Border.widthEach { top = 0, right = 0, bottom = 0, left = 10 }
            , Element.padding 10
            , Element.Border.color (Element.rgb255 145 145 145)
            , Element.Background.color (Element.rgb255 245 245 245)
            ]
    , unorderedList =
        column [ Element.spacing 15 ]
            << List.map
                (\(ListItem task children) ->
                    row [ spacing 5 ]
                        [ row
                            [ Element.alignTop ]
                            ((case task of
                                IncompleteTask ->
                                    Element.Input.defaultCheckbox False

                                CompletedTask ->
                                    Element.Input.defaultCheckbox True

                                NoTask ->
                                    text "•"
                             )
                                :: text " "
                                :: children
                            )
                        ]
                )
    , orderedList =
        \startingIndex ->
            column [ spacing 15 ]
                << List.indexedMap
                    (\index itemBlocks ->
                        row [ spacing 5 ]
                            [ el
                                [ alignTop
                                , width shrink
                                ]
                                << text
                              <|
                                String.fromInt (index + startingIndex)
                                    ++ ". "
                            , paragraph [ width fill ] itemBlocks
                            ]
                    )
    , codeBlock = text << .body
    , html = Markdown.Html.oneOf []
    , table = column []
    , tableHeader = column []
    , tableBody = column []
    , tableRow = row []
    , tableHeaderCell = always <| paragraph []
    , tableCell = always <| paragraph []
    }


heading :
    { level : Block.HeadingLevel
    , rawText : String
    , children : List (Element msg)
    }
    -> Element msg
heading { level, rawText, children } =
    Element.paragraph
        [ case level of
            Block.H1 ->
                fontSizeScaled 3

            Block.H2 ->
                fontSizeScaled 2

            _ ->
                fontSizeScaled 1
        , Font.bold
        , Element.htmlAttribute
            (Html.Attributes.attribute "name" (rawTextToId rawText))
        , Element.htmlAttribute
            (Html.Attributes.id (rawTextToId rawText))
        ]
        children


rawTextToId : String -> String
rawTextToId =
    String.toLower
        << String.join "-"
        << String.split " "
