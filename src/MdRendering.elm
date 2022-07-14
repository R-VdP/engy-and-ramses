module MdRendering exposing (rawTextToId, viewMarkdown)

import Content
    exposing
        ( fontSizeScaled
        , introBackgroundColour
        , spacingScaled
        , textSpacing
        , titleFont
        )
import Element
    exposing
        ( Element
        , alignTop
        , column
        , el
        , fill
        , html
        , htmlAttribute
        , image
        , newTabLink
        , padding
        , paragraph
        , rgb255
        , row
        , shrink
        , spacing
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Html
import Html.Attributes
import Markdown.Block as Block exposing (ListItem(..), Task(..))
import Markdown.Html
import Markdown.Renderer
import Types exposing (Width, handleResult)


viewMarkdown :
    Width
    -> Result String (List Block.Block)
    -> List (Element msg)
viewMarkdown width blocks =
    handleResult
        (List.singleton << text)
        identity
    <|
        Result.andThen (render width) blocks


render : Width -> List Block.Block -> Result String (List (Element msg))
render width =
    Markdown.Renderer.render <| elmUiRenderer width


elmUiRenderer : Width -> Markdown.Renderer.Renderer (Element msg)
elmUiRenderer windowWidth =
    { heading = heading windowWidth
    , paragraph = paragraph []
    , thematicBreak = Element.none
    , text = text
    , strong = row [ Font.heavy, Font.underline ]
    , emphasis = row [ Font.italic ]
    , strikethrough = row [ Font.strike ]
    , codeSpan = text
    , link =
        \{ destination } body ->
            newTabLink []
                { url = destination
                , label = paragraph [ Font.underline ] body
                }
    , hardLineBreak = html <| Html.br [] []
    , image =
        \img ->
            image [ width fill ]
                { src = img.src, description = img.alt }
    , blockQuote =
        column
            [ Border.widthEach { top = 0, right = 0, bottom = 0, left = 10 }
            , padding 10
            , Border.color (rgb255 145 145 145)
            , Background.color (rgb255 245 245 245)
            ]
    , unorderedList =
        column [ spacingScaled windowWidth (textSpacing // 2) ]
            << List.map
                (\(ListItem task children) ->
                    row [ spacingScaled windowWidth 5 ]
                        [ el
                            [ alignTop
                            , width shrink
                            ]
                          <|
                            case task of
                                IncompleteTask ->
                                    Input.defaultCheckbox False

                                CompletedTask ->
                                    Input.defaultCheckbox True

                                NoTask ->
                                    text "• "
                        , paragraph [ width fill ] children
                        ]
                )
    , orderedList =
        \startingIndex ->
            column [ spacingScaled windowWidth (textSpacing // 2) ]
                << List.indexedMap
                    (\index itemBlocks ->
                        row [ spacingScaled windowWidth 5 ]
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
    Width
    ->
        { level : Block.HeadingLevel
        , rawText : String
        , children : List (Element msg)
        }
    -> Element msg
heading windowWidth { level, rawText, children } =
    paragraph
        [ case level of
            Block.H1 ->
                fontSizeScaled windowWidth 4

            Block.H2 ->
                fontSizeScaled windowWidth 2

            _ ->
                fontSizeScaled windowWidth 1
        , Font.bold
        , Font.color introBackgroundColour
        , Font.family [ titleFont ]
        , htmlAttribute
            << Html.Attributes.id
          <|
            rawTextToId rawText
        ]
        children


rawTextToId : String -> String
rawTextToId =
    String.toLower
        << String.join "-"
        << String.split " "
