module MdRendering exposing (rawTextToId, viewMarkdown)

import Content
    exposing
        ( defaultFontSizeScale
        , defaultTextSpacing
        , introBackgroundColour
        , spacingScaled
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
        , minimum
        , newTabLink
        , padding
        , paragraph
        , rgb255
        , row
        , shrink
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
    , strong = row [ Font.heavy ]
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
        column [ spacingScaled windowWidth (defaultTextSpacing // 2) ]
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
        \startingIndex listItems ->
            let
                --| Calculate the number of digits in the highest index
                -- in this list
                numberOfIndexDigits : Int
                numberOfIndexDigits =
                    -- We need to add one to get the number of digits
                    (+) 1
                        << floor
                        << logBase 10
                        << toFloat
                        -- Compensate for rounding errors in the logBase function
                        -- >>> logBase 10 1000
                        -- 2.9999999999999996
                        << (+) 1
                    <|
                        List.length listItems
                            + startingIndex

                --| The minimum width of the index element in front of every block
                -- TODO: does this actually work properly for indexes > 10 ?
                minimumIndexWidth : Int
                minimumIndexWidth =
                    let
                        fixedStartAmount : Float
                        fixedStartAmount =
                            1

                        additionalAmount : Float
                        additionalAmount =
                            0.5 * toFloat (numberOfIndexDigits - 1)
                    in
                    floor <|
                        (fixedStartAmount + additionalAmount)
                            * toFloat (defaultFontSizeScale windowWidth)

                viewItem : Int -> List (Element msg) -> Element msg
                viewItem index itemElements =
                    row [ spacingScaled windowWidth 5 ]
                        [ el
                            [ alignTop
                            , width <| minimum minimumIndexWidth shrink
                            ]
                            << text
                          <|
                            String.fromInt (index + startingIndex)
                                ++ ". "
                        , paragraph [ width fill ] itemElements
                        ]
            in
            column [ spacingScaled windowWidth (defaultTextSpacing // 2) ] <|
                List.indexedMap viewItem listItems
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
                Content.fontSizeH1 windowWidth

            Block.H2 ->
                Content.fontSizeH2 windowWidth

            _ ->
                Content.fontSizeH3 windowWidth
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
