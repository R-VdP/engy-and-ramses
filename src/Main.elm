port module Main exposing (Model, Msg, ParsedMarkdown, main)

import Browser
import Browser.Dom as Dom
import Content
    exposing
        ( almostWhite
        , arabicFont
        , blackTransparent
        , darkYellow
        , fontSizeScaled
        , introBackgroundColour
        , introFont
        , mainFont
        , mainTitleColour
        , maxContentTextWidth
        , maxContentWidth
        , minWindowWidth
        , paddingScaled
        , pageMenuButtonPadding
        , poemLines
        , scaleFontSize
        , scaleSpacing
        , spacingScaled
        , subtitleColour
        , textColour
        , titleColour
        , titleFont
        )
import Element
    exposing
        ( Attribute
        , Element
        , Length
        , alignBottom
        , alignRight
        , alignTop
        , behindContent
        , centerX
        , centerY
        , column
        , el
        , fill
        , fillPortion
        , height
        , htmlAttribute
        , image
        , inFront
        , maximum
        , minimum
        , moveDown
        , moveLeft
        , moveRight
        , moveUp
        , newTabLink
        , padding
        , paddingXY
        , paragraph
        , px
        , rotate
        , shrink
        , text
        , textColumn
        , width
        , wrappedRow
        )
import Element.Background as Background
import Element.Border as Border
import Element.Font as Font
import Element.Input as Input
import Element.Lazy exposing (lazy, lazy2, lazy3)
import Html exposing (Html)
import Html.Attributes exposing (id, style)
import Json.Decode as JDecode
import Markdown.Block as MdBlock
import Markdown.Parser
import MdRendering exposing (rawTextToId)
import Task
import Types exposing (Height(..), Width(..), WindowSize, widthToInt)


{-| Make a request to receive window.innerWidth and window.innerHeight from JS.
-}
port getInnerDimensions : () -> Cmd msg


{-| Receive window.innerWidth and window.innerHeight as a JSON value.
-}
port receiveInnerDimensions : (JDecode.Value -> msg) -> Sub msg


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view =
            \model ->
                { title = "Engy & Ramses"
                , body = [ view model ]
                }
        }


type alias Page =
    { title : String
    , shortTitle : Maybe String -- If Nothing then we simply reuse the main title
    , view : Model -> String -> Element Msg
    }


pages : List Page
pages =
    [ { title = "Home"
      , shortTitle = Nothing
      , view = \model -> lazy2 viewIntro model.windowSize
      }
    , { title = "Events"
      , shortTitle = Nothing
      , view = \model -> lazy2 viewEvents model.windowSize.width
      }
    , { title = "Accommodation"
      , shortTitle = Nothing
      , view = \model -> lazy3 mkMarkdownPage model.windowSize.width accomodationContent
      }
    , { title = "About Egypt"
      , shortTitle = Just "Egypt"
      , view = \model -> lazy3 mkMarkdownPage model.windowSize.width aboutEgyptContent
      }
    ]


type alias ParsedMarkdown =
    Result String (List MdBlock.Block)


accomodationContent : ParsedMarkdown
accomodationContent =
    parseMarkdown Content.accomodation


aboutEgyptContent : ParsedMarkdown
aboutEgyptContent =
    parseMarkdown Content.aboutEgypt


parseMarkdown : String -> ParsedMarkdown
parseMarkdown =
    Result.mapError
        (String.join "\n" << List.map Markdown.Parser.deadEndToString)
        << Markdown.Parser.parse


pageShortTitle : Page -> String
pageShortTitle page =
    Maybe.withDefault page.title page.shortTitle


titleToId : String -> String
titleToId =
    (++) "page-id-" << rawTextToId


titleToIdAttr : String -> Attribute Msg
titleToIdAttr =
    htmlAttribute << id << titleToId


type Msg
    = GoToPage Page
    | WindowResized WindowSize
    | NoOp



-- TODO no need for Model to be a record anymore


type alias Model =
    { windowSize : WindowSize
    }


type alias Event =
    { name : String
    , datetime : String
    , location : String
    , location2 : String
    , mapsUrl : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { windowSize = { width = MkWidth 0, height = MkHeight 0 } }
    , getInnerDimensions ()
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        WindowResized newSize ->
            ( { model | windowSize = newSize }
            , Cmd.none
            )

        GoToPage page ->
            ( model, jumpToPage model.windowSize.width page )


subscriptions : Model -> Sub Msg
subscriptions _ =
    let
        intFieldDecoder : String -> JDecode.Decoder Int
        intFieldDecoder fieldName =
            JDecode.field fieldName JDecode.int

        innerDimensionsDecoder : JDecode.Decoder Msg
        innerDimensionsDecoder =
            JDecode.map2 (\w h -> WindowResized { width = w, height = h })
                (JDecode.map MkWidth <| intFieldDecoder "innerWidth")
                (JDecode.map MkHeight <| intFieldDecoder "innerHeight")

        decodeInnerDimensions : JDecode.Value -> Msg
        decodeInnerDimensions =
            Result.withDefault NoOp
                << JDecode.decodeValue innerDimensionsDecoder
    in
    receiveInnerDimensions decodeInnerDimensions


headerHeight : Width -> Int
headerHeight width =
    menuFontSize width + 2 * pageMenuButtonPadding width


jumpToPage : Width -> Page -> Cmd Msg
jumpToPage windowWidth =
    Task.attempt (always NoOp)
        << Task.andThen
            (\info ->
                Dom.setViewport info.element.x (info.element.y - toFloat (headerHeight windowWidth))
            )
        << Dom.getElement
        << titleToId
        << .title


mkMarkdownPage : Width -> ParsedMarkdown -> String -> Element Msg
mkMarkdownPage windowWidth parsed title =
    mkStdTxtPage windowWidth title <| MdRendering.viewMarkdown windowWidth parsed


mkStdTxtPage : Width -> String -> List (Element Msg) -> Element Msg
mkStdTxtPage windowWidth title =
    mkPage windowWidth title
        << el
            [ width fill
            , paddingScaled windowWidth 11
            ]
        << textColumn
            [ width <| maximum maxContentTextWidth fill
            , centerX
            , Font.justify
            , spacingScaled windowWidth 13
            ]


mkPage : Width -> String -> Element Msg -> Element Msg
mkPage windowWidth title content =
    column
        [ titleToIdAttr title
        , width fill
        , paddingScaled windowWidth 11
        , spacingScaled windowWidth 13
        ]
        [ viewPageTitle windowWidth title
        , content
        ]


viewPageTitle : Width -> String -> Element Msg
viewPageTitle windowWidth title =
    paragraph
        [ alignTop
        , fontSizeScaled windowWidth 7
        , Font.bold
        , Font.center
        , Font.color titleColour
        , Font.family [ titleFont ]
        ]
        [ text <| title ]


view : Model -> Html Msg
view model =
    let
        focusStyle : Element.FocusStyle
        focusStyle =
            { borderColor = Nothing
            , backgroundColor = Nothing
            , shadow = Nothing
            }

        windowWidth : Length
        windowWidth =
            minimum minWindowWidth fill
    in
    Element.layoutWith { options = [ Element.focusStyle focusStyle ] }
        [ width windowWidth
        , fontSizeScaled model.windowSize.width -1
        , Font.family
            [ mainFont
            , Font.serif
            ]
        , inFront << el [ width windowWidth ] <| lazy menu model.windowSize.width
        ]
        (lazy viewElement model)


menu : Width -> Element Msg
menu windowWidth =
    el
        [ width fill
        , Background.color blackTransparent
        ]
    <|
        wrappedRow
            [ centerX
            , alignTop
            , Font.size <| menuFontSize windowWidth
            , Font.color almostWhite
            ]
            (List.map (pageMenuButton windowWidth) pages)


menuFontSize : Width -> Int
menuFontSize width =
    scaleFontSize width 0


pageMenuButton : Width -> Page -> Element Msg
pageMenuButton width page =
    Input.button
        [ Border.width 0
        , Font.bold
        ]
        { onPress = Just (GoToPage page)
        , label =
            el [ padding <| pageMenuButtonPadding width ]
                << text
                << pageShortTitle
            <|
                page
        }


viewElement : Model -> Element Msg
viewElement model =
    column
        [ width fill
        , spacingScaled model.windowSize.width 14
        , Background.color almostWhite
        , Font.color textColour
        ]
        (List.map (\p -> p.view model p.title) pages)


viewPoem : Width -> Element Msg
viewPoem windowWidth =
    let
        lineToStr : List Int -> String
        lineToStr =
            String.fromList << List.map Char.fromCode

        lineToParagraph : List Int -> Element Msg
        lineToParagraph =
            paragraph [ Font.center ] << List.singleton << text << lineToStr
    in
    textColumn
        [ width fill
        , fontSizeScaled windowWidth 3
        , Font.family [ arabicFont ]
        , Font.color subtitleColour
        , spacingScaled windowWidth 13
        ]
    <|
        List.map lineToParagraph poemLines


viewIntro : WindowSize -> String -> Element Msg
viewIntro windowSize title =
    let
        showLargeVerticalPhotos : Bool
        showLargeVerticalPhotos =
            screenSizeLimits windowSize.width
                windowSize.height
                [ ( 1170, 430 ) ]

        showSmallVerticalPhotos : Bool
        showSmallVerticalPhotos =
            not showLargeHorizontalPhotos
                && not showLargeVerticalPhotos
                && screenSizeLimits windowSize.width
                    windowSize.height
                    [ ( 825, 300 ) ]

        showLargeHorizontalPhotos : Bool
        showLargeHorizontalPhotos =
            not showLargeVerticalPhotos
                && screenSizeLimits windowSize.width
                    windowSize.height
                    [ ( 445, 700 ) ]

        showSmallHorizontalPhotos : Bool
        showSmallHorizontalPhotos =
            not showSmallVerticalPhotos
                && not showLargeVerticalPhotos
                && screenSizeLimits
                    windowSize.width
                    windowSize.height
                    [ ( 0, 660 ), ( 550, 630 ) ]

        desertPhoto : Int -> Element Msg
        desertPhoto size =
            mkPhoto size "assets/cropped_desert.jpg" "in the dessert" (pi / 24)

        berlinPhoto : Int -> Element Msg
        berlinPhoto size =
            mkPhoto size "assets/berlin.jpg" "in berlin" (-pi / 32)

        mkPhoto : Int -> String -> String -> Float -> Element Msg
        mkPhoto size src desc angle =
            el
                [ width shrink
                , height shrink
                , Border.width <| scaleSpacing windowSize.width 9
                , Border.color almostWhite
                , Background.color almostWhite
                , rotate angle
                ]
            <|
                image
                    [ width <| maximum size shrink
                    , height <| maximum size shrink
                    ]
                    { src = src
                    , description = desc
                    }

        verticalPhotos : Element Msg
        verticalPhotos =
            el
                [ centerX
                , centerY
                , moveLeft 40
                , moveDown 70
                , alignRight
                , behindContent <|
                    el [ moveUp 150, moveLeft 80 ] <|
                        berlinPhoto 200
                ]
            <|
                desertPhoto 200

        smallVerticalPhotos : Element Msg
        smallVerticalPhotos =
            el
                [ centerX
                , centerY
                , moveLeft 20
                , moveDown 30
                , alignRight
                , behindContent <|
                    el [ moveUp 90, moveLeft 40 ] <|
                        berlinPhoto 120
                ]
            <|
                desertPhoto 120

        smallHorizontalPhotos : Element Msg
        smallHorizontalPhotos =
            el
                [ centerX
                , alignTop
                , moveRight 60
                , moveUp 10
                , inFront <|
                    el [ moveUp 10, moveLeft 130 ] <|
                        berlinPhoto 150
                ]
            <|
                desertPhoto 150

        horizontalPhotos : Element Msg
        horizontalPhotos =
            el
                [ centerX
                , alignTop
                , moveUp 10
                , moveRight 90
                , behindContent <|
                    el [ moveUp 30, moveLeft 180 ] <|
                        berlinPhoto 200
                ]
            <|
                desertPhoto 200

        introBaseHeight : Int
        introBaseHeight =
            ceiling <| toFloat (widthToInt windowSize.width) * tan (pi / 32) / 2
    in
    column [ titleToIdAttr title, width fill ]
        [ column
            [ width fill
            , htmlAttribute <| style "height" "100vh"
            , htmlAttribute <| style "min-height" "100vh"
            , paddingScaled windowSize.width 5
            , Background.color introBackgroundColour
            ]
            [ el [ height << px <| headerHeight windowSize.width ] Element.none
            , el
                [ centerY
                , width fill
                , height <| fillPortion 2
                , behindContent <|
                    if showLargeVerticalPhotos then
                        verticalPhotos

                    else if showSmallVerticalPhotos then
                        smallVerticalPhotos

                    else
                        Element.none
                ]
              <|
                textColumn
                    [ centerY
                    , width fill
                    , spacingScaled windowSize.width 17
                    ]
                    [ paragraph
                        [ Font.center
                        , fontSizeScaled windowSize.width 11
                        , Font.color mainTitleColour
                        , Font.bold
                        , Font.family
                            [ introFont
                            , Font.serif
                            ]
                        ]
                        [ text "Engy & Ramses" ]
                    , viewPoem windowSize.width
                    , paragraph
                        [ Font.center
                        , fontSizeScaled windowSize.width 3
                        , Font.color subtitleColour
                        , Font.family
                            [ titleFont
                            , Font.serif
                            ]
                        ]
                        [ text <|
                            "We would like you to join us "
                                ++ "in celebrating our marriage"
                        ]
                    ]

            -- We pin the pictures to the bottom.
            -- Alternatively, we can set the textColumn and this element
            -- to height fill and center the pictures in their surrounding
            -- element.
            , if showLargeHorizontalPhotos then
                el
                    [ width fill
                    , height <| fillPortion 1
                    , paddingScaled windowSize.width 8
                    ]
                    horizontalPhotos

              else if showSmallHorizontalPhotos then
                el
                    [ width fill
                    , height <| fillPortion 1
                    , paddingScaled windowSize.width 8
                    ]
                    smallHorizontalPhotos

              else
                Element.none
            ]
        , el
            [ width fill
            , height <| px introBaseHeight
            , Background.color introBackgroundColour
            , htmlAttribute <| style "clip-path" "polygon(0 0, 50% 100%, 100% 0)"
            ]
            Element.none
        ]


screenSizeLimits : Width -> Height -> List ( Int, Int ) -> Bool
screenSizeLimits (MkWidth width) (MkHeight height) =
    List.any (\( w, h ) -> width >= w && height >= h)


viewEvents : Width -> String -> Element Msg
viewEvents windowWidth title =
    let
        officiationEvent : Event
        officiationEvent =
            { name = "Officiation"
            , datetime = "3 November 2022, 17h - 20h"
            , location = "Salah Al-Din Al-Ayoubi Citadel"
            , location2 = "Cairo, Egypt"
            , mapsUrl = "https://goo.gl/maps/gQcxFyWGz5HKwtM89"
            }

        partyEvent : Event
        partyEvent =
            { name = "Party"
            , datetime = "5 November 2022, 16h - 22h"
            , location = "Taracina Wedding on the Nile"
            , location2 = "Giza, Egypt"
            , mapsUrl = "https://goo.gl/maps/b4oi6ZFC8ouFvU7x6"
            }

        viewEvent : Event -> Element Msg
        viewEvent =
            el
                [ width fill
                , paddingXY (scaleSpacing windowWidth 11)
                    (scaleSpacing windowWidth 8)
                ]
                << viewEventSummary windowWidth
    in
    mkPage windowWidth title <|
        column
            [ width <| maximum maxContentWidth fill
            , centerX
            , spacingScaled windowWidth 13
            ]
            [ wrappedRow [ width fill ]
                [ viewEvent officiationEvent
                , viewEvent partyEvent
                ]
            , el [ width fill, paddingScaled windowWidth 11 ] <|
                textColumn
                    [ width <| maximum maxContentTextWidth fill
                    , centerX
                    , Font.justify
                    ]
                    [ paragraph []
                        [ text "More info on how to get to the events will "
                        , text "follow closer to the date."
                        ]
                    ]
            ]


viewEventSummary : Width -> Event -> Element Msg
viewEventSummary windowWidth event =
    column
        [ width <| maximum 290 fill
        , height shrink
        , Border.color introBackgroundColour
        , Border.width <| scaleSpacing windowWidth 0
        , Border.solid
        , Border.rounded 3
        , spacingScaled windowWidth 11

        -- TODO can we replace this by spacing on the row containing these?
        , paddingScaled windowWidth 5
        , centerX
        ]
        [ el
            [ fontSizeScaled windowWidth 4
            , Font.bold
            , Font.color introBackgroundColour
            , Font.family [ titleFont ]
            , centerX
            ]
          <|
            text event.name
        , el [ centerX ] <| text event.datetime
        , el [ centerX ] <| text event.location
        , el [ centerX ] <| text event.location2
        , newTabLink
            [ Border.width 0
            , alignBottom
            , centerX
            ]
            { url = event.mapsUrl
            , label =
                el
                    [ centerX
                    , paddingScaled windowWidth 6
                    , Background.color darkYellow
                    , Font.color textColour
                    , Font.regular
                    ]
                <|
                    text "Open in maps"
            }
        ]
