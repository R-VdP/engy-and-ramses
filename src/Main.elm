port module Main exposing (Model, Msg, ParsedMarkdown, main)

import Browser
import Browser.Dom as Dom
import Content
    exposing
        ( almostWhite
        , arabicFont
        , blackTransparent
        , darkYellow
        , defaultFontSize
        , fontSizeH1
        , fontSizeScaled
        , introBackgroundColour
        , introFont
        , mainFont
        , mainTitleColour
        , maxContentTextWidth
        , menuFontSize
        , minWindowWidth
        , paddingScaled
        , pageMenuButtonPadding
        , pagePadding
        , pageTitlefontSize
        , poemLines
        , scaleSpacing
        , shadowGrey
        , spacingScaled
        , subtitleColour
        , textColour
        , textSpacing
        , titleColour
        , titleFont
        )
import Element
    exposing
        ( Attribute
        , Element
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
        , paddingEach
        , paddingXY
        , paragraph
        , px
        , rotate
        , row
        , shrink
        , spacing
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
import Html.Attributes as HA
import Json.Decode as JDecode
import Markdown.Block as MdBlock
import Markdown.Parser
import MdRendering exposing (rawTextToId)
import Task
import Types
    exposing
        ( Height(..)
        , Width(..)
        , WindowSize
        , mkWindowSize
        , widthToInt
        )


{-| Receive window.innerWidth and window.innerHeight as a JSON value.
-}
port receiveWindowSize : (JDecode.Value -> msg) -> Sub msg


main : Program JDecode.Value Model Msg
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
      , view = \model -> lazy2 viewIntro model
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
    htmlAttribute << HA.id << titleToId


type Msg
    = GoToPage Page
    | WindowResized WindowSize
    | NoOp


type alias Model =
    { introFullVpId : String
    , windowSize : WindowSize
    }


type alias Event =
    { name : String
    , datetime : String
    , location : String
    , location2 : String
    , mapsUrl : String
    }


type alias Flags =
    Model


init : JDecode.Value -> ( Model, Cmd Msg )
init value =
    let
        defaultFlags : Flags
        defaultFlags =
            { introFullVpId = ""
            , windowSize = mkWindowSize (MkWidth 0) (MkHeight 0)
            }

        mkFlags : String -> WindowSize -> Flags
        mkFlags id size =
            { introFullVpId = id, windowSize = size }

        decodeFlags : JDecode.Value -> Flags
        decodeFlags =
            decodeWithDefault defaultFlags (flagsDecoder mkWindowSize mkFlags)
    in
    ( decodeFlags value, Cmd.none )


decodeWithDefault : a -> JDecode.Decoder a -> JDecode.Value -> a
decodeWithDefault def decoder =
    Result.withDefault def
        << JDecode.decodeValue decoder


flagsDecoder :
    (Width -> Height -> windowSize)
    -> (String -> windowSize -> flags)
    -> JDecode.Decoder flags
flagsDecoder windowSizeCont flagsCont =
    JDecode.map2 flagsCont
        (JDecode.field "introFullVpId" JDecode.string)
        (JDecode.field "windowSize" <| windowSizeDecoder windowSizeCont)


windowSizeDecoder : (Width -> Height -> windowSize) -> JDecode.Decoder windowSize
windowSizeDecoder cont =
    let
        intFieldDecoder : String -> JDecode.Decoder Int
        intFieldDecoder fieldName =
            JDecode.field fieldName JDecode.int
    in
    JDecode.map2 cont
        (JDecode.map MkWidth <| intFieldDecoder "width")
        (JDecode.map MkHeight <| intFieldDecoder "height")


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
    receiveWindowSize <|
        decodeWithDefault NoOp <|
            windowSizeDecoder (\w h -> WindowResized <| mkWindowSize w h)


headerHeight : Width -> Int
headerHeight width =
    menuFontSize width + 2 * pageMenuButtonPadding width


jumpToPage : Width -> Page -> Cmd Msg
jumpToPage windowWidth =
    let
        headerHeightFloat =
            toFloat <| headerHeight windowWidth
    in
    Task.attempt (always NoOp)
        << Task.andThen
            (\info ->
                Dom.setViewport
                    info.element.x
                    (info.element.y - headerHeightFloat)
            )
        << Dom.getElement
        << titleToId
        << .title


mkMarkdownPage : Width -> ParsedMarkdown -> String -> Element Msg
mkMarkdownPage windowWidth parsed title =
    mkStdTxtPage windowWidth title <|
        MdRendering.viewMarkdown windowWidth parsed


mkStdTxtPage : Width -> String -> List (Element Msg) -> Element Msg
mkStdTxtPage windowWidth title =
    mkPage windowWidth title
        << textColumn
            [ width <| maximum maxContentTextWidth fill
            , centerX
            , Font.justify
            , textSpacing windowWidth
            ]


mkPage : Width -> String -> Element Msg -> Element Msg
mkPage windowWidth title content =
    column
        [ titleToIdAttr title
        , width fill
        , padding <| pagePadding windowWidth
        , textSpacing windowWidth
        ]
        [ viewPageTitle windowWidth title
        , el
            [ width fill
            , paddingScaled windowWidth 8
            ]
            content
        ]


viewPageTitle : Width -> String -> Element Msg
viewPageTitle windowWidth title =
    paragraph
        [ alignTop
        , pageTitlefontSize windowWidth
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
    in
    Element.layoutWith { options = [ Element.focusStyle focusStyle ] }
        [ htmlAttribute <| HA.id "main-element"
        , width <| minimum minWindowWidth fill
        , defaultFontSize model.windowSize.width
        , Font.family
            [ mainFont
            , Font.serif
            ]
        ]
        (lazy viewElement model)


menu : Width -> Element Msg
menu windowWidth =
    el
        [ htmlAttribute <| HA.id "main-menu"
        , width fill
        , Background.color blackTransparent

        -- TODO: is there a way to get sticky positioning in elm-ui so that
        --       the menu always sticks to the top of the viewport but
        --       scrolls horizontally with the surrounding container when the
        --       viewport width is less than minWindowWidth ?
        , htmlAttribute <| HA.style "position" "sticky"
        , htmlAttribute <| HA.style "top" "0"
        ]
        << el [ width <| maximum maxContentTextWidth fill, centerX ]
    <|
        row
            [ htmlAttribute <| HA.id "main-menu-button-container"
            , width fill
            , paddingXY (pageMenuButtonPadding windowWidth) 0

            -- We put a space of menuFontSize between the menu elements
            -- This space scales dynamically together with the font size
            , spacing <| menuFontSize windowWidth
            , alignTop
            ]
        <|
            List.map (pageMenuButton windowWidth) pages


pageMenuButton : Width -> Page -> Element Msg
pageMenuButton windowWidth page =
    Input.button
        [ htmlAttribute << HA.id <| "main-menu-button-" ++ pageShortTitle page
        , Border.width 0
        , Font.size <| menuFontSize windowWidth
        , Font.color almostWhite
        , Font.bold
        , centerX
        ]
        { onPress = Just (GoToPage page)
        , label =
            el [ paddingXY 0 <| pageMenuButtonPadding windowWidth, centerX ]
                << text
            <|
                pageShortTitle page
        }


viewElement : Model -> Element Msg
viewElement model =
    column
        [ htmlAttribute <| HA.id "page-container"
        , width fill
        , textSpacing model.windowSize.width
        , Background.color almostWhite
        , Font.color textColour
        , inFront <| lazy menu model.windowSize.width
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
        [ htmlAttribute <| HA.id "poem"
        , width fill
        , fontSizeScaled windowWidth 3
        , Font.family [ arabicFont ]
        , Font.color subtitleColour
        , textSpacing windowWidth
        ]
    <|
        List.map lineToParagraph poemLines


viewIntro : Model -> String -> Element Msg
viewIntro model title =
    let
        windowSize : WindowSize
        windowSize =
            model.windowSize

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
                    [ ( 295, 660 ), ( 550, 630 ) ]

        desertPhoto : Int -> Element Msg
        desertPhoto size =
            mkPhoto size "assets/cropped_desert.webp" "in the dessert" (pi / 24)

        berlinPhoto : Int -> Element Msg
        berlinPhoto size =
            mkPhoto size "assets/berlin.webp" "in berlin" (-pi / 32)

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

        verticalPhotos : () -> Element Msg
        verticalPhotos _ =
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

        smallVerticalPhotos : () -> Element Msg
        smallVerticalPhotos _ =
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

        smallHorizontalPhotos : () -> Element Msg
        smallHorizontalPhotos _ =
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

        horizontalPhotos : () -> Element Msg
        horizontalPhotos _ =
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
    column
        [ titleToIdAttr title
        , width fill
        , paddingEach
            { top = 0
            , left = 0
            , right = 0
            , bottom = pagePadding windowSize.width
            }
        ]
        [ column
            [ width fill
            , htmlAttribute <| HA.style "height" "100vh"
            , htmlAttribute <| HA.style "min-height" "100vh"
            , paddingScaled windowSize.width 5
            , Background.color introBackgroundColour
            , htmlAttribute <| HA.id model.introFullVpId
            ]
            [ el [ height << px <| headerHeight windowSize.width ] Element.none
            , el
                [ centerY
                , width fill
                , height <| fillPortion 2
                , behindContent <|
                    if showLargeVerticalPhotos then
                        verticalPhotos ()

                    else if showSmallVerticalPhotos then
                        smallVerticalPhotos ()

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
                <|
                    horizontalPhotos ()

              else if showSmallHorizontalPhotos then
                el
                    [ width fill
                    , height <| fillPortion 1
                    , paddingScaled windowSize.width 8
                    ]
                <|
                    smallHorizontalPhotos ()

              else
                Element.none
            ]
        , el
            [ width fill
            , height <| px introBaseHeight
            , Background.color introBackgroundColour
            , htmlAttribute <| HA.style "clip-path" "polygon(0 0, 50% 100%, 100% 0)"
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
            el [ width fill ]
                << viewEventSummary windowWidth
    in
    mkPage windowWidth title <|
        column
            [ width <| maximum maxContentTextWidth fill
            , centerX
            , textSpacing windowWidth
            ]
            [ wrappedRow
                [ width fill
                , spacingScaled windowWidth 10
                ]
                [ viewEvent officiationEvent
                , viewEvent partyEvent
                ]
            , textColumn
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
        , textSpacing windowWidth
        , paddingScaled windowWidth 5
        , centerX
        ]
        [ el
            [ fontSizeH1 windowWidth
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
                row
                    [ centerX
                    , spacingScaled windowWidth 1
                    , paddingScaled windowWidth 6
                    , Border.rounded 3
                    , Border.shadow
                        { offset = ( 1, 1 )
                        , size = 1
                        , blur = 0
                        , color = shadowGrey
                        }
                    , Background.color darkYellow
                    , Font.color textColour
                    , Font.regular
                    ]
                    [ el
                        [ htmlAttribute <| HA.class "material-icons"
                        , fontSizeScaled windowWidth 0
                        , Font.family [ Font.typeface "Material Icons Outlined" ]
                        , alignTop
                        ]
                      <|
                        text "place"
                    , el [ alignBottom, Font.bold ] <| text "Open in maps"
                    ]
            }
        ]
