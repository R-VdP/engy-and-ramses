module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Browser.Events as BrowserE
import Colours
    exposing
        ( almostWhite
        , black
        , blackTransparent
        , darkPastelGreen
        , darkPink
        , darkYellow
        , white
        )
import Element
    exposing
        ( Attribute
        , Color
        , Element
        , Length
        , alignBottom
        , alignRight
        , alignTop
        , behindContent
        , centerX
        , centerY
        , clip
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
import Element.Font as Font exposing (Font)
import Element.Input as Input
import Html exposing (Html)
import Html.Attributes exposing (id)
import Task


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = \model -> { title = "Engy & Ramses", body = [ view model ] }
        }


type Page
    = Home
    | Events
    | Accommodation
    | AboutEgypt


pageTitle : Page -> String
pageTitle page =
    case page of
        Home ->
            "Home"

        Events ->
            "Events"

        Accommodation ->
            "Accomodation"

        AboutEgypt ->
            "About Egypt"


pageTitleShort : Page -> String
pageTitleShort page =
    case page of
        AboutEgypt ->
            "Egypt"

        _ ->
            pageTitle page


pageId : Page -> String
pageId =
    (++) "page-id-" << String.toLower << String.replace " " "-" << pageTitle


pageIdAttr : Page -> Attribute Msg
pageIdAttr =
    htmlAttribute << id << pageId


type alias WindowSize =
    { width : Int, height : Int }


type Msg
    = GoToPage Page
    | WindowResized WindowSize
    | NoOp


type alias Model =
    { windowSize : WindowSize }


type alias Event =
    { name : String
    , datetime : String
    , location : String
    , location2 : String
    , mapsUrl : String
    }


introBackgroundColour : Color
introBackgroundColour =
    darkPastelGreen


mainTitleColour : Color
mainTitleColour =
    darkYellow


subtitleColour : Color
subtitleColour =
    almostWhite


titleColour : Color
titleColour =
    darkPink


textColour : Color
textColour =
    black


mainFont : Font
mainFont =
    Font.typeface "Arima Madurai"


titleFont : Font
titleFont =
    Font.typeface "Dancing Script"


introFont : Font
introFont =
    Font.typeface "The Nautigal"


arabicFont : Font
arabicFont =
    Font.typeface "Gulzar"


scaleFontSize : Int -> Int
scaleFontSize =
    round << Element.modular 20 1.15


fontSizeScaled : Int -> Attribute Msg
fontSizeScaled =
    Font.size << scaleFontSize


scaleSpacing : Int -> Int
scaleSpacing =
    round << Element.modular 5 1.15


spacingScaled : Int -> Attribute Msg
spacingScaled =
    Element.spacing << scaleSpacing


paddingScaled : Int -> Attribute Msg
paddingScaled =
    Element.padding << scaleSpacing


menuFontSize : Int
menuFontSize =
    scaleFontSize 0


pageMenuButtonPadding : Int
pageMenuButtonPadding =
    scaleSpacing 0


maxContentWidth : Int
maxContentWidth =
    800


maxContentTextWidth : Int
maxContentTextWidth =
    700


init : () -> ( Model, Cmd Msg )
init _ =
    let
        {- We use floor since Browser.Event.onResize returns only Ints.
           We will only use this info to estimate the size of the window,
           so the loss of accuracy is not really important here.
        -}
        handleViewportInfo : Dom.Viewport -> Msg
        handleViewportInfo vp =
            WindowResized
                { width = floor vp.viewport.width
                , height = floor vp.viewport.height
                }
    in
    ( { windowSize = { width = 0, height = 0 } }
    , Task.perform handleViewportInfo Dom.getViewport
    )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        updateSizeInfo : Model -> Int -> Int -> Model
        updateSizeInfo { windowSize } newWidth newHeight =
            case windowSize of
                { height } ->
                    let
                        heightDelta =
                            abs (height - newHeight)

                        realNewHeight =
                            if heightDelta <= 100 then
                                height

                            else
                                newHeight
                    in
                    { windowSize =
                        { width = newWidth
                        , height = realNewHeight
                        }
                    }
    in
    case msg of
        NoOp ->
            ( model, Cmd.none )

        WindowResized { width, height } ->
            ( updateSizeInfo model width height, Cmd.none )

        GoToPage page ->
            ( model, jumpToPage page )


subscriptions : Model -> Sub Msg
subscriptions _ =
    BrowserE.onResize
        (\w h ->
            WindowResized { width = w, height = h }
        )


headerHeight : Int
headerHeight =
    menuFontSize + 2 * pageMenuButtonPadding


jumpToPage : Page -> Cmd Msg
jumpToPage =
    Task.attempt (\_ -> NoOp)
        << Task.andThen
            (\info ->
                Dom.setViewport info.element.x (info.element.y - toFloat headerHeight)
            )
        << Dom.getElement
        << pageId


mkStdTxtPage : Page -> List (Element Msg) -> Element Msg
mkStdTxtPage page =
    mkPage page
        << el
            [ width fill
            , paddingScaled 11
            ]
        << textColumn
            [ width <| maximum maxContentTextWidth fill
            , centerX
            , Font.justify
            , spacingScaled 15
            ]


mkPage : Page -> Element Msg -> Element Msg
mkPage page content =
    column
        [ pageIdAttr page
        , width fill
        , paddingScaled 11
        , spacingScaled 13
        ]
        [ viewPageTitle page
        , content
        ]


viewPageTitle : Page -> Element Msg
viewPageTitle page =
    paragraph
        [ alignTop
        , fontSizeScaled 7
        , Font.bold
        , Font.center
        , Font.color titleColour
        , Font.family [ titleFont ]
        ]
        [ text << pageTitle <| page ]


view : Model -> Html Msg
view model =
    let
        focusStyle =
            { borderColor = Nothing
            , backgroundColor = Nothing
            , shadow = Nothing
            }

        windowWidth : Length
        windowWidth =
            minimum 350 fill
    in
    Element.layoutWith { options = [ Element.focusStyle focusStyle ] }
        [ width windowWidth
        , fontSizeScaled -1
        , Font.family
            [ mainFont
            , Font.serif
            ]
        , inFront <| el [ width windowWidth ] menu
        ]
        (viewElement model)


menu : Element Msg
menu =
    el
        [ width fill
        , Background.color blackTransparent
        ]
    <|
        wrappedRow
            [ centerX
            , alignTop
            , Font.size menuFontSize
            , Font.color white
            ]
            (List.map pageMenuButton
                [ Home
                , Events
                , Accommodation
                , AboutEgypt
                ]
            )


pageMenuButton : Page -> Element Msg
pageMenuButton page =
    Input.button
        [ Border.width 0
        , Font.bold
        ]
        { onPress = Just (GoToPage page)
        , label =
            el [ padding pageMenuButtonPadding ] << text << pageTitleShort <| page
        }


viewElement : Model -> Element Msg
viewElement model =
    column
        [ width fill
        , spacingScaled 14
        , Background.color white
        , Font.color textColour
        ]
        [ viewIntro model
        , viewEvents model
        , viewAccomodation model
        , viewAboutEgypt model
        ]


viewPoem : Element Msg
viewPoem =
    let
        lineToStr =
            String.fromList << List.map Char.fromCode

        lineToParagraph =
            paragraph [ Font.center ] << List.singleton << text << lineToStr
    in
    textColumn
        [ width fill
        , fontSizeScaled 3
        , Font.family [ arabicFont ]
        , Font.color subtitleColour
        , spacingScaled 13
        ]
        [ lineToParagraph poemLine1
        , lineToParagraph poemLine2
        ]


viewIntro : Model -> Element Msg
viewIntro { windowSize } =
    let
        showVerticalPictures : Bool
        showVerticalPictures =
            screenSizeLimits windowSize.width
                windowSize.height
                [ ( 1170, 430 ) ]

        showHorizontalPictures : Bool
        showHorizontalPictures =
            not showVerticalPictures
                && screenSizeLimits windowSize.width
                    windowSize.height
                    [ ( 445, 700 ) ]

        showSmallPictures : Bool
        showSmallPictures =
            not showVerticalPictures
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
                , Border.width <| scaleSpacing 9
                , Border.color white
                , Background.color white
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

        smallPhotos : Element Msg
        smallPhotos =
            el
                [ centerX
                , centerY
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
                , centerY
                , moveUp 10
                , moveRight 90
                , behindContent <|
                    el [ moveUp 30, moveLeft 180 ] <|
                        berlinPhoto 200
                ]
            <|
                desertPhoto 200

        {- The angled bottom of the intro page is made from two rectangles
           each filling one half of the screen width.
           They are rotated by an angle around the bottom midpoint of a
           rectangle at the bottom of the intro page.
           In this section, we calculate the needed lengths to construct these
           two rectangles.
        -}
        introRectAngle =
            pi / 32

        introBaseHeight =
            ceiling <| toFloat windowSize.width * tan introRectAngle / 2

        introRectHeight =
            (+) 1 << ceiling <| toFloat windowSize.width * sin introRectAngle / 2

        introRectWidth =
            ceiling <| toFloat windowSize.width / (2 * cos introRectAngle)

        introRectMoveRightDelta =
            toFloat windowSize.width / 2 * (1 - (tan introRectAngle * sin introRectAngle))

        mkHalfWidthRect : Float -> Float -> Element Msg
        mkHalfWidthRect rotateAngle mvRightDelta =
            el
                [ width <| px introRectWidth
                , height <| px introRectHeight
                , Background.color introBackgroundColour
                , rotate rotateAngle
                , moveUp <| toFloat introRectHeight / 2
                , moveRight mvRightDelta
                ]
                Element.none
    in
    column [ pageIdAttr Home, width fill ]
        [ column
            [ width fill
            , height << px <| windowSize.height
            , paddingScaled 5
            , Background.color introBackgroundColour
            ]
            [ el [ height <| px headerHeight ] Element.none
            , el
                [ centerY
                , width fill
                , height <| fillPortion 3
                , behindContent <|
                    if showVerticalPictures then
                        verticalPhotos

                    else
                        Element.none
                ]
              <|
                textColumn
                    [ centerY
                    , width fill
                    , spacingScaled 17
                    ]
                    [ paragraph
                        [ Font.center
                        , fontSizeScaled <|
                            if windowSize.width >= 380 then
                                11

                            else
                                9
                        , Font.color mainTitleColour
                        , Font.bold
                        , Font.family
                            [ introFont
                            , Font.serif
                            ]
                        ]
                        [ text "Engy & Ramses" ]
                    , viewPoem
                    , paragraph
                        [ Font.center
                        , fontSizeScaled 3
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
            , if showHorizontalPictures then
                el [ width fill, height <| fillPortion 1, paddingScaled 8 ] horizontalPhotos

              else if showSmallPictures then
                el [ width fill, height <| fillPortion 1, paddingScaled 8 ] smallPhotos

              else
                Element.none
            ]
        , el
            [ width fill
            , height <| px introBaseHeight
            , clip
            , behindContent <|
                mkHalfWidthRect introRectAngle 0
            , behindContent <|
                mkHalfWidthRect (negate introRectAngle) introRectMoveRightDelta
            ]
            Element.none
        ]


screenSizeLimits : Int -> Int -> List ( Int, Int ) -> Bool
screenSizeLimits windowWidth windowHeight =
    List.any (\( w, h ) -> windowWidth >= w && windowHeight >= h)


viewEvents : Model -> Element Msg
viewEvents _ =
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
                , paddingXY (scaleSpacing 11) (scaleSpacing 8)
                ]
                << viewEventSummary
    in
    mkPage Events <|
        wrappedRow
            [ width <| maximum maxContentWidth fill
            , centerX
            ]
            [ viewEvent officiationEvent
            , viewEvent partyEvent
            ]


viewEventSummary : Event -> Element Msg
viewEventSummary event =
    column
        [ width <| maximum 290 fill
        , height shrink
        , Border.color introBackgroundColour
        , Border.width << scaleSpacing <| 0
        , Border.solid
        , Border.rounded 3
        , spacingScaled 11

        -- TODO can we replace this by spacing on the row containing these?
        , paddingScaled 5
        , centerX
        ]
        [ el
            [ fontSizeScaled 4
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
                    , paddingScaled 6
                    , Background.color darkYellow
                    , Font.color textColour
                    , Font.regular
                    ]
                <|
                    text "Open in maps"
            }
        ]


viewAccomodation : Model -> Element Msg
viewAccomodation _ =
    let
        content : List (Element Msg)
        content =
            [ paragraph []
                [ text """
                      The wedding will take place in two different locations in
                      Cairo, Egypt.
                      """
                ]
            , paragraph []
                [ text """
                      For guests travelling from abroad, we recommend to arrive
                      in the weekend of 29/11 and to stay until 06/11.
                      During this week, we will organise some day trips to
                      discover Cairo and the many historical sites surrounding it.
                      """
                ]
            , paragraph []
                [ text """
                      In terms of accommodation, we recommend to stay in the
                      Maadi area in Cairo.
                      This green, calm and central area is ideally located both
                      to attend the wedding and to discover downtown Cairo.
                      """
                ]
            , paragraph []
                [ text """
                      If you wish to stay in a hotel, we can recommend either the
                      """
                , text " "
                , newTabLink []
                    { url = "https://goo.gl/maps/RVxAZFCCAXZoAxon9"
                    , label =
                        el [ Font.underline ] <| text "Holiday Inn"
                    }
                , text " or the "
                , newTabLink []
                    { url = "https://goo.gl/maps/7hjch9G8Z6vj39dx7"
                    , label =
                        el [ Font.underline ] <| text "Pearl"
                    }
                , text " hotels."
                ]
            , paragraph []
                [ text """
                      Alternatively, you can also rent a room or apartment on
                      AirBnB.
                      There are many good options in Cairo, but if you do not
                      know the city, you can best confirm the location with us
                      before booking.
                      We also created
                      """
                , text " "
                , newTabLink []
                    { url = "https://www.airbnb.fr/wishlists/v/1066413648"
                    , label =
                        el [ Font.underline ] <| text "a list with good options"
                    }
                , text " on AirBnB."
                ]
            ]
    in
    mkStdTxtPage Accommodation content


viewAboutEgypt : Model -> Element Msg
viewAboutEgypt _ =
    let
        content : List (Element Msg)
        content =
            [ paragraph [] [ text "Coming soon..." ] ]
    in
    mkStdTxtPage AboutEgypt content


poemLine1 : List Int
poemLine1 =
    [ 0x0625
    , 0x0646
    , 0x20
    , 0x063A
    , 0x0627
    , 0x0628
    , 0x064E
    , 0x20
    , 0x0639
    , 0x0646
    , 0x064A
    , 0x20
    , 0x0641
    , 0x0627
    , 0x0644
    , 0x0631
    , 0x0648
    , 0x062D
    , 0x064F
    , 0x20
    , 0x0645
    , 0x064E
    , 0x0633
    , 0x0643
    , 0x0646
    , 0x0647
    , 0x064F
    ]


poemLine2 : List Int
poemLine2 =
    [ 0x0A
    , 0x0645
    , 0x064E
    , 0x0646
    , 0x20
    , 0x064A
    , 0x0633
    , 0x0643
    , 0x0646
    , 0x064F
    , 0x20
    , 0x0627
    , 0x0644
    , 0x0631
    , 0x0648
    , 0x062D
    , 0x20
    , 0x0643
    , 0x064A
    , 0x0641
    , 0x20
    , 0x0627
    , 0x0644
    , 0x0642
    , 0x0644
    , 0x0628
    , 0x064F
    , 0x20
    , 0x064A
    , 0x0646
    , 0x0633
    , 0x0627
    , 0x0647
    ]
