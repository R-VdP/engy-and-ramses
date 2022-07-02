module Main exposing (main)

import Browser
import Browser.Dom as Dom
import Element
    exposing
        ( Attribute
        , Color
        , Element
        , above
        , alignBottom
        , alignLeft
        , alignRight
        , alignTop
        , behindContent
        , centerX
        , centerY
        , column
        , el
        , fill
        , height
        , htmlAttribute
        , image
        , inFront
        , maximum
        , minimum
        , newTabLink
        , onRight
        , padding
        , paragraph
        , px
        , rgb255
        , rgba255
        , rotate
        , row
        , scale
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
import Html exposing (Html)
import Html.Attributes exposing (id)
import Task


main : Program () Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = \model -> { title = "Engy and Ramses", body = [ view model ] }
        }


type Page
    = Home
    | Events
    | Accommodation
    | FAQ


pageTitle : Page -> String
pageTitle page =
    case page of
        Home ->
            "Home"

        Events ->
            "Events"

        Accommodation ->
            "Accomodation"

        FAQ ->
            "Frequently asked questions"


pageTitleShort : Page -> String
pageTitleShort page =
    case page of
        FAQ ->
            "F.A.Q."

        _ ->
            pageTitle page


pageId : Page -> String
pageId page =
    let
        uriName : String
        uriName =
            case page of
                Home ->
                    "home"

                Events ->
                    "events"

                Accommodation ->
                    "accommodation"

                FAQ ->
                    "faq"
    in
    "page-id-" ++ uriName


pageIdAttr : Page -> Attribute Msg
pageIdAttr =
    htmlAttribute << id << pageId


type Msg
    = GoToPage Page
    | NoOp


type Model
    = MkModel


type Event
    = MkEvent
        { name : String
        , datetime : String
        , location : String
        , location2 : String
        , mapsUrl : String
        }


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


white : Color
white =
    rgb255 255 255 255


black : Color
black =
    rgb255 0 0 0


blackTransparent : Color
blackTransparent =
    rgba255 0 0 0 0.4


pastelBlue : Color
pastelBlue =
    rgb255 118 156 172


pastelLightBlue : Color
pastelLightBlue =
    rgb255 182 222 232


pastelYellow : Color
pastelYellow =
    rgb255 249 247 209


init : () -> ( Model, Cmd Msg )
init _ =
    ( MkModel, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        NoOp ->
            ( model, Cmd.none )

        GoToPage page ->
            ( model, jumpToPage page )


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


view : Model -> Html Msg
view model =
    let
        focusStyle =
            { borderColor = Nothing
            , backgroundColor = Nothing
            , shadow = Nothing
            }
    in
    Element.layoutWith { options = [ Element.focusStyle focusStyle ] }
        [ width fill
        , height fill
        , fontSizeScaled 1
        , Font.family
            [ Font.typeface "Cormorant Garamond"
            , Font.serif
            ]
        , inFront menu
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
                , FAQ
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
        , Background.color pastelBlue
        , Font.color pastelYellow
        ]
        [ viewIntro model
        , viewEvents model
        , viewAccomodation model
        , viewFAQ model
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
        , centerX
        , fontSizeScaled 3
        , Font.family [ Font.typeface "Amiri" ]
        , spacingScaled 13
        ]
        [ lineToParagraph poemLine1
        , lineToParagraph poemLine2
        ]


viewIntro : Model -> Element Msg
viewIntro _ =
    let
        desertImg : Element Msg
        desertImg =
            el [ width shrink, height shrink, alignTop ] <|
                image
                    [ alignTop
                    , alignRight
                    , width <| maximum 400 shrink
                    , height <| maximum 400 shrink
                    , rotate <| -pi / 6

                    --, scale 0.2
                    , Border.width <| scaleSpacing 9
                    , Border.color white
                    ]
                    { src = "assets/cropped_desert.jpg"
                    , description = "in the dessert"
                    }
    in
    column
        [ pageIdAttr Home
        , centerX
        , width fill
        , height <| minimum 500 fill
        ]
        [ el [ height << px <| headerHeight ] Element.none
        , el [ centerX, centerY ] <|
            textColumn
                [ centerX
                , width fill
                , height fill
                , spacingScaled 17
                , paddingScaled 0
                ]
                [ paragraph
                    [ Font.center
                    , fontSizeScaled 11
                    , Font.bold
                    , Font.family
                        [ Font.typeface "Tangerine"
                        , Font.serif
                        ]
                    ]
                    [ text "Engy and Ramses" ]
                , viewPoem
                ]
        ]


viewPageTitle : Page -> Element Msg
viewPageTitle page =
    paragraph
        [ alignTop
        , centerX
        , fontSizeScaled 5
        , Font.bold
        , Font.center
        ]
        [ text << pageTitle <| page ]



{-
   nonBreakingSpace : String
   nonBreakingSpace =
       String.fromChar '\u{00A0}'
-}


viewEvents : Model -> Element Msg
viewEvents _ =
    let
        officiationEvent : Event
        officiationEvent =
            MkEvent
                { name = "Officiation"
                , datetime = "3 November 2022, 17h - 20h"
                , location = "Salah Al-Din Al-Ayoubi Citadel"
                , location2 = "Cairo, Egypt"
                , mapsUrl = "https://goo.gl/maps/gQcxFyWGz5HKwtM89"
                }

        partyEvent : Event
        partyEvent =
            MkEvent
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
                , height fill
                , fontSizeScaled 1
                , alignBottom
                ]
                << viewEventSummary
    in
    column
        [ pageIdAttr Events
        , width fill
        , spacingScaled 13
        ]
        [ viewPageTitle Events
        , wrappedRow
            [ width <| maximum 1100 fill
            , height fill
            , centerX
            , paddingScaled 5
            , spacingScaled 14
            ]
            [ viewEvent officiationEvent
            , viewEvent partyEvent
            ]
        ]


viewEventSummary : Event -> Element Msg
viewEventSummary (MkEvent event) =
    column
        [ width shrink
        , height shrink
        , Border.color pastelLightBlue
        , Border.width << scaleSpacing <| 0
        , Border.solid
        , Border.rounded 3
        , spacingScaled 11
        , paddingScaled 5
        , centerX
        ]
        [ el
            [ Font.center
            , centerX
            , fontSizeScaled 5
            , Font.bold
            ]
          <|
            text event.name
        , el [ Font.center, centerX ] <| text event.datetime
        , el [ Font.center, centerX ] <| text event.location
        , el [ Font.center, centerX ] <| text event.location2
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
                    , Background.color pastelYellow
                    , Font.color pastelBlue
                    , Font.regular
                    ]
                <|
                    text "Open in maps"
            }
        ]


viewAccomodation : Model -> Element Msg
viewAccomodation _ =
    let
        content : Element Msg
        content =
            textColumn
                [ width <| maximum 600 fill
                , centerX
                , Font.center
                , fontSizeScaled 2
                , spacingScaled 15
                ]
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
                      """
                    ]
                ]
    in
    column
        [ pageIdAttr Accommodation
        , width fill
        , paddingScaled 11
        , spacingScaled 13
        ]
        [ viewPageTitle Accommodation
        , el
            [ width fill
            , paddingScaled 11
            ]
            content
        ]


viewFAQ : Model -> Element Msg
viewFAQ _ =
    let
        content : Element Msg
        content =
            Element.none
    in
    column
        [ pageIdAttr FAQ
        , width fill
        , paddingScaled 11
        , spacingScaled 13
        ]
        [ viewPageTitle FAQ
        , el
            [ width fill
            , paddingScaled 11
            ]
            content
        ]


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
