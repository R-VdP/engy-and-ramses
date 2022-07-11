module Content exposing (..)

import Element
    exposing
        ( Attribute
        , Color
        , rgb255
        , rgba255
        )
import Element.Font as Font exposing (Font)


accomodationContent : String
accomodationContent =
    """
  The wedding will take place in two different locations in Cairo, Egypt.

  For guests travelling from abroad, we recommend to arrive
  in the weekend of 29/10 and to stay until 06/11.
  During this week, we will organise some day trips to
  discover Cairo and the many historical sites surrounding it.

  In terms of accommodation, we recommend to stay in the Maadi area in Cairo.
  This green, calm and central area is ideally located both to attend
  the wedding and to discover downtown Cairo.

  If you wish to stay in a hotel, we can recommend either the
  [Holiday Inn](https://goo.gl/maps/RVxAZFCCAXZoAxon9) or the
  [Pearl]("https://goo.gl/maps/7hjch9G8Z6vj39dx7") hotels.

  Alternatively, you can also rent a room or apartment on AirBnB.
  There are many good options in Cairo, but if you do not know the city,
  you can best confirm the location with us before booking.
  We also created
  [a list with good options]("https://www.airbnb.fr/wishlists/v/1066413648")
  on AirBnB.
  """


aboutEgyptContent : String
aboutEgyptContent =
    """
  # Transportation

  Cairo is a very big and densely populated city that’s notorious for its traffic.
  The best way to get around in Cairo is either with Uber or by taking the metro.

  For Uber, it is advisable to install the Uber application beforehand and
  to link it with a credit card,
  this will save you a lot of hassle since not all the drivers are very good at
  English.
  The prices for Uber rides are very reasonable, but it is best to avoid the
  rush hours when both prices and the time you will spend in the car can go up
  quite a lot.

  Alternatively, you can use the Cairo Metro.
  The metro is very cheap and does not get stuck in the busy Cairo traffic.
  Be aware that it can also get very busy in rush hour though.
  If your location is not close to a metro station,
  there is still the option to take an Uber for the final part of your journey,
  but to do part of the journey by metro to avoid traffic.

  Walking is also an option, if you do not need to go far.
  Be careful though when walking in busy streets or when crossing the street.

  # Food and drinks

  Cairo has everything to offer when it comes to food, and quality and hygiene
  standards are very good.
  When staying in Maadi, we can recommend the following restaurants:
  1. [Lokali](https://goo.gl/maps/6GxZSeZTnYnXJDzLA) has a great garden and very
     nice fresh food
  1. [Ovio](https://goo.gl/maps/zc5cu8QqxQ7mwfvWA) is great for breakfast or
     lunch
  1. [Zooba](https://goo.gl/maps/tNtdtaLuN5TrfirH6) has great taameyya, the
     Egyptian version of falafel, and other kinds of "street food".
     This street offers many more options and is one of the main streets in
     Maadi for food and drinks

  We can recommend a lot more places, also in other areas, so do not hesitate
  to ask!

  Regarding water, it is best to avoid drinking tap water, and to buy bottled
  water instead or to use the water dispenser in your hotel or apartment if
  there is one.
  Cooking or brushing your teeth with tap water is perfectly fine though.
  """


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
    almostBlack


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


fontSizeScaled : Int -> Attribute msg
fontSizeScaled =
    Font.size << scaleFontSize


scaleSpacing : Int -> Int
scaleSpacing =
    round << Element.modular 5 1.15


spacingScaled : Int -> Attribute msg
spacingScaled =
    Element.spacing << scaleSpacing


paddingScaled : Int -> Attribute msg
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


white : Color
white =
    rgb255 255 255 255


black : Color
black =
    rgb255 0 0 0


almostBlack : Color
almostBlack =
    rgb255 68 68 68


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


lemonYellow : Color
lemonYellow =
    rgb255 255 253 152


darkPink : Color
darkPink =
    rgb255 255 115 142


lightPink : Color
lightPink =
    rgb255 255 197 209


mintGreen : Color
mintGreen =
    rgb255 200 253 233


darkYellow : Color
darkYellow =
    rgb255 255 233 123


darkPastelBlue : Color
darkPastelBlue =
    rgb255 157 197 208


darkMintGreen : Color
darkMintGreen =
    rgb255 141 233 220


pastelRed : Color
pastelRed =
    rgb255 237 135 131


pastelLightBlue2 : Color
pastelLightBlue2 =
    rgb255 184 225 221


darkPastelGreen : Color
darkPastelGreen =
    rgb255 0 129 133


almostWhite : Color
almostWhite =
    rgb255 251 250 248


poemLines : List (List Int)
poemLines =
    [ [ 0x0625
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
    , [ 0x0A
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
    ]