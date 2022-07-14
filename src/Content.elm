module Content exposing
    ( aboutEgypt
    , accomodation
    , almostWhite
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
        , Color
        , rgb255
        , rgba255
        )
import Element.Font as Font exposing (Font)
import Types exposing (Width(..))


accomodation : String
accomodation =
    """
The wedding will take place in two different locations in Cairo, Egypt.

For guests travelling from abroad, we recommend to arrive
in the weekend of 29/10 and to stay until 06/11.
During this week, we will organise some day trips to
discover Cairo and the many historical sites surrounding it.

In terms of accommodation, we recommend to stay in the Maadi area in Cairo.
This green, calm and central area is ideally located both to attend
the wedding and to discover downtown Cairo.

If you wish to stay in a hotel, we can recommend the following hotels:
1. [Holiday Inn][holiday_inn]
1. [Pearl][pearl]
1. [Villa Belle Epoque][belle_epoque]

Alternatively, you can also rent a room or apartment on AirBnB.
There are many good options in Cairo, but if you do not know the city,
you can best confirm the location with us before booking.
We also created
[a list with good options][airbnb] on AirBnB,
but make sure to check that the places corresponds to your preferences as well.

[holiday_inn]: https://goo.gl/maps/RVxAZFCCAXZoAxon9
[pearl]: https://goo.gl/maps/7hjch9G8Z6vj39dx7
[belle_epoque]: https://goo.gl/maps/949bc8RyEin4hKTt5
[airbnb]: https://www.airbnb.fr/wishlists/v/1066413648
"""



-- TODO add dress code for evening events


aboutEgypt : String
aboutEgypt =
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

Walking is also an option, if you do not need to go far, and there are many
nice areas to stroll around.
Be mindful of the traffic though when walking in busy streets and
when crossing the street.

[Here's a little teaser][cairo_traffic] in case you are curious.

[cairo_traffic]: https://www.youtube.com/watch?v=3y_NiOvvALc

# Food and drinks

Cairo has everything to offer when it comes to food, and quality and hygiene
standards are very good.
When staying in Maadi, we can recommend the following restaurants:
1. [Lokali][lokali] has a great garden and very nice fresh food.
1. [Ovio][ovio] is great for breakfast or lunch.
1. [Zooba][zooba] has great taameyya, the Egyptian version of falafel, and
   other kinds of "street food".
   The street where Zooba is located, Road 9, offers many more options and is
   one of the main streets in Maadi for food and drinks.
1. [The Platform][platform] is a very nice open-air area overlooking the Nile.
   There are several restaurants located here that are worth checking out.
   Highly recommended for a sunset dinner.
1. [KMT House][kmt_house] a restaurant located in a beautiful old villa in Maadi.
   You can sit outside in their garden for drinks or foods.
1. [Tawlet Yvonne][tawlet] In the same atmosphere as the KMT House, a nice
   and calm garden with very good oriental food.

[lokali]: https://goo.gl/maps/6GxZSeZTnYnXJDzLA
[ovio]: https://goo.gl/maps/zc5cu8QqxQ7mwfvWA
[zooba]: https://goo.gl/maps/tNtdtaLuN5TrfirH6
[platform]: https://goo.gl/maps/CGkjJWM1Ywfcv6Wc6
[kmt_house]: https://goo.gl/maps/yL74KXq4KDxWD5QS9
[tawlet]: https://goo.gl/maps/t1zT7Jg9AHppCMF28

We can recommend a lot more places, also in other areas, so do not hesitate
to ask!

Regarding water, it is best to avoid drinking tap water, and to buy bottled
water instead or to use the water dispenser in your hotel or apartment if
there is one.
Cooking or brushing your teeth with tap water is perfectly fine though.

# Weather

The weather in November should be very pleasant.
Skies are usually clear with plenty of sunshine.
Be aware though that evenings can still be quite cool, so you best pack a
sweater, a light jacket, and long pants as well!

# Dress code

Egypt is a rather conservative country, and so it is advisable to dress rather
modestly.
When walking on the street, it's best to avoid short skirts (above the knee),
and to avoid showing shoulders or cleavage.

# Visas

Most non-Egyptians will require a visa to enter Egypt.
information on Egypt's visa policy can be found on
[this Wikipedia page][egypt_visa_policy].
However, it is best to cross-check this information either with
the travel advisories provided by the foreign affairs ministry of your country
or with the Egyptian embassy.

[egypt_visa_policy]: https://en.wikipedia.org/wiki/Visa_policy_of_Egypt

## On-arrival visa

If you have a passport that makes you eligible for a visa on arrival,
then this is by far the easiest option.
In that case you do not need any documents beside your passport and
the address where you will be staying.
You can buy the on-arrival visa at the airport in Cairo at the counter on
the right just before the immigration checkpoint.
The on-arrival visa will cost you $25 and needs to be paid in cash, either in
euro or in USD.
You will receive a visa sticker,
**do not put the visa in your passport yourself**,
but give it to the officer at the immigration checkpoint, who will put it in your
passport and stamp it.

## e-Visa

Alternatively, you can buy an e-Visa online on the [Egypt e-Visa Portal][evisa].
In the FAQ you will find the list of nationalities eligible for an e-Visa.

## Visa from the embassy

If you are not eligible for either an on-arrival visa or an e-Visa, you will
need to contact the nearest Egyptian embassy to introduce a visa application.
In this case, it is probably best to inform yourself well in advance about the
duration of the procedure.

[evisa]: https://visa2egypt.gov.eg/eVisa/
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


scalingBase : Width -> ( Float, Float, Float ) -> Float
scalingBase (MkWidth width) ( normal, smaller, smallest ) =
    if width > 380 then
        normal

    else if width >= 310 then
        smaller

    else
        smallest


scaleFontSize : Width -> Int -> Int
scaleFontSize width =
    let
        base : Float
        base =
            scalingBase width ( 20, 16, 12 )
    in
    round << Element.modular base 1.15


fontSizeScaled : Width -> Int -> Attribute msg
fontSizeScaled width =
    Font.size << scaleFontSize width


scaleSpacing : Width -> Int -> Int
scaleSpacing width =
    let
        base : Float
        base =
            scalingBase width ( 5, 3, 1 )
    in
    round << Element.modular base 1.15


spacingScaled : Width -> Int -> Attribute msg
spacingScaled width =
    Element.spacing << scaleSpacing width


paddingScaled : Width -> Int -> Attribute msg
paddingScaled width =
    Element.padding << scaleSpacing width


pageMenuButtonPadding : Width -> Int
pageMenuButtonPadding width =
    scaleSpacing width 0


minWindowWidth : Int
minWindowWidth =
    230


maxContentWidth : Int
maxContentWidth =
    800


maxContentTextWidth : Int
maxContentTextWidth =
    700


almostBlack : Color
almostBlack =
    rgb255 68 68 68


blackTransparent : Color
blackTransparent =
    rgba255 0 0 0 0.4



{-
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
-}


darkPink : Color
darkPink =
    rgb255 255 115 142



{-
   lightPink : Color
   lightPink =
       rgb255 255 197 209


   mintGreen : Color
   mintGreen =
       rgb255 200 253 233
-}


darkYellow : Color
darkYellow =
    rgb255 255 233 123



{-
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
-}


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
