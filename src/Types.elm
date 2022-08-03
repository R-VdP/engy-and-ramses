module Types exposing
    ( HeadingLevel(..)
    , Height(..)
    , Width(..)
    , WindowSize
    , handleResult
    , heightToInt
    , maximumBy
    , mkWindowSize
    , widthToInt
    )


type HeadingLevel
    = H1
    | H2
    | H3
    | H4


type Width
    = MkWidth Int


widthToInt : Width -> Int
widthToInt (MkWidth w) =
    w


type Height
    = MkHeight Int


heightToInt : Height -> Int
heightToInt (MkHeight w) =
    w


type alias WindowSize =
    { width : Width, height : Height }


mkWindowSize : Width -> Height -> WindowSize
mkWindowSize width height =
    { width = width, height = height }


handleResult : (err -> a) -> (val -> a) -> Result err val -> a
handleResult errCont valCont result =
    case result of
        Ok val ->
            valCont val

        Err err ->
            errCont err


maximumBy : (a -> comparable) -> List a -> Maybe a
maximumBy toComparable l =
    let
        maximumByNonEmpty : a -> List a -> a
        maximumByNonEmpty =
            List.foldl
                (\a b ->
                    if toComparable a >= toComparable b then
                        a

                    else
                        b
                )
    in
    case l of
        [] ->
            Nothing

        x :: xs ->
            Just <| maximumByNonEmpty x xs
