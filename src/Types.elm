module Types exposing
    ( Height(..)
    , Width(..)
    , WindowSize
    , handleResult
    , heightToInt
    , widthToInt
    )


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


handleResult : (err -> a) -> (val -> a) -> Result err val -> a
handleResult errCont valCont result =
    case result of
        Ok val ->
            valCont val

        Err err ->
            errCont err
