module Data.StartEnd exposing (..)

import Env
import Time
import Url


type alias Point =
    { day : Int
    , hour : Int
    , minute : Int
    , second : Int
    }


type alias Date =
    { start : Point
    , end : Point
    }


type alias StringPoint =
    { day : String
    , hour : String
    , minute : String
    , second : String
    }


type alias StringDate =
    { start : StringPoint
    , end : StringPoint
    }


newDate : Date
newDate =
    { start =
        posixToPoint Time.utc (Time.millisToPosix 0)
    , end =
        posixToPoint Time.utc (Time.millisToPosix 0)
    }


toStringPoint : Point -> StringPoint
toStringPoint d =
    { day = String.padLeft 2 '0' (String.fromInt d.day)
    , hour = String.padLeft 2 '0' (String.fromInt d.hour)
    , minute = String.padLeft 2 '0' (String.fromInt d.minute)
    , second = String.padLeft 2 '0' (String.fromInt d.second)
    }


toStringDate : Date -> StringDate
toStringDate d =
    let
        strStart =
            toStringPoint d.start

        strEnd =
            toStringPoint d.end
    in
    { start = strStart, end = strEnd }


posixToPoint : Time.Zone -> Time.Posix -> Point
posixToPoint zone time =
    { day = Time.toDay zone time
    , hour = Time.toHour zone time
    , minute = Time.toMinute zone time
    , second = Time.toSecond zone time
    }


toCompare : Point -> Int
toCompare d =
    d.day * 10000 + d.hour * 100 + d.minute


createLink : Url.Url -> Date -> String
createLink url d =
    Env.makeAppTop url
        ++ "start/"
        ++ String.fromInt d.start.day
        ++ "/"
        ++ String.fromInt d.start.hour
        ++ "/"
        ++ String.fromInt d.start.minute
        ++ "/end/"
        ++ String.fromInt d.end.day
        ++ "/"
        ++ String.fromInt d.end.hour
        ++ "/"
        ++ String.fromInt d.end.minute
        ++ "/"
