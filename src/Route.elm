module Route exposing (Route(..), parse)

import Data.StartEnd as StartEnd
import Url exposing (Url)
import Url.Parser exposing (..)


type Route
    = Top
    | Start StartEnd.Date


parse : Url -> Maybe Route
parse url =
    Url.Parser.parse parser url


datePointParser : Parser (StartEnd.Point -> a) a
datePointParser =
    let
        fun a b c =
            StartEnd.Point a b c 0
    in
    map fun (top </> int </> int </> int)


startEndParser : Parser (StartEnd.Date -> a) a
startEndParser =
    map StartEnd.Date (s "start" </> datePointParser </> s "end" </> datePointParser)


parser : Parser (Route -> a) a
parser =
    s "stimer"
        </> oneOf
                [ map Top top
                , map Start startEndParser
                ]
