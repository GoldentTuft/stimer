module Env exposing (Env, appName, appTop, makeAppTop)

import Browser.Navigation as Nav
import Url


type alias Env =
    { url : Url.Url
    , key : Nav.Key
    }


appName =
    "stimer"


appTop =
    "https://pokosuko.work/" ++ appName ++ "/"


makeAppTop : Url.Url -> String
makeAppTop url =
    if url.host == "localhost" then
        let
            p =
                Maybe.withDefault 1234 url.port_
        in
        "http://localhost:" ++ String.fromInt p ++ "/" ++ appName ++ "/"

    else
        appTop
