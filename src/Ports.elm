port module Ports exposing (..)

import Json.Decode
import Json.Encode


port playSound : Json.Encode.Value -> Cmd msg


port copyString : Json.Encode.Value -> Cmd msg


port openLink : Json.Encode.Value -> Cmd msg
