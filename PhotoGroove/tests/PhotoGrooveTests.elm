module PhotoGrooveTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Json.Decode as D
import PhotoGroove

decoderTest : Test
decoderTest = 
    test "title defaults to (untitled"
        (\_ -> 
            """
                {"url": "fruits.com", "size": 5}
            """
                |> D.decodeString PhotoGroove.photoDecoder
                |> Expect.equal
                    (Ok { url = "fruits.com", size = 5, title = "(untitled)" })
        )
