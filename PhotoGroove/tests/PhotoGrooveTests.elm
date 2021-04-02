module PhotoGrooveTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Test exposing (..)
import Json.Decode as D
import Json.Encode as E
import PhotoGroove exposing (Model, Msg(..), Photo, initialModel, update)

fuzzyDecoderTest : Test
fuzzyDecoderTest = 
    fuzz2 string int "title defaults to (untitled)" <| 
        \url size -> 
            [ ("url", E.string url)
            , ("size", E.int size)
            ] -- List (String, Value)
                |> E.object -- Value
                |> D.decodeValue PhotoGroove.photoDecoder -- Result Photo Err
                |> Result.map .title -- Result String Err
                |> Expect.equal (Ok "(untitled)" ) -- Expectation
        


decoderTest : Test
decoderTest = 
    test "title defaults to (untitled" <|
        \_ -> 
            [ ("url", E.string "fruits.com")
            , ("size", E.int 5)
            ] -- List (String, Value)
                |> E.object -- Value
                |> D.decodeValue PhotoGroove.photoDecoder -- Result Photo Err
                |> Result.map .title -- Result String Err
                |> Expect.equal (Ok "(untitled)" ) -- Expectation

-- Update tests
slidHueSetsHue : Test
slidHueSetsHue = 
    fuzz int "SlidHue sets the hue" <|
        \amount ->
            initialModel
                |> update (SlidHue amount) 
                |> Tuple.first
                |> .hue
                |> Expect.equal amount