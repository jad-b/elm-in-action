module PhotoGrooveTests exposing (..)

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer, int, list, string)
import Html.Attributes as Attr exposing (src)
import Json.Decode as D
import Json.Encode as E
import PhotoGroove
    exposing
        ( Model
        , Msg(..)
        , Photo
        , Status(..)
        , initialModel
        , update
        , urlPrefix
        , view
        )
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, tag, text)


fuzzyDecoderTest : Test
fuzzyDecoderTest =
    fuzz2 string int "title defaults to (untitled)" <|
        \url size ->
            [ ( "url", E.string url )
            , ( "size", E.int size )
            ]
                -- List (String, Value)
                |> E.object
                -- Value
                |> D.decodeValue PhotoGroove.photoDecoder
                -- Result Photo Err
                |> Result.map .title
                -- Result String Err
                |> Expect.equal (Ok "(untitled)")



-- Expectation


decoderTest : Test
decoderTest =
    test "title defaults to (untitled" <|
        \_ ->
            [ ( "url", E.string "fruits.com" )
            , ( "size", E.int 5 )
            ]
                -- List (String, Value)
                |> E.object
                -- Value
                |> D.decodeValue PhotoGroove.photoDecoder
                -- Result Photo Err
                |> Result.map .title
                -- Result String Err
                |> Expect.equal (Ok "(untitled)")



-- Expectation
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


sliders : Test
sliders =
    describe "Slider sets the desired field in the Model"
        [ testSlider "SlidHue" SlidHue .hue
        , testSlider "SlidRipple" SlidRipple .ripple
        , testSlider "SlidNoise" SlidNoise .noise
        ]


testSlider : String -> (Int -> Msg) -> (Model -> Int) -> Test
testSlider description toMsg amountFromModel =
    fuzz int description <|
        \amount ->
            initialModel
                |> update (toMsg amount)
                |> Tuple.first
                |> amountFromModel
                |> Expect.equal amount



-- View Tests


noPhotosNoThumbnails : Test
noPhotosNoThumbnails =
    test "No thumbnails render when there are no photos to render" <|
        \_ ->
            -- Model
            initialModel
                -- Model -> Html Msg
                |> PhotoGroove.view
                -- Html msg -> Query.Single Msg
                |> Query.fromHtml
                -- List Selector -> Query.Single Msg -> Query.Multiple Msg
                |> Query.findAll [ tag "img" ]
                -- (Int -> Expectation) -> Query.Multiple Msg -> Expectation
                |> Query.count (Expect.equal 0)


thumbnailRendered : String -> Query.Single msg -> Expectation
thumbnailRendered url query =
    query
        |> Query.findAll [ tag "img", attribute (Attr.src (urlPrefix ++ url)) ]
        |> Query.count (Expect.atLeast 1)


photoFromUrl : String -> Photo
photoFromUrl url =
    { url = url, size = 0, title = "" }


thumbnailsWork : Test
thumbnailsWork =
    fuzz (Fuzz.intRange 1 5) "URLs render as thumbnais" <|
        \urlCount ->
            let
                urls : List String
                urls =
                    List.range 1 urlCount
                        |> List.map (\num -> String.fromInt num ++ ".png")

                thumbnailChecks : List (Query.Single msg -> Expectation)
                thumbnailChecks =
                    List.map thumbnailRendered urls
            in
            { initialModel | status = Loaded (List.map photoFromUrl urls) "" }
                |> view
                |> Query.fromHtml
                |> Expect.all thumbnailChecks
