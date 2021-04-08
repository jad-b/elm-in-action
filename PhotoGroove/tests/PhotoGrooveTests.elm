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
import Test.Html.Event as Event
import Test.Html.Query as Query
import Test.Html.Selector exposing (attribute, tag, text)



{-
   Decoder Tests
-}


decoderTest : Test
decoderTest =
    fuzz2 string int "title defaults to (untitled)" <|
        \url size ->
            -- List (String, Value)
            [ ( "url", E.string url )
            , ( "size", E.int size )
            ]
                -- Value
                |> E.object
                -- Result Photo Err
                |> D.decodeValue PhotoGroove.photoDecoder
                -- Result String Err
                |> Result.map .title
                |> Expect.equal (Ok "(untitled)")



{-
   Update Tests
-}


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



{-
   View Tests
-}


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


urlFuzzer : Fuzzer (List String)
urlFuzzer =
    Fuzz.intRange 1 5
        |> Fuzz.map urlsFromCount


urlsFromCount : Int -> List String
urlsFromCount urlCount =
    List.range 1 urlCount
        |> List.map (\num -> String.fromInt num ++ ".png")


thumbnailsWork : Test
thumbnailsWork =
    fuzz urlFuzzer "URLs render as thumbnais" <|
        \urls ->
            let
                thumbnailChecks : List (Query.Single msg -> Expectation)
                thumbnailChecks =
                    List.map thumbnailRendered urls
            in
            { initialModel | status = Loaded (List.map photoFromUrl urls) "" }
                |> view
                |> Query.fromHtml
                |> Expect.all thumbnailChecks



{-
   Event tests
-}


clickThumbnail : Test
clickThumbnail =
    fuzz3 urlFuzzer string urlFuzzer "clicking a thumbnail selects it" <|
        \urlsBefore urlToSelect urlsAfter ->
            let
                url =
                    urlToSelect ++ ".jpeg"

                photos =
                    --
                    (urlsBefore ++ url :: urlsAfter)
                        |> List.map photoFromUrl

                srcToClick =
                    urlPrefix ++ url
            in
            { initialModel | status = Loaded photos "" }
                |> view
                |> Query.fromHtml
                |> Query.find [ tag "img", attribute (Attr.src srcToClick) ]
                |> Event.simulate Event.click
                |> Event.expect (ClickedPhoto url)
