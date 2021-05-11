port module PhotoGallery exposing
    ( Model
    , Msg(..)
    , Photo
    , Status(..)
    , init
    , initialModel
    , main
    , photoDecoder
    , subscriptions
    , update
    , urlPrefix
    , view
    )

import Browser
import Html exposing (..)
import Html.Attributes as Attr
    exposing
        ( class
        , classList
        , id
        , name
        , src
        , title
        , type_
        )
import Html.Events exposing (on, onClick)
import Http
import Json.Decode as Decode
import Json.Decode.Pipeline exposing (optional, required)
import Json.Encode as Encode
import Random


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


type Msg
    = ClickedPhoto String
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe
    | GotRandomPhoto Photo
    | GotPhotos (Result Http.Error (List Photo))
    | SlidHue Int
    | SlidRipple Int
    | SlidNoise Int
    | GotActivity String
    | Damn String


view : Model -> Html Msg
view model =
    div [ class "content" ] <|
        case model.status of
            Loaded photos selectedUrl ->
                viewLoaded photos selectedUrl model

            Loading ->
                []

            Errored errorMessage ->
                [ text ("Error:" ++ errorMessage) ]


viewFilter : (Int -> Msg) -> String -> Int -> Html Msg
viewFilter toMsg name magnitude =
    div [ class "filter-slider" ]
        [ label [] [ text name ]
        , rangeSlider
            [ Attr.max "11"
            , Attr.property "val" (Encode.int magnitude)
            , onSlide toMsg
            ]
            []
        , label [] [ text (String.fromInt magnitude) ]
        ]


viewLoaded : List Photo -> String -> Model -> List (Html Msg)
viewLoaded photos selectedUrl model =
    [ button
        [ onClick ClickedSurpriseMe ]
        [ text "Surprise Me!" ]
    , div [ class "activity" ]
        [ text model.activity ]
    , div [ class "filters" ]
        [ viewFilter SlidHue "Hue" model.hue
        , viewFilter SlidRipple "Ripple" model.ripple
        , viewFilter SlidNoise "Noise" model.noise
        ]
    , h3 [] [ text "Thumbnail Size:" ]
    , div [ id "choose-size" ]
        (List.map viewSizeChooser [ Small, Medium, Large ])
    , div [ id "thumbnails", class (sizeToString model.chosenSize) ]
        (List.map (viewThumbnail selectedUrl) photos)
    , canvas
        [ id "main-canvas", class "large" ]
        []
    ]


viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl thumb =
    img
        [ src (urlPrefix ++ thumb.url)
        , title (thumb.title ++ " [" ++ String.fromInt thumb.size ++ " KB]")
        , classList [ ( "selected", selectedUrl == thumb.url ) ]
        , onClick (ClickedPhoto thumb.url)
        ]
        []


viewSizeChooser : ThumbnailSize -> Html Msg
viewSizeChooser size =
    label []
        [ input [ type_ "radio", name "size", onClick (ClickedSize size) ] []
        , text (sizeToString size)
        ]


sizeToString : ThumbnailSize -> String
sizeToString size =
    case size of
        Large ->
            "large"

        Medium ->
            "med"

        Small ->
            "small"


type ThumbnailSize
    = Small
    | Medium
    | Large



-- Command port
-- Given a set of arguments, produce a Cmd for the Elm runtime to execute
-- The lack of a 'msg' parameter indicates no data will be returned, as there's
-- not enough information in the type signature to know what it could be


port setFilters : FilterOptions -> Cmd msg



-- Subscription port
-- Given a transform, return the results of applying the transform to the JS
-- event in a Sub.
-- Unlike the `setFilters` command port above, this time `msg` implies non-empty
-- data will be returned, as we know the type from the transform we provide.


port activityChanges : (Decode.Value -> msg) -> Sub msg


type alias FilterOptions =
    { url : String
    , filters : List { name : String, amount : Float }
    }


type alias Photo =
    { url : String
    , size : Int
    , title : String
    }


photoDecoder : Decode.Decoder Photo
photoDecoder =
    Decode.succeed Photo
        |> Json.Decode.Pipeline.required "url" Decode.string
        |> Json.Decode.Pipeline.required "size" Decode.int
        |> optional "title" Decode.string "(untitled)"


type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String


type alias Model =
    { status : Status
    , chosenSize : ThumbnailSize
    , hue : Int
    , ripple : Int
    , noise : Int
    , activity : String
    }


initialModel : Model
initialModel =
    { status = Loading
    , chosenSize = Large
    , hue = 5
    , ripple = 5
    , noise = 5
    , activity = ""
    }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GotRandomPhoto photo ->
            applyFilters { model | status = selectUrl photo.url model.status }

        ClickedPhoto url ->
            applyFilters { model | status = selectUrl url model.status }

        ClickedSize size ->
            ( { model | chosenSize = size }, Cmd.none )

        ClickedSurpriseMe ->
            case model.status of
                Loaded (firstPhoto :: otherPhotos) _ ->
                    Random.uniform firstPhoto otherPhotos
                        |> Random.generate GotRandomPhoto
                        |> Tuple.pair model

                Loaded [] _ ->
                    ( model, Cmd.none )

                Loading ->
                    ( model, Cmd.none )

                Errored errorMessage ->
                    ( model, Cmd.none )

        GotPhotos (Ok photos) ->
            case photos of
                x :: xs ->
                    applyFilters
                        { model
                            | status =
                                case List.head photos of
                                    Just photo ->
                                        Loaded photos photo.url

                                    Nothing ->
                                        Loaded [] ""
                        }

                [] ->
                    ( { model | status = Errored "0 photos found!" }, Cmd.none )

        GotPhotos (Err _) ->
            ( { model | status = Errored "Server Error!" }, Cmd.none )

        SlidHue hue ->
            applyFilters { model | hue = hue }

        SlidRipple ripple ->
            applyFilters { model | ripple = ripple }

        SlidNoise noise ->
            applyFilters { model | noise = noise }

        GotActivity activity ->
            ( { model | activity = activity }
            , Cmd.none
            )

        Damn errMsg ->
            ( { model | status = Errored "Server Error!" }, Cmd.none )


applyFilters : Model -> ( Model, Cmd Msg )
applyFilters model =
    case model.status of
        Loaded photos selectedUrl ->
            let
                filters =
                    [ { name = "Hue", amount = toFloat model.hue / 11 }
                    , { name = "Ripple", amount = toFloat model.ripple / 11 }
                    , { name = "Noise", amount = toFloat model.noise / 11 }
                    ]

                url =
                    urlPrefix ++ "large/" ++ selectedUrl
            in
            ( model, setFilters { url = url, filters = filters } )

        Loading ->
            ( model, Cmd.none )

        Errored errorMessage ->
            ( model, Cmd.none )


selectUrl : String -> Status -> Status
selectUrl url status =
    case status of
        Loaded photos _ ->
            Loaded photos url

        Loading ->
            status

        Errored errorMessage ->
            status


initialCmd : Cmd Msg
initialCmd =
    Http.get
        { url = "http://elm-in-action.com/photos/list.json"
        , expect = Http.expectJson GotPhotos (Decode.list photoDecoder)
        }


main : Program Decode.Value Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        }


init : Decode.Value -> ( Model, Cmd Msg )
init flags =
    let
        activity =
            case Decode.decodeValue Decode.float flags of
                Ok float ->
                    "Initializing Pasta v" ++ String.fromFloat float

                Err _ ->
                    "Error retrieving Pasta version. Oh well."
    in
    ( { initialModel | activity = activity }, initialCmd )


subscriptions : Model -> Sub Msg
subscriptions model =
    let
        -- : Decoder Msg
        msgDecoder =
            Decode.map GotActivity Decode.string

        -- : Decode.Value -> Result Error Msg
        tryDecode =
            Decode.decodeValue msgDecoder

        -- : Result Decode.Error Msg -> Msg
        toMsg res =
            case res of
                Ok msg ->
                    msg

                Err decodeError ->
                    Damn <| Decode.errorToString decodeError
    in
    activityChanges <| tryDecode >> toMsg


rangeSlider : List (Attribute msg) -> List (Html msg) -> Html msg
rangeSlider attributes children =
    node "range-slider" attributes children


onSlide : (Int -> msg) -> Attribute msg
onSlide toMsg =
    Decode.at [ "detail", "userSlidTo" ] Decode.int
        |> Decode.map toMsg
        |> on "slide"
