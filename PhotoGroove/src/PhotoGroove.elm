-- Next: 3.2.3


module PhotoGroove exposing (main)

import Array exposing (Array)
import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick)
import Random


urlPrefix : String
urlPrefix =
    "http://elm-in-action.com/"


type ThumbnailSize
    = Small
    | Medium
    | Large


type Msg
    = ClickedPhoto String
    | ClickedSize ThumbnailSize
    | ClickedSurpriseMe
    | GotSelectedIndex Int


type alias Photo =
    { url : String }


type Status
    = Loading
    | Loaded (List Photo) String
    | Errored String


type alias Model =
    { status : Status
    , chosenSize : ThumbnailSize
    }


initialModel : Model
initialModel =
    { status = Loading
    , chosenSize = Large
    }


randomPhotoPicker : Random.Generator Int
randomPhotoPicker =
    Random.int 0 (Array.length photoArray - 1)


photoArray : Array Photo
photoArray =
    case initialModel of
        Loaded photos _ ->
            Array.fromList photos

        _ ->
            Array.fromList []


view : Model -> Html Msg
view model =
    div [ class "content" ] <|
        case model.status of
            Loaded photos selectedUrl -> 
                viewLoaded photos selectedUrl model.chosenSize

            Loading -> 
                []

            Errored errorMessage ->
                [ text ("Error:" ++ errorMessage) ]
        
        

viewLoaded : List Photo -> String -> ThumbnailSize -> List (Html Msg)
viewLoaded photos selectedUrl chosenSize = 
    [ h1 [] [ text "Photo Groove" ]
    , button
        [ onClick ClickedSurpriseMe ]
        [ text "Surprise Me!" ]
    , h3 [] [ text "Thumbnail Size:" ]
    , div [ id "choose-size" ]
        (List.map viewSizeChooser [ Small, Medium, Large ])
    , div [ id "thumbnails", class (sizeToString chosenSize) ]
        (List.map (viewThumbnail selectedUrl) photos)
    , img
        [ class "large"
        , src (urlPrefix ++ "large/" ++ selectedUrl)
        ]
        []
    ]


viewThumbnail : String -> Photo -> Html Msg
viewThumbnail selectedUrl thumb =
    img
        [ src (urlPrefix ++ thumb.url)
        , classList [ ( "selected", selectedUrl == thumb.url ) ]
        , onClick (ClickedPhoto thumb.url)
        ]
        []


viewSizeChooser : ThumbnailSize -> Html Msg
viewSizeChooser size =
    label []
        [ input [ type_ "radio", name "size",  onClick (ClickedSize size) ] []
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


getPhotoUrl : Int -> String
getPhotoUrl index =
    case Array.get index photoArray of
        Just photo ->
            photo.url

        Nothing ->
            ""


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPhoto url ->
            ( { model | selectedUrl = url }, Cmd.none )

        ClickedSize size ->
            ( { model | chosenSize = size }, Cmd.none )

        ClickedSurpriseMe ->
            ( { model | selectedUrl = "2.jpeg" }
            , Random.generate GotSelectedIndex randomPhotoPicker
            )

        GotSelectedIndex index ->
            ( { model | selectedUrl = getPhotoUrl index }, Cmd.none )

main : Program () Model Msg
main =
    Browser.element
        { init = \flags -> ( initialModel, Cmd.none )
        , view = view
        , update = update
        , subscriptions = \model -> Sub.none
        }
