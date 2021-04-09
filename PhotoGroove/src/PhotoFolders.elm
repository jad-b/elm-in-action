module PhotoFolders exposing (main)

import Browser
import Dict exposing (Dict)
import Html exposing (..)
import Html.Attributes exposing (class, src)
import Html.Events exposing (onClick)
import Http
import Json.Decode as Decode exposing (Decoder, int, list, string)
import Json.Decode.Pipeline exposing (required)


type alias Model =
    { selectedPhotoUrl : Maybe String
    , photos : Dict String Photo
    }


initialModel : Model
initialModel =
    { selectedPhotoUrl = Nothing 
    , photos = Dict.empty
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( initialModel
    , Http.get
        { url = "http://elm-in-action.com/folders/list"
        , expect = Http.expectJson GotInitialModel modelDecoder
        }
    )


modelDecoder : Decoder Model
modelDecoder =
    Decode.succeed initialModel


type Msg
    = ClickedPhoto String
    | GotInitialModel (Result Http.Error Model)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickedPhoto url ->
            ( { model | selectedPhotoUrl = Just url }, Cmd.none )

        GotInitialModel (Ok newModel) ->
            ( newModel, Cmd.none )

        GotInitialModel (Err _) ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    h1 [] [ text "The Grooviest Folders the world has ever seen" ]


main : Program () Model Msg
main =
    Browser.element
        { init = init
        , view = view
        , update = update
        , subscriptions = \_ -> Sub.none
        }


type alias Photo = 
    { title : String
    , size : int
    , relatedUrls : List String
    , url : String
    }

viewSelectedPhoto : Photo -> Html Msg
viewSelectedPhoto photo = 
    div
        [ class "selected-photo" ]
        [ h2 [ text photo.title ] 
        , img [src (urlPrefix ++ "photos/" ++ photo.url ++ "/full")] []
        , span [] [ text (String.fromInt photo.size ++ "KB") ]
        , h3 [] [ text "Related" ]
        , div [ class "related-photos" ]
            (List.map viewRelatedPhoto photo.relatedUrls)
        ]


selectedPhoto : Html Msg
selectedPhoto = 
    case model.selectedPhotoUrl of
        Just url -> 
            case Dict.get url model.photos of  
                Just photo -> 
                    viewSelectedPhoto photo

                Nothing -> 
                    text ""

        Nothing -> 
            text ""


viewRelatedPhoto : String -> Html Msg
viewRelatedPhoto url = 
    img
        [ class "related-photos"
        , onClick (ClickedPhoto url)
        , src (urlPrefix ++ "photos/" ++ url ++ "/thumb")
        ]