module Main exposing (main)

import Browser exposing (Document)
import Html exposing (Html, a, footer, h1, li, nav, text, ul)
import Html.Attributes exposing (classList, href)


type alias Model =
    {}


-- NEXT
view : Model -> Document Msg
view model =
    { title = "Photo Groove, SPA Style"
    , body = [ text "This isn't even my final form!" ]
    }


type Msg
    = NothingYet


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


main : Program () Model Msg
main =
    Browser.document
        { init = \_ -> ( {}, Cmd.none )
        , subscriptions = subscriptions
        , update = update
        -- Browser.element.view must reutrn Html Msg
        -- Browser.document.view must return Document Msg
        -- Document gives Elm access to the entire web page, instead of a (parent) DOM node
        , view = view
        }
