module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (src, style)
import Html.Events exposing (..)
import Http
import Json.Decode exposing (Decoder, field, int, list, map3, string)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Photos =
    { id : Int
    , url : String
    , bigUrl : String
    }


type APIStagePhotos
    = FailureP
    | LoadingP
    | SuccessP (List Photos)


type alias Model =
    { apiStagePhotos : APIStagePhotos
    , apiPage : Int
    , apiQuery : String
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { apiStagePhotos = LoadingP
      , apiPage = 1
      , apiQuery = ""
      }
    , getInitPhotos
    )



-- UPDATE


type Msg
    = MorePlease
    | IncrementPage
    | ResetPage
    | GotPhotos (Result Http.Error (List Photos))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MorePlease ->
            ( { model | apiStagePhotos = LoadingP }, getInitPhotos )

        GotPhotos result ->
            case result of
                Ok photos ->
                    ( { model | apiStagePhotos = SuccessP photos }, Cmd.none )

                Err _ ->
                    ( { model | apiStagePhotos = FailureP }, Cmd.none )

        IncrementPage ->
            ( { model | apiPage = model.apiPage + 1 }, Cmd.none )

        ResetPage ->
            ( { model | apiPage = 1 }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ h2 [] [ text "Photos" ]
        , viewPhotos model.apiStagePhotos
        ]


viewPhotos : APIStagePhotos -> Html Msg
viewPhotos model =
    case model of
        FailureP ->
            div []
                [ text "I could not load a random cat for some reason. "
                , button [ onClick MorePlease ] [ text "Try Again!" ]
                ]

        LoadingP ->
            text "Loading..."

        SuccessP photos ->
            div []
                [ ul [] (List.map vievOnePhoto photos)
                , button [ onClick MorePlease, style "display" "block" ] [ text "More Please!" ]
                ]


vievOnePhoto : Photos -> Html Msg
vievOnePhoto photo =
    img [ src photo.url ] []



-- HTTP


getInitPhotos : Cmd Msg
getInitPhotos =
    Http.get
        { url = "https://pixabay.com/api/?q=&key=1992005-0c423af9a07f13d941d831108&image_type=photo&orientation=horizontal&per_page=4&page=1"
        , expect = Http.expectJson GotPhotos photoDecoder
        }


createUrl : Model -> String
createUrl model =
    "https://pixabay.com/api/?q=" ++ model.apiQuery ++ "&key=1992005-0c423af9a07f13d941d831108&image_type=photo&orientation=horizontal&per_page=12&page=" ++ String.fromInt model.apiPage


photoDecoder : Decoder (List Photos)
photoDecoder =
    field "hits"
        (list
            (map3 Photos
                (field "id" int)
                (field "webformatURL" string)
                (field "largeImageURL" string)
            )
        )
