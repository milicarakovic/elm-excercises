module trys.part5_jsonDecodePipeline exposing (..)
module Main exposing (..)

import Html exposing (..)
import Html.Attributes exposing (class, href, property, target)
import Html.Events exposing (..)
import Json.Decode as Decode exposing (Decoder, decodeString, float, int, nullable, string, list)
import Json.Decode.Pipeline exposing (required, optional, hardcoded)
import SampleResponse
import Browser
import Debug exposing (toString)

main = Browser.sandbox
          { init = initialModel
          , view = view
          , update = update
          }


searchResultDecoder : Decoder SearchResult
searchResultDecoder =
    Decode.succeed SearchResult
        |> required "id" int
        |> required "full_name" string
        |> required "stargazers_count" int


type alias Model =
    { query : String
    , results : List SearchResult
    }

type alias SearchResult =
    { id : Int
    , name : String
    , stars : Int
    }


initialModel : Model
initialModel =
    { query = "tutorial"
    , results = decodeResults SampleResponse.json
    }


responseDecoder : Decoder (List SearchResult)
responseDecoder =
    Decode.succeed identity
        |> required "items" (list searchResultDecoder)


decodeResults : String -> List SearchResult
decodeResults json =
-- decodeString takes 2 arguments: decoder and json that you want that decoder on
    case decodeString responseDecoder json of
      Ok searchResults ->
        searchResults
      Err _ ->
        let
            _ = Debug.log "error"
        in
        []


view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ header []
            [ h1 [] [ text "ElmHub" ]
            , span [ class "tagline" ] [ text "Like GitHub, but for Elm things." ]
            ]
        , input [ class "search-query", onInput SetQuery ] []
        , button [ class "search-button" ] [ text "Search" ]
        , ul [ class "results" ]
            (List.map viewSearchResult model.results)
        ]


viewSearchResult : SearchResult -> Html Msg
viewSearchResult result =
    li []
        [ span [ class "star-count" ] [ text (toString result.stars) ]
        , a [ href ("https://github.com/" ++ result.name), target "_blank" ]
            [ text result.name ]
        , button [ class "hide-result", onClick (DeleteById result.id) ]
            [ text "X" ]
        ]


type Msg
    = SetQuery String
    | DeleteById Int


update : Msg -> Model -> Model
update msg model =
    case msg of
        SetQuery query ->
            { model | query = query }

        DeleteById idToHide ->
            let
                newResults =
                    List.filter (\{ id } -> id /= idToHide) model.results
            in
            { model | results = newResults }
