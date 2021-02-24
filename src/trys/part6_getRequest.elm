module trys.part6_getRequest exposing (..)module Main exposing (..)

import Auth
import Browser
import Html exposing (..)
import Html.Attributes exposing (class, href, property, target)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (Decoder, map3, decodeString, float, int, nullable, string, list, map2,field)
import Json.Decode.Pipeline exposing (required, optional, hardcoded)
import Debug exposing (toString)
import Json.Decode
import Task
import Debug
import Http
--elm install the-sett/auth-elm

-- main : Program Never Model Msg
main =
  Browser.element
    { init = initialModel
    , update = update
    , subscriptions = subscriptions
    , view = view
    }

subscriptions : Model -> Sub Msg
subscriptions model =
  Sub.none

searchFeed : String -> Cmd Msg
searchFeed query =
    let
        url =
            "https://api.github.com/search/repositories?access_token="
                ++ Auth.token
                ++ "&q="
                ++ query
                ++ "+language:elm&sort=stars&order=desc"
        -- _ = Debug.log "query" query 
        
    in    
        Http.get
            {url = url
            , expect = Http.expectJson HandleSearchResponse (responseDecoder)
            }



responseDecoder : Decoder (List SearchResult)
responseDecoder =
    Decode.at ["items"] (Json.Decode.list searchResultDecoder)

searchResultDecoder : Decoder SearchResult
searchResultDecoder =
    Decode.succeed  SearchResult
        |> required "id" Json.Decode.int
        |> required "full_name" Json.Decode.string
        |> required "stargazers_count" Json.Decode.int
        --Chec Json.Decode.int vs just int
    
    

type alias Model =
    { query : String
    , results : List SearchResult
    , errorMessage : Maybe String
    }


type alias SearchResult =
    { id : Int
    , name : String
    , stars : Int
    }


initialModel : () -> (Model, Cmd Msg) 
initialModel _ =
   ({ query = "tutorial"
    , results = []
    , errorMessage = Nothing
    }
   , 
    Cmd.none
   )

view : Model -> Html Msg
view model =
    div [ class "content" ]
        [ header []
            [ h1 [] [ text "ElmHub" ]
            , span [ class "tagline" ] [ text "Like GitHub, but for Elm things." ]
            ]
        , input [ class "search-query", onInput SetQuery ] []
        , button [ class "search-button", onClick Search ] [ text "Search" ]
        , viewErrorMessage model.errorMessage
        , ul [ class "results" ] (List.map viewSearchResult model.results)
        ]


viewErrorMessage : Maybe String -> Html Msg
viewErrorMessage errorMessage =
    case errorMessage of
        Just message ->
            div [ class "error" ] [ text message ]

        Nothing ->
            text ""


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
    = Search
    | SetQuery String
    | DeleteById Int
    | HandleSearchResponse (Result Http.Error (List SearchResult))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Search ->
            ( model, searchFeed model.query )

        HandleSearchResponse result ->
            case result of
                Ok results ->
                    ( { model | results = results }, Cmd.none )

                Err error ->
                    let
                        errorMessage =
                            case error of
                                Http.BadUrl message ->
                                    message

                                Http.Timeout ->
                                    "Server is taking too long to respond. Please try again later."

                                Http.NetworkError ->
                                    "Unable to reach server."

                                Http.BadStatus statusCode ->
                                    "Request failed with status code: " ++ String.fromInt statusCode

                                Http.BadBody message ->
                                   "Error " ++ message
                                
                    in
                        ( {model | errorMessage = Just errorMessage}, Cmd.none )

        SetQuery query ->
            ( { model | query = query }, Cmd.none )

        DeleteById idToHide ->
            let
                newResults =
                    model.results
                        |> List.filter (\{ id } -> id /= idToHide)

                newModel =
                    { model | results = newResults }
            in
            ( newModel, Cmd.none )
