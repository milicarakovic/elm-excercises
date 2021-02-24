module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Debug exposing (toString)
import Browser exposing (sandbox)
import Html.Events exposing (onClick)

initialModel =
    { query = "tutorial"
    , results =
        [ { id = 1
          , name = "TheSeamau5/elm-checkerboardgrid-tutorial"
          , stars = 66
          }
        , { id = 2
          , name = "grzegorzbalcerek/elm-by-example"
          , stars = 41
          }
        , { id = 3
          , name = "sporto/elm-tutorial-app"
          , stars = 35
          }
        , { id = 4
          , name = "jvoigtlaender/Elm-Tutorium"
          , stars = 10
          }
        , { id = 5
          , name = "sporto/elm-tutorial-assets"
          , stars = 7
          }
        ]
    }

elmHubHeader =
        header []
            [ h1 [] [ text "ElmHub" ]
            , span [ class "tagline" ] [ text "Like GitHub, but for Elm things." ]
            ]

view model =
    div [ class "content" ]
        [ elmHubHeader
        , ul [ class "results" ] (List.map viewSearchResult model.results)
        ]

viewSearchResult result =
    li []
        [ span [ class "star-count" ] [ text (toString result.stars) ]
        , a [ href ("https://github.com/" ++ result.name), target "_blank" ]
            [ text result.name ]
        , button
            [ class "hide-result", onClick {operation = "DELETE_BY_ID", data = result.id} ]
            [ text "X" ]
        ]

update msg model =
    if msg.operation == "DELETE_BY_ID" then
      {model | results = List.filter (\result -> result.id /= msg.data) model.results}
    else
      model

main = Browser.sandbox
          { init = initialModel
          , view = view
          , update = update
          }
