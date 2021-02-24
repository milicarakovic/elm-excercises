module Main exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Debug exposing (toString)


---- PROGRAM ----
pluralize singular plural quantity =
    let
        -- you can not reassign these. They are like const
        quantityStr =
            toString quantity
        prefix =
            quantityStr ++ " "
    in
        if quantity == 1 then
            prefix ++ singular
        else
            prefix ++ plural


model =
    { result =
        {id = 1
        , name = "https://github.com/TheSeamau5/elm-checkerboardgrid-tutorial"
        , stars = 66
        }
    }

main =
    let
        elmHubHeader = 
            header []
                [ h1 [] [text "ElmHub"]              
                , span [class "tagline"] [text "Hello"]
                ]
            

        
    in
    div [ class "content" ]
            [ elmHubHeader
            , ul [ class "results" ]
                [ li []
                    [ span [ class "star-count" ] [ text (toString model.result.stars)]
                    , a [href ("https://github.com/" ++ model.result.name)] [text model.result.name]
                    ]
                ]
            ]

