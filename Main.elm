module Main exposing (..)

import Color exposing (Color)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.App as App
import Html.Attributes as Attr
import Html.Events as Events


type alias Scientist =
    { name : String
    , bio : String
    , color : Color
    }


type alias Model =
    { scientists : Dict String Scientist
    , selected : Maybe Scientist
    }


init : Model
init =
    { scientists =
        Dict.fromList
            [ ( "Payne"
              , { name = "Payne"
                , bio = "Cecilia Payne-Gaposchkin was an astronomer and astrophysicist who, in 1925, proposed in her Ph.D. thesis an explanation for the composition of stars in terms of the relative abundances of hydrogen and helium."
                , color = Color.red
                }
              )
            , ( "Perlman"
              , { name = "Perlman"
                , bio = "Radia Perlman is a software designer and network engineer and most famous for her invention of the spanning-tree protocol (STP)."
                , color = Color.orange
                }
              )
            , ( "Poitras"
              , { name = "Poitras"
                , bio = "Laura Poitras is a director and producer whose work, made possible by open source crypto tools, advances the causes of truth and freedom of information by reporting disclosures by whistleblowers such as Edward Snowden."
                , color = Color.yellow
                }
              )
            , ( "Ride"
              , { name = "Ride"
                , bio = "Sally Kristen Ride was an American physicist and astronaut. She was the first American woman in space, and the youngest American astronaut."
                , color = Color.green
                }
              )
            ]
    , selected = Nothing
    }


type Msg
    = Select String


update : Msg -> Model -> Model
update msg model =
    case msg of
        Select who ->
            case Dict.get who model.scientists of
                Nothing ->
                    model

                scientist ->
                    { model | selected = scientist }


toCss : Color -> List ( String, String )
toCss =
    Color.toRgb
        >> (\c ->
                "rgba("
                    ++ (toString c.red)
                    ++ ", "
                    ++ (toString c.green)
                    ++ ", "
                    ++ (toString c.blue)
                    ++ ", "
                    ++ (toString c.alpha)
                    ++ ")"
           )
        >> (,) "color"
        >> (\x -> [ x ])


itemView : Scientist -> Html Msg
itemView scientist =
    Html.li
        [ Attr.class "scientist"
        , scientist.color |> toCss |> Attr.style
        , Events.onClick (Select scientist.name)
        ]
        [ Html.text scientist.name ]


detailView : Maybe Scientist -> Html Msg
detailView s =
    case s of
        Nothing ->
            Html.text "No scientist selected."

        Just scientist ->
            Html.div [ scientist.color |> toCss |> Attr.style ]
                [ Html.h2 [] [ Html.text scientist.name ]
                , Html.p [] [ Html.text scientist.bio ]
                ]


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.ul [] (model.scientists |> Dict.values |> List.map itemView)
        , detailView model.selected
        ]


main : Program Never
main =
    App.beginnerProgram
        { model = init
        , update = update
        , view = view
        }
