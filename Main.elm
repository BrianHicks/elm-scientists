module Main exposing (..)

import Color exposing (Color)
import Dict exposing (Dict)
import Html exposing (Html)
import Html.App as App
import Html.Attributes as Attr
import Html.Events as Events
import Maybe


type alias Scientist =
    { name : String
    , bio : String
    , color : Color
    }


type alias Model =
    { scientists : Dict String Scientist
    , selected : Maybe String
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
    | ChangeColor String Color


update : Msg -> Model -> Model
update msg model =
    case msg of
        Select who ->
            { model | selected = Just who }

        ChangeColor who color ->
            case Dict.get who model.scientists of
                Nothing ->
                    model

                Just scientist ->
                    { model
                        | scientists =
                            model.scientists
                                |> Dict.insert who { scientist | color = color }
                    }


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


colors : Scientist -> Html Msg
colors scientist =
    Html.ul []
        [ Html.li [ Events.onClick (ChangeColor scientist.name Color.red) ] [ Html.text "red" ]
        , Html.li [ Events.onClick (ChangeColor scientist.name Color.orange) ] [ Html.text "orange" ]
        , Html.li [ Events.onClick (ChangeColor scientist.name Color.yellow) ] [ Html.text "yellow" ]
        , Html.li [ Events.onClick (ChangeColor scientist.name Color.green) ] [ Html.text "green" ]
        , Html.li [ Events.onClick (ChangeColor scientist.name Color.blue) ] [ Html.text "blue" ]
        , Html.li [ Events.onClick (ChangeColor scientist.name Color.purple) ] [ Html.text "purple" ]
        ]


itemView : Scientist -> Html Msg
itemView scientist =
    Html.li
        [ Attr.class "scientist"
        , scientist.color |> toCss |> Attr.style
        ]
        [ Html.p [ Events.onClick (Select scientist.name) ] [ Html.text scientist.name ]
        , colors scientist
        ]


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
        , lookupByID model.selected model.scientists
            |> detailView
        ]


main : Program Never
main =
    App.beginnerProgram
        { model = init
        , update = update
        , view = view
        }
