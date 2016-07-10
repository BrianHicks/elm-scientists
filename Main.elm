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


{-| We're populating our Model with a bunch of scientists here in init to save
having to load them over JSON. No need to make this example more complex than it
has to be. :)
-}
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


{-| This is really similar to some publicly available functions in
elm-color-extra, but again with the simple examples.
-}
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


{-| This is a little more verbose than it has to be. If I was doing this for a
production app, I would probably find some way to map (String, Color) to List
Html. But this works just fine. :)
-}
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


{-| We're dealing with a Maybe Scientist here instead of a Scientist or a Maybe
String. This means that we don't care how the scientists are stored, we just
render the selection as-is. Types!
-}
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


{-| LookupByID does what it says on the tin. The order of arguments matches that
of the rest of the collection functions from the stanrdard library (a selector
followed by the collection so it's easier to do, say, `model.scientists |>
lookupByID (Just "Ride")`)

We're using this as the concrete signature `Maybe String -> Dict String
Scientist -> Maybe Scientist`, but I've left the type variables in to
demonstrate that we can use this anywhere we're looking up a value in a Dict.
-}
lookupByID : Maybe comparable -> Dict comparable a -> Maybe a
lookupByID id collection =
    id `Maybe.andThen` (\inner -> Dict.get inner collection)


view : Model -> Html Msg
view model =
    Html.div []
        [ Html.ul [] (model.scientists |> Dict.values |> List.map itemView)
        , lookupByID model.selected model.scientists
            |> detailView
        ]


{-| And lastly, we're using App.beginnerProgram instead of App.program because
we deliberately don't have any effects. This is meant to be super simple. This
technique will work just as well with App.program, however.
-}
main : Program Never
main =
    App.beginnerProgram
        { model = init
        , update = update
        , view = view
        }
