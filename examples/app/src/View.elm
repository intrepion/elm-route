module View exposing (view)

import Html exposing (..)
import Html.Attributes exposing (href)
import Html.Events exposing (onWithOptions)
import Json.Decode as Json
import Data
import Routes exposing (Sitemap(..))
import Update exposing (Msg(..), Model)


notFound : Html Msg
notFound =
    h1 [] [ text "Page not found" ]


home : Html Msg
home =
    div []
        [ h1 [] [ text "Home Page" ]
        , p [] [ link (PostRoute "123") "This post does not exist" ]
        ]


about : Html Msg
about =
    h1 [] [ text "About" ]


loading : Html Msg
loading =
    h1 [] [ text "Loading..." ]


post : Data.Post -> Html Msg
post post =
    div []
        [ h1 [] [ text post.title ]
        , p [] [ text post.body ]
        ]


posts : List Data.Post -> Html Msg
posts posts =
    let
        postLink post =
            li [] [ link (PostRoute post.id) post.title ]
    in
        div []
            [ h1 [] [ text "Posts" ]
            , ul [] (List.map postLink posts)
            ]


view : Model -> Html Msg
view model =
    div []
        [ ul []
            [ li [] [ link (HomeRoute ()) "Home" ]
            , li [] [ link (PostsRoute ()) "Posts" ]
            , li [] [ link (AboutRoute ()) "About" ]
            ]
        , case model.route of
            HomeRoute () ->
                home

            PostsRoute () ->
                if model.postsGetAllReady then
                    posts model.posts
                else
                    loading

            PostRoute id ->
                case ( model.postsGetSingleReady, model.post ) of
                    ( False, _ ) ->
                        loading

                    ( True, Nothing ) ->
                        notFound

                    ( True, Just p ) ->
                        post p

            AboutRoute () ->
                about

            NotFoundRoute ->
                notFound
        ]


link : Sitemap -> String -> Html Msg
link route label =
    let
        opts =
            { preventDefault = True
            , stopPropagation = False
            }
    in
        a
            [ href (Routes.toString route)
            , onWithOptions "click" opts (Json.succeed <| RouteTo route)
            ]
            [ text label ]
