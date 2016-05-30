module Routes exposing (Sitemap(..), parsePath, navigateTo, toString)

import Navigation exposing (Location)
import Route exposing (..)


type Sitemap
    = HomeRoute ()
    | PostsRoute ()
    | PostRoute String
    | AboutRoute ()
    | NotFoundRoute


homeRoute =
    HomeRoute := static ""


postsRoute =
    PostsRoute := static "posts"


postRoute =
    PostRoute := "posts" <//> string


aboutRoute =
    AboutRoute := static "about"


sitemap =
    router [ homeRoute, postsRoute, postRoute, aboutRoute ]


match : String -> Sitemap
match =
    Route.match sitemap
        >> Maybe.withDefault NotFoundRoute


toString : Sitemap -> String
toString route =
    case route of
        HomeRoute () ->
            reverse homeRoute []

        PostsRoute () ->
            reverse postsRoute []

        PostRoute string ->
            reverse postRoute [ string ]

        AboutRoute () ->
            reverse aboutRoute []

        NotFoundRoute ->
            Debug.crash "cannot render NotFound"


parsePath : Location -> Sitemap
parsePath =
    .pathname >> match


navigateTo : Sitemap -> Cmd msg
navigateTo =
    toString >> Navigation.newUrl
