module Update exposing (Msg(..), Model, init, update, urlUpdate)

import Data exposing (Post, postsGetAll, postsGetSingle)
import Http
import Navigation exposing (Location)
import Routes exposing (Sitemap(..))
import Task


type alias Model =
    { route : Sitemap
    , postsGetAllReady : Bool
    , posts : List Post
    , postsGetSingleReady : Bool
    , post : Maybe Post
    , postsGetAllError : Maybe String
    , postsGetSingleError : Maybe String
    }


type Msg
    = RouteTo Sitemap
    | PostsGetAllError Http.Error
    | PostsGetAllSuccess (List Post)
    | PostsGetSingleError Http.Error
    | PostsGetSingleSuccess Post


init : Sitemap -> ( Model, Cmd Msg )
init route =
    urlUpdate route
        { route = route
        , postsGetAllReady = False
        , posts = []
        , postsGetSingleReady = False
        , post = Nothing
        , postsGetAllError = Nothing
        , postsGetSingleError = Nothing
        }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg ({ route } as model) =
    case msg of
        RouteTo route ->
            model ! [ Routes.navigateTo route ]

        PostsGetAllError postsGetAllError ->
            { model | postsGetAllError = Just (toString postsGetAllError) } ! []

        PostsGetAllSuccess posts ->
            urlUpdate route { model | postsGetAllReady = True, posts = posts }

        PostsGetSingleError postsGetSingleError ->
            { model | postsGetSingleError = Just (toString postsGetSingleError) } ! []

        PostsGetSingleSuccess post ->
            urlUpdate route { model | postsGetSingleReady = True, post = Just post }


urlUpdate : Sitemap -> Model -> ( Model, Cmd Msg )
urlUpdate route ({ postsGetAllReady, postsGetSingleReady } as m) =
    let
        model =
            { m | route = route }
    in
        case route of
            PostsRoute () ->
                if postsGetAllReady then
                    model ! []
                else
                    model ! [ postsGetAll ]

            PostRoute id ->
                if postsGetSingleReady && id == (postIdGet model.post) then
                    model ! []
                else
                    model ! [ postsGetSingle id ]

            _ ->
                model ! []


postsGetAll : Cmd Msg
postsGetAll =
    Task.perform PostsGetAllError PostsGetAllSuccess Data.postsGetAll


postsGetSingle : String -> Cmd Msg
postsGetSingle id =
    Task.perform PostsGetSingleError PostsGetSingleSuccess (Data.postsGetSingle id)


postIdGet : Maybe Post -> String
postIdGet maybePost =
    case maybePost of
        Just post ->
            post.id
        Nothing ->
            ""