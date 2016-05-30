module Data exposing (Post, postsGetAll, postsGetSingle)

import Http
import Json.Decode as Json exposing ((:=), Decoder, string)
import Task exposing (Task)


type alias Post =
    { id : String
    , title : String
    , body : String
    }


post : Decoder Post
post =
    Json.object3 Post
        ("id" := string)
        ("title" := string)
        ("body" := string)


posts : Decoder (List Post)
posts =
    Json.list post


postsGetAll : Task Http.Error (List Post)
postsGetAll =
    Http.get posts "/api/posts"

postsGetSingle : String -> Task Http.Error Post
postsGetSingle string =
    Http.get post ("/api/posts/" ++ string)
