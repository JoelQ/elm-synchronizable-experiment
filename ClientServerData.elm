module ClientServerData exposing (ClientServerData(..), sync)


type ClientServerData id local remote
    = OnClientOnly local
    | Diverging id local remote
    | Synced id remote


sync : (a -> Cmd msg) -> (id -> a -> Cmd msg) -> ClientServerData id a b -> Cmd msg
sync create update sync =
    case sync of
        OnClientOnly local ->
            create local

        Diverging id local _ ->
            update id local

        Synced _ _ ->
            Cmd.none
