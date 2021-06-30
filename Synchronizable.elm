module Synchronizable exposing (Synchronizable(..), sync, resolve)

import ClientServerData exposing (ClientServerData)
import Http


type Synchronizable id local remote
    = AtRest (ClientServerData id local remote)
    | InFlight (ClientServerData id local remote)
    | FailedSync Http.Error (ClientServerData id local remote)


sync :
    (a -> Cmd msg)
    -> (id -> a -> Cmd msg)
    -> Synchronizable id a b
    -> ( Synchronizable id a b, Cmd msg )
sync create update sync =
    case sync of
        AtRest data ->
            ( InFlight data, ClientServerData.sync create update data )

        InFlight data ->
            ( InFlight data, Cmd.none )

        FailedSync _ data ->
            ( InFlight data, ClientServerData.sync create update data )


resolve :
    Result Http.Error ( id, remote )
    -> Synchronizable id local remote
    -> Synchronizable id local remote
resolve result sync =
    case sync of
        AtRest ->
            sync

        InFlight ->
            resolveHelper result

        FailedSync ->
            sync


resolveHelper : Result Http.Error ( id, remote ) -> Synchronizable id local remote
resolveHelper result =
    case result of
        Ok ( id, remote ) ->
            AtRest (Synced id remote)

        Err error ->
            FailedSync error data
