module Common.Pages.Users.Edit.Routes exposing (UserEditRoute(..))

import Uuid exposing (Uuid)


type UserEditRoute
    = Profile
    | Password
    | ConnectedAccounts
    | Language
    | Tours
    | ApiKeys
    | ActiveSessions
    | SubmissionSettings
    | SubmissionAccounts (Maybe String) (Maybe String) (Maybe String)
    | PluginSettings Uuid
