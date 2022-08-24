port module Login exposing (..)

import Browser
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (onClick, onInput)
import Http
import Json.Decode as JD
import Json.Encode as JE



-- MAIN


main : Program String Model Msg
main =
    Browser.element
        { init = init
        , update = update
        , view = view
        , subscriptions = subscriptions
        }



-- MODEL


type alias Model =
    { email : String
    , password : String
    , status : Status
    , apiBaseUrl : String
    }


type Status
    = Ready
    | Authenticating
    | Failure
    | Problem String
    | LoggedIn AuthToken


type AuthToken
    = TokenString String


tokenToString : AuthToken -> String
tokenToString token =
    case token of
        TokenString str ->
            str


authTokenDecoder : JD.Decoder AuthToken
authTokenDecoder =
    JD.map TokenString (JD.field "access_token" JD.string)


authHeader : AuthToken -> Http.Header
authHeader token =
    case token of
        TokenString str ->
            Http.header "X-WWW-AUTH-TOKEN" str


init : String -> ( Model, Cmd Msg )
init apiBaseUrl =
    ( Model "" "" Ready apiBaseUrl, fetchAuthToken () )



-- UPDATE


type Msg
    = Email String
    | Password String
    | SignIn
    | GotAuthFromServer (Result Http.Error AuthToken)
    | GotAuthFromCookie (Maybe String)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Email email ->
            ( { model | email = email, status = Ready }, Cmd.none )

        Password password ->
            ( { model | password = password, status = Ready }, Cmd.none )

        SignIn ->
            ( { model | status = Authenticating }, getAuth model )

        GotAuthFromServer result ->
            case result of
                Ok token ->
                    ( { model | status = LoggedIn token }, setAuthTokenCookie (tokenToString token) )

                Err e ->
                    case e of
                        Http.BadStatus 401 ->
                            ( { model | status = Failure }, Cmd.none )

                        _ ->
                            ( { model | status = Problem (prettyHttpError e) }, Cmd.none )

        GotAuthFromCookie maybeTokenString ->
            case maybeTokenString of
                Just token ->
                    ( { model | status = LoggedIn (TokenString token) }, setAuthTokenCookie token )

                Nothing ->
                    ( { model | status = Ready }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    case model.status of
        LoggedIn _ ->
            text ""

        _ ->
            div []
                [ viewStatus model.status
                , viewSignInForm model
                ]


viewSignInForm : Model -> Html Msg
viewSignInForm model =
    div []
        [ viewInput "text" "Email" model.email Email
        , viewInput "password" "Password" model.password Password
        , viewSignInButton model.status
        ]


viewSignInButton : Status -> Html Msg
viewSignInButton status =
    case status of
        Ready ->
            button [ onClick SignIn ] [ text "Sign in" ]

        _ ->
            button [ disabled True ] [ text "Sign in" ]


viewStatus : Status -> Html Msg
viewStatus status =
    case status of
        Ready ->
            text ""

        Problem description ->
            text description

        LoggedIn _ ->
            text "Logged in!"

        Authenticating ->
            text ""

        Failure ->
            text "Incorrect email or password, try again."


viewInput : String -> String -> String -> (String -> msg) -> Html msg
viewInput t p v toMsg =
    input [ type_ t, placeholder p, value v, onInput toMsg ] []



-- HTTP


getAuth : Model -> Cmd Msg
getAuth model =
    Http.post
        { url = model.apiBaseUrl ++ "login.json"
        , body = Http.jsonBody (encodeCredentials model.email model.password)
        , expect = Http.expectJson GotAuthFromServer authTokenDecoder
        }


encodeCredentials : String -> String -> JE.Value
encodeCredentials email password =
    JE.object
        [ ( "email", JE.string email )
        , ( "password", JE.string password )
        ]



-- PORTS


port fetchAuthToken : () -> Cmd msg


port authTokenReceiver : (Maybe String -> msg) -> Sub msg


port setAuthTokenCookie : String -> Cmd msg



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    authTokenReceiver GotAuthFromCookie



-- String util


prettyHttpError : Http.Error -> String
prettyHttpError err =
    case err of
        Http.BadUrl url ->
            "malformed url: " ++ url

        Http.Timeout ->
            "request timeout"

        Http.NetworkError ->
            "network error, maybe the server or your internet connection is down"

        Http.BadStatus status ->
            "status " ++ String.fromInt status ++ ", take a screenshot and show it to BT, he can help with this"

        Http.BadBody errDescription ->
            "could not decode response body: " ++ errDescription ++ ", take a screenshot and show it to BT, he can help with this"
