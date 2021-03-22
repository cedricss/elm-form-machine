module Form.Machine exposing (FormError, State(..), Event(..), Config, transition)

{-| A state machine to handle forms in elm.

@docs FormError, State, Event, Config, transition

-}

import Validate


{-| An error about one specific field.
-}
type alias FormError formField =
    ( formField, String )


{-| The form machine can have 5 states.
-}
type State object objectField
    = Displaying object
    | Editing
        { object : object
        , errors : List (FormError objectField)
        }
    | Failed String
    | Loading
    | Unloaded


{-| Events to move the machine from one state to another
-}
type Event object objectField customEvents
    = Create
    | Display object
    | Edit objectField
    | Fail String
    | Perform customEvents
    | Request
    | Save


{-| Custom handlers for the machine. Read [transition](#transition).
-}
type alias Config object objectField customEvents msg =
    { badTransition :
        Event object objectField customEvents
        -> State object objectField
        -> ( State object objectField, Cmd msg )
    , default : object
    , perform :
        customEvents
        -> State object objectField
        -> ( State object objectField, Cmd msg )
    , save : Validate.Valid object -> Cmd msg
    , update : object -> objectField -> object
    , validator : Validate.Validator (FormError objectField) object
    }


{-| Perform a transition with the given configuration, event and current state.

If it's impossible to handle such event for the current state, the `badTransition` function is called, for example to log it to your error monitoring platform.

The `update` function will update one specific field of the object, for example after the user typed into the related input.

The `save` function only accepts a `Validate.Valid object`: you can be certain the object is validated with `validator` before saving.
Note: you can unwrap the valid object with [`Validate.fromValid`](https://package.elm-lang.org/packages/rtfeldman/elm-validate/4.0.1/Validate#fromValid)

To handle any additional custom events, send a `Perform customEvent` event and the `perform` function will be called with this customEvent.

    Common.transition
        { badTransition = logBadRobotFormTransition
        , default = defaultRobot
        , perform = performCustomEvent
        , save = Request.saveRobot SavedRobot
        , update = updateRobotField
        , validator = robotValidator
        }
        event
        state

-}
transition :
    Config object objectField customEvents msg
    -> Event object objectField customEvents
    -> State object objectField
    -> ( State object objectField, Cmd msg )
transition config event state =
    case ( event, state ) of
        ( Create, Unloaded ) ->
            ( Displaying config.default, Cmd.none )

        ( Request, Unloaded ) ->
            ( Loading, Cmd.none )

        ( Display object, Unloaded ) ->
            ( Displaying object, Cmd.none )

        ( Display object, Loading ) ->
            ( Displaying object, Cmd.none )

        ( Edit field, Displaying object ) ->
            ( Editing { object = config.update object field, errors = [] }, Cmd.none )

        ( Edit field, Editing edition ) ->
            ( Editing { edition | object = config.update edition.object field }, Cmd.none )

        ( Fail message, _ ) ->
            ( Failed message, Cmd.none )

        ( Perform customEvent, _ ) ->
            config.perform customEvent state

        ( Save, Displaying object ) ->
            case Validate.validate config.validator object of
                Ok validObject ->
                    ( state, config.save validObject )

                Err errors ->
                    ( Editing { object = object, errors = errors }, Cmd.none )

        ( Save, Editing edition ) ->
            case Validate.validate config.validator edition.object of
                Ok validObject ->
                    ( Editing { edition | errors = [] }, config.save validObject )

                Err errors ->
                    ( Editing { edition | errors = errors }, Cmd.none )

        _ ->
            config.badTransition event state
