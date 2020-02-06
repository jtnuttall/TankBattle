module Drawing.AnimationData exposing
    ( AnimationCommand(..)
    , AnimationData
    , AnimationState(..)
    , AnimationUpdates
    , getFrame
    , init
    , update
    )

import Array as Array exposing (Array)
import Canvas.Texture exposing (Texture)
import Maybe exposing (withDefault)
import Utility exposing (cycleI)


type alias AnimationData =
    { frameIndex : Int
    , state : AnimationState
    , time : Float
    , step : Float
    , zeroFrame : Texture
    , frames : Array Texture
    }


type AnimationState
    = Running
    | Finishing
    | Stopped


type AnimationCommand
    = Run
    | Finish
    | Stop
    | NoChange


type alias AnimationUpdates =
    { tank :
        { body : AnimationCommand
        , gun : AnimationCommand
        }
    }


init : Float -> Texture -> Array Texture -> AnimationData
init step zeroFrame frames =
    { frameIndex = 0
    , state = Stopped
    , time = 0
    , step = step
    , zeroFrame = zeroFrame
    , frames = frames
    }


getFrame : AnimationData -> Texture
getFrame data =
    data.frames
        |> Array.get data.frameIndex
        |> withDefault data.zeroFrame


updateAnimation : Float -> AnimationData -> AnimationData
updateAnimation deltaTime data =
    let
        time =
            data.time

        doUpdate =
            time >= data.step

        cycleFrames =
            cycleI 0 (Array.length data.frames - 1)

        nextFrame =
            cycleFrames (data.frameIndex + 1)
    in
    { data
        | time =
            if doUpdate then
                0

            else
                data.time + deltaTime
        , frameIndex =
            if doUpdate then
                cycleFrames (data.frameIndex + 1)

            else
                data.frameIndex
        , state =
            case data.state of
                Finishing ->
                    if nextFrame == 0 then
                        Stopped

                    else
                        Finishing

                state ->
                    state
    }


update : Float -> AnimationCommand -> AnimationData -> AnimationData
update deltaTime command data =
    case ( command, data.state ) of
        ( Run, Running ) ->
            data
                |> updateAnimation deltaTime

        ( Run, Finishing ) ->
            { data | state = Running }
                |> updateAnimation deltaTime

        ( Run, Stopped ) ->
            { data | state = Running }
                |> updateAnimation deltaTime

        ( Finish, Running ) ->
            { data | state = Finishing }
                |> updateAnimation deltaTime

        ( Finish, Finishing ) ->
            data
                |> updateAnimation deltaTime

        ( Finish, Stopped ) ->
            data

        ( Stop, Running ) ->
            { data | state = Stopped }
                |> updateAnimation deltaTime

        ( Stop, Finishing ) ->
            data
                |> updateAnimation deltaTime

        ( Stop, Stopped ) ->
            data

        ( NoChange, Running ) ->
            data
                |> updateAnimation deltaTime

        ( NoChange, Finishing ) ->
            data
                |> updateAnimation deltaTime

        ( NoChange, Stopped ) ->
            data
