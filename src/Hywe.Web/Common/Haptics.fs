namespace Hywe.Web

open Microsoft.JSInterop
open Elmish

/// <summary>
/// Domain model for tactile haptic feedback patterns on touch-enabled devices.
/// </summary>
type HapticPattern =
    | Tick
    | Snap
    | DragStart
    | Select
    | Drop
    | Success
    | Warning
    | Delete
    | Pulse of int
    | Chord of int list

module Haptics =
    /// <summary>
    /// Pure mapping from high-level semantic HapticPattern to millisecond vibration arrays or integers.
    /// </summary>
    let toVibrationArg (pattern: HapticPattern) : obj =
        match pattern with
        | Tick | Snap -> box 8
        | DragStart -> box 12
        | Select -> box 15
        | Drop | Success -> box [| 15; 35; 20 |]
        | Warning | Delete -> box [| 25; 40; 25 |]
        | Pulse ms -> box ms
        | Chord msList -> box (List.toArray msList)

    /// <summary>
    /// Triggers tactile vibration on the client device via JS interop.
    /// Degrades gracefully on unsupported devices (e.g. desktop, iOS Safari) and guarantees
    /// that no unobserved Task/ValueTask exceptions can interrupt the Elmish render loop.
    /// </summary>
    let trigger (js: IJSRuntime) (pattern: HapticPattern) : unit =
        try
            if not (isNull (box js)) then
                let arg = toVibrationArg pattern
                let vt = js.InvokeVoidAsync("vibrateHaptic", [| arg |])
                if not vt.IsCompletedSuccessfully then
                    vt.AsTask().ContinueWith(fun (t: System.Threading.Tasks.Task) -> ()) |> ignore
        with _ -> ()

    /// <summary>
    /// Elmish command generator for executing haptic feedback within MVU update loops.
    /// </summary>
    let triggerCmd (js: IJSRuntime) (pattern: HapticPattern) : Cmd<'msg> =
        trigger js pattern
        Cmd.none
