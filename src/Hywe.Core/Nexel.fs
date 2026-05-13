namespace Hywe.Core

/// <summary> 
/// Nexel (Nested Engine) 
/// Handles logic for Nested Coxels (N# markers).
/// </summary>
module Nexel =
    open System
    open Lexel

    type NestData = {
        Marker: string
        BaseNodeId: string
        Attributes: LexelAttributes
        Nodes: LexelNode list
    }

    /// <summary> 
    /// Checks if a marker is a Nest marker (e.g., N1, N2).
    /// </summary>
    let isNestMarker (marker: string) =
        not (String.IsNullOrEmpty(marker)) && marker.StartsWith("N")

    /// <summary> 
    /// Extracts the nest index from a marker.
    /// </summary>
    let getNestIndex (marker: string) =
        if isNestMarker marker then
            match Int32.TryParse(marker.Substring(1)) with
            | true, v -> Some v
            | _ -> None
        else None
