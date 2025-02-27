open CommonTypes
open Utility
open Types

let show_kind_inference
= Settings.(flag "show_kind_inference"
            |> depends Debug.enabled
            |> convert parse_bool
            |> sync)