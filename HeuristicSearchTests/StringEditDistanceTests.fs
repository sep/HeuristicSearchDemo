module StringEditDistanceTests

open NUnit.Framework
open Microsoft.FSharp.Reflection
module SED = StringEditDistance
module UCS = UniformCostSearch

[<SetUp>]
let Setup () =
    ()

[<Test>]
let alphabetic_indexing () =
    let characters = [ SED.A; SED.B; SED.C; SED.D; SED.E; SED.F; SED.G; SED.H; SED.I; SED.J; SED.K; SED.L;
                       SED.M; SED.N; SED.O; SED.P; SED.Q; SED.R; SED.S; SED.T; SED.U; SED.V; SED.W; SED.X;
                       SED.Y; SED.Z ]
    List.iteri (fun index element ->
        let me = List.item index characters
        let expected = index + 1
        let experienced = SED.alphabetical_number me
        Assert.AreEqual(expected, experienced)) characters

[<Test>]
let alphabetic_distances () =
    let characters = [ SED.A; SED.B; SED.C; SED.D; SED.E; SED.F; SED.G; SED.H; SED.I; SED.J; SED.K; SED.L;
                       SED.M; SED.N; SED.O; SED.P; SED.Q; SED.R; SED.S; SED.T; SED.U; SED.V; SED.W; SED.X;
                       SED.Y; SED.Z ]
    let expected_maximums = List.map float [ 13..25 ]
    let expected_maximums = (List.rev expected_maximums) @ expected_maximums
    List.iteri2 (fun index element expected_max ->
        let me = List.item index characters
        let experienced_distances = List.map (SED.alphabetical_distance me) characters
        let experienced_max = List.fold max 0. experienced_distances
        let zero_index = List.findIndex (fun el -> el = 0.) experienced_distances
        Assert.AreEqual(index, zero_index)
        Assert.AreEqual(expected_max, experienced_max)) characters expected_maximums

[<Test>]
let string_conversion () =
    let expected_output = "__HELLO WORLD__"
    let element_ar = [| SED.Null; SED.Null; SED.Alphabetical SED.H; SED.Alphabetical SED.E;
                        SED.Alphabetical SED.L; SED.Alphabetical SED.L;
                        SED.Alphabetical SED.O; SED.Alphabetical SED.Whitespace; SED.Alphabetical SED.W;
                        SED.Alphabetical SED.O; SED.Alphabetical SED.R; SED.Alphabetical SED.L;
                        SED.Alphabetical SED.D; SED.Null; SED.Null |]
    let as_string = SED.string_of_element_array element_ar
    Assert.AreEqual(expected_output, as_string)

[<Test>]
let ingestion () =
    let expected_output = [| SED.Null; SED.Null; SED.Alphabetical SED.H; SED.Alphabetical SED.E;
    SED.Alphabetical SED.L; SED.Alphabetical SED.L;
    SED.Alphabetical SED.O; SED.Alphabetical SED.Whitespace; SED.Alphabetical SED.W;
    SED.Alphabetical SED.O; SED.Alphabetical SED.R; SED.Alphabetical SED.L;
    SED.Alphabetical SED.D; SED.Null; SED.Null |]
    let input = "__HELLO WORLD__"
    let actual_output = SED.element_array_of_string input in
    Assert.AreEqual(expected_output, actual_output)

[<Test>]
let test_shift_left() =
    let expected_output = "HELLO WORLD____"
    let base_ar = SED.element_array_of_string "__HELLO WORLD__"
    let state = { SED.state = base_ar; SED.generated_by = SED.Noop }
    let state' = SED.apply_nondestructive state SED.ShiftLeft
    let state'' = SED.apply_nondestructive state' SED.ShiftLeft
    let as_string = SED.string_of_element_array state''.state
    Assert.AreEqual(expected_output, as_string)

[<Test>]
let test_shift_right() =
    let expected_output = "____HELLO WORLD"
    let base_ar = SED.element_array_of_string "__HELLO WORLD__"
    let state = { SED.state = base_ar; SED.generated_by = SED.Noop }
    let state' = SED.apply_nondestructive state SED.ShiftRight
    let state'' = SED.apply_nondestructive state' SED.ShiftRight
    let as_string = SED.string_of_element_array state''.state
    Assert.AreEqual(expected_output, as_string)

[<Test>]
let test_invalid_shift_right() =
    let base_ar = SED.element_array_of_string "__HELLO WORLD"
    let state = { SED.state = base_ar; SED.generated_by = SED.Noop }

    try
        ignore (SED.apply_nondestructive state SED.ShiftRight)
        Assert.Fail "Should have failed on shift right"
    with error ->
        let expectedMessage = "Invalid action Shift Right"
        Assert.AreEqual(error.Message, expectedMessage)

[<Test>]
let test_invalid_shift_left() =
    let base_ar = SED.element_array_of_string "HELLO WORLD__"
    let state = { SED.state = base_ar; SED.generated_by = SED.Noop }

    try
        ignore (SED.apply_nondestructive state SED.ShiftLeft)
        Assert.Fail "Should have failed on shift left"
    with error ->
        let expectedMessage = "Invalid action Shift Left"
        Assert.AreEqual(expectedMessage, error.Message)

[<Test>]
let remove_fails_on_null_value() =
    let base_ar = SED.element_array_of_string "_____"
    let state = { SED.state = base_ar; SED.generated_by = SED.Noop }
    try
        ignore(SED.apply_nondestructive state (SED.Remove 0))
        Assert.Fail "Shouldn't be able to remove any characters from null string"
    with error ->
        Assert.AreEqual ("Invalid action Remove @ 0", error.Message)

[<Test>]
let remove_succeed() =
    let base_ar = SED.element_array_of_string "ABCDE"
    let state = { SED.state = base_ar; SED.generated_by = SED.Noop }
    let action = SED.Remove 2
    let expected_output = "ABDE_"
    let state' = SED.apply_nondestructive state action
    let actual_output = SED.string_of_element_array state'.state
    Assert.AreEqual(expected_output, actual_output)

[<Test>]
let add_succeed() =
    let base_ar = SED.element_array_of_string "ABDE_"
    let state = { SED.state = base_ar; SED.generated_by = SED.Noop }
    let action = SED.Add { SED.index = 2; SED.character = SED.C }
    let expected_output = "ABCDE"
    let state' = SED.apply_nondestructive state action
    let actual_output = SED.string_of_element_array state'.state
    Assert.AreEqual(expected_output, actual_output)

[<Test>]
let add_fails_on_full_string() =
    let base_ar = SED.element_array_of_string "ABCDE"
    let state = { SED.state = base_ar; SED.generated_by = SED.Noop }
    let action = SED.Add { SED.index = 2; SED.character = SED.C }
    try
        SED.apply_nondestructive state action |> ignore
        Assert.Fail "Expected add on full string to fail"
    with error ->
        Assert.AreEqual("Invalid action Add C @ 2", error.Message)

[<Test>]
let replace_succeeds () =
    let base_ar = SED.element_array_of_string "ABCDE"
    let state = { SED.state = base_ar; SED.generated_by = SED.Noop }
    let action = SED.Replace { SED.index = 2; SED.character = SED.Z }
    let state' = SED.apply_nondestructive state action
    let expected_output = "ABZDE"
    let actual_output = SED.string_of_element_array state'.state
    Assert.AreEqual(expected_output, actual_output)

[<Test>]
let expand_does_not_allow_shift_after_non_shift () =
    let base_ar = SED.element_array_of_string "_"
    let state = { SED.state = base_ar; SED.generated_by = SED.Remove 0 }
    let problem = { SED.start = base_ar; SED.finish = base_ar }
    let successors = SED.expand problem state
    let shift_exists = List.exists (fun (state : SED.State, _) ->
        state.generated_by = SED.ShiftLeft || state.generated_by = SED.ShiftRight) successors
    Assert.IsFalse(shift_exists)

[<Test>]
let expand_does_allows_shift_after_noop () =
    let base_ar = SED.element_array_of_string "A_"
    let state = { SED.state = base_ar; SED.generated_by = SED.Noop }
    let problem = { SED.start = base_ar; SED.finish = base_ar }
    let successors = SED.expand problem state
    let shift_exists = List.exists (fun (state : SED.State, _) ->
        state.generated_by = SED.ShiftLeft || state.generated_by = SED.ShiftRight) successors
    Assert.IsTrue(shift_exists)

[<Test>]
let expands_contain_reasonable_elements () =
    let problem = SED.instance_of_strings "HERE" "THERE"
    let state = SED.make_initial_state problem
    let successors = SED.expand problem state
    let expected_length = 37
    let has_remove ((el : SED.State), _) = match el.generated_by with SED.Remove _ -> true | _ -> false
    let has_add ((el : SED.State), _) = match el.generated_by with SED.Add _ -> true | _ -> false
    let has_replace ((el : SED.State), _) = match el.generated_by with SED.Replace _ -> true | _ -> false
    let has_shiftleft ((el : SED.State), _) = match el.generated_by with SED.ShiftLeft -> true | _ -> false
    let has_shiftright ((el : SED.State), _) = match el.generated_by with SED.ShiftRight -> true | _ -> false
    let has_noop ((el : SED.State), _) = match el.generated_by with SED.Noop -> true | _ -> false
    List.exists has_remove successors |> Assert.True
    List.exists has_add successors |> Assert.True
    List.exists has_replace successors |> Assert.True
    List.exists has_shiftleft successors |> Assert.False
    List.exists has_shiftright successors |> Assert.True
    List.exists has_noop successors |> Assert.False
    Assert.AreEqual(expected_length, successors.Length)