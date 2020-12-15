open Stdio
open Base

let stopwords = ["the";"a";"by";"on";"for";"of";"are";"with";"just";"but";"and";"to";"the";"my";"I";"has";"some";"in"]

let lines = In_channel.(create "text.txt" |> input_lines)
let line_count = List.length lines
let text = List.reduce_exn ~f:(^) lines

let word_count = String.split ~on:' ' text |> List.length
let character_count = String.length text
let character_count_no_space = Str.(global_substitute (regexp "\\s+") (fun _ -> "") text |> String.length)
let paragraph_count = List.(filter ~f:String.is_empty lines |> length)
let sentence_count = String.split_on_chars ~on:['.';'?';'!'] text

let all_words = Str.(split (regexp "\\w+") text)



