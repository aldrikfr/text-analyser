open Stdio
open Base

let stopwords = ["the";"a";"by";"on";"for";"of";"are";"with";"just";"but";"and";"to";"the";"my";"I";"has";"some";"in"]

let lines = In_channel.(create "text.txt" |> input_lines)
let line_count = List.length lines
let text = List.reduce_exn ~f:(^) lines

let word_count = String.split ~on:' ' text |> List.length
let character_count = String.length text
let character_count_no_space = text |> Str.(global_substitute (regexp "\\s+") (fun _ -> "") ) |> String.length
let paragraph_count = List.(filter ~f:String.is_empty lines |> length)
let sentence_count = String.split_on_chars ~on:['.';'?';'!'] text |> List.length

let all_words = Str.(split (regexp "\\w+") text)
let good_words =
  let open List in
  let is_good_word s = for_all ~f:(String.(<>) s) stopwords in
  filter ~f:is_good_word all_words
let good_percentage = List.(length good_words / length all_words) * 100

let () =
  Out_channel.printf "%u lines" line_count;
  Out_channel.printf "%u characters" character_count;
  Out_channel.printf "%u characters (excluding spaces)" character_count_no_space;
  Out_channel.printf "%u words" word_count;
  Out_channel.printf "%u sentences" sentence_count;
  Out_channel.printf "%u paragraphs" paragraph_count;
  Out_channel.printf "%u sentences per paragraph
(average)" (sentence_count / paragraph_count);
  Out_channel.printf "%u words per sentence (average)" (word_count / sentence_count);
  Out_channel.printf "%u of words are non-fluff words" good_percentage


