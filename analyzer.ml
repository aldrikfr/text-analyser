open Stdio
open Base

let stopwords =
  [
    "the";
    "a";
    "by";
    "on";
    "for";
    "of";
    "are";
    "with";
    "just";
    "but";
    "and";
    "to";
    "the";
    "my";
    "I";
    "has";
    "some";
    "in";
  ]

let lines = In_channel.(create "text.txt" |> input_lines)

let line_count = List.length lines

let text = List.reduce_exn ~f:( ^ ) lines

let word_count = String.split ~on:' ' text |> List.length

let character_count = String.length text

let character_count_no_space =
  text |> Str.(global_substitute (regexp "\\s+") (fun _ -> "")) |> String.length

let paragraph_count = List.(filter ~f:String.is_empty lines |> length)

let sentence_count =
  String.split_on_chars ~on:[ '.'; '?'; '!' ] text |> List.length

let all_words = Str.(split (regexp "\\w+") text)

let good_words =
  let open List in
  let is_good_word s = for_all ~f:(String.( <> ) s) stopwords in
  filter ~f:is_good_word all_words

let good_percentage = List.(length good_words / length all_words) * 100

let () =
  [
    ("lines", line_count);
    ("characters", character_count);
    ("characters (excluding spaces)", character_count_no_space);
    ("words", word_count);
    ("sentences", sentence_count);
    ("paragraphs", paragraph_count);
    ("sentences per paragraph (average)", sentence_count / paragraph_count);
    ("words per sentence (average)", word_count / sentence_count);
    ("of words are non-fluff words", good_percentage);
  ]
  |> List.iter ~f:(fun (value_label, n) ->
         Out_channel.printf "%u %s\n" n value_label)
