open Base
open Stdio

module Stat = struct
type t = {
    line_count : int;
    word_count : int;
    character_count : int;
    cc_no_space : int;
    paragraph_count : int;
    sentence_count : int;
    all_words : int;
    good_words : int
}

let empty =
    {
      line_count = 0;
      word_count = 0;
      character_count = 0;
      cc_no_space = 0;
      paragraph_count = 0;
      sentence_count = 0;
      all_words = 0;
      good_words = 0
    }


let is_good_word s =
  ["the";"a";"by";"on";"for";"of";"are";"with";"just";"but";"and";"to";"the";"my";"I";"has";"some";"in"]
  |> List.for_all ~f:(String.(<>) s)

let update old line =
    let new_words = Str.(split (regexp "\\w+") line) in
    {
      line_count = Int.succ old.line_count;
      word_count = old.word_count + (String.split ~on:' ' line |> List.length);
      character_count = old.character_count + String.length line;
      cc_no_space = old.cc_no_space + (line |> Str.(global_substitute (regexp "\\s+") (fun _ -> "") ) |> String.length);
      paragraph_count = old.paragraph_count + (if String.is_empty line then 1 else 0);
      sentence_count = old.sentence_count - 1 + (String.split_on_chars ~on:['.';'?';'!'] line |> List.length);
      all_words = old.all_words + List.length new_words;
      good_words = old.good_words + List.(filter ~f:is_good_word new_words |> length)
    }

let to_string s =
  Out_channel.printf
"%u lines
%u characters
%u characters (excluding spaces)
%u words
%u sentences
%u paragraphs
%u sentences per paragraph (average)
%u words per sentence (average)
%u %% of words that are non-fluff words\n"
    s.line_count
    s.character_count
    s.cc_no_space
    s.word_count
    s.sentence_count
    s.paragraph_count
    (s.sentence_count / s.paragraph_count)
    (s.word_count / s.sentence_count)
    (s.good_words / s.all_words * 100)
end


let () =
  In_channel.(create "text.txt" |> fold_lines ~f:Stat.update ~init:Stat.empty) |> Stat.to_string
