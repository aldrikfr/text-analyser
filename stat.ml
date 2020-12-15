open Base

  type t = {
    line_count : int;
    word_count : int;
    character_count : int;
    cc_no_space : int;
    paragraph_count : int;
    sentence_count : int;
    all_words : int;
    good_words : int;
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
      good_words = 0;
    }

  let is_good_word s =
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
    |> List.for_all ~f:(String.( <> ) s)

  let wc_from s = String.split ~on:' ' s |> List.length

  let wc_no_space_from s =
    Str.(global_substitute (regexp "\\s+") (fun _ -> "")) s |> String.length

  let paragraph_count_from s = if String.is_empty s then 1 else 0

  let sentence_count_from s =
    (String.split_on_chars ~on:[ '.'; '?'; '!' ] s |> List.length) - 1

  let update old line =
    let new_words = Str.(split (regexp "\\w+") line) in
    {
      line_count = Int.succ old.line_count;
      word_count = old.word_count + wc_from line;
      character_count = old.character_count + String.length line;
      cc_no_space = old.cc_no_space + wc_no_space_from line;
      paragraph_count = old.paragraph_count + paragraph_count_from line;
      sentence_count = old.sentence_count + sentence_count_from line;
      all_words = old.all_words + List.length new_words;
      good_words =
        (old.good_words + List.(filter ~f:is_good_word new_words |> length));
    }

  let to_string s =
    Printf.sprintf
      "%u lines\n\
       %u characters\n\
       %u characters (excluding spaces)\n\
       %u words\n\
       %u sentences\n\
       %u paragraphs\n\
       %u sentences per paragraph (average)\n\
       %u words per sentence (average)\n\
       %u %% of words are non-fluff words\n"
      s.line_count s.character_count s.cc_no_space s.word_count s.sentence_count
      s.paragraph_count
      (s.sentence_count / s.paragraph_count)
      (s.word_count / s.sentence_count)
      (s.good_words / s.all_words * 100)
