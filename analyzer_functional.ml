open Stdio

let () =
  In_channel.(create "text.txt" |> fold_lines ~f:Stat.update ~init:Stat.empty)
  |> Stat.to_string |> Out_channel.print_endline
