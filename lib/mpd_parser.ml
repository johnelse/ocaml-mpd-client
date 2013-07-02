let split ~str ~c =
  let rec rev_split' ~str ~c ~acc =
    try
      let index = String.index str c in
      let before = String.sub str 0 index in
      let after = String.sub str (index + 1) (String.length str - index - 1) in
      rev_split' ~str:after ~c ~acc:(before :: acc)
    with Not_found ->
      str :: acc
  in
  List.rev (rev_split' ~str ~c ~acc:[])

type ack = {
  error: string;
  command_num: int;
  command: string;
  message_text: string;
}

type response =
  | Ack of ack
  | Ok of string list
  | Parse_failure of string

let re_ack =
  Re.compile (Re_perl.re "^ACK \\[([0-9]*)@([0-9]*)\\] \\{([a-z]*)\\} (.*)")

let parse_ack ~response =
  try
    let matches = Re.exec re_ack (String.trim response) in
    Ack {
      error = Re.get matches 1;
      command_num = int_of_string (Re.get matches 2);
      command = Re.get matches 3;
      message_text = Re.get matches 4;
    }
  with Failure _ | Not_found ->
    Parse_failure response

let parse_ok ~response =
  let lines = split (String.trim response) '\n' in
  let rec parse acc lines =
    match lines with
    | ["OK"] -> Ok (List.rev acc)
    | line :: rest -> parse (line :: acc) rest
    | _ -> Parse_failure response
  in
  parse [] lines

let parse_response ~response =
  if String.sub response 0 3 = "ACK"
  then parse_ack ~response
  else parse_ok ~response
