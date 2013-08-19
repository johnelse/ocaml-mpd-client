type selection =
  | Single of int
  | Range of (int * int)

let string_of_selection = function
  | Single x -> string_of_int x
  | Range (first, last) -> Printf.sprintf "%d:%d" first last

let quote str = "\"" ^ str ^ "\""

let string_of_flag = function
  | true -> "1" | false -> "0"

exception Received_ack of Mpd_parser.ack
exception Bad_response of string

module Make(Io: Mpd_transport.IO) = struct
  module Connection = struct
    type t = {
      sock: Io.file_descr;
      version: string;
    }

    let version_of ~connection = connection.version
  end

  open Io

  let rec really_write ~sock ~data ~offset ~length =
    if length = 0 then return () else
      Io.write sock data offset length
      >>= (fun chars_written ->
        really_write ~sock ~data
          ~offset:(offset + chars_written)
          ~length:(length - chars_written))

  let read_all ~sock =
    let read_length = 1024 in
    let read_data = String.create read_length in
    let rec read_all' ~buffer =
      Io.read sock read_data 0 read_length
      >>= (fun chars_read ->
        if chars_read = 0
        then return (Buffer.contents buffer)
        else if chars_read < read_length then begin
          Buffer.add_substring buffer read_data 0 chars_read;
          return (Buffer.contents buffer)
        end else begin
          Buffer.add_string buffer read_data;
          read_all' ~buffer
        end)
    in
    let buffer = Buffer.create 0 in
    read_all' ~buffer

  let parse_connection_response ~response =
    try
      Scanf.sscanf response "OK MPD %s\n" (fun version -> version)
    with Scanf.Scan_failure _ ->
      raise (Bad_response response)

  let connect ~addr =
    Io.open_socket addr
    >>= (fun sock ->
      read_all ~sock
      >>= (fun response ->
        let version = parse_connection_response ~response in
        return {
          Connection.sock = sock;
          version = version;
        }))

  let send_raw ~connection ~data =
    let formatted_data = Printf.sprintf "%s\n" (String.concat " " data) in
    let length = String.length formatted_data in
    let sock = connection.Connection.sock in
    really_write ~sock ~data:formatted_data ~offset:0 ~length

  let expect_ok ~connection =
    let open Mpd_parser in
    read_all ~sock:connection.Connection.sock
    >>= (fun response ->
      match parse_response ~response with
      | Ok body -> return body
      | Ack ack -> raise (Received_ack ack)
      | _ -> raise (Bad_response response))

  let send_raw_get_response ~connection ~data =
    send_raw ~connection ~data
    >>= (fun () -> expect_ok ~connection)

  module Admin = struct
    let disableoutput ~connection ~outputid =
      send_raw_get_response
        ~connection ~data:["disableoutput"; string_of_int outputid]
      >|= ignore

    let enableoutput ~connection ~outputid =
      send_raw_get_response
        ~connection ~data:["enableoutput"; string_of_int outputid]
      >|= ignore
  end

  module Info = struct
    let commands ~connection =
      send_raw_get_response ~connection ~data:["commands"]
      >|= List.map snd

    let decoders ~connection =
      send_raw_get_response ~connection ~data:["decoders"]
      >|= Mpd_types.Decoder.multiple_of_kvpairs

    let notcommands ~connection =
      send_raw_get_response ~connection ~data:["notcommands"]
      >|= List.map snd

    let outputs ~connection =
      send_raw_get_response ~connection ~data:["outputs"]
      >|= Mpd_types.Output.multiple_of_kvpairs

    let stats ~connection =
      send_raw_get_response ~connection ~data:["stats"]
      >|= Mpd_types.Stats.of_kvpairs

    let status ~connection =
      send_raw_get_response ~connection ~data:["status"]
      >|= Mpd_types.Status.of_kvpairs

    let tagtypes ~connection =
      send_raw_get_response ~connection ~data:["tagtypes"]
      >|= List.map snd

    let urlhandlers ~connection =
      send_raw_get_response ~connection ~data:["urlhandlers"]
      >|= List.map snd
  end

  module Misc = struct
    let close ~connection =
      send_raw ~connection ~data:["close"]
      >>= (fun () ->
        let sock = connection.Connection.sock in
        Io.close_socket sock)

    let password ~connection ~password =
      send_raw_get_response ~connection ~data:["password"; password]
      >|= ignore

    let ping ~connection =
      send_raw_get_response ~connection ~data:["ping"]
      >|= ignore
  end

  module Playback = struct
    let consume ~connection ~flag =
      send_raw_get_response ~connection ~data:["consume"; string_of_flag flag]
      >|= ignore

    let next ~connection =
      send_raw_get_response ~connection ~data:["next"]
      >|= ignore

    let pause ~connection ~flag =
      send_raw_get_response ~connection ~data:["pause"; string_of_flag flag]
      >|= ignore

    let play ~connection ~song =
      send_raw_get_response ~connection ~data:["play"; string_of_int song]
      >|= ignore

    let previous ~connection =
      send_raw_get_response ~connection ~data:["previous"]
      >|= ignore

    let random ~connection ~flag =
      send_raw_get_response ~connection ~data:["random"; string_of_flag flag]
      >|= ignore

    let repeat ~connection ~flag =
      send_raw_get_response ~connection ~data:["repeat"; string_of_flag flag]
      >|= ignore

    let seek ~connection ~song ~time =
      send_raw_get_response ~connection
        ~data:["seek"; string_of_int song; string_of_int time]
      >|= ignore

    let seekid ~connection ~songid ~time =
      send_raw_get_response ~connection
        ~data:["seekid"; string_of_int songid; string_of_int time]
      >|= ignore

    let single ~connection ~flag =
      send_raw_get_response ~connection ~data:["single"; string_of_flag flag]
      >|= ignore

    let stop ~connection =
      send_raw_get_response ~connection ~data:["stop"]
      >|= ignore
  end

  module Playlist = struct
    (* Commands for manipulating the current playlist. *)

    let add ~connection ~uri =
      send_raw_get_response ~connection ~data:["add"; quote uri]
      >|= ignore

    let addid ~connection ~uri ~position =
      let data =
        ["addid"; quote uri] @
        (match position with Some x -> [string_of_int x] | None -> [])
      in
      send_raw_get_response ~connection ~data
      >|= (fun response -> int_of_string (snd (List.nth response 0)))

    let clear ~connection =
      send_raw_get_response ~connection ~data:["clear"]
      >|= ignore

    let delete ~connection ~selection =
      send_raw_get_response ~connection
        ~data:["delete"; string_of_selection selection]
      >|= ignore

    let deleteid ~connection ~songid =
      send_raw_get_response ~connection ~data:["deleteid"; string_of_int songid]
      >|= ignore

    let move ~connection ~selection ~position =
      send_raw_get_response ~connection
        ~data:["move"; string_of_selection selection; string_of_int position]
      >|= ignore

    let moveid ~connection ~songid ~position =
      send_raw_get_response ~connection
        ~data:["moveid"; string_of_int songid; string_of_int position]
      >|= ignore

    let swap ~connection ~position1 ~position2 =
      send_raw_get_response ~connection
        ~data:["swap"; string_of_int position1; string_of_int position2]
      >|= ignore

    let swapid ~connection ~songid1 ~songid2 =
      send_raw_get_response ~connection
        ~data:["swapid"; string_of_int songid1; string_of_int songid2]
      >|= ignore

    (* Commands for managing multiple playlists. *)

    let listplaylist ~connection ~name =
      send_raw_get_response ~connection ~data:["listplaylist"; name]
      >|= List.map snd

    let playlistadd ~connection ~name ~uri =
      send_raw_get_response ~connection ~data:["playlistadd"; name; quote uri]
      >|= ignore

    let playlistclear ~connection ~name =
      send_raw_get_response ~connection ~data:["playlistclear"; name]
      >|= ignore

    let playlistdelete ~connection ~name ~songpos =
      send_raw_get_response ~connection
        ~data:["playlistdelete"; name; string_of_int songpos]
      >|= ignore

    let rename ~connection ~name ~new_name =
      send_raw_get_response ~connection ~data:["rename"; name; new_name]
      >|= ignore

    let rm ~connection ~name =
      send_raw_get_response ~connection ~data:["rm"; name]
      >|= ignore

    let save ~connection ~name =
      send_raw_get_response ~connection ~data:["save"; name]
      >|= ignore
  end
end
