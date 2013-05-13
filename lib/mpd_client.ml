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

  exception Bad_connection_response

  let parse_connection_response ~response =
    try
      Scanf.sscanf response "OK MPD %s\n" (fun version -> version)
    with Scanf.Scan_failure _ ->
      raise Bad_connection_response

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
    let formatted_data = Printf.sprintf "%s\n" data in
    let length = String.length formatted_data in
    let sock = connection.Connection.sock in
    really_write ~sock ~data:formatted_data ~offset:0 ~length

  let disconnect ~connection =
    send_raw ~connection ~data:"close\n"
    >>= (fun () ->
      let sock = connection.Connection.sock in
      Io.close_socket sock)
end
