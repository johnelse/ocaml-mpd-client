module Make(Io: Mpd_transport.IO) = struct
  type connection_t = {
    sock: Io.file_descr;
  }

  open Io

  let rec really_write ~connection ~data ~offset ~length =
    if length = 0 then return () else
      Io.write connection.sock data offset length
      >>= (fun chars_written ->
        really_write ~connection ~data
          ~offset:(offset + chars_written)
          ~length:(length - chars_written))

  let read_all ~connection =
    let read_length = 1024 in
    let read_data = String.create read_length in
    let rec read_all' ~buffer =
      Io.read connection.sock read_data 0 read_length
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

  let send_raw ~connection ~data =
    let formatted_data = Printf.sprintf "%s\n" data in
    let length = String.length formatted_data in
    really_write ~connection ~data:formatted_data ~offset:0 ~length
end
