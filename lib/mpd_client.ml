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

  let send_raw ~connection ~data =
    let formatted_data = Printf.sprintf "%s\n" data in
    let length = String.length formatted_data in
    really_write ~connection ~data:formatted_data ~offset:0 ~length
end
