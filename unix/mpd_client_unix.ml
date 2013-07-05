module Io = struct
  type 'a t = 'a
  let (>>=) x f = f x
  let (>|=) x f = f x
  let return x = x

  type sockaddr = Unix.sockaddr
  type file_descr = Unix.file_descr

  let open_socket addr =
    let domain = Unix.domain_of_sockaddr addr in
    let sock = Unix.socket domain Unix.SOCK_STREAM 0 in
    Unix.connect sock addr;
    sock

  let close_socket = Unix.close

  let read = Unix.read
  let write = Unix.write
end

module Client = Mpd_client.Make(Io)
