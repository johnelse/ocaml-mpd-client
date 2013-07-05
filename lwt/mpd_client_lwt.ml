module Io = struct
  type 'a t = 'a Lwt.t
  let (>>=) = Lwt.bind
  let (>|=) = Lwt.(>|=)
  let return = Lwt.return

  type sockaddr = Lwt_unix.sockaddr
  type file_descr = Lwt_unix.file_descr

  let open_socket addr =
    let domain = Unix.domain_of_sockaddr addr in
    let sock = Lwt_unix.socket domain Lwt_unix.SOCK_STREAM 0 in
    lwt () = Lwt_unix.connect sock addr in
    return sock

  let close_socket = Lwt_unix.close

  let read = Lwt_unix.read
  let write = Lwt_unix.write
end

module Client = Mpd_client.Make(Io)
