module Make : functor (Io: Mpd_transport.IO) -> sig
  module Connection : sig
    type t
    val version_of : connection:t -> string
  end

  val connect : addr:Io.sockaddr -> Connection.t Io.t
  val close : connection:Connection.t -> unit Io.t
end
