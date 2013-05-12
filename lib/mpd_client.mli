module Make : functor (Io: Mpd_transport.IO) -> sig
  module Connection : sig
    type t
  end

  val connect : addr:Io.sockaddr -> Connection.t Io.t
  val disconnect : connection:Connection.t -> unit Io.t
end
