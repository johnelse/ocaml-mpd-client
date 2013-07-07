module Make : functor (Io: Mpd_transport.IO) -> sig
  module Connection : sig
    (** Abstract type of connections to an MPD process. *)
    type t

    (** [version_of connection] returns the version string returned by MPD
     *  when [connection] was established. *)
    val version_of : connection:t -> string
  end

  exception Received_ack of Mpd_parser.ack
  exception Bad_response of string

  (** [connect addr] opens a connection with an MPD process listening on
   *  address [addr]. *)
  val connect : addr:Io.sockaddr -> Connection.t Io.t

  module Info : sig
    (** [commands connection] returns the list of commands available to the
     *  current user. *)
    val commands : connection:Connection.t -> string list Io.t

    (** [notcommands connection] returns the list of commands not available to
     *  the current user. *)
    val notcommands : connection:Connection.t -> string list Io.t

    (** [tagtypes connection] returns the list of available song metadata. *)
    val tagtypes : connection:Connection.t -> string list Io.t

    (** [urlhandlers connection] returns list of URL handlers which the
     *  connected MPD has available. *)
    val urlhandlers : connection:Connection.t -> string list Io.t
  end

  module Misc : sig
    (** [close connection] closes the specified MPD connection. The underlying
     *  file descriptor will be closed, and the connection will no longer be
     *  usable for communicating with MPD. *)
    val close : connection:Connection.t -> unit Io.t

    (** [ping connection] tests that MPD is still responsive. It will return
     *  unit if MPD is responsive, otherwise will raise an exception. *)
    val ping : connection:Connection.t -> unit Io.t
  end
end
