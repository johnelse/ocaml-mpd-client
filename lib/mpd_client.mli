module Make : functor (Io: Mpd_transport.IO) -> sig
  module Connection : sig
    (** Abstract type of connections to an MPD process. *)
    type t

    (** [version_of connection] returns the version string returned by MPD
     *  when [connection] was established. *)
    val version_of : connection:t -> string
  end

  (** Receiving an ACK from MPD indicates that an error occurred, possibly
   *  because MPD couldn't understand the command we sent it. *)
  exception Received_ack of Mpd_parser.ack

  (** Bad_response will be raised if we could't parse the response received
   *  from MPD as an OK or an ACK. *)
  exception Bad_response of string

  (** [connect addr] opens a connection with an MPD process listening on
   *  address [addr]. *)
  val connect : addr:Io.sockaddr -> Connection.t Io.t

  module Admin : sig
    (** [disableoutput connection outputid] disables the output with id
     *  outputid. *)
    val disableoutput : connection:Connection.t -> outputid:int -> unit Io.t

    (** [enableoutput connection outputid] enables the output with id
     *  outputid. *)
    val enableoutput : connection:Connection.t -> outputid:int -> unit Io.t
  end

  module Info : sig
    (** [commands connection] returns the list of commands available to the
     *  current user. *)
    val commands : connection:Connection.t -> string list Io.t

    (** [notcommands connection] returns the list of commands not available to
     *  the current user. *)
    val notcommands : connection:Connection.t -> string list Io.t

    (** [outputs connection] returns information about all available outputs. *)
    val outputs : connection:Connection.t -> Mpd_types.Output.t list Io.t

    (** [stats connection] returns statistics for the connected MPD. *)
    val stats : connection:Connection.t -> Mpd_types.Stats.t Io.t

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
