(** Represents a selection of songs in a playlist. The [Range] variant
 *  represents the selection of all tracks from the first index (inclusive)
 *  to the last index (exclusive). *)
type selection =
  | Single of int
  | Range of (int * int)

(** Receiving an ACK from MPD indicates that an error occurred, possibly
 *  because MPD couldn't understand the command we sent it. *)
exception Received_ack of Mpd_parser.ack

(** Bad_response will be raised if we could't parse the response received
 *  from MPD as an OK or an ACK. *)
exception Bad_response of string

module Make : functor (Io: Mpd_transport.IO) -> sig
  module Connection : sig
    (** Abstract type of connections to an MPD process. *)
    type t

    (** [version_of connection] returns the version string returned by MPD
     *  when [connection] was established. *)
    val version_of : connection:t -> string
  end

  (** [connect addr] opens a connection with an MPD process listening on
   *  address [addr]. *)
  val connect : addr:Io.sockaddr -> Connection.t Io.t

  module Admin : sig
    (** [disableoutput connection outputid] disables the output with id
     *  [outputid]. *)
    val disableoutput : connection:Connection.t -> outputid:int -> unit Io.t

    (** [enableoutput connection outputid] enables the output with id
     *  [outputid]. *)
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

    (** [status connection] returns the playback status and volume
     *  level of MPD. *)
    val status : connection:Connection.t -> Mpd_types.Status.t Io.t

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

  module Playback : sig
    (** [consume flag] turns consume mode on or off, which determines
     *  whether or not songs are removed from the current playlist once
     *  played. *)
    val consume : connection:Connection.t -> flag:bool -> unit Io.t

    (** [next connection] commands the connected MPD to play the next song
     *  in the current playlist. *)
    val next : connection:Connection.t -> unit Io.t

    (** [pause connection flag] pauses or unpauses the connected MPD, if
     *  [flag] is true or false respectively. *)
    val pause : connection:Connection.t -> flag:bool -> unit Io.t

    (** [play connection song] plays the song numbered [song] in the
     *  connected MPD's current playlist. *)
    val play : connection:Connection.t -> song:int -> unit Io.t

    (** [previous connection] commands the connected MPD to play the previous
     *  song in the current playlist. *)
    val previous : connection:Connection.t -> unit Io.t

    (** [random connection flag] turns random playback on or off, depending
     *  on the value of [flag]. *)
    val random : connection:Connection.t -> flag:bool -> unit Io.t

    (** [random connection flag] turns repeat playback on or off, depending
     *  on the value of [flag]. *)
    val repeat : connection:Connection.t -> flag:bool -> unit Io.t

    (** [seek connection song time] instructs the connected MPD to skip to
     *  position [time] in song [song] in the current playlist. *)
    val seek : connection:Connection.t -> song:int -> time:int -> unit Io.t

    (** [seekid connection songid time] instructs the connected MPD to skip to
     *  position [time] in the song in the current playlist
     *  given by [songid]. *)
    val seekid : connection:Connection.t -> songid:int -> time:int -> unit Io.t

    (** [single connection flag] turns single-track playback on or off,
     *  depending on the value of [flag]. *)
    val single : connection:Connection.t -> flag:bool -> unit Io.t

    (** [stop connection] commands the connected MPD to stop playback. *)
    val stop : connection:Connection.t -> unit Io.t
  end

  module Playlist : sig
    (* Commands for manipulating the current playlist. *)

    (** [add connection uri] recursively adds file [uri] to the end of the
     *  current playlist. If [uri] is a directory, then all files in that
     *  directory will be added recursively. *)
    val add : connection:Connection.t -> uri:string -> unit Io.t

    (** [addid connection uri position] adds file [uri] to the playlist at
     *  position [position], and returns the songid. *)
    val addid : connection:Connection.t ->
      uri:string -> position:(int option) -> int Io.t

    (** [clear connection] clears all tracks from the current playlist. *)
    val clear : connection:Connection.t -> unit Io.t

    (** [delete connection selection] deletes the tracks indicated by
     *  [selection] from the current playlist. *)
    val delete : connection:Connection.t -> selection:selection -> unit Io.t

    (** [deleteid connection songid] deletes song [songid] from the current
     *  playlist. *)
    val deleteid : connection:Connection.t -> songid:int -> unit Io.t

    (** [move connection selection position] moves the tracks specified by
     *  [selection] to [position] in the current playlist. *)
    val move : connection:Connection.t ->
      selection:selection -> position:int -> unit Io.t

    (** [moveid connection songid position] moves [songid] to [position] in the
     *  current playlist. *)
    val moveid : connection:Connection.t ->
      songid:int -> position:int -> unit Io.t

    (** [swap connection position1 position2] swaps the songs in the current
     *  playlist in positions [position1] and [position2]. *)
    val swap : connection:Connection.t ->
      position1:int -> position2:int -> unit Io.t

    (** [swapid connection songid1 songid2] swaps the songs in the current
     *  playlist with songids [songid1] and [songid2]. *)
    val swapid : connection:Connection.t ->
      songid1:int -> songid2:int -> unit Io.t

    (* Commands for managing multiple playlists. *)

    (** [rename connection name new_name] renames the playlist [name.m3u] to
     *  [new_name.m3u]. *)
    val rename : connection:Connection.t ->
      name:string -> new_name:string -> unit Io.t

    (** Removes the playlist with [name.m3u] from the playlist directory. *)
    val rm : connection:Connection.t -> name:string -> unit Io.t

    (** Saves the current playlist as [name.m3u]. *)
    val save : connection:Connection.t -> name:string -> unit Io.t
  end
end
