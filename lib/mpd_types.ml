exception Missing_key of string
exception Unexpected_value of (string * string)

let find key kvpairs =
  try List.assoc key kvpairs
  with Not_found -> raise (Missing_key key)

let find_opt key kvpairs =
  try Some (find key kvpairs)
  with Missing_key _ -> None

let map_opt f x =
  match x with
  | None -> None
  | Some value -> Some (f value)

let take_while f items =
  let rec take_while' acc f items =
    match items with
    | [] -> acc, []
    | head :: rest as all ->
      if f head
      then take_while' (head :: acc) f rest
      else acc, all
  in
  match take_while' [] f items with
  | first, second -> List.rev first, second

module Decoder = struct
  type t = {
    plugin: string;
    suffixes: string list;
    mime_types: string list;
  }

  let of_kvpairs kvpairs =
    let plugin = match List.hd kvpairs with
    | "plugin", value -> value
    | _ -> raise (Missing_key "plugin")
    in
    let suffixes, mime_types = List.fold_left
      (fun (suffixes, mime_types) (key, value) ->
        match key, value with
        | "suffix", value -> (value :: suffixes, mime_types)
        | "mime_type", value -> (suffixes, value :: mime_types)
        | key, value -> raise (Unexpected_value (key, value)))
      ([], []) (List.tl kvpairs)
    in
    {
      plugin = plugin;
      suffixes = List.rev suffixes;
      mime_types = List.rev mime_types;
    }

  let multiple_of_kvpairs kvpairs =
    let rec partition acc kvpairs =
      (* Turn a list into a list of lists, where each list contains the
       * key-value pairs for one decoder. *)
      match kvpairs with
      | [] -> acc
      | _ ->
        let first_pair = List.hd kvpairs in
        let other_pairs, rest =
          take_while (fun (key, _) -> key <> "plugin") (List.tl kvpairs)
        in
        partition ((first_pair :: other_pairs) :: acc) rest
    in
    List.rev (List.map of_kvpairs (partition [] kvpairs))
end

module Output = struct
  type t = {
    outputid: int;
    outputname: string;
    outputenabled: bool;
  }

  let of_kvpairs kvpairs =
    {
      outputid = int_of_string (find "outputid" kvpairs);
      outputname = find "outputname" kvpairs;
      outputenabled =
        match find "outputenabled" kvpairs with "1" -> true | _ -> false;
    }

  let multiple_of_kvpairs kvpairs =
    let rec read_all acc kvpairs =
      match kvpairs with
      | a :: b :: c :: rest ->
        let output = of_kvpairs [a; b; c] in
        read_all (output :: acc) rest
      | _ -> acc
    in
    List.rev (read_all [] kvpairs)
end

module PathList = struct
  type t = {
    directories: string list;
    files: string list
  }

  let of_kvpairs kvpairs =
    let directories, files =
      List.fold_left
        (fun (directories, files) (key, value) ->
          match key with
          | "directory" -> value :: directories, files
          | "file" -> directories, value :: files
          | _ -> raise (Unexpected_value (key, value)))
        ([], []) (List.rev kvpairs)
    in
    {
      directories = directories;
      files = files;
    }
end

module Scope = struct
  type t =
    | Any
    | Filename
    | Album
    | Artist
    | Comment
    | Composer
    | Date
    | Disc
    | Genre
    | Name
    | Performer
    | Title
    | Track

  let to_string = function
    | Any -> "any"
    | Filename -> "filename"
    | Album -> "album"
    | Artist -> "artist"
    | Comment -> "comment"
    | Composer -> "composer"
    | Date -> "date"
    | Disc -> "disc"
    | Genre -> "genre"
    | Name -> "name"
    | Performer -> "performer"
    | Title -> "title"
    | Track -> "track"
end

module Stats = struct
  type t = {
    artists: int;
    albums: int;
    songs: int;
    uptime: int;
    playtime: int;
    db_playtime: int64;
    db_update: int64;
  }

  let of_kvpairs kvpairs =
    {
      artists = int_of_string (find "artists" kvpairs);
      albums = int_of_string (find "albums" kvpairs);
      songs = int_of_string (find "songs" kvpairs);
      uptime = int_of_string (find "uptime" kvpairs);
      playtime = int_of_string (find "playtime" kvpairs);
      db_playtime = Int64.of_string (find "db_playtime" kvpairs);
      db_update = Int64.of_string (find "db_update" kvpairs);
    }
end

module Status = struct
  type state_t =
    | Playing
    | Paused
    | Stopped

  type time_t = {
    elapsed: int;
    total: int;
  }

  type audio_t = {
    sample_rate: int;
    bits: int;
    channels: int;
  }

  type t = {
    volume: int;
    repeat: bool;
    random: bool;
    single: bool;
    consume: bool;
    playlist: int;
    playlistlength: int;
    xfade: int;
    mixrampdb: float;
    mixrampdelay: float;
    state: state_t;
    song: int option;
    songid: int option;
    nextsong: int option;
    nextsongid: int option;
    time: time_t option;
    elapsed_highres: float option;
    bitrate: int option;
    audio: audio_t option;
    updating_db: int option;
    error: string option;
  }

  let of_kvpairs kvpairs =
    {
      volume = int_of_string (find "volume" kvpairs);
      repeat = (match (find "repeat" kvpairs) with "1" -> true | _ -> false);
      random = (match (find "random" kvpairs) with "1" -> true | _ -> false);
      single =
        (match find_opt "single" kvpairs with
        | Some "1" -> true | _ -> false);
      consume =
        (match (find_opt "consume" kvpairs) with
        | Some "1" -> true | _ -> false);
      playlist = int_of_string (find "playlist" kvpairs);
      playlistlength = int_of_string (find "playlistlength" kvpairs);
      xfade = int_of_string (find "xfade" kvpairs);
      mixrampdb = float_of_string (find "mixrampdb" kvpairs);
      mixrampdelay = float_of_string (find "mixrampdelay" kvpairs);
      state =
        (match find "state" kvpairs with
        | "play" -> Playing
        | "pause" -> Paused
        | _ -> Stopped);
      song = map_opt int_of_string (find_opt "song" kvpairs);
      songid = map_opt int_of_string (find_opt "songid" kvpairs);
      nextsong = map_opt int_of_string (find_opt "nextsong" kvpairs);
      nextsongid = map_opt int_of_string (find_opt "nextsongid" kvpairs);
      time =
        map_opt
          (fun str ->
            match Mpd_parser.split str ':' with
            | elapsed :: total :: _ -> {
              elapsed = int_of_string elapsed;
              total = int_of_string total
            }
            | _ -> raise (Unexpected_value ("time", str)))
          (find_opt "time" kvpairs);
      elapsed_highres = map_opt float_of_string (find_opt "elapsed" kvpairs);
      bitrate = map_opt int_of_string (find_opt "bitrate" kvpairs);
      audio =
        map_opt
          (fun str ->
            match Mpd_parser.split str ':' with
            | sample_rate :: bits :: channels :: _ -> {
              sample_rate = int_of_string sample_rate;
              bits = int_of_string bits;
              channels = int_of_string channels
            }
            | _ -> raise (Unexpected_value ("audio", str)))
          (find_opt "audio" kvpairs);
      updating_db = map_opt int_of_string (find_opt "updating_db" kvpairs);
      error = find_opt "error" kvpairs;
    }
end
