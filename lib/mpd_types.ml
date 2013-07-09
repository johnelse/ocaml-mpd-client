exception Missing_key of string

let make_reader kvpairs =
  (* Reversing the list is a bit of a hack - assignments to the record are
   * evaluated in reverse order compared to the record field definitions. *)
  let cache = ref (List.rev kvpairs) in
  (fun key ->
    let remaining_kvpairs = !cache in
    let (key', value) =
      try List.hd remaining_kvpairs
      with Failure "hd" ->
        raise (Missing_key key)
    in
    if key = key'
    then begin
      cache := List.tl remaining_kvpairs;
      value
    end
    else raise (Missing_key key))

module Output = struct
  type t = {
    outputid: int;
    outputname: string;
    outputenabled: bool;
  }

  let of_kvpairs kvpairs =
    let read = make_reader kvpairs in
    {
      outputid = int_of_string (read "outputid");
      outputname = read "outputname";
      outputenabled =
        match read "outputenabled" with "1" -> true | _ -> false;
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
    let read = make_reader kvpairs in
    {
      artists = int_of_string (read "artists");
      albums = int_of_string (read "albums");
      songs = int_of_string (read "songs");
      uptime = int_of_string (read "uptime");
      playtime = int_of_string (read "playtime");
      db_playtime = Int64.of_string (read "db_playtime");
      db_update = Int64.of_string (read "db_update");
    }
end
