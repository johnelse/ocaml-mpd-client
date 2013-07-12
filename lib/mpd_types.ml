exception Missing_key of string

let find key kvpairs =
  try List.assoc key kvpairs
  with Not_found -> raise (Missing_key key)

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
