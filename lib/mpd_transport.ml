module type IO = sig
  type 'a t
  val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
  val return : 'a -> 'a t

  type sockaddr
  type file_descr

  val open_socket : sockaddr -> file_descr t
  val close_socket : file_descr -> unit t

  val read : file_descr -> string -> int -> int -> int t
  val write : file_descr -> string -> int -> int -> int t
end
