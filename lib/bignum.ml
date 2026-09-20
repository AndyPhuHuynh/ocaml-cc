module Int = struct
  type t = Z.t

  let pp = Z.pp_print
end

module Float = struct
  type t = Q.t

  let pp = Q.pp_print
end
