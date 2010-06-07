// customized version of Set

module S =
  Set.Make
    (struct
      type t = Id.t
      let compare = compare
    end)
include S

def of_list(l) = List.fold_left(s(e) => add(e, s), empty, l)