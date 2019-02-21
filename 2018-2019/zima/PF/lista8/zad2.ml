module type VERTEX =
sig
    type t
    type label

    val equal : t -> t -> bool
    val create : label -> t
    val label : t -> label
end

module type EDGE = 
sig
    type vertex
    type t
    type label

    val equal : t -> t -> bool
    val create : vertex -> vertex -> label -> t
    val label : t -> label
    val v_begin : t -> vertex
    val v_end : t -> vertex
end

module Vertex : VERTEX with type label = string =
struct
    type label = string
    type t = V of label

    let equal v1 v2 = (v1 = v2)
    let create label = V label
    let label (V label) = label
end

module Edge : EDGE with type label = Vertex.label and type vertex = Vertex.t =
struct
    type vertex = Vertex.t
    type label = Vertex.label
    type t = E of label * vertex * vertex

    let equal e1 e2 = (e1 = e2)
    let create vb ve label = E (label, vb, ve)
    let label (E (label, _, _)) = label
    let v_begin (E (_, vb, _)) = vb
    let v_end (E (_, _, ve)) = ve
end
