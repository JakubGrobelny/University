module type PQUEUE =
sig
    type priority
    type 'a t
    
    exception EmptyPQueue
    
    val empty : 'a t
    val insert : 'a t -> priority -> 'a -> 'a t
    val remove : 'a t -> priority * 'a * 'a t
end

module PQueue : PQUEUE with type priority = int =
struct
    type priority = int
    type 'a t =
        | Empty
        | PQueue of ('a * priority) * ('a t)

    exception EmptyPQueue

    let empty = Empty
    
    let rec insert queue priority element =
        match queue with
        | Empty -> PQueue ((element, priority), Empty)
        | PQueue ((head, head_priority), tail) ->
            if head_priority > priority
                then let tail = insert tail priority element in
                    PQueue ((head, head_priority), tail)
                else PQueue ((element, priority), queue)

    let rec remove queue =
        match queue with
        | Empty -> raise EmptyPQueue
        | PQueue ((head, priority), tail) -> priority, head, tail
end

let sort_int xs =
    let rec to_queue xs =
        match xs with
        | [] -> PQueue.empty
        | x::xs -> PQueue.insert (to_queue xs) x x
    in let rec to_list q =
        if q = PQueue.empty
            then []
            else let _, elem, tail = PQueue.remove q in
                elem :: to_list tail
    in List.rev (to_list (to_queue xs))

module type ORDTYPE =
sig
    type t
    type comparison = 
        | LT 
        | EQ 
        | GT

    val compare : t -> t -> comparison
end

module FPQueue (OrdType : ORDTYPE) : PQUEUE with type priority = OrdType.t =
struct
    type priority = OrdType.t
    type 'a t =
        | Empty
        | PQueue of ('a * priority) * ('a t)
    
    exception EmptyPQueue

    let empty = Empty

    let rec insert queue priority element =
        match queue with
        | Empty -> PQueue ((element, priority), Empty)
        | PQueue ((head, head_priority), tail) ->
            match OrdType.compare head_priority priority with
            | OrdType.GT -> let tail = insert tail priority element in
                PQueue ((head, head_priority), tail)
            | _ -> PQueue ((element, priority), queue)

    let rec remove queue =
        match queue with
        | Empty -> raise EmptyPQueue
        | PQueue ((head, priority), tail) -> priority, head, tail
end

module OrdTypeInt : ORDTYPE with type t = int =
struct
    type t = int
    type comparison = 
        | LT 
        | EQ 
        | GT

    let compare lhs rhs =
        if lhs = rhs
            then EQ
            else if rhs > lhs
                then GT
                else LT
end

module IntPQueue = FPQueue (OrdTypeInt)

let sort_int' xs =
    let rec to_queue xs =
        match xs with
        | [] -> IntPQueue.empty
        | x::xs -> IntPQueue.insert (to_queue xs) x x
    in let rec to_list q =
        if q = IntPQueue.empty
            then []
            else let _, elem, tail = IntPQueue.remove q in
                elem :: to_list tail
    in to_list (to_queue xs)

let sort (type a) (ord : (module ORDTYPE with type t = a)) (xs : a list) =
    let (module OrdType) = ord
    in let (module Queue) =
        (module FPQueue (OrdType) : PQUEUE with type priority = OrdType.t)
    in let rec to_queue xs =
        match xs with
        | [] -> Queue.empty
        | x::xs -> Queue.insert (to_queue xs) x x
    in let rec to_list q =
        if q = Queue.empty
            then []
            else let _, elem, tail = Queue.remove q in
                elem :: to_list tail
    in to_list (to_queue xs)
