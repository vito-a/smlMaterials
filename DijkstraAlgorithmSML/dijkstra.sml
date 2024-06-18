(* Dijkstra's Algorithm in Standard ML *)

structure PriorityQueue :> sig
    type 'a pq
    val empty : 'a pq
    val insert : int * 'a -> 'a pq -> 'a pq
    val extractMin : 'a pq -> (int * 'a) option * 'a pq
end = struct
    type 'a pq = (int * 'a) list

    val empty = []

    fun insert (key, value) pq =
        (key, value) :: pq

    fun extractMin pq =
        let
            fun findMin [] min = min
              | findMin ((k, v) :: rest) ((mk, mv) as min) =
                  if k < mk then findMin rest (k, v) else findMin rest min
              | findMin (x :: rest) NONE = findMin rest (SOME x)
        in
            case findMin pq NONE of
                NONE => (NONE, pq)
              | SOME min =>
                  let
                      val pq' = List.filter (fn x => x <> min) pq
                  in
                      (SOME min, pq')
                  end
        end
end

(* Function to initialize distances *)
fun initDist (n: int, source: int) =
    Array.tabulate(n, fn i => if i = source then 0 else Int.maxInt)

(* Dijkstra's Algorithm *)
fun dijkstra (graph: (int * int) list array, source: int) =
    let
        val n = Array.length graph
        val dist = initDist(n, source)
        val visited = Array.array(n, false)
        val pq = ref (PriorityQueue.insert (0, source) PriorityQueue.empty)

        fun relax (u: int, v: int, weight: int) =
            let
                val alt = Array.sub(dist, u) + weight
            in
                if alt < Array.sub(dist, v) then
                    (Array.update(dist, v, alt);
                     pq := PriorityQueue.insert (alt, v) (!pq))
                else
                    ()
            end

        fun processQueue () =
            case PriorityQueue.extractMin (!pq) of
                (NONE, _) => ()
              | (SOME (d, u), pq') =>
                  if Array.sub(visited, u) then
                      (pq := pq'; processQueue ())
                  else
                      (Array.update(visited, u, true);
                       List.app (fn (v, weight) => relax(u, v, weight)) (Array.sub(graph, u));
                       pq := pq';
                       processQueue ())
    in
        processQueue ();
        Array.tabulate(n, fn i => (i, Array.sub(dist, i)))
    end

(* Example usage *)
val graph = Array.fromList [
    [(1, 4), (2, 1)],   (* edges from vertex 0 *)
    [(3, 1)],           (* edges from vertex 1 *)
    [(1, 2), (3, 5)],   (* edges from vertex 2 *)
    []                  (* edges from vertex 3 *)
]

val source = 0
val distances = dijkstra(graph, source)

(* Output the result *)
val () = List.app (fn (v, d) => print ("Vertex " ^ Int.toString v ^ ": " ^ Int.toString d ^ "\n")) distances
