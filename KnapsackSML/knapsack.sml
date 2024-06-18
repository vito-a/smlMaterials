(* Knapsack problem in Standard ML *)

(* Define a type for items with weight and value *)
type item = { weight: int, value: int }

(* Function to solve the knapsack problem *)
fun knapsack (items: item list, capacity: int) =
    let
        (* Create an array to store the maximum value for each weight *)
        val dp = Array.array(capacity + 1, 0)

        (* Function to update the dp array for a given item *)
        fun updateDP ({ weight, value }: item) =
            let
                fun update (w: int) =
                    if w >= weight then
                        let
                            val newValue = Array.sub(dp, w - weight) + value
                            val oldValue = Array.sub(dp, w)
                        in
                            if newValue > oldValue then
                                Array.update(dp, w, newValue)
                            else
                                ()
                        end
                    else
                        ()
            in
                (* Update dp array from right to left to avoid overwriting *)
                List.app update (List.tabulate(capacity + 1, fn x => capacity - x))
            end

    in
        (* Process each item *)
        List.app updateDP items;

        (* Return the maximum value for the given capacity *)
        Array.sub(dp, capacity)
    end

(* Example usage *)
val items = [{ weight = 2, value = 3 }, { weight = 3, value = 4 }, { weight = 4, value = 5 }, { weight = 5, value = 8 }]
val capacity = 5
val max_value = knapsack(items, capacity)

(* Output the result *)
val () = print ("Maximum value: " ^ Int.toString(max_value) ^ "\n")
