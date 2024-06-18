(* N-Queens Problem in Standard ML *)

(* Function to check if a position is safe *)
fun isSafe (board: int list, row: int, col: int) =
    let
        fun check (r, []) = true
          | check (r, c::cs) =
              if col = c orelse abs(col - c) = abs(row - r) then false
              else check (r + 1, cs)
    in
        check (0, board)
    end

(* Function to solve the N-Queens problem *)
fun solveNQueens (n: int) =
    let
        (* Helper function to place queens on the board *)
        fun placeQueens (0, board) = [board]
          | placeQueens (k, board) =
              List.concat (List.tabulate (n, fn col =>
                if isSafe (board, n - k, col) then
                    placeQueens (k - 1, col :: board)
                else
                    []))
    in
        placeQueens (n, [])
    end

(* Function to print the board *)
fun printBoard board =
    let
        fun printRow col =
            let
                fun cell i = if i = col then "Q " else ". "
            in
                String.concat (List.tabulate (length board, cell)) ^ "\n"
            end
    in
        List.app (fn col => print (printRow col)) board;
        print "\n"
    end

(* Example usage *)
val n = 8
val solutions = solveNQueens n

(* Output all solutions *)
val () = List.app (fn solution => (printBoard (rev solution); print "\n")) solutions;
print ("Number of solutions: " ^ Int.toString (length solutions) ^ "\n")
