(* Longest Common Subsequence in Standard ML *)

(* Function to find the length of the longest common subsequence *)
fun lcs (seq1: char list, seq2: char list) =
    let
        (* Lengths of the sequences *)
        val m = List.length seq1
        val n = List.length seq2

        (* Create a 2D array to store the lengths of LCS *)
        val dp = Array2.array(m + 1, n + 1, 0)

        (* Function to update the dp array *)
        fun updateDP (i: int, j: int) =
            if i = 0 orelse j = 0 then
                0
            else if List.nth(seq1, i - 1) = List.nth(seq2, j - 1) then
                Array2.update(dp, (i, j), Array2.sub(dp, (i - 1, j - 1)) + 1)
            else
                Array2.update(dp, (i, j), Int.max(Array2.sub(dp, (i - 1, j)), Array2.sub(dp, (i, j - 1))))

        (* Fill the dp array *)
        fun fillDP (i: int, j: int) =
            if i <= m then
                (updateDP(i, j); fillDP(i, j + 1))
            else if j <= n then
                fillDP(1, j)
            else
                ()

    in
        fillDP(1, 1);

        (* Return the length of the longest common subsequence *)
        Array2.sub(dp, (m, n))
    end

(* Example usage *)
val seq1 = explode "ABCBDAB"
val seq2 = explode "BDCAB"
val length_of_lcs = lcs(seq1, seq2)

(* Output the result *)
val () = print ("Length of LCS: " ^ Int.toString(length_of_lcs) ^ "\n")
