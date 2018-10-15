structure Reversi_AI =
struct
    (* ALL your code goes here, inside the structure *)
    (* datatype player = Black | White
    datatype move = Pass | Move of int *)
    type field = player option;
    val size = 8;
    type board = field list;

    (* T = active player + boad *)
    type T = player * board;         
    val author = "Hung Phan";
    val nickname = "Hung Phan";
    
    fun player_of ((p, _) : T) = p;

    fun opponent Black = White
        | opponent White = Black;

    fun init player : T =
        let
          fun init_board i = 
                            if i = 64 then
                                []
                            else if i = 28 orelse i = 35 then
                                SOME(Black)::(init_board (i+1))
                            else if i = 27 orelse i = 36 then
                                SOME(White)::(init_board (i+1))
                            else
                                NONE::(init_board (i+1)) 
        in
          (player, init_board 0)
        end;

    (* fun value_of (NONE : field) = "-"
        | value_of (SOME(Black)) = "O"
        | value_of (SOME(White)) = "X";
    
    fun print_board ((_, []): T) _ = "\n"
        | print_board (p, f::fs) i = if i = 64 then
                                        ""
                                    else if (i mod size) = 7 then
                                        (value_of f) ^ "\n" ^ (print_board (p, fs) (i+1))
                                    else
                                        (value_of f) ^ "  " ^ (print_board (p, fs) (i+1)) ;

    val to_String = print_board (init Black) 0;  *)

    fun get_field (board_arg: board) i = List.nth (board_arg, i);

    fun get_valid_moves ((player_arg, board_arg): T) =
        let
            fun is_valid_up_move i =
                let

                    fun is_valid_up_move' j c = if j < 0 then
                                                    false
                                                else case (get_field board_arg j) of
                                                    SOME(p) => if p = opponent player_arg then
                                                                    if c > 0
                                                                        then true
                                                                    else false
                                                                else
                                                                    is_valid_up_move' (j-8) (c+1)
                                                     | NONE => false  
                in
                    is_valid_up_move' (i-8) 0
                end


            fun is_valid_down_move i =
                let

                    fun is_valid_down_move' j c = if j > 63  then
                                                    false
                                                else case (get_field board_arg j) of
                                                    SOME(p) => if p = opponent player_arg then
                                                                    if c > 0
                                                                        then true
                                                                    else false
                                                                else
                                                                    is_valid_down_move' (j+8) (c+1)
                                                    | NONE => false  
                in
                    is_valid_down_move' (i+8) 0
                end

            
            fun is_valid_left_move i =
                let

                    fun is_valid_left_move' j c = if (i-j) > (i mod size)  then
                                                    false
                                                else case (get_field board_arg j) of
                                                    SOME(p) => if p = opponent player_arg then
                                                                    if c > 0
                                                                        then true
                                                                    else false
                                                                else
                                                                    is_valid_left_move' (j-1) (c+1)
                                                    | NONE => false
                in
                    is_valid_left_move' (i-1) 0
                end

            fun is_valid_right_move i =
                let

                    fun is_valid_right_move' j c = if (j-i + (i mod size)) >= size  then
                                                    false
                                                else case (get_field board_arg j) of
                                                    SOME(p) => if p = opponent player_arg then
                                                                    if c > 0
                                                                        then true
                                                                    else false
                                                                else
                                                                    is_valid_right_move' (j+1) (c+1)
                                                    | NONE => false
                in
                    is_valid_right_move' (i+1) 0
                end
            
            fun get_valid_moves' i = if i > 63 then
                                        []
                                    else if (is_valid_up_move i) orelse (is_valid_down_move i) orelse (is_valid_left_move i) orelse (is_valid_right_move i) then
                                        Move(i)::(get_valid_moves' (i+1))
                                    else
                                        get_valid_moves' (i+1)
        in
            get_valid_moves' 0
        end; 

    fun next_move (position: T) =
        let
            val valid_moves = get_valid_moves position
        in
            case valid_moves of
            [] => Pass
            | (m::ms) => m
        end;
 
    fun make_move ((player_arg, board_arg: board): T) move : T=
        let
            fun make_move' i j [] = []
                | make_move' i j (f::fs) = if i = j then 
                                            (SOME(player_arg))::fs
                                        else 
                                            f::(make_move' i (j+1) fs)
        in
            case move of
            Pass => (player_arg, board_arg)
            | Move(i) => (player_arg, make_move' i 0 board_arg)
        end; 

    fun think (position, m, t) = 
        let
            val current_position = make_move position m
            val next_m = next_move current_position
        in
            (next_m, make_move current_position next_m)
        end;



end;

val x = Reversi_AI.get_valid_moves (Reversi_AI.init Reversi_AI.Black);