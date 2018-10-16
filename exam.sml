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

    fun value_of (NONE : field) = "-"
        | value_of (SOME(Black)) = "O"
        | value_of (SOME(White)) = "X";
    
    fun print_board ((_, []): T) _ = "\n"
        | print_board (p, f::fs) i = if i = 64 then
                                        ""
                                    else if (i mod size) = 7 then
                                        (value_of f) ^ "\n" ^ (print_board (p, fs) (i+1))
                                    else
                                        (value_of f) ^ "  " ^ (print_board (p, fs) (i+1)) ;

    fun to_String position = print_board position 0; 

    fun get_field (board_arg: board) i = List.nth (board_arg, i);

    fun is_valid_up_move ((player_arg, board_arg): T) i =
                let
                    fun is_valid_up_move' j c = if j < 0 then
                                                    false
                                                else case (get_field board_arg j) of
                                                    SOME(p) => if p = player_arg then
                                                                    if c > 0
                                                                        then true
                                                                    else false
                                                                else
                                                                    is_valid_up_move' (j-8) (c+1)
                                                     | NONE => false  
                in
                    is_valid_up_move' (i-8) 0
                end;

    fun is_valid_down_move ((player_arg, board_arg): T) i =
                let

                    fun is_valid_down_move' j c = if j > 63  then
                                                    false
                                                else case (get_field board_arg j) of
                                                    SOME(p) => if p = player_arg then
                                                                    if c > 0
                                                                        then true
                                                                    else false
                                                                else
                                                                    is_valid_down_move' (j+8) (c+1)
                                                    | NONE => false  
                in
                    is_valid_down_move' (i+8) 0
                end;

    fun is_valid_left_move ((player_arg, board_arg): T) i =
                let

                    fun is_valid_left_move' j c = if (i-j) > (i mod size)  then
                                                    false
                                                else case (get_field board_arg j) of
                                                    SOME(p) => if p = player_arg then
                                                                    if c > 0
                                                                        then true
                                                                    else false
                                                                else
                                                                    is_valid_left_move' (j-1) (c+1)
                                                    | NONE => false
                in
                    is_valid_left_move' (i-1) 0
                end;

    fun is_valid_right_move ((player_arg, board_arg): T) i =
                let
                    fun is_valid_right_move' j c = if (j-i + (i mod size)) >= size  then
                                                    false
                                                else case (get_field board_arg j) of
                                                    SOME(p) => if p = player_arg then
                                                                    if c > 0
                                                                        then true
                                                                    else false
                                                                else
                                                                    is_valid_right_move' (j+1) (c+1)
                                                    | NONE => false
                in
                    is_valid_right_move' (i+1) 0
                end;


    fun get_valid_moves ((player_arg, board_arg): T) =
        let
            fun get_valid_moves' i = if i > 63 then
                                        []
                                    else if (is_valid_up_move (player_arg, board_arg) i) orelse (is_valid_down_move (player_arg, board_arg) i) orelse (is_valid_left_move (player_arg, board_arg) i) orelse (is_valid_right_move (player_arg, board_arg) i) then
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
            List.hd valid_moves handle Empty => Pass
        end;
 
    fun make_move ((player_arg, board_arg: board): T) move active_player : T =
        let
            fun make_move' i j [] = []
                | make_move' i j (f::fs) = if i = j then 
                                            (SOME(active_player))::fs
                                        else 
                                            f::(make_move' i (j+1) fs)
            exception FLIPNONE;

            fun flip_disc_up ((player_arg, board_arg: board): T) i = 
                let
                    fun flip_disc_up' j ((player_arg, board_arg: board): T) = case (get_field board_arg j) of
                                                                                SOME(p) => if p = active_player then 
                                                                                                (player_arg, board_arg)
                                                                                            else
                                                                                                flip_disc_up' (j-8) (player_arg, make_move' j 0 board_arg)
                                                                                | NONE => raise FLIPNONE;                
                in
                    if is_valid_up_move (player_arg, board_arg) i then
                        flip_disc_up' (i-8) (player_arg, board_arg)
                    else (player_arg, board_arg)
                end;

            fun flip_disc_down ((player_arg, board_arg: board): T) i =
                let
                    fun flip_disc_down' j ((player_arg, board_arg: board): T) = case (get_field board_arg j) of
                                                                                SOME(p) => if p = active_player then 
                                                                                                (player_arg, board_arg)
                                                                                            else
                                                                                                flip_disc_down' (j+8) (player_arg, make_move' j 0 board_arg)
                                                                                | NONE => raise FLIPNONE;                
                in
                    if is_valid_down_move (player_arg, board_arg) i then
                        flip_disc_down' (i+8) (player_arg, board_arg)
                    else (player_arg, board_arg)
                end;                                                           

            fun flip_disc_left ((player_arg, board_arg: board): T) i =
                let
                    fun flip_disc_left' j ((player_arg, board_arg: board): T) = case (get_field board_arg j) of
                                                                                SOME(p) => if p = active_player then 
                                                                                                (player_arg, board_arg)
                                                                                            else
                                                                                                flip_disc_left' (j-1) (player_arg, make_move' j 0 board_arg)
                                                                                | NONE => raise FLIPNONE;                
                in
                    if is_valid_left_move (player_arg, board_arg) i then
                        flip_disc_left' (i-1) (player_arg, board_arg)
                    else (player_arg, board_arg)                
                end;

            fun flip_disc_right ((player_arg, board_arg: board): T) i =
                let 
                    fun flip_disc_right' j ((player_arg, board_arg: board): T) = case (get_field board_arg j) of
                                                                                SOME(p) => if p = active_player then 
                                                                                                (player_arg, board_arg)
                                                                                            else
                                                                                                flip_disc_right' (j+1) (player_arg, make_move' j 0 board_arg)
                                                                                | NONE => raise FLIPNONE;                 
                in
                    if is_valid_right_move (player_arg, board_arg) i then
                        flip_disc_right' (i+1) (player_arg, board_arg)
                    else (player_arg, board_arg)                
                end;

                                                                       

            fun final_position ((player_arg, board_arg: board): T) i = flip_disc_right (flip_disc_left (flip_disc_down (flip_disc_up (player_arg, make_move' i 0 board_arg) i) i) i) i;
            
                            
        in
            case move of
            Pass => (player_arg, board_arg)
            | Move(i) => final_position (player_arg, board_arg) i
        end; 

    fun think ((player_arg, board_arg): T, m, t) = 
        let
            val current_position = make_move (player_arg, board_arg) m (opponent player_arg)
            val next_m = next_move current_position
        in
            (next_m, make_move current_position next_m player_arg)
        end;



end;

 