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
    fun board_of ((_, b): T) = b;

    fun opponent Black = White
        | opponent White = Black;

    val board_list = [  0, 0, 0, 0, 0, 0, 0, 0, 
                        0, 0, 0, 0, 0, 0, 0, 0, 
                        0, 0, 0, 0, 0, 0, 0, 0, 
                        0, 0, 0, ~1, 1, 0, 0, 0, 
                        0, 0, 0, 1, ~1, 0, 0, 0, 
                        0, 0, 0, 0, 0, 0, 0, 0, 
                        0, 0, 0, 0, 0, 0, 0, 0, 
                        0, 0, 0, 0, 0, 0, 0, 0];


    fun to_board [] = []
        | to_board (x::xs) = if x = 1 then
                                SOME(Black)::(to_board xs)
                            else if x = ~1 then
                                SOME(White)::(to_board xs)
                            else 
                                NONE::(to_board xs);

    fun init player : T =
        let
          val my_board = to_board board_list
        in
          (player, my_board)
        end; 



    (* fun init player : T =
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
        end; *)

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

    fun get_field (position: T) i = List.nth (board_of position, i);

    fun is_valid_up_move (position: T) i =
                let
                    fun is_valid_up_move' j c = if j < 0 then
                                                    false
                                                else case (get_field position j) of
                                                    SOME(p) => if p = (player_of position) then
                                                                    if c > 0
                                                                        then true
                                                                    else false
                                                                else
                                                                    is_valid_up_move' (j-8) (c+1)
                                                     | NONE => false  
                in
                    is_valid_up_move' (i-8) 0
                end;

    fun is_valid_down_move (position: T) i =
                let
                    fun is_valid_down_move' j c = if j > 63  then
                                                    false
                                                else case (get_field position j) of
                                                    SOME(p) => if p = (player_of position) then
                                                                    if c > 0
                                                                        then true
                                                                    else false
                                                                else
                                                                    is_valid_down_move' (j+8) (c+1)
                                                    | NONE => false  
                in
                    is_valid_down_move' (i+8) 0
                end;

    fun is_valid_left_move (position: T) i =
                let

                    fun is_valid_left_move' j c = if (i-j) > (i mod size)  then
                                                    false
                                                else case (get_field position j) of
                                                    SOME(p) => if p = (player_of position) then
                                                                    if c > 0
                                                                        then true
                                                                    else false
                                                                else
                                                                    is_valid_left_move' (j-1) (c+1)
                                                    | NONE => false
                in
                    is_valid_left_move' (i-1) 0
                end;

    fun is_valid_right_move (position: T) i =
                let
                    fun is_valid_right_move' j c = if (j-i + (i mod size)) >= size  then
                                                    false
                                                else case (get_field position j) of
                                                    SOME(p) => if p = (player_of position) then
                                                                    if c > 0
                                                                        then true
                                                                    else false
                                                                else
                                                                    is_valid_right_move' (j+1) (c+1)
                                                    | NONE => false
                in
                    is_valid_right_move' (i+1) 0
                end;

    fun is_valid_up_left_move (position: T) i =
                let
                    fun is_valid_up_left_move' j c c' = if j < 0 orelse c' > (i mod size) then
                                                    false
                                                else case (get_field position j) of
                                                    SOME(p) => if p = (player_of position) then
                                                                    if c > 0
                                                                        then true
                                                                    else false
                                                                else
                                                                    is_valid_up_left_move' (j-9) (c+1) (c'+1)
                                                    | NONE => false
                in
                    is_valid_up_left_move' (i-9) 0 1
                end;

    fun is_valid_down_left_move (position: T) i =
                let
                    fun is_valid_down_left_move' j c c' = if j > 63 orelse c' > (i mod size) then
                                                    false
                                                else case (get_field position j) of
                                                    SOME(p) => if p = (player_of position) then
                                                                    if c > 0
                                                                        then true
                                                                    else false
                                                                else
                                                                    is_valid_down_left_move' (j+7) (c+1) (c'+1)
                                                    | NONE => false
                in
                    is_valid_down_left_move' (i+7) 0 1
                end;      

    fun is_valid_up_right_move (position: T) i =
                let
                    fun is_valid_up_right_move' j c c' = if j < 0 orelse c' > (size - 1 - (i mod size)) then
                                                    false
                                                else case (get_field position j) of
                                                    SOME(p) => if p = (player_of position) then
                                                                    if c > 0
                                                                        then true
                                                                    else false
                                                                else
                                                                    is_valid_up_right_move' (j-7) (c+1) (c'+1)
                                                    | NONE => false
                in
                    is_valid_up_right_move' (i-7) 0 1
                end;     

    fun is_valid_down_right_move (position: T) i =
                let
                    fun is_valid_down_right_move' j c c' = if j > 63 orelse c' > (size - 1 - (i mod size)) then
                                                    false
                                                else case (get_field position j) of
                                                    SOME(p) => if p = (player_of position) then
                                                                    if c > 0
                                                                        then true
                                                                    else false
                                                                else
                                                                    is_valid_down_right_move' (j+9) (c+1) (c'+1)
                                                    | NONE => false
                in
                    is_valid_down_right_move' (i+9) 0 1
                end;            
    
    
    fun get_valid_moves (position: T) =
        let
            fun get_valid_moves' i = if i > 63 then
                                        []
                                    else if (get_field position i) = NONE andalso ((is_valid_up_move position i) 
                                                                    orelse (is_valid_down_move position i) 
                                                                    orelse (is_valid_left_move position i) 
                                                                    orelse (is_valid_right_move position i)
                                                                    orelse (is_valid_down_left_move position i)
                                                                    orelse (is_valid_up_left_move position i)
                                                                    orelse (is_valid_up_right_move position i)
                                                                    orelse (is_valid_down_right_move position i)) then
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
 
    fun make_move (position: T) move active_player : T =
        let
            fun make_move' _ _ [] _ = []
                | make_move' i j (f::fs) active_player = if i = j then 
                                                            (SOME(active_player))::fs
                                                        else 
                                                            f::(make_move' i (j+1) fs active_player)
            exception FLIPNONE;

            fun flip_disc_up (position: T) i = 
                let
                    fun flip_disc_up' j (position: T) = case (get_field position j) of
                                                            SOME(p) => if p = active_player then 
                                                                            position
                                                                        else
                                                                            flip_disc_up' (j-8) (player_of position, make_move' j 0 (board_of position) active_player)
                                                            | NONE => raise FLIPNONE;                
                in
                    if is_valid_up_move position i then
                        flip_disc_up' (i-8) position
                    else position
                end;

            fun flip_disc_down (position: T) i =
                let
                    fun flip_disc_down' j (position: T) = case (get_field position j) of
                                                                SOME(p) => if p = active_player then 
                                                                                position
                                                                            else
                                                                                flip_disc_down' (j+8) (player_of position, make_move' j 0 (board_of position) active_player)
                                                                | NONE => raise FLIPNONE;                
                in
                    if is_valid_down_move position i then
                        flip_disc_down' (i+8) position
                    else position
                end;                                                           

            fun flip_disc_left (position: T) i =
                let
                    fun flip_disc_left' j (position: T) = case (get_field position j) of
                                                                SOME(p) => if p = active_player then 
                                                                                position
                                                                            else
                                                                                flip_disc_left' (j-1) (player_of position, make_move' j 0 (board_of position) active_player)
                                                                | NONE => raise FLIPNONE;                
                in
                    if is_valid_left_move position i then
                        flip_disc_left' (i-1) position
                    else position                
                end;

            fun flip_disc_right (position: T) i =
                let 
                    fun flip_disc_right' j (position: T) = case (get_field position j) of
                                                                SOME(p) => if p = active_player then 
                                                                                position
                                                                            else
                                                                                flip_disc_right' (j+1) (player_of position, make_move' j 0 (board_of position) active_player)
                                                                | NONE => raise FLIPNONE;                 
                in
                    if is_valid_right_move position i then
                        flip_disc_right' (i+1) position
                    else position                
                end;

            fun flip_disc_up_left (position: T) i =
                let
                    fun flip_disc_up_left' j (position: T) = case (get_field position j) of 
                                                                SOME(p) => if p = active_player then
                                                                                position
                                                                            else flip_disc_up_left' (j-9) (player_of position, make_move' j 0 (board_of position) active_player)
                                                                | NONE => raise FLIPNONE
                in
                    if is_valid_up_left_move position i then
                        flip_disc_up_left' (i-7) position
                    else position
                end;

            fun flip_disc_up_right (position: T) i =
                let
                    fun flip_disc_up_right' j (position: T) = case (get_field position j) of 
                                                                SOME(p) => if p = active_player then
                                                                                position
                                                                            else flip_disc_up_right' (j-7) (player_of position, make_move' j 0 (board_of position) active_player)
                                                                | NONE => raise FLIPNONE
                in
                    if is_valid_up_right_move position i then
                        flip_disc_up_right' (i-7) position
                    else position
                end;    

            fun flip_disc_down_right (position: T) i =
                let
                    fun flip_disc_down_right' j (position: T) = case (get_field position j) of 
                                                                SOME(p) => if p = active_player then
                                                                                position
                                                                            else flip_disc_down_right' (j+9) (player_of position, make_move' j 0 (board_of position) active_player)
                                                                | NONE => raise FLIPNONE
                in
                    if is_valid_down_right_move position i then
                        flip_disc_down_right' (i+9) position
                    else position
                end;    

            fun flip_disc_down_left (position: T) i =
                let
                    fun flip_disc_down_left' j (position: T) = case (get_field position j) of 
                                                                SOME(p) => if p = active_player then
                                                                                position
                                                                            else flip_disc_down_left' (j+7) (player_of position, make_move' j 0 (board_of position) active_player)
                                                                | NONE => raise FLIPNONE
                in
                    if is_valid_down_left_move position i then
                        flip_disc_down_left' (i+9) position
                    else position
                end; 
                                                                       

            fun final_position (position: T) i = flip_disc_up_left (
                                                flip_disc_up_right (
                                                flip_disc_down_right (
                                                flip_disc_down_left (
                                                flip_disc_right (
                                                flip_disc_left (
                                                flip_disc_down (
                                                flip_disc_up (player_of position, make_move' i 0 (board_of position) active_player) i) i) i) i
                                                ) i) i) i) i;
                            
        in
            case move of
            Pass => position
            | Move(i) => final_position position i
        end; 

    fun think (position: T, m, t) = 
        let
            val current_position = make_move position m (opponent (player_of position))
            val next_m = next_move current_position
        in
            (next_m, make_move current_position next_m (player_of position))
        end;

end;

(* val x = Reversi_AI.init Reversi_AI.Black;
print (Reversi_AI.to_String x);

Reversi_AI.get_valid_moves x;
val m = Reversi_AI.next_move x;
val x = Reversi_AI.make_move x m Reversi_AI.Black;
print (Reversi_AI.to_String x);

  *)