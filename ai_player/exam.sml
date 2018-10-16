structure Reversi_AI =
struct
    (* ALL your code goes here, inside the structure *)
    (* datatype player = Black | White
    datatype move = Pass | Move of int *)
    type field = player option;
    val size = 8;
    val game_tree_height = 5;
    type board = field list;

    (* T = active player + boad *)
    type T = player * board;         
    val author = "Hung Phan";
    val nickname = "Hung Phan";
    
    fun player_of ((p, _) : T) = p;
    fun board_of ((_, b): T) = b;
    fun get_field (position: T) i = List.nth (board_of position, i);


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

    val marks = [
                    99, ~8, 8, 6, 6, 8, ~8, 99, 
                    ~8, ~24, ~4, ~3, ~3, ~4, ~24, ~8, 
                    8, ~4, 7, 4, 4, 7, ~4, 8, 
                    6, ~3, 4, 0, 0, 4, ~3, 6, 
                    6, ~3, 4, 0, 0, 4, ~3, 6, 
                    8, ~4, 7, 4, 4, 7, ~4, 8, 
                    ~8, ~24, ~4, ~3, ~3, ~4, ~24, ~8, 
                    99, ~8, 8, 6, 6, 8, ~8, 99
                ];

    fun mark_of position = 
        let
            fun mark_of' i = if i > 63 then
                                0
                            else case (get_field position i) of
                                SOME(p) => if p = (player_of position) then
                                                (List.nth (marks, i)) + (mark_of' (i+1))
                                            else
                                                ~(List.nth (marks, i)) + (mark_of' (i+1))
                                                
                                | NONE => mark_of' (i+1)
        in
            mark_of' 0
        end;

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

    fun is_valid_up_move (position: T) i active_player =
                let
                    fun is_valid_up_move' j c = if j < 0 then
                                                    false
                                                else case (get_field position j) of
                                                    SOME(p) => if p = active_player then
                                                                    if c > 0
                                                                        then true
                                                                    else false
                                                                else
                                                                    is_valid_up_move' (j-8) (c+1)
                                                     | NONE => false  
                in
                    is_valid_up_move' (i-8) 0
                end;

    fun is_valid_down_move (position: T) i  active_player =
                let
                    fun is_valid_down_move' j c = if j > 63  then
                                                    false
                                                else case (get_field position j) of
                                                    SOME(p) => if p = active_player then
                                                                    if c > 0
                                                                        then true
                                                                    else false
                                                                else
                                                                    is_valid_down_move' (j+8) (c+1)
                                                    | NONE => false  
                in
                    is_valid_down_move' (i+8) 0
                end;

    fun is_valid_left_move (position: T) i active_player =
                let

                    fun is_valid_left_move' j c = if (i-j) > (i mod size)  then
                                                    false
                                                else case (get_field position j) of
                                                    SOME(p) => if p = active_player then
                                                                    if c > 0
                                                                        then true
                                                                    else false
                                                                else
                                                                    is_valid_left_move' (j-1) (c+1)
                                                    | NONE => false
                in
                    is_valid_left_move' (i-1) 0
                end;

    fun is_valid_right_move (position: T) i active_player =
                let
                    fun is_valid_right_move' j c = if (j-i + (i mod size)) >= size  then
                                                    false
                                                else case (get_field position j) of
                                                    SOME(p) => if p = active_player then
                                                                    if c > 0
                                                                        then true
                                                                    else false
                                                                else
                                                                    is_valid_right_move' (j+1) (c+1)
                                                    | NONE => false
                in
                    is_valid_right_move' (i+1) 0
                end;

    fun is_valid_up_left_move (position: T) i active_player =
                let
                    fun is_valid_up_left_move' j c c' = if j < 0 orelse c' > (i mod size) then
                                                    false
                                                else case (get_field position j) of
                                                    SOME(p) => if p = active_player then
                                                                    if c > 0
                                                                        then true
                                                                    else false
                                                                else
                                                                    is_valid_up_left_move' (j-9) (c+1) (c'+1)
                                                    | NONE => false
                in
                    is_valid_up_left_move' (i-9) 0 1
                end;

    fun is_valid_down_left_move (position: T) i active_player =
                let
                    fun is_valid_down_left_move' j c c' = if j > 63 orelse c' > (i mod size) then
                                                    false
                                                else case (get_field position j) of
                                                    SOME(p) => if p = active_player then
                                                                    if c > 0
                                                                        then true
                                                                    else false
                                                                else
                                                                    is_valid_down_left_move' (j+7) (c+1) (c'+1)
                                                    | NONE => false
                in
                    is_valid_down_left_move' (i+7) 0 1
                end;      

    fun is_valid_up_right_move (position: T) i active_player =
                let
                    fun is_valid_up_right_move' j c c' = if j < 0 orelse c' > (size - 1 - (i mod size)) then
                                                    false
                                                else case (get_field position j) of
                                                    SOME(p) => if p = active_player then
                                                                    if c > 0
                                                                        then true
                                                                    else false
                                                                else
                                                                    is_valid_up_right_move' (j-7) (c+1) (c'+1)
                                                    | NONE => false
                in
                    is_valid_up_right_move' (i-7) 0 1
                end;     

    fun is_valid_down_right_move (position: T) i active_player =
                let
                    fun is_valid_down_right_move' j c c' = if j > 63 orelse c' > (size - 1 - (i mod size)) then
                                                    false
                                                else case (get_field position j) of
                                                    SOME(p) => if p = active_player then
                                                                    if c > 0
                                                                        then true
                                                                    else false
                                                                else
                                                                    is_valid_down_right_move' (j+9) (c+1) (c'+1)
                                                    | NONE => false
                in
                    is_valid_down_right_move' (i+9) 0 1
                end;            
    
    
    fun get_valid_moves (position: T) player_arg =
        let
            fun get_valid_moves' i = if i > 63 then
                                        []
                                    else if (get_field position i) = NONE andalso ((is_valid_up_move position i player_arg) 
                                                                    orelse (is_valid_down_move position i player_arg) 
                                                                    orelse (is_valid_left_move position i player_arg) 
                                                                    orelse (is_valid_right_move position i player_arg)
                                                                    orelse (is_valid_down_left_move position i player_arg)
                                                                    orelse (is_valid_up_left_move position i player_arg)
                                                                    orelse (is_valid_up_right_move position i player_arg)
                                                                    orelse (is_valid_down_right_move position i player_arg)) then
                                        Move(i)::(get_valid_moves' (i+1))
                                    else
                                        get_valid_moves' (i+1)
        in
            get_valid_moves' 0
        end; 

 
    fun make_move (position: T) active_player m : T =
        let
            fun make_move' _ _ [] _ = []
                | make_move' i j (f::fs) active_player = if i = j then 
                                                            (SOME(active_player))::fs
                                                        else 
                                                            f::(make_move' i (j+1) fs active_player)
            exception FLIPNONE

            fun flip_disc_up (position: T) i = 
                let
                    fun flip_disc_up' j (position: T) = case (get_field position j) of
                                                            SOME(p) => if p = active_player then 
                                                                            position
                                                                        else
                                                                            flip_disc_up' (j-8) (player_of position, make_move' j 0 (board_of position) active_player)
                                                            | NONE => raise FLIPNONE              
                in
                    if is_valid_up_move position i active_player then
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
                                                                | NONE => raise FLIPNONE             
                in
                    if is_valid_down_move position i active_player then
                        flip_disc_down' (i+8) position
                    else position
                end                                                           

            fun flip_disc_left (position: T) i =
                let
                    fun flip_disc_left' j (position: T) = case (get_field position j) of
                                                                SOME(p) => if p = active_player then 
                                                                                position
                                                                            else
                                                                                flip_disc_left' (j-1) (player_of position, make_move' j 0 (board_of position) active_player)
                                                                | NONE => raise FLIPNONE               
                in
                    if is_valid_left_move position i active_player then
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
                                                                | NONE => raise FLIPNONE                
                in
                    if is_valid_right_move position i active_player then
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
                    if is_valid_up_left_move position i active_player then
                        flip_disc_up_left' (i-9) position
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
                    if is_valid_up_right_move position i active_player then
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
                    if is_valid_down_right_move position i active_player then
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
                    if is_valid_down_left_move position i active_player then
                        flip_disc_down_left' (i+7) position
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
            case m of
            Pass => position
            | Move(i) => final_position position i
        end; 

    fun minimax position =
        let
            fun minimax' i position =
                let
                    fun extremum cmp [] = (Pass, mark_of position)
                    | extremum cmp [(m, v)] = (m, v)
                    | extremum cmp ((m1, v1)::(m2, v2)::xs) =
                        if Int.compare (v1, v2) = cmp then
                            extremum cmp ((m1, v1)::xs)
                        else
                            extremum cmp ((m2, v2)::xs)
                    val max = extremum GREATER
                    val min = extremum LESS
                in
                    if i > game_tree_height then
                        (Pass, mark_of position)
                    else if i mod 2 = 1 then
                        let
                            val moves = get_valid_moves position (player_of position)
                            val positions = map (make_move position (player_of position)) moves
                            val moves_and_values = map (minimax' (i+1)) positions
                            val values = map (fn (_, v) => v) moves_and_values
                        in
                            max (ListPair.zipEq (moves, values))
                        end
                    else
                        let
                            val moves = get_valid_moves position (opponent (player_of position))
                            val positions = map (make_move position (opponent (player_of position))) moves
                            val moves_and_values = map (minimax' (i+1)) positions
                            val values = map (fn (_, v) => v) moves_and_values
                        in
                            min (ListPair.zipEq (moves, values))
                        end
                end
        in 
            minimax' 1 position
        end;

     fun next_move (position: T) =
        let
            val valid_moves = get_valid_moves position (player_of position)
            val next_m = minimax position
        in
            #1 next_m
        end;

    fun think (position: T, m, t) = 
        let
            val current_position = make_move position (opponent (player_of position)) m
            val next_m = next_move current_position
            val next_position = make_move current_position (player_of position) next_m 
        in
            (next_m, next_position)
        end;

end;
