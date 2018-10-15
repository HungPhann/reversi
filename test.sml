

fun increase i = i + 1;

fun f i = case i of 
        (increase 1) => true
        | _ => false;

f 2;