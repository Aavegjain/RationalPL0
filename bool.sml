structure Bool = 
struct
exception read_string_not_a_boolean

fun bool_and(x : DataTypes.Boolean, y : DataTypes.Boolean) : DataTypes.Boolean = 
    if (x = DataTypes.ff) then DataTypes.ff else y 


fun bool_or(x : DataTypes.Boolean, y : DataTypes.Boolean) : DataTypes.Boolean = 
    if (x = DataTypes.tt) then DataTypes.tt else y 

fun bool_not(x : DataTypes.Boolean) = 
    if (x = DataTypes.tt) then DataTypes.ff else DataTypes.tt  

fun equal(x : DataTypes.Boolean ,y : DataTypes.Boolean) = 
    if ((x = DataTypes.tt andalso y = DataTypes.tt) orelse (x = DataTypes.ff andalso y = DataTypes.ff))
    then true else false 

fun bool_to_str(x : DataTypes.Boolean) = if (x = DataTypes.tt) then "tt" else "ff" 

fun make_from_str(s : string) = case s of 
                                "tt" => DataTypes.tt 
                            |   "ff" => DataTypes.ff 
                            | _ => raise read_string_not_a_boolean
end ; 