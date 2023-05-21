(* defining two symbol tables - one for values and one for type  *)


structure SymTable = struct 

  type val_symtable  = (string * ( DataTypes.myval option) ) list 
  type type_symtable = (string * DataTypes.Typ) list

  fun symtable_lookup(symtable : val_symtable, identity : string ) = 
                        let fun check(id : string, value : DataTypes.myval) = (id = identity)  
                            val checked = List.find(fn (id, _) => id = identity) symtable 
                        in case checked of 
                            SOME(id,value)  => (true, SOME(value) )  
                        |   _ =>  (false, NONE )
                        end 
  fun symtable_add(symtable : val_symtable, identity : string, value : DataTypes.myval) = (identity,SOME(value) ) :: symtable 
  
  fun symtable_add_var(symtable : val_symtable, identity : string) = (identity, NONE) :: symtable

  fun symtable_extend(symtable : val_symtable, new_symtable : val_symtable) = 
    if (new_symtable = []) then symtable 
    else symtable_extend(hd(new_symtable) :: symtable, tl(new_symtable))   

  fun symtable_delete_helper(symtable : val_symtable, id : string , ans ) = if (symtable = []) then rev(ans) 
                                                          else if ((#1 (hd(symtable)) ) = id) then symtable_delete_helper(tl(symtable), id, ans) 
                                                          else  symtable_delete_helper(tl(symtable), id, hd(symtable) :: ans) 

  fun symtable_delete(symtable : val_symtable , id : string) = symtable_delete_helper(symtable, id,[]) 

  fun symtable_update_helper(symtable : val_symtable, id : string, new_val : DataTypes.myval, ans) = 
      if (symtable = []) then ans 
      else if ( (#1 (hd(symtable))) = id) then List.rev(ans) @( (id,SOME(new_val) ) :: tl(symtable)) 
      else symtable_update_helper(tl(symtable) , id, new_val, hd(symtable) :: ans) 

  fun symtable_update(symtable : val_symtable, id : string , new_val : DataTypes.myval) = 
      symtable_update_helper(symtable, id, new_val, []) 

  
  fun typtable_lookup(typtable : type_symtable, identity : string ) = 
                        let fun check(id , value  ) = (id = identity)  
                            val checked = List.find(check) typtable 
                        in case checked of 
                            SOME(id,value)  => (true, SOME(value) )  
                        |   _ =>  (false, NONE )
                        end 
  fun typtable_add(typtable : type_symtable, identity : string, value : DataTypes.Typ) = (identity,value) :: typtable 
  fun typtable_extend(typtable : type_symtable, new_typtable : type_symtable) = 
    if (new_typtable = []) then typtable 
    else typtable_extend(hd(new_typtable) :: typtable, tl(new_typtable))   

  fun typtable_delete_helper(typtable : type_symtable, id : string , ans ) = if (typtable = []) then rev(ans) 
                                                          else if ((#1 (hd(typtable)) ) = id) then typtable_delete_helper(tl(typtable), id, ans) 
                                                          else  typtable_delete_helper(tl(typtable), id, hd(typtable) :: ans) 

  fun typtable_delete(typtable : type_symtable , id : string) = typtable_delete_helper(typtable, id,[]) 

  fun typtable_update_helper(typtable : type_symtable, id : string, new_val : DataTypes.Typ, ans) = 
      if (typtable = []) then ans 
      else if ( (#1 (hd(typtable))) = id) then List.rev(ans) @( (id,new_val) :: tl(typtable)) 
      else typtable_update_helper(tl(typtable) , id, new_val, hd(typtable) :: ans) 

  fun typtable_update(typtable : type_symtable, id : string , new_val : DataTypes.Typ) = 
      typtable_update_helper(typtable, id, new_val, []) 



end ; 