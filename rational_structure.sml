
signature RATIONAL =
sig
eqtype rational
exception rat_error
val make_rat: Bigint.bigint * Bigint.bigint-> rational option
val rat: Bigint.bigint-> rational option
val reci: Bigint.bigint-> rational option
val neg: rational-> rational
val inverse : rational-> rational option
val equal : rational * rational-> bool (* equality *)
val less : rational * rational-> bool (* less than *)
val add : rational * rational-> rational (* addition *)
val subtract : rational * rational-> rational (* subtraction *)
val multiply : rational * rational-> rational (* multiplication *)
val divide : rational * rational-> rational option (* division *)
val showRat : rational-> string
val showDecimal : rational-> string
val fromDecimal : string-> rational
val toDecimal : rational-> string
end;


functor Rational_functor( bigint : BIGINT) : RATIONAL =
struct
open Bigint ;

(* avoiding name collison from bindings from bigint.sml *)
val add_int = add
val sub_int = sub
val mult_int = mult
val divide_int = divide
val less_int = less

datatype sign = plus | minus
datatype rational = rational of bigint * bigint ;
exception rat_error

(* checking if a string can represent an integer *)
fun check_int2([]) = true |
    check_int2(hd :: tl) = ( (hd = #"1" orelse hd = #"2" orelse hd = #"3" orelse hd = #"4"
    orelse hd = #"5" orelse hd = #"6" orelse hd = #"7" orelse hd = #"8" orelse hd = #"9"
    orelse hd = #"0" ) andalso check_int2(tl) )

fun check_int(l as hd :: tl) = if (hd = #"+" orelse hd = #"~" ) then check_int(tl)
                          else check_int2(l)
|   check_int([]) = false

(* gcd of two non negative integers *)
fun gcd(x,y) = if (equal(y,"0")) then x
                else let val (q,r) = divide_int(x,y)
                     in gcd(y,r)
                     end


fun  make_rat(x,y) = if (not (check_int(explode(x)) andalso check_int(explode(y))) ) then NONE
                     else if (equal(y,"0")) then NONE
                     else if (equal(x,"0")) then SOME(rational(make_from_str("0"),make_from_str("1") ))
                    else if (sign(x) = sign(y)) then
                         let val hcf = gcd(x,y)
                             val q1 = #1(divide_int(x,hcf))
                             val q2 = #1(divide_int(y,hcf))
                         in  SOME(rational(q1,q2))
                         end
                    else if (sign(x) = 1) then
                        let  val x = negate(x)
                             val hcf = gcd(x,y)
                             val q1 = #1(divide_int(x,hcf))
                             val q2 = #1(divide_int(y,hcf))
                         in  SOME(rational(negate(q1),q2))
                         end
                    else
                        let  val y = negate(y)
                             val hcf = gcd(x,y)
                             val q1 = #1(divide_int(x,hcf))
                             val q2 = #1(divide_int(y,hcf))
                         in  SOME(rational(negate(q1) ,q2))
                         end

fun rat(x) = if (check_int(explode(x))) then SOME(rational(normalize(x),"1"))
             else NONE

fun reci(x) = make_rat("1",x)

fun neg(rational(x,y)) = let val rational(w,z) = valOf(make_rat(x,y))
                           in valOf(make_rat(negate(w), z))
                           end

fun inverse(rational(x,y)) = make_rat(y,x)

fun  add(rational(x1,x2), rational(y1,y2)) = let val num = add_int(mult_int(x1,y2), mult_int(y1,x2))
                                                     val denum = mult_int(x2,y2)
                                                 in valOf(make_rat(num, denum))
                                                 end
fun subtract(x,y) = add(x,neg(y))

fun multiply(rational(x1,x2), rational(y1,y2)) = valOf(make_rat(mult_int(x1,y1), mult_int(x2,y2)))

fun divide(x as rational(x1,x2), y as rational(y1,y2)) = if (inverse(y) = NONE) then NONE
                                                        else SOME(multiply(x , valOf(inverse(y))) )



fun equal(x as rational(x1,x2), y as rational(y1,y2)) = (x1 = y1 andalso x2 = y2)

fun less(x as rational(x1,x2), y as rational(y1,y2)) = if (sign(x1) = 0 andalso sign(y1) = 0)
                                                           then less_int(mult_int(x1,y2), mult_int(x2,y1))

                                                           else if (sign(x1) < sign(y1)) then false
                                                           else if (sign(x1) > sign(y1)) then true
                                                           else ((less(neg(y), neg(x))))

fun get_sublist([],n, ans) = (ans, []) |
    get_sublist(list, "0",ans) = (ans,list) |
    get_sublist(hd :: tl, n, ans ) = get_sublist(tl, sub(n,"1"), hd ::ans )

fun get_last([]) = ([],[]) |
    get_last(list) = let val len = (length(list) - 1)
                     in get_sublist(list, make_from_int(len),[])
                     end

fun check_if_present(x,[],ans) = (false,ans) |
    check_if_present(x,hd :: tl,ans) = if (hd = x) then (true,add_int(ans,"1"))
                                   else check_if_present(x,tl,add_int(ans,"1"))

fun generate_rem(q_list, r_list,q) = let val qn = hd(q_list) ; val rn = hd(r_list)
                                        val (q_new,r_new) = divide_int(mult_int("10",rn),q)
                                        val (check,ans) = check_if_present(r_new,r_list,"0")
                                     in  if (check) then (q_new :: q_list, ans)
                                         else generate_rem(q_new::q_list, r_new :: r_list, q)
                                    end

fun convert_to_decimal(rational(p,q)) = if (sign(p) = 0) then
                                        let
                                            val (quotient,rem) = divide_int(p,q)
                                            val (q_list,ans) = generate_rem([quotient],[rem],q)
                                            val (recc,q_list) = get_sublist(q_list,ans,[])
                                            val (non_recc,[quotient]) = get_last(q_list)

                                        in  (plus, quotient,concat(non_recc), concat(recc))
                                        end

                                        else
                                            let val p = negate(p)
                                            val (quotient,rem) = divide_int(p,q)
                                            val (q_list,ans) = generate_rem([quotient],[rem],q)
                                            val (recc,q_list) = get_sublist(q_list,ans,[])
                                            val (non_recc,[quotient]) = get_last(q_list)

                                        in  (minus, quotient,make_from_str(concat(non_recc)), make_from_str(concat(recc)))
                                        end


fun convert_to_fraction(sign,q, non_recc, recc) = let val q = if (q = "") then "0" else q
                                                     val temp1 = non_recc ^ recc
                                                     val temp2 = sub_int(temp1, non_recc)
                                                     val temp3 = sub_int(power("10", add_int(make_from_int(size(recc)) , make_from_int(size(non_recc)) )),
                                                                 power("10",make_from_int(size(non_recc))) )
                                                     val temp4 = valOf(make_rat(temp2, temp3))
                                                     val ans = add(valOf(rat(q)), temp4)

                                                 in if (sign = plus) then ans
                                                    else neg(ans)

                                                 end

fun showRat(x as rational(x1,x2)) = x1 ^ "/" ^ x2

fun showDecimal(x as rational(x1,x2)) = let val (sign, quotient, non_recc, recc) = convert_to_decimal(x)
                                          in if (sign = plus) then
                                                                    if (quotient="0") then "." ^ non_recc ^ "(" ^ recc ^ ")"
                                                                    else quotient ^ "." ^ non_recc ^ "(" ^ recc ^ ")"
                                             else if (quotient="0") then "~" ^ "." ^ non_recc ^ "(" ^ recc ^ ")"
                                                                    else "~" ^ quotient ^ "." ^ non_recc ^ "(" ^ recc ^ ")"
                                          end

(* sign int point non rec recc *)
fun fromDecimal(s) = check_sign(explode(s), [])
(* checking whether the decimal is in valid form through a fsm *)
and check_sign(l as hd :: tl , temp) = ( case hd of
                                       #"+" => digit_star(plus, tl, [])
                                    |  #"~" => digit_star(minus, tl, [])
                                    |  #"." => digit_star2(plus, "0", tl, [])
                                    |  _ => digit_star(plus, l, []) )

    | check_sign([], temp) = raise rat_error

and digit_star(sign, l as hd :: tl, temp) = ( case hd of
                                            #"0" => digit_star(sign, tl, hd :: temp)
                                        |   #"1" => digit_star(sign, tl, hd :: temp)
                                        |   #"2" => digit_star(sign, tl, hd :: temp)
                                        |   #"3" => digit_star(sign, tl, hd :: temp)
                                        |   #"4" => digit_star(sign, tl, hd :: temp)
                                        |   #"5" => digit_star(sign, tl, hd :: temp)
                                        |   #"6" => digit_star(sign, tl, hd :: temp)
                                        |   #"7" => digit_star(sign, tl, hd :: temp)
                                        |   #"8" => digit_star(sign, tl, hd :: temp)
                                        |   #"9" => digit_star(sign, tl, hd :: temp)
                                        |   #"." => check_paran(sign, implode(rev(temp)), tl, [])
                                        |    _ => raise rat_error )
    | digit_star(sign, [], temp) = raise rat_error

and check_paran(sign, integral, l as hd :: tl, temp) = if (hd = #"(") then digit_plus(sign, integral,"", tl, [])
                                                        else digit_star2(sign, integral, l, temp)
    | check_paran(sign, integral,[],temp) = raise rat_error
and digit_star2(sign, integral, l as hd :: tl, temp) = ( case hd of
                                            #"0" => digit_star2(sign, integral, tl, hd :: temp)
                                        |   #"1" => digit_star2(sign,integral, tl, hd :: temp)
                                        |   #"2" => digit_star2(sign, integral,tl, hd :: temp)
                                        |   #"3" => digit_star2(sign, integral,tl, hd :: temp)
                                        |   #"4" => digit_star2(sign, integral,tl, hd :: temp)
                                        |   #"5" => digit_star2(sign, integral,tl, hd :: temp)
                                        |   #"6" => digit_star2(sign, integral,tl, hd :: temp)
                                        |   #"7" => digit_star2(sign, integral,tl, hd :: temp)
                                        |   #"8" => digit_star2(sign, integral,tl, hd :: temp)
                                        |   #"9" => digit_star2(sign, integral,tl, hd :: temp)
                                        |   #"(" => digit_plus(sign, integral, implode(rev(temp)), tl, [])
                                        |    _ => raise rat_error )
    | digit_star2(sign, integral, [], temp) = raise rat_error

and digit_plus(sign, integral, non_recc, l as hd :: tl, temp) = ( case hd of
                                            #"0" => digit_plus(sign, integral, non_recc, tl, hd :: temp)
                                        |   #"1" => digit_plus(sign,integral, non_recc, tl, hd :: temp)
                                        |   #"2" => digit_plus(sign, integral, non_recc,tl, hd :: temp)
                                        |   #"3" => digit_plus(sign, integral,non_recc,tl, hd :: temp)
                                        |   #"4" => digit_plus(sign, integral,non_recc,tl, hd :: temp)
                                        |   #"5" => digit_plus(sign, integral,non_recc,tl, hd :: temp)
                                        |   #"6" => digit_plus(sign, integral,non_recc,tl, hd :: temp)
                                        |   #"7" => digit_plus(sign, integral,non_recc,tl, hd :: temp)
                                        |   #"8" => digit_plus(sign, integral,non_recc,tl, hd :: temp)
                                        |   #"9" => digit_plus(sign, integral,non_recc,tl, hd :: temp)
                                        |   #")" => if (length(temp) > 0) then final_check(sign, integral, non_recc, implode(rev(temp)), tl)
                                                    else raise rat_error
                                        |   _ => raise rat_error )
    | digit_plus(sign, integral, non_recc, [], temp) = raise rat_error

and final_check(sign, integral, non_recc, recc, l) = if (l = []) then convert_to_fraction(sign, integral, non_recc, recc) else raise rat_error

and toDecimal(s) = showDecimal(s)



end ;

structure Rational :> RATIONAL = Rational_functor(Bigint) ;