signature BIGINT =
sig
type bigint
exception divide_by_zero
exception arithmetic_error
exception invalid_integer_argument
val make_from_str : string -> bigint
val make_from_int : int -> bigint
val add : bigint * bigint -> bigint 
val sub : bigint * bigint -> bigint
val mult :  bigint * bigint -> bigint
(* val divide : bigint * bigint -> bigint * bigint returns (quotient , remainder)   *)
val equal : bigint * bigint -> bool
val less : bigint * bigint -> bool
val negate : bigint -> bigint
val sign : bigint -> int
val divide : bigint * bigint -> bigint * bigint  (* takes two non negative integers, and returns quotient and remainder *)
val normalize : bigint -> bigint (* converts an integer in non standard form to a std form *)
val power : bigint * bigint -> bigint
end

structure Bigint : BIGINT  =
struct
  type bigint = string  ;
  exception divide_by_zero ;
  exception arithmetic_error ;
  exception invalid_integer_argument ; 
(* this fn takes a list of char and convert it to a list of numbers in same order, with a sign  *)
fun str_to_int([],sign,ans,done) = (sign, rev(ans)) |
    str_to_int(hd :: tl, sign, ans ,done) = if (hd = #"~" andalso (done = false) ) then str_to_int(tl, 1, ans,true)
                                       else if (hd = #"+" andalso (done = false) ) then str_to_int(tl, 0, ans,true)
                                       else if (hd = #"1" orelse hd = #"2" orelse hd = #"3"  orelse hd = #"4" orelse
                                       hd = #"5" orelse hd = #"6" orelse hd = #"7"  orelse hd = #"8" orelse hd = #"9" orelse hd= #"0" ) then 
                                                str_to_int(tl, sign, ord(hd)- ord(#"0") :: ans, true) 
                                       else raise invalid_integer_argument 

(* takes a list of int and gives corr list of char *)
fun int_to_str([],ans) = rev(ans) |
    int_to_str(hd :: tl, ans) = int_to_str(tl, chr(ord(#"0") + hd):: ans)


fun strip([]) = [0] |
    strip(l) = if (hd(l) = 0) then strip(tl(l))
                else l

fun check_zero(l) = (hd(strip(l)) = 0)  (* checks if l represents zero without sign *)

(* takes a string and outputs the corresponding no, with a - if the no. is negative *)
fun convert_to_list(s) = let val (sign,l)  = str_to_int(explode(s), 0, [],false)
                         in sign :: strip(l)
                         end


(* takes a stripped list as input an=d converts it to a string *)
fun convert_to_string(l as hd :: tl) = if (hd = 0) then implode(int_to_str(tl,[]))
                                  else if (not (check_zero(tl)) ) then "~" ^ implode(int_to_str(tl,[]))
                                  else implode(int_to_str(tl,[]))
|   convert_to_string([]) = "0"



local
  (* this fn assumes both x, y to be positive and adds them ; no leading sign digit and ans in rev form *)
  fun addition_helper([] , [] , carry , ans) = if carry = 0 then rev ans else rev (carry :: ans) |
    addition_helper([] , h ::  tl , carry , ans) = let
                                                val sum = carry + h
                                                val d1 = sum div 10
                                                val d2 = sum mod 10
                                            in
                                                addition_helper([] , tl , d1 , d2 :: ans)
                                            end|

    addition_helper(h ::  tl , [] , carry , ans) = let
                                                val sum = carry + h
                                                val d1 = sum div 10
                                                val d2 = sum mod 10
                                            in
                                               addition_helper(tl , [] , d1 , d2 :: ans)
                                            end |
    addition_helper(d1 :: tl1 , d2 :: tl2 , carry , ans) = let
                                                val sum = d1 + d2 +carry
                                                val tens = sum div 10
                                                val ones = sum mod 10
                                                in
                                                    addition_helper(tl1 , tl2 , tens , ones :: ans)
                                                end

  (* this fn assumes both x, y to be positive and subtracts y from x. no leading sign digit in x,y and x is bigger than y. ans in rev form *)
fun subtraction_helper([] , [] , carry , ans) = rev ans |

        subtraction_helper(h :: nil, [] , carry , ans) = if carry = 1 andalso h >= 2
                                                then subtraction_helper(nil , [] , 0 , (h - 1) :: ans)
                                                else if carry = 1 andalso h = 1 then
                                                     subtraction_helper(nil , [] , 0 , ans)
                                                else subtraction_helper(nil , [] , 0 , h :: ans) |

          subtraction_helper(h1 :: nil,h2 :: nil  , carry , ans) = if carry = 1 andalso h1 - h2 -1 > 0
                                                then subtraction_helper(nil , [] , 0 , (h1 - h2 - 1) :: ans)
                                                else if carry = 1 andalso h1 - h2 -1 = 0 then
                                                     subtraction_helper(nil , [] , 0 , ans)
                                                else if h1 = h2 then subtraction_helper(nil , [] , 0 , ans)
                                                else subtraction_helper(nil , [] , 0 , (h1 - h2) :: ans) |

        subtraction_helper(h :: tl, [] , carry , ans) = if h = 0 andalso carry = 1
                                                 then subtraction_helper(tl , [] , 1 , 9 :: ans)
                                                 else if h  >0 andalso carry = 1
                                                then subtraction_helper(tl , [] , 0 , (h - 1) :: ans)
                                                else subtraction_helper(tl , [] , 0 , h :: ans) |
        subtraction_helper(h1 :: tl1 , h2 :: tl2 , carry , ans) =
                                                if carry= 1 andalso h1 = 0 then subtraction_helper(tl1 , tl2 , 1 , (9 - h2) :: ans)
                                                else if carry = 1 andalso h1 > 0 andalso (h1 -1) >= h2 then subtraction_helper(tl1 , tl2 , 0 , h1 - 1 - h2 :: ans)
                                                else if carry = 1 andalso h1 > 0 andalso (h1 - 1) < h2  then subtraction_helper(tl1 , tl2 , 1 , 9 + h1 - h2 :: ans)
                                                else if carry = 0 andalso h1 < h2 then subtraction_helper(tl1 , tl2 , 1 , 10 + h1 - h2 :: ans)
                                                else subtraction_helper(tl1 , tl2 , 0 , h1 - h2 :: ans) |
        subtraction_helper([],l,carry,ans) = raise arithmetic_error


  (* this function multiplies a number (in form of list- reversed) with a single digit d ; gives ans in non reversed form *)
  fun multiplication_digit([] , d ,carry, ans) = if d = 0 then [] else ans |
    multiplication_digit(h :: nil , d , carry , ans) = if d = 0 then [] else let
                                                                 val prod = h * d  + carry
                                                                 val d1 = prod div 10
                                                                 val d2 = prod mod 10
                                                             in
                                                                if d1 = 0 andalso not(d2 = 0) then
                                                                    multiplication_digit(nil ,  d  , 0 , d2 :: ans)
                                                                else if d1 = 0 andalso d2 = 0 then
                                                                    multiplication_digit(nil ,  d  , 0 , ans)
                                                                else
                                                                    multiplication_digit(nil ,  d  , 0 ,d1 :: d2 :: ans)
                                                             end |

    multiplication_digit(h :: tl , d , carry , ans) = if d = 0 then [] else let
                                                                 val prod = h * d  + carry
                                                                 val d1 = prod div 10
                                                                 val d2 = prod mod 10
                                                             in
                                                                multiplication_digit(tl , d  , d1 , d2 :: ans)
                                                             end
(* assuming that x has larger length than y, and that x is in reversed form, y is NOT . gives answer in non reversed form , no sign digits assumed*)

    fun multiplication_helper(x,[], ans) = rev ans |
    multiplication_helper(x,y,ans) = if  (length(x) < length(y)) then multiplication_helper(rev(y),rev(x),ans)
                                      else  let   val curr = rev(multiplication_digit(x, hd(y), 0,[]))
                                                        val ans' = addition_helper(0::ans,curr,0, [])
                                                  in multiplication_helper(x, tl(y), ans')
                                                  end

(* compares two numbers represented as two lists(l1,l2 respectively) non reversed and without sign and returns whether l1 < l2  *)
    fun lt(l1 , l2 ) = let
                                val len1= length l1
                                val len2 = length l2
                            in if (not(len1 = len2)) then len1 < len2
                               else
                                let
                                    fun compare(nil , nil) = false |
                                    compare(h1 :: t1 , h2 :: t2) = if h1 = h2 then compare(t1 , t2)
                                                                        else h1 < h2 |
                                    compare(_,_) = raise arithmetic_error


                                in
                                    compare(l1,l2)
                                end
                            end
     (* thi fn finds the largest digit d s.t x.d <= y  *)
    fun find_digit_helper(x,y, d) = if (not ( lt(strip(rev(y)), strip(multiplication_digit(x,d,0,[])) ) ) andalso d < 10) then find_digit_helper(x,y, d+1)
                                    else d-1
    fun find_digit(x,y) = find_digit_helper(x, y , 0)

    fun division_helper(quotient,_,[],rem) = (quotient, rem) |
    division_helper(quotient, divisor, dividend, rem) = let
                                                                val d = find_digit(divisor,hd(dividend)::rem)
                                                                val quotient' = d :: quotient
                                                                val dividend' = tl(dividend)
                                                                val op1 = hd(dividend)::rem
                                                                val op2= rev(multiplication_digit(divisor,d,0,[]) )
                                                                val rem' = rev(strip(rev( subtraction_helper( op1, op2  , 0,[]) )))
                                                            in
                                                                division_helper(quotient', divisor,dividend', rem')
                                                            end
in
  fun make_from_int(x) = Int.toString(x)
  fun make_from_str(x) = convert_to_string(convert_to_list(x)) 
  fun add(x1,y1) = let val (x,y) = (convert_to_list(x1), convert_to_list(y1))
                 in if (hd(x) = 0 andalso hd(y) = 0) then let
                                                            val sum  = rev(addition_helper(rev(tl(x)), rev(tl(y)),0,[]))
                                                          in convert_to_string(0 :: (strip(sum)) )
                                                          end
                    else if (hd(x) = 0 andalso hd(y) = 1) then let val lt = lt(tl(x), tl(y))
                                                               in if (lt) then let val diff = rev(subtraction_helper(rev(tl(y)), rev(tl(x)), 0, []))
                                                                          in convert_to_string(1 :: (strip(diff)) )
                                                                          end
                                                                  else let val diff = rev(subtraction_helper(rev(tl(x)), rev(tl(y)), 0, []))
                                                                        in  convert_to_string(0 :: (strip(diff)) )
                                                                        end
                                                                end
                    else if ((hd(x) = 1 andalso hd(y) = 0)) then add(y1,x1)
                    else let
                            val sum  = rev(addition_helper(rev(tl(x)), rev(tl(y)),0,[]))
                            in convert_to_string(1 :: (strip(sum)) )
                            end
                 end

    fun negate(x) = let val l = convert_to_list(x)
                        val sign = hd(l)
                    in
                        if (sign = 0 andalso not (check_zero(tl(l))) ) then "~" ^ convert_to_string(l)
                        else if (sign = 0) then "0"
                        else convert_to_string(0 :: tl(l))
                    end
    (* subtracts y from x *)
    fun sub(x1,y1) = add(x1, negate(y1))

    fun mult(x1,y1) = let val (x,y) = (convert_to_list(x1), convert_to_list(y1))
                      in if (hd(x) = hd(y)) then let val prod = 0 :: (strip(multiplication_helper(rev(tl(x)), tl(y), [])))
                                                 in convert_to_string(prod)
                                                 end
                         else let val prod = 1 :: (strip(multiplication_helper(rev(tl(x)), tl(y), [])) )
                                                 in convert_to_string(prod)
                                                 end
                      end
    fun less(x1,y1) = let val (x,y) = (convert_to_list(x1), convert_to_list(y1))
                      in if (hd(x) = 0 andalso hd(y) = 1) then false
                          else if (hd(x) = 1 andalso hd(y) = 0) then true
                          else if (hd(x) = 0 andalso hd(y) = 0) then lt(tl(x), tl(y))
                          else not (lt(tl(x), tl(y)))
                      end

    fun equal(x,y) = ( convert_to_string(convert_to_list(x)) = convert_to_string(convert_to_list(y)) ) ; 
    (* sign of 0 is +  *)
    fun sign(x) = if (hd(explode(x)) = #"~") then 1 else 0

    (* fun divide_slow(x, y) = if check_zero(tl(convert_to_list(y))) then raise divide_by_zero
                       else let val (x1,y1) = (tl(convert_to_list(x)), tl(convert_to_list(y)))
                                fun div_help(x,y) = if (lt(x,y)) then [0] (* returns quotient in reversed form *)
                                                else addition_helper(div_help(strip(rev(subtraction_helper(rev(x),rev(y),0,[]))), y), [1], 0, [])
                                val quotient = convert_to_string(0 :: (strip(rev(div_help(x1,y1)))))
                                val rem = sub(x, mult(y, quotient))
                            in (quotient, rem)
                            end   *)



    fun divide(x,y) = if check_zero(tl(convert_to_list(y))) then raise divide_by_zero
                      else let val(temp1,temp2) = (convert_to_list(x), convert_to_list(y)) ;
                               val (x1,y1) = (tl(temp1), tl(temp2) ) 
                               val (quotient, rem) = division_helper([],rev(y1), x1, [])
                               val q = if (hd(temp1) = hd(temp2)) then convert_to_string(0 ::strip(rev(quotient))) 
                                        else  convert_to_string(1 ::strip(rev(quotient))) 
                               val r =  if (hd(temp1) = hd(temp2)) then convert_to_string(0 ::strip(rev(rem))) 
                                        else  convert_to_string(1 ::strip(rev(rem))) 

                            in (q,r)
                            end


    fun normalize(x) = convert_to_string(convert_to_list(x))

    fun power(x,n) = if (n = "0") then "1"
                    else let val (q,rem) = (divide(n,"2"))
                         in if (rem = "0") then let val temp = power(x,q)
                                                in mult(temp,temp)
                                                end
                            else mult(x, power(x, sub(n,"1")))
                         end



end

end ;