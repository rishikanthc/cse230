(*
 * art.ml
 * cs334
 * based on code by Chris Stone
 *)

#use "misc.ml"
#use "expr.ml"

(******************* Functions you need to write **********)

(* build: (int*int->int) * int -> Expr 
   Build an expression tree.  The second argument is the depth, 
   the first is a random function.  A call to rand(2,5) will give
   you a random number in the range [2,5)  
   (2 inclusive, and 5 exclusive).

   Your code should call buildX, buildSine, etc. to construct
   the expression.
*)

(* chooseOp: int -> expr
 * This function is a helper function for build and build2.
 * When build or build2 hits their base case (depth 0) then it chooses a random number 1/2 which is then passed to this function.
 * 1 returns x and 2 returns y
 *)
let chooseOp x = match x with
  | 1 -> buildX()
  | _ -> buildY()
;;

(* build: (int*int -> int)*int -> expr
 * The function takes as input a random generator function and depth.
 * A random number in range of 8 is generated to choose an operator randomly.
 * if the the depth is 0, then buildX or buildY is chosen as the operator
 * The function recursively calls itself to generate arguments of each chosen operator, each time decrementing depth, till depth is 0 and hits the base case.
 * The output is a nested expression of the given depth.
 *)
let rec build (rand,depth) = if depth = 0 then chooseOp(rand(1,3))
  else match rand(3, 8) with
    | 3 -> buildSine(build(rand, (depth - 1)))
    | 4 -> buildCosine(build(rand, (depth - 1)))
    | 5 -> buildAverage(build(rand, (depth - 1)), build(rand, (depth - 1)))
    | 6 -> buildTimes(build(rand, (depth - 1)), build(rand, (depth - 1)))
    | 7 -> buildThresh(build(rand, (depth - 1)), build(rand, (depth - 1)),build(rand, (depth - 1)),build(rand, (depth - 1)))
    | _ -> buildX()
;;

(* build2: (int*int -> int)*int -> expr
 * it performs the same function as build, but the random number limits are increased so it can choose to pick form the 2 additional custom operators I added in expr.ml
 *)
let rec build2 (rand,depth) = if depth = 0 then chooseOp(rand(1,3))
  else match rand(3, 10) with
    | 3 -> buildSine(build2(rand, (depth - 1)))
    | 4 -> buildCosine(build2(rand, (depth - 1)))
    | 5 -> buildAverage(build2(rand, (depth - 1)), build2(rand, (depth - 1)))
    | 6 -> buildTimes(build2(rand, (depth - 1)), build2(rand, (depth - 1)))
    | 7 -> buildThresh(build2(rand, (depth - 1)), build2(rand, (depth - 1)),build2(rand, (depth - 1)),build2(rand, (depth - 1)))
    | 8 -> buildExpo(build2(rand, (depth - 1)))
    | 9 -> buildCustom(build2(rand, (depth - 1)), build2(rand, (depth - 1)), build2(rand, (depth - 1)))
    | _ -> buildX()
;;
(* Please fill in ALL of g1,g2,g3,c1,c2,c3 regardless of whether you
 * are aiming for extra credit. 
 *)

(* g1,g2,g3,c1,c2,c3 : unit -> int * int * int
 * these functions should return the parameters needed to create your 
 * top three color / grayscale pictures.
 * they should return (depth,seed1,seed2)
 *)

let g1 () = (9,69,96);;
let g2 () = (9,150,28);;
let g3 () = (11,167,59);;

let c1 () = (9,69,96);;
let c2 () = (9,60,97);;
let c3 () = (11,150,78);;

(**** You should not need to modify any code below here ****)


(******************** Random Number Generators ************)

(* makeRand int * int -> (int * int -> int)
   Returns a function that, given a low and a high, returns
   a random int between the limits.  seed1 and seed2 are the
   random number seeds.  Pass the result of this function
   to build 

   Example:
      let rand = makeRand(10,39) in 
      let x =  rand(1,4) in 
          (* x is 1,2,3, or 4 *)
*)

let makeRand (seed1, seed2) = 
  let seed = (Array.of_list [seed1;seed2]) in
  let s = Random.State.make seed in
  (fun (x,y) -> (x + (Random.State.int s (y-x))))


let rec rseq g r n =
  if n <= 0 then [] else (g r)::(rseq g r (n-1))

(********************* Bitmap creation code ***************)

(* 
   You should not have to modify the remaining functions.
   Add testing code to the bottom of the file.
*)
  
(* Converts an integer i from the range [-N,N] into a float in [-1,1] *)
let toReal (i,n) = (float_of_int i) /. (float_of_int n)

(* Converts real in [-1,1] to an integer in the range [0,255]  *)
let toIntensity z = int_of_float (127.5 +. (127.5 *. z))

let ffor (low,high,f) = if low > high then () else let _ = f low in ffor (low+1,high,f)

(* emitGrayscale :  ((real * real) -> real) * int -> unit
 emitGrayscale(f, N) emits the values of the expression
 f (converted to intensity) to the file art.pgm for an 
 2N+1 by 2N+1 grid of points taken from [-1,1] x [-1,1].
 
 See "man pgm" on turing for a full description of the file format,
 but it's essentially a one-line header followed by
 one byte (representing gray value 0..255) per pixel.
 *)

let emitGrayscale (f,n,fname) =
    (* Open the output file and write the header *)
    let chan = open_out (fname^".pgm") in
    (* Picture will be 2*N+1 pixels on a side *)
    let n2p1 = n*2+1 in   
    let _ = output_string chan (Printf.sprintf "P5 %d %d 255\n" n2p1 n2p1) in
    let _ = 
      ffor (-n, n, 
        fun ix ->
          ffor (-n, n, 
            fun iy ->
              (* Convert grid locations to [-1,1] *)
              let x = toReal(ix,n) in
              let y = toReal(iy,n) in
              (* Apply the given random function *)
              let z = f (x,y) in
              (* Convert the result to a grayscale value *)
              let iz = toIntensity(z) in
              (* Emit one byte for this pixel *)
              output_char chan (char_of_int iz))) in 
    close_out chan;
    ignore(Sys.command ("convert "^fname^".pgm "^fname^".jpg"));
    ignore(Sys.command ("rm "^fname^".pgm"))

(* doRandomGray : int * int * int -> unit
 Given a depth and two seeds for the random number generator,
 create a single random expression and convert it to a
 grayscale picture with the name "art.pgm" *)

let doRandomGrayBuilder (depth,seed1,seed2,builder,prefix) =
  (* Initialize random-number generator g *)
  let g = makeRand(seed1,seed2) in
  (* Generate a random expression, and turn it into an ML function *)
  let e = builder (g,depth) in
  let _ = print_string (exprToString e) in
  let f = eval_fn e in
  (* 301 x 301 pixels *)
  let n = 150 in
  (* Emit the picture *)
  let name = Printf.sprintf "%s_%d_%d_%d" prefix depth seed1 seed2 in
  emitGrayscale (f,n,name)

let doRandomGray  (depth,seed1,seed2) = doRandomGrayBuilder (depth,seed1,seed2,build,"art_g_1")
let doRandomGray2 (depth,seed1,seed2) = doRandomGrayBuilder (depth,seed1,seed2,build2,"art_g_2")


(* emitColor : (real*real->real) * (real*real->real) *
               (real*real->real) * int -> unit
 emitColor(f1, f2, f3, N) emits the values of the expressions
 f1, f2, and f3 (converted to RGB intensities) to the output
 file art.ppm for an 2N+1 by 2N+1 grid of points taken 
 from [-1,1] x [-1,1].
 
 See "man ppm" on turing for a full description of the file format,
 but it's essentially a one-line header followed by
 three bytes (representing red, green, and blue values in the
 range 0..255) per pixel.
 *)
let emitColor (f1,f2,f3,n,fname) =
    (* Open the output file and write the header *)
    let chan = open_out (fname^".ppm") in
    (* Picture will be 2*N+1 pixels on a side *)
    let n2p1 = n*2+1 in   
    let _ = output_string chan (Printf.sprintf "P6 %d %d 255\n" n2p1 n2p1) in
    let _ = 
      ffor (-n, n, 
        fun ix ->
          ffor (-n, n, 
            fun iy ->
              (* Convert grid locations to [-1,1] *)
              let x = toReal(ix,n) in
              let y = toReal(iy,n) in
              (* Apply the given random function *)
              let z1 = f1 (x,y) in
              let z2 = f2 (x,y) in
              let z3 = f3 (x,y) in

              (* Convert the result to a grayscale value *)
              let iz1 = toIntensity(z1) in
              let iz2 = toIntensity(z2) in
              let iz3 = toIntensity(z3) in
              
              (* Emit one byte per color for this pixel *)
              output_char chan (char_of_int iz1);
              output_char chan (char_of_int iz2);
              output_char chan (char_of_int iz3);
         )) in  
    close_out chan;
    ignore(Sys.command ("convert "^fname^".ppm  "^fname^".jpg"));
    ignore(Sys.command ("rm "^fname^".ppm")) 

(* doRandomColor : int * int * int -> unit
 Given a depth and two seeds for the random number generator,
 create a single random expression and convert it to a
 color picture with the name "art.ppm"  (note the different
 extension from toGray) 
 *)
let doRandomColorBuilder (depth,seed1,seed2,builder,prefix) =
  (* Initialize random-number generator g *)
  let g = makeRand (seed1,seed2) in
  (* Generate a random expression, and turn it into an ML function *)
  let e1 = builder (g, depth) in
  let e2 = builder (g, depth) in
  let e3 = builder (g, depth) in
  
  let _ = Printf.printf "red   = %s \n" (exprToString e1) in
  let _ = Printf.printf "green = %s \n" (exprToString e2) in
  let _ = Printf.printf "blue  = %s \n" (exprToString e3) in

  let f1 = eval_fn e1 in
  let f2 = eval_fn e2 in
  let f3 = eval_fn e3 in

  (* 301 x 301 pixels *)
  let n = 150 in
  (* Emit the picture *)
  let name = Printf.sprintf "%s_%d_%d_%d" prefix depth seed1 seed2 in
  emitColor (f1,f2,f3,n,name)

let doRandomColor  (depth,seed1,seed2) = doRandomColorBuilder (depth,seed1,seed2,build,"art_c_1")
let doRandomColor2 (depth,seed1,seed2) = doRandomColorBuilder (depth,seed1,seed2,build2,"art_c_2")
  
(*************** Insert Testing Code Here ******************)
