 (* Ben and Vedant's implementation of sphere tracing in OCaml for testing purposes.
    We've provided an example output in this directory.
 *)
 module SphereTrace = struct
   let shift_scale = 16

   type vec3 = { x : int64; y : int64; z : int64 }
   type ray = { position : vec3; dir : vec3 }

   let vec3_zero = { x = 0L; y = 0L; z = 0L }
   let vec3_from x y z = { x; y; z }

   let vec3_add a b =
     { x = Int64.add a.x b.x; y = Int64.add a.y b.y; z = Int64.add a.z b.z }

   let vec3_sub a b =
     { x = Int64.sub a.x b.x; y = Int64.sub a.y b.y; z = Int64.sub a.z b.z }

   let scaled_mul a b = Int64.shift_right (Int64.mul a b) shift_scale

   let vec3_scale v f =
     { x = scaled_mul v.x f; y = scaled_mul v.y f; z = scaled_mul v.z f }

   let vec3_dot a b =
     Int64.add (Int64.add (scaled_mul a.x b.x) (scaled_mul a.y b.y)) (scaled_mul a.z b.z)

   let sphere_hit center radius ray =
     let oc = vec3_sub ray.position center in
     let a = vec3_dot ray.dir ray.dir in
     let b = Int64.mul 2L (vec3_dot oc ray.dir) in
     let c = Int64.sub (vec3_dot oc oc) (scaled_mul radius radius) in
     let discriminant =
       Int64.sub (scaled_mul b b) (Int64.mul 4L (scaled_mul a c))
     in
     discriminant >= 0L

   let create_ppm image width height =
     let header =
       Printf.sprintf "P3\n%d %d\n255\n" width height in
     let body = Array.fold_right
       (fun { x; y; z } acc ->
         (Printf.sprintf "%Ld %Ld %Ld\n" x y z) ^ acc)
       image "" in
     header ^ body

   (* The main trace function: see sphere_trace.oat for an explanation.
    Almost entirely copied verbatim, without memory optimizations of course. *)
   let trace (width_shift : int) =
     let width = 1 lsl width_shift in
     let height = 1 lsl width_shift in

     let camera = vec3_zero in
     let sphere_center = vec3_from 0L 0L (Int64.shift_left 10L shift_scale) in
     let radius = Int64.shift_left 1L (shift_scale - 1) in

     let viewport_width = 1L in
     let viewport_height = 1L in

     let viewport_u = vec3_from (Int64.shift_left viewport_width shift_scale) 0L 0L in
     let viewport_v = vec3_from 0L (Int64.shift_left viewport_height shift_scale) 0L in

     let pixel_delta_u =
       { viewport_u with x = Int64.shift_right viewport_u.x width_shift }
     in
     let pixel_delta_v =
       { viewport_v with y = Int64.shift_right viewport_v.y width_shift }
     in

     let upper_left =
       vec3_from
         (Int64.neg (Int64.shift_left 1L (shift_scale - 1)))
         (Int64.neg (Int64.shift_left 1L (shift_scale - 1)))
         (Int64.shift_left 1L shift_scale)
     in

     let red = vec3_from 255L 0L 0L in
     let black = vec3_zero in
     let image = Array.make (width * height) black in

     for y = 0 to height - 1 do
       for x = 0 to width - 1 do
         let pu = vec3_scale pixel_delta_u (Int64.shift_left (Int64.of_int x) shift_scale) in
         let pv = vec3_scale pixel_delta_v (Int64.shift_left (Int64.of_int y) shift_scale) in
         let pixel_center = vec3_add upper_left (vec3_add pu pv) in
         let ray_dir = vec3_sub pixel_center camera in
         let ray = { position = camera; dir = ray_dir } in
         let color =
           if sphere_hit sphere_center radius ray then red else black
         in
         image.((y * width) + x) <- color
       done
     done;
     create_ppm image width height

   let rec n_ones n =
     match n with
     | 0 -> ""
     | n -> "1" ^ n_ones (n - 1)

  let expected_trace (width_shift: int)
    = trace width_shift ^ " 0"
 end

(** See the project instructions for more details about test case
    requirements.

    Add your test case to this list.

    Each test case is a triple of the form:
    (<filename>,<stdin>,<expected_stdout>)

    - <filename> should name an *.oat file that appears in this directory
      It should should declare the standard oat entry point:

      int program(int argc, string[] argv)

    - <stdin> should be the string passed as the command-line arguments
      to the executable generaged by compiling <filename> with
      the following sequence of commands:

      ./oatc <filename> bin/runtime.c
      ./a.out <stdin>

    - <expected_stdout> is the string representing the expected result
      obtained by running the compiled <filename> on <stdin> and
      concatenating the status code (0-255) returned by the call.

    You can use the command line to compile and run such tests like this:

    > ./oatc sp26_hw3_tests/demo_test.oat bin/runtime.c
    > ./a.out abc
    abcabc

    These test cases will be run via Gradedtests.oat_file_test.  For
    additional examples, see the tests/gradedtests.ml file.
 *)

let student_tests : (string * string * string) list = [
    (* provided demo example *)
    ("demo_test.oat", "abc", "abcabc0");
    ("demo_test.oat", "defg", "defgdefg0");
    (* Colin Baird, Jishnu Roychoudhury *)
    ("nac_k.oat", "", "50");
    ("nac_k.oat", "test2", "impossible0");

    (* Ayush and Isaac *)
    ("big_decimal.oat", "", "135.75 0");
    ("big_decimal.oat", "a", "575.31 0");
    ("big_decimal.oat", "a a", "652.43 0");

    (* Raheem Idowu *)
    ("mt19937_64.oat", "6", "-67287963618453421920");
    ("mt19937_64.oat", "67", "-57951761824575148100");
    ("mt19937_64.oat", "6767676767", "69277503470687894370");

    (* Ben Aepli and Vedant Badoni *)
    ("sphere_trace.oat", SphereTrace.n_ones 6, SphereTrace.expected_trace 6 );
    ("sphere_trace.oat", SphereTrace.n_ones 7, SphereTrace.expected_trace 7 );
    ("sphere_trace.oat", SphereTrace.n_ones 8, SphereTrace.expected_trace 8 );
  ]
