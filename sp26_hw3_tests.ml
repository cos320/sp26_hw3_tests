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
  ]
