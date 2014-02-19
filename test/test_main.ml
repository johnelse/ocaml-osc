open OUnit

let suite =
  "base_suite" >:::
    [
      Test_string.suite;
      Test_unix.suite;
    ]

let _ = run_test_tt_main suite
