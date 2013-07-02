open OUnit

let test_parse_ack =
  let test ~msg ~input ~expected_output () =
    let parsed = Mpd_parser.parse_ack input in
    assert_equal ~msg parsed expected_output
  in
  "test_parse_ack" >:::
    [
      "test_basic_ack" >::
        test ~msg:"Parsing a basic ACK message"
          ~input:"ACK [4@0] {kill} description\n"
          ~expected_output:(Mpd_parser.Ack {
            Mpd_parser.error="4";
            command_num=0;
            command="kill";
            message_text="description";
          });
    ]

let base_suite =
  "base_suite" >:::
    [
      test_parse_ack;
    ]

let _ = run_test_tt_main base_suite
