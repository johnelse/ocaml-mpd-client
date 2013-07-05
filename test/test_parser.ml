open OUnit

let test_parse_ok =
  let test ~msg ~input ~expected_output () =
    let parsed = Mpd_parser.parse_ok input in
    assert_equal ~msg parsed expected_output
  in
  "test_parse_ok" >:::
    [
      "test_empty_response" >::
        test ~msg:"Parsing an empty OK response"
          ~input:"OK\n"
          ~expected_output:(Mpd_parser.Ok []);
      "test_response_with_data" >::
        test ~msg:"Parsing an OK response with some data"
          ~input:"key1: value1\nkey2: value2\nOK\n"
          ~expected_output:(Mpd_parser.Ok [
            "key1", "value1";
            "key2", "value2";
          ]);
    ]

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
      test_parse_ok;
      test_parse_ack;
    ]

let _ = run_test_tt_main base_suite
