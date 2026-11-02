let () =
  Alcotest.run "geneweb-plugin-api"
    [ ("Privacy_filter", Privacy_filter_test.tests) ]
