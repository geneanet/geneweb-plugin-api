let () =
  Alcotest.run "geneweb-plugin-api"
    [
      ("ConvertDateApi", Convert_date_test.tests);
      ("Privacy_filter", Privacy_filter_test.tests)
    ]
