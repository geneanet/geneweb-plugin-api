let () =
  Alcotest.run "geneweb-plugin-api"
    [ ("ConvertDateApi", Convert_date_test.tests) ]
