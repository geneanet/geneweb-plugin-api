let test_gregorian_to_multiple_calendars () =
  let conf = Geneweb.Config.empty in
  let date =
    {
      Plugin_api_lib.Api_piqi.Date.cal = Some `gregorian;
      prec = Some `sure;
      dmy =
        Some
          {
            Plugin_api_lib.Api_piqi.Dmy.day = 14l;
            month = 7l;
            year = 1789l;
            delta = 0l;
          };
      dmy2 = None;
      text = None;
    }
  in
  let req =
    {
      Plugin_api_lib.Api_piqi.Convert_date_request.date;
      target_calendars = [ `gregorian; `julian; `french ];
    }
  in
  let response_str = Plugin_api_lib.Api.ConvertDateApi.convert_date conf req `pb in
  let res = Plugin_api_lib.Api_piqi_ext.parse_convert_date_response response_str `pb in
  Alcotest.(check int) "converted_dates count" 3 (List.length res.converted_dates);
  let greg = List.nth res.converted_dates 0 in
  let julian = List.nth res.converted_dates 1 in
  let french = List.nth res.converted_dates 2 in
  Alcotest.(check bool)
    "gregorian calendar matches"
    true
    (greg.converted_date.cal = Some `gregorian);
  Alcotest.(check bool)
    "julian calendar matches"
    true
    (julian.converted_date.cal = Some `julian);
  Alcotest.(check bool)
    "french calendar matches"
    true
    (french.converted_date.cal = Some `french);
  let julian_dmy = Option.get julian.converted_date.dmy in
  Alcotest.(check int32) "julian day is 3" 3l julian_dmy.day;
  Alcotest.(check int32) "julian month is 7" 7l julian_dmy.month;
  Alcotest.(check int32) "julian year is 1789" 1789l julian_dmy.year;
  Alcotest.(check bool) "date_str is defined" true (String.length greg.date_str > 0);
  Alcotest.(check bool) "date_long_str is defined" true (String.length greg.date_long_str > 0)

let test_default_all_calendars () =
  let conf = Geneweb.Config.empty in
  let date =
    {
      Plugin_api_lib.Api_piqi.Date.cal = Some `gregorian;
      prec = Some `sure;
      dmy =
        Some
          {
            Plugin_api_lib.Api_piqi.Dmy.day = 1l;
            month = 1l;
            year = 2000l;
            delta = 0l;
          };
      dmy2 = None;
      text = None;
    }
  in
  let req =
    {
      Plugin_api_lib.Api_piqi.Convert_date_request.date;
      target_calendars = [];
    }
  in
  let response_str = Plugin_api_lib.Api.ConvertDateApi.convert_date conf req `pb in
  let res = Plugin_api_lib.Api_piqi_ext.parse_convert_date_response response_str `pb in
  Alcotest.(check int)
    "converted_dates defaults to 5 calendars"
    5
    (List.length res.converted_dates)

let test_imprecise_date () =
  let conf = Geneweb.Config.empty in
  let date =
    {
      Plugin_api_lib.Api_piqi.Date.cal = Some `gregorian;
      prec = Some `about;
      dmy =
        Some
          {
            Plugin_api_lib.Api_piqi.Dmy.day = 0l;
            month = 0l;
            year = 1789l;
            delta = 0l;
          };
      dmy2 = None;
      text = None;
    }
  in
  let req = { Plugin_api_lib.Api_piqi.Convert_date_request.date; target_calendars = [ `gregorian ] } in
  let response_str = Plugin_api_lib.Api.ConvertDateApi.convert_date conf req `pb in
  let res = Plugin_api_lib.Api_piqi_ext.parse_convert_date_response response_str `pb in
  Alcotest.(check int) "converted_dates count" 1 (List.length res.converted_dates);
  let greg = List.nth res.converted_dates 0 in
  Alcotest.(check bool) "precision is about" true (greg.converted_date.prec = Some `about);
  let dmy = Option.get greg.converted_date.dmy in
  Alcotest.(check int32) "day is 0" 0l dmy.day;
  Alcotest.(check int32) "month is 0" 0l dmy.month;
  Alcotest.(check int32) "year is 1789" 1789l dmy.year

let test_text_date () =
  let conf = Geneweb.Config.empty in
  let date =
    {
      Plugin_api_lib.Api_piqi.Date.cal = None;
      prec = None;
      dmy = None;
      dmy2 = None;
      text = Some "Spring 1789";
    }
  in
  let req = { Plugin_api_lib.Api_piqi.Convert_date_request.date; target_calendars = [ `gregorian ] } in
  let response_str = Plugin_api_lib.Api.ConvertDateApi.convert_date conf req `pb in
  let res = Plugin_api_lib.Api_piqi_ext.parse_convert_date_response response_str `pb in
  Alcotest.(check int) "converted_dates count" 1 (List.length res.converted_dates);
  let res_item = List.nth res.converted_dates 0 in
  Alcotest.(check bool) "text matches" true (res_item.converted_date.text = Some "Spring 1789");
  Alcotest.(check string) "date_str matches text" "Spring 1789" res_item.date_str

let test_julian_source () =
  let conf = Geneweb.Config.empty in
  let date =
    {
      Plugin_api_lib.Api_piqi.Date.cal = Some `julian;
      prec = Some `sure;
      dmy =
        Some
          {
            Plugin_api_lib.Api_piqi.Dmy.day = 3l;
            month = 7l;
            year = 1789l;
            delta = 0l;
          };
      dmy2 = None;
      text = None;
    }
  in
  let req = { Plugin_api_lib.Api_piqi.Convert_date_request.date; target_calendars = [ `gregorian ] } in
  let response_str = Plugin_api_lib.Api.ConvertDateApi.convert_date conf req `pb in
  let res = Plugin_api_lib.Api_piqi_ext.parse_convert_date_response response_str `pb in
  Alcotest.(check int) "converted_dates count" 1 (List.length res.converted_dates);
  let greg = List.nth res.converted_dates 0 in
  Alcotest.(check bool) "calendar is gregorian" true (greg.converted_date.cal = Some `gregorian);
  let dmy = Option.get greg.converted_date.dmy in
  Alcotest.(check int32) "converted day is 14" 14l dmy.day;
  Alcotest.(check int32) "converted month is 7" 7l dmy.month;
  Alcotest.(check int32) "converted year is 1789" 1789l dmy.year

let tests =
  [
    Alcotest.test_case
      "ConvertDateApi.convert_date (Gregorian to multiple calendars)"
      `Quick
      test_gregorian_to_multiple_calendars;
    Alcotest.test_case
      "ConvertDateApi.convert_date (Empty targets default to all calendars)"
      `Quick
      test_default_all_calendars;
    Alcotest.test_case
      "ConvertDateApi.convert_date (Imprecise date handling)"
      `Quick
      test_imprecise_date;
    Alcotest.test_case
      "ConvertDateApi.convert_date (Text date bypass)"
      `Quick
      test_text_date;
    Alcotest.test_case
      "ConvertDateApi.convert_date (Julian source to Gregorian)"
      `Quick
      test_julian_source;
  ]
