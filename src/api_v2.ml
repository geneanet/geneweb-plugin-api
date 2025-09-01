let not_found conf = Api_util.print_error conf `not_found ""

let bad_request conf = Api_util.print_error conf `bad_request ""

let handler conf base =
  let piqi_request = Api_util.get_params conf Api_v2_piqi_ext.parse_request in
  let person_request = Request.request_of_piqi_request piqi_request in
  match person_request with
  | Some request ->
    (match Response.response conf base request with
    | Some response ->
      let data = Response.to_piqi response in
      Api_util.print_result conf (Api_v2_piqi_ext.gen_person data)
    | None -> not_found conf)
  | None -> bad_request conf
