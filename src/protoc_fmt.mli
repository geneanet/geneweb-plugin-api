type t = Protobuf | Json
val protobuf : t
val json : t
val to_piqi : t -> Piqirun_ext.output_format
