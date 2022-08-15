type channels = { input: in_channel; output: out_channel }

val with_null_io: ('a -> 'b) -> 'a -> 'b
val with_channel_io: channels -> ('a -> 'b) -> 'a -> 'b
