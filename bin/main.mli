val cmds : int Cmdliner.Cmd.t list
val doc : string
val sdocs : string
val man : [> `P of string | `S of string ] list
val main : int Cmdliner.Cmd.t
val main : unit -> 'a
