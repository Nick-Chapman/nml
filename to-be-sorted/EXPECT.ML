structure Expect = struct val expect = ref None : bool option ref end
fun EXPECT F_bool = (Expect.expect := Some (F_bool ()))
