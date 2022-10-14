open Fswatch

val init_session : Monitor.t -> handle * Event.t array Async_kernel.Mvar.Read_write.t
val start_monitor : handle -> unit -> unit Async_kernel.Deferred.t

