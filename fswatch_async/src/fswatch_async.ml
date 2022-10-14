let init_session monitor=
  let mbox= Async_kernel.Mvar.create () in
  let callback events=
    Async_unix.Thread_safe.run_in_async_wait_exn
    (fun ()-> Async_kernel.Mvar.put mbox events)
  in
  (Fswatch.init_session monitor callback, mbox)

let start_monitor handle ()=
  Async_unix.In_thread.run @@ fun ()-> Fswatch.start_monitor handle
