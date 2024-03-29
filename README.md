# ocaml\_fswatch

Bindings for the [fswatch](https://emcrisostomo.github.io/fswatch/doc/) library

## Usage

The API of fswatch is not so convenient. If you `start_monitor` on a handle, the function will not return until `stop_monitor` is called. But since we are blocking on the function, we can only stop it being inside the callback function associated with the handle.

Hence, to integrate with other event loop, e.g. Async or Lwt, is not straightforward. The Async/Lwt extension of this binding invokes `start_monitor` in a new thread and then invoke the very callback function in the Async/Lwt main thread to make sure the fswatch event loop working in another thread will not mess up the main Async/Lwt thread.

### example

``` ocaml
open Fswatch

let callback events=
  Printf.printf "events:\n";
  Array.iter (fun event->
    let time= Unix.localtime event.Event.time in
    Printf.printf "  %s %d:%d:%d\n"
      event.Event.path
      time.tm_hour time.tm_min time.tm_sec)
    events;
  flush stdout

let ()=
  match init_library () with
  | Status.FSW_OK->
    let handle= init_session Monitor.System_default callback in
    add_path handle "/tmp/";
    start_monitor handle
  | err-> Printf.eprintf "%s\n" (Status.t_to_string err)
```

### async example

``` ocaml
open Async_kernel
open Fswatch

let rec listen msgBox=
  Mvar.take msgBox >>= fun events->
  Printf.printf "events:\n";
  Array.iter (fun event->
    let time= Unix.localtime event.Event.time in
    Printf.printf "  %s %d:%d:%d\n"
      event.Event.path
      time.tm_hour time.tm_min time.tm_sec)
    events;
  flush stdout;
  listen msgBox

let main ()=
  match init_library () with
  | Status.FSW_OK->
    let handle, msgBox= Fswatch_async.init_session Monitor.System_default in
    add_path handle "/tmp/";
    Async.Deferred.don't_wait_for (Fswatch_async.start_monitor handle ());
    listen msgBox
  | err-> Async.Writer.writef
    (Lazy.force Async.Writer.stderr)
    "%s\n" (Status.t_to_string err);
    Async.Writer.flushed (Lazy.force Async.Writer.stderr)

let ()=
  Async_unix.Thread_safe.block_on_async_exn main
```

### lwt example

``` ocaml
open Fswatch
open Lwt

let rec listen msgBox=
  Lwt_mvar.take msgBox >>= fun events->
  Printf.printf "events:\n";
  Array.iter (fun event->
    let time= Unix.localtime event.Event.time in
    Printf.printf "  %s %d:%d:%d\n"
      event.Event.path
      time.tm_hour time.tm_min time.tm_sec)
    events;
  flush stdout;
  listen msgBox

let main ()=
  match init_library () with
  | Status.FSW_OK->
    let handle, msgBox= Fswatch_lwt.init_session Monitor.System_default in
    add_path handle "/tmp/";
    async (Fswatch_lwt.start_monitor handle);
    listen msgBox
  | err-> Lwt_io.eprintf "%s\n" (Status.t_to_string err)

let ()=
  Lwt_main.run @@ main ()
```
