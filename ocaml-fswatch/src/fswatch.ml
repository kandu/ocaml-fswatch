(*
 * fswatch.ml
 * -----------
 * Copyright : (c) 2019, ZAN DoYe <zandoye@gmail.com>
 * Licence   : MIT
 *
 * This file is a part of ocaml-fswatch.
 *)


module SessionMap = Map.Make(Nativeint)

type handle= {
  session: nativeint;
  callback: Stub.Event.callback;
  mutable alive: bool;
}

let sessions= ref (SessionMap.empty: handle SessionMap.t)

module Status = struct
  include Stub.Status
end

module Event = struct
  include Stub.Event
end

module Monitor = struct
  include Stub.Monitor
end

module Filter = struct
  include Stub.Filter
  type monitor_filter = cmonitor_filter = {
    text : string;
    filter_type : filter_type;
    case_sensitive : bool;
    extended : bool;
  }
end

let init_library ()=
  let callback session events=
    match SessionMap.find session !sessions with
    | handle-> handle.callback events; flush_all ()
    | exception Not_found-> ()
  in
  Callback.register "callback" callback;
  match Stub.FSW.init_library () |> Stub.Status.t_of_int with
  | Stub.Status.FSW_OK as r-> r
  | _ as r-> r

let init_session monitor callback=
  let session= Stub.FSW.init_session monitor in
  let handle= { session; callback; alive= true } in
  sessions:= SessionMap.add session handle !sessions;
  Gc.finalise
    (fun handle->
       if handle.alive then
        begin
         Stub.FSW.destroy_session handle.session |> ignore;
         handle.alive <- false;
       end;
       sessions:= SessionMap.remove handle.session !sessions)
    handle;
  handle

let add_path handle path=
  Stub.FSW.add_path handle.session path |> Stub.Status.t_of_int

let add_property handle ~name ~value=
  Stub.FSW.add_property handle.session ~name ~value |> Stub.Status.t_of_int

let set_allow_overflow handle allow=
  Stub.FSW.set_allow_overflow handle.session allow |> Stub.Status.t_of_int

let set_recursive handle recursive=
  Stub.FSW.set_recursive handle.session recursive |> Stub.Status.t_of_int

let set_directory_only handle directory_only=
  Stub.FSW.set_directory_only handle.session directory_only
  |> Stub.Status.t_of_int

let set_follow_symlinks handle follow=
  Stub.FSW.set_follow_symlinks handle.session follow |> Stub.Status.t_of_int

let add_event_type_filter handle filter=
  Stub.FSW.add_event_type_filter handle.session filter
  |> Stub.Status.t_of_int

let add_filter handle filter=
  Stub.FSW.add_filter handle.session filter |> Stub.Status.t_of_int

let start_monitor handle=
  Stub.FSW.start_monitor handle.session |> Stub.Status.t_of_int

let start_monitor_thread handle= Thread.create start_monitor handle

let stop_monitor handle=
  Stub.FSW.stop_monitor handle.session |> Stub.Status.t_of_int

let is_running handle= Stub.FSW.is_running handle.session
let destroy_session handle=
  Stub.FSW.destroy_session handle.session |> Stub.Status.t_of_int

let last_error ()= Stub.FSW.last_error () |> Stub.Status.t_of_int

let is_verbose= Stub.FSW.is_verbose
let set_verbose= Stub.FSW.set_verbose

