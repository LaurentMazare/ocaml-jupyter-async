open! Core

(* The worker part does not use async, only import it locally for the parent
   process.  *)

let worker_buf_len = 4096

type worker_input =
  | Execute of { code : string }
  | Complete of { code : string }
[@@deriving bin_io]

type worker_output =
  | Execute_output of unit Or_error.t
  | Complete_output of (int * (string * string) list)
[@@deriving bin_io]

type t =
  { pid : Pid.t
  ; reader : Async.Reader.t
  ; stdout_reader : Async.Reader.t
  ; stderr_reader : Async.Reader.t
  ; writer : Async.Writer.t
  ; sequencer : unit Async.Sequencer.t
  }

let worker ~worker_in_read ~worker_out_write =
  Ocaml_toploop.maybe_initialize ();
  Complete.reset ();
  let unpack_buffer =
    Unpack_buffer.Unpack_one.create_bin_prot bin_reader_worker_input
    |> Unpack_buffer.create
  in
  let in_channel = Unix.in_channel_of_descr worker_in_read in
  let out_channel = Unix.out_channel_of_descr worker_out_write in
  let send_output result =
    (* It is quite inefficient to re-allocate the bigstring but this should be small
       enough not to be a problem. *)
    let bigstring =
      Bigstring.create
        (Bin_prot.Utils.size_header_length + bin_writer_worker_output.size result)
    in
    let _w = Bigstring.write_bin_prot bigstring bin_writer_worker_output result in
    let result = Bigstring.to_string bigstring in
    Out_channel.output_string out_channel result;
    Out_channel.flush out_channel
  in
  let on_input input =
    let result =
      match input with
      | Execute { code } ->
        let result = Ocaml_toploop.toploop_eval code in
        Complete.reset ();
        Execute_output result
      | Complete { code } -> Complete_output (Complete.complete code)
    in
    send_output result
  in
  let buf = Bytes.create worker_buf_len in
  let rec loop () =
    let bytes_read = In_channel.input in_channel ~buf ~pos:0 ~len:worker_buf_len in
    Unpack_buffer.feed_bytes unpack_buffer buf ~len:bytes_read |> ok_exn;
    Unpack_buffer.unpack_iter unpack_buffer ~f:on_input |> ok_exn;
    loop ()
  in
  loop ()

let spawn () =
  let worker_in_read, worker_in_write = Unix.pipe () in
  let worker_out_read, worker_out_write = Unix.pipe () in
  let stdout_read, stdout_write = Unix.pipe () in
  let stderr_read, stderr_write = Unix.pipe () in
  (* This has to be called before starting the async scheduler. *)
  match Unix.fork () with
  | `In_the_child ->
    (ok_exn Linux_ext.pr_set_pdeathsig) Signal.term;
    List.iter [ worker_in_write; worker_out_read; stdout_read; stderr_read ] ~f:Unix.close;
    Unix.dup2 ~src:stdout_write ~dst:Unix.stdout;
    Unix.dup2 ~src:stderr_write ~dst:Unix.stderr;
    Unix.close stdout_write;
    Unix.close stderr_write;
    worker ~worker_in_read ~worker_out_write
  | `In_the_parent pid ->
    List.iter
      [ worker_in_read; worker_out_write; stdout_write; stderr_write ]
      ~f:Unix.close;
    let open Async in
    let stderr_reader =
      Fd.create Char stderr_read (Info.of_string "stderr-read") |> Reader.create
    in
    let stdout_reader =
      Fd.create Char stdout_read (Info.of_string "stdout-read") |> Reader.create
    in
    let reader =
      Fd.create Char worker_out_read (Info.of_string "worker-out-read") |> Reader.create
    in
    let writer =
      Fd.create Char worker_in_write (Info.of_string "worker-in-write") |> Writer.create
    in
    Async.upon (Reader.close_finished stdout_reader) (fun () ->
        Log.Global.error "worker process connection closed";
        Async.shutdown 1);
    { pid
    ; reader
    ; writer
    ; stdout_reader
    ; stderr_reader
    ; sequencer = Async.Sequencer.create ()
    }

let execute t ~code =
  let open Async in
  Throttle.enqueue t.sequencer (fun () ->
      Writer.write_bin_prot t.writer bin_writer_worker_input (Execute { code });
      let%map result = Reader.read_bin_prot t.reader bin_reader_worker_output in
      match result with
      | `Ok (Execute_output result) -> result
      | `Ok (Complete_output _) -> assert false
      | `Eof -> failwith "unexpected end of file from worker")

let complete t ~code =
  let open Async in
  if Async.Throttle.num_jobs_running t.sequencer = 0
  then
    Throttle.enqueue t.sequencer (fun () ->
        Writer.write_bin_prot t.writer bin_writer_worker_input (Complete { code });
        let%map result = Reader.read_bin_prot t.reader bin_reader_worker_output in
        match result with
        | `Ok (Complete_output res) -> `ok res
        | `Ok (Execute_output _) -> assert false
        | `Eof -> failwith "unexpected end of file from worker")
  else return `busy

let stdout_reader t = t.stdout_reader
let stderr_reader t = t.stderr_reader
let pid t = t.pid
