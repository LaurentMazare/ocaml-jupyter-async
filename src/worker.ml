open! Core

(* The worker part does not use async, only import it locally for the parent
   process.  *)

let worker_buf_len = 4096

type worker_input = { code : string } [@@deriving bin_io]
type worker_output = unit Or_error.t [@@deriving bin_io]

type t =
  { pid : Pid.t
  ; reader : Reader.t
  ; writer : Writer.t
  }

let worker ~worker_in_read ~worker_out_write =
  let unpack_buffer =
    Unpack_buffer.Unpack_one.create_bin_prot bin_reader_worker_input
    |> Unpack_buffer.create
  in
  let in_channel = Core.Unix.in_channel_of_descr worker_in_read in
  let out_channel = Core.Unix.out_channel_of_descr worker_out_write in
  let on_input { code } =
    let result = Ocaml_toploop.toploop_eval code ~verbose:true in
    let result = Bin_prot.Writer.to_string bin_writer_worker_output result in
    Out_channel.output_value out_channel result
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
  let worker_in_read, worker_in_write = Core.Unix.pipe () in
  let worker_out_read, worker_out_write = Core.Unix.pipe () in
  (* This has to be called before starting the async scheduler. *)
  match Core.Unix.fork () with
  | `In_the_child -> worker ~worker_in_read ~worker_out_write
  | `In_the_parent pid ->
    let open Async in
    let worker_out_read =
      Fd.create Char worker_out_read (Info.of_string "worker-out-read")
    in
    let worker_in_write =
      Fd.create Char worker_in_write (Info.of_string "worker-in-write")
    in
    let reader = Reader.create worker_out_read in
    let writer = Writer.create worker_in_write in
    { pid; reader; writer }

let execute t ~code =
  let open Async in
  Writer.write_bin_prot t.writer bin_writer_worker_input { code };
  let%map result = Reader.read_bin_prot t.reader bin_reader_worker_output in
  match result with
  | `Ok result -> result
  | `Eof -> failwith "unexpected end of file from worker"
