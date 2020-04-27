open! Core
open! Async
module Msg = Jupyter_async_message.Message

module Config = struct
  type t =
    { control_port : int
    ; shell_port : int
    ; transport : string
    ; signature_scheme : string
    ; stdin_port : int
    ; hb_port : int
    ; ip : string
    ; iopub_port : int
    ; key : string
    }
  [@@deriving yojson, sexp]
end

type t =
  { shell_socket : [ `Dealer ] Zmq_async.Socket.t
  ; iopub_socket : [ `Sub ] Zmq_async.Socket.t
  ; ctx : Zmq.Context.t
  ; key : string
  ; process : Process.t
  }

let start () =
  let connection_file = Filename.temp_file "jupyter-ocaml" "conn" in
  let key = "a0436f6c-1916-498b-8eb9-e81ab9368e84" in
  let config =
    { Config.control_port = 5555
    ; shell_port = 5556
    ; transport = "tcp"
    ; signature_scheme = "hmac-sha256"
    ; stdin_port = 5557
    ; hb_port = 5558
    ; ip = "127.0.0.1"
    ; iopub_port = 5559
    ; key
    }
  in
  let contents = Config.yojson_of_t config |> Yojson.Safe.to_string in
  let%bind () = Writer.save connection_file ~contents in
  let%bind process =
    Process.create_exn
      ~prog:"../bin/jupyter_async_kernel.bc"
      ~args:[ "-connection-file"; connection_file ]
      ()
  in
  let%bind () = after (sec 0.5) in
  let ctx = Zmq.Context.create () in
  let shell_socket = Zmq.Socket.create ctx Zmq.Socket.dealer in
  Zmq.Socket.connect shell_socket (sprintf "tcp://127.0.0.1:%d" config.shell_port);
  let shell_socket = Zmq_async.Socket.of_socket shell_socket in
  let iopub_socket = Zmq.Socket.create ctx Zmq.Socket.sub in
  Zmq.Socket.connect iopub_socket (sprintf "tcp://127.0.0.1:%d" config.iopub_port);
  (* Subscribe to all topics. *)
  Zmq.Socket.subscribe iopub_socket "";
  let iopub_socket = Zmq_async.Socket.of_socket iopub_socket in
  return { shell_socket; iopub_socket; ctx; key; process }

let close t =
  Signal.send_exn Signal.term (`Pid (Process.pid t.process));
  let%bind () = Zmq_async.Socket.close t.shell_socket in
  let%bind () = Zmq_async.Socket.close t.iopub_socket in
  Zmq.Context.terminate t.ctx;
  Deferred.unit

let receive_and_print t which =
  let%bind msg =
    match which with
    | `iopub -> Msg.read t.iopub_socket ~key:t.key
    | `shell -> Msg.read t.shell_socket ~key:t.key
  in
  let content = Msg.content msg in
  print_endline (Msg.Content.sexp_of_t content |> Sexp.to_string);
  Deferred.unit

let%expect_test _ =
  let%bind t = start () in
  let msg = Msg.For_testing.execute_request ~code:"1 + 1;;" in
  let%bind () = Msg.send msg t.shell_socket ~key:t.key in
  let%bind () = receive_and_print t `shell in
  let%bind () = receive_and_print t `iopub in
  let%bind () = receive_and_print t `iopub in
  let%bind () = receive_and_print t `iopub in
  let%bind () = close t in
  [%expect
    {|
      (Unsupported(msg_type execute_reply)(content"{\"status\":\"ok\",\"execution_count\":1,\"user_expressions\":{}}"))
      (Unsupported(msg_type status)(content"{\"execution_state\":\"busy\"}"))
      (Unsupported(msg_type stream)(content"{\"name\":\"stdout\",\"text\":\"- : int = 2\\n\"}"))
      (Unsupported(msg_type status)(content"{\"execution_state\":\"idle\"}")) |}]
