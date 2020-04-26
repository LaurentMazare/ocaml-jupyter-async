open! Core
open! Async

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
  { shell_socket : [ `Dealer ] Zmq.Socket.t
  ; ctx : Zmq.Context.t
  }

let start () =
  let connection_file = Filename.temp_file "jupyter-ocaml" "conn" in
  let shell_port = 5556 in
  let contents =
    Config.yojson_of_t
      { Config.control_port = 5555
      ; shell_port
      ; transport = "tcp"
      ; signature_scheme = "hmac-sha256"
      ; stdin_port = 5557
      ; hb_port = 5558
      ; ip = "127.0.0.1"
      ; iopub_port = 5559
      ; key = "a0436f6c-1916-498b-8eb9-e81ab9368e84"
      }
    |> Yojson.Safe.to_string
  in
  let%bind () = Writer.save connection_file ~contents in
  don't_wait_for
    (Process.run_exn
       ~prog:"../bin/jupyter_async_kernel.bc"
       ~args:[ "-connection-file"; connection_file ]
       ()
    >>| fun s -> failwith s);
  let%bind () = after (sec 0.5) in
  let ctx = Zmq.Context.create () in
  let shell_socket = Zmq.Socket.create ctx Zmq.Socket.dealer in
  Zmq.Socket.connect shell_socket (sprintf "tcp://127.0.0.1:%d" shell_port);
  return { shell_socket; ctx }

let close t =
  Zmq.Socket.close t.shell_socket;
  Zmq.Context.terminate t.ctx

let%expect_test _ =
  let%bind t = start () in
  close t;
  print_endline "Hello, world!";
  [%expect {| Hello, world! |}]
