open Tsdl

let unwrap msg = function
  | Error (`Msg e) ->
      Sdl.log "%s: %s" msg e;
      exit 1
  | Ok v -> v

let run = ref true

let main () =
  Sdl.init Sdl.Init.(video + events + audio) |> unwrap "Init error";
  let _tsdl_image = Tsdl_image.Image.(init Init.png) in
  let w = 240 and h = 200 in
  let window =
    Sdl.create_window ~w ~h "SDL OpenGL" Sdl.Window.opengl
    |> unwrap "Create window error"
  in
  Sdl.set_window_bordered window false;
  Sdl.set_window_title window "ocamlrun";
  let renderer = Sdl.create_renderer window |> unwrap "Create renderer" in
  let texture =
    Tsdl_image.Image.load_texture renderer "sprites.png"
    |> unwrap "Couldn't load image"
  in
  let frames = 20 in
  let quads =
    Array.init frames (fun i -> Sdl.Rect.create ~x:(w * i) ~y:0 ~w ~h)
  in
  let i = ref 0 in
  let event = Sdl.Event.create () in

  let audio_spec =
    Sdl.
      {
        as_freq = 0;
        as_format = Sdl.Audio.f32;
        as_channels = 2;
        as_silence = 0;
        as_samples = 0;
        as_size = 0l;
        as_callback = None;
      }
  in

  let rw_ops =
    Sdl.rw_from_file "ocaml_commercial_jingle.wav" "r" |> unwrap "rw_from_file"
  in
  let audio_spec, data =
    Sdl.load_wav_rw rw_ops audio_spec Bigarray.Float32 |> unwrap "load_wav_rw"
  in

  let id, _audio_spec =
    Sdl.open_audio_device None false audio_spec Sdl.Audio.allow_any_change
    |> unwrap "audio_device"
  in

  Sdl.queue_audio id data |> unwrap "queue audio";
  Sdl.pause_audio_device id false;

  (* let start = Sdl.get_performance_counter () |> Int64.to_float in *)
  while Sdl.get_queued_audio_size id > 0 && !run do
    if Sdl.poll_event (Some event) && Sdl.Event.(get event typ = quit) then
      run := false;
    Sdl.render_clear renderer |> unwrap "Render clear";
    Sdl.set_render_draw_color renderer 255 255 255 255
    |> unwrap "Render draw color";
    Sdl.render_copy ~src:quads.(!i) renderer texture |> unwrap "Render copy";
    incr i;
    if !i >= frames then i := 0;
    Sdl.render_present renderer;
    Sdl.delay 35l
    (* let finish = Sdl.get_performance_counter () |> Int64.to_float in *)
    (* let elapsed = (finish -. start) /. (Sdl.get_performance_frequency () |> Int64.to_float) in *)
    (* if elapsed >= 8.0 then *)
    (*   run := false *)
  done;

  Array.set Sys.argv 0 "ocamlrun";
  let ocamlrun =
    Unix.create_process "ocamlrun" Sys.argv Unix.stdin Unix.stdout Unix.stderr
  in
  let status =
    match Unix.waitpid [] ocamlrun with
    | _, Unix.WEXITED status -> status
    | _, Unix.WSIGNALED _ -> 1
    | _, Unix.WSTOPPED _ -> 1
  in

  Tsdl_image.Image.quit ();
  Sdl.destroy_window window;
  Sdl.quit ();
  exit status

let () = main ()
