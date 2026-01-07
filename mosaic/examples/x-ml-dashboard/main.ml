open Mosaic

(* Types *)

type training_state = Running | Paused

type model = {
  state : training_state;
  step : int;
  epoch : int;
  accuracy : float;
  elapsed_time : float;            
  dataset : string;
  model_name : string;
}

type msg =
  | Tick of float
  | TogglePause
  | Quit

(* Constants*)

(* Color Palette *)
let header_bg = Ansi.Color.of_rgb 30 80 100
let header_row1_bg = Ansi.Color.of_rgb 25 70 90  
let header_row2_bg = Ansi.Color.of_rgb 35 90 110 
let footer_bg = Ansi.Color.grayscale ~level:3
let muted = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:16) ()
let hint = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:14) ()

(* Metric Colors *)
let step_color = Ansi.Color.cyan
let epoch_color = Ansi.Color.cyan
let accuracy_color = Ansi.Color.green
let title_color = Ansi.Color.white
let label_color = Ansi.Color.grayscale ~level:14


(* ========== Helper Functions ========== *)

let format_time seconds =
  let total_seconds = int_of_float seconds in
  let hours = total_seconds / 3600 in
  let minutes = (total_seconds mod 3600) / 60 in
  let secs = total_seconds mod 60 in
  if hours > 0 then
    Printf.sprintf "%02d:%02d:%02d" hours minutes secs
  else
    Printf.sprintf "%02d:%02d" minutes secs

let pill ~label ~color =
  box ~padding:(padding 1) ~background:color
    [
      text ~style:(Ansi.Style.make ~bold:true ~fg:Ansi.Color.white ()) label;
    ]

(* View Components *)

let view_header m =
  let status_label, status_color =
    match m.state with
    | Running -> ("LIVE", Ansi.Color.green)
    | Paused -> ("PAUSED", Ansi.Color.yellow)
  in
  box ~padding:(padding 0) ~background:header_bg
    ~size:{ width = pct 100; height = auto }
    [
      box ~flex_direction:Column ~gap:(gap 0)
        ~size:{ width = pct 100; height = auto }
        [
          (* First Row: Run name + dataset + model - Centered *)
          box ~padding:(padding 1) ~background:header_row1_bg
            ~flex_direction:Row ~justify_content:Center ~align_items:Center
            ~size:{ width = pct 100; height = auto }
            ~gap:(gap 2)
            [
              text ~style:(Ansi.Style.make ~bold:true ~fg:title_color ()) "MNIST Training";
              text ~style:(Ansi.Style.make ~fg:label_color ()) "|";
              text ~style:(Ansi.Style.make ~fg:label_color ()) (Printf.sprintf "Dataset: %s" m.dataset);
              text ~style:(Ansi.Style.make ~fg:label_color ()) "|";
              text ~style:(Ansi.Style.make ~fg:label_color ()) (Printf.sprintf "Model: %s" m.model_name);
            ];
          
          (* Second Row: Metrics + Status - All centered (thinner: minimal padding) *)
          box ~padding:(padding 0) ~background:header_row2_bg
            ~flex_direction:Row ~justify_content:Center ~align_items:Center
            ~size:{ width = pct 100; height = auto }
            ~gap:(gap 3)
            [
              (* Metrics: Step, Epoch, Accuracy *)
              box ~flex_direction:Row ~gap:(gap 2) ~align_items:Center
                [
                  box ~flex_direction:Row ~gap:(gap 1) ~align_items:Center
                    [
                      text ~style:(Ansi.Style.make ~fg:label_color ()) "Step:";
                      text
                        ~style:(Ansi.Style.make ~bold:true ~fg:step_color ())
                        (Printf.sprintf "%d" m.step);
                    ];
                  text ~style:(Ansi.Style.make ~fg:label_color ()) "|";
                  box ~flex_direction:Row ~gap:(gap 1) ~align_items:Center
                    [
                      text ~style:(Ansi.Style.make ~fg:label_color ()) "Epoch:";
                      text
                        ~style:(Ansi.Style.make ~bold:true ~fg:epoch_color ())
                        (Printf.sprintf "%d" m.epoch);
                    ];
                  text ~style:(Ansi.Style.make ~fg:label_color ()) "|";
                  box ~flex_direction:Row ~gap:(gap 1) ~align_items:Center
                    [
                      text ~style:(Ansi.Style.make ~fg:label_color ()) "Accuracy:";
                      text
                        ~style:(Ansi.Style.make ~bold:true ~fg:accuracy_color ())
                        (Printf.sprintf "%.1f%%" m.accuracy);
                    ];
                ];
              
              (* Elapsed time + Status badge *)
              box ~flex_direction:Row ~gap:(gap 2) ~align_items:Center
                [
                  text ~style:(Ansi.Style.make ~fg:label_color ()) 
                    (Printf.sprintf "Elapsed: %s" (format_time m.elapsed_time));
                  pill ~label:status_label ~color:status_color;
                ];
            ];
        ];
    ]

let view_footer () =
  box ~padding:(padding 1) ~background:footer_bg
    [
      text ~style:hint "[Space] pause/resume  •  [Q] quit";
    ]

let view m =
  box ~flex_direction:Column
    ~size:{ width = pct 100; height = pct 100 }
    [
   
      box ~flex_grow:1. ~padding:(padding 1)
        [
          text ~style:muted "Section 2: Metrics Overview Panel (coming soon)";
        ];
      view_footer ();
    ]

(* TEA Core *)

let init () =
  ( {
      state = Running;
      step = 0;
      epoch = 0;
      accuracy = 50.0;
      elapsed_time = 0.0;
      dataset = "MNIST";
      model_name = "Simple CNN";
    },
    Cmd.none )

let update msg m =
  match msg with
  | Tick dt ->
      (* For now, just update elapsed time when running *)
      let new_elapsed =
        if m.state = Running then m.elapsed_time +. dt else m.elapsed_time
      in
      ({ m with elapsed_time = new_elapsed }, Cmd.none)
  | TogglePause -> (
      match m.state with
      | Running -> ({ m with state = Paused }, Cmd.none)
      | Paused -> ({ m with state = Running }, Cmd.none))
  | Quit -> (m, Cmd.quit)

let subscriptions _model =
  Sub.batch
    [
      Sub.on_tick (fun ~dt -> Tick dt);
      Sub.on_key (fun ev ->
          match (Mosaic_ui.Event.Key.data ev).key with
          | Char c when Uchar.equal c (Uchar.of_char ' ') -> Some TogglePause
          | Char c when Uchar.equal c (Uchar.of_char 'q') -> Some Quit
          | Char c when Uchar.equal c (Uchar.of_char 'Q') -> Some Quit
          | Escape -> Some Quit
          | _ -> None);
    ]

let () = run { init; update; view; subscriptions }

