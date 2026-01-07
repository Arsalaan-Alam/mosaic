open Mosaic
module Charts = Matrix_charts

(* Types *)

type training_state = Running | Paused

type point = { x : float; y : float }

type model = {
  state : training_state;
  step : int;
  epoch : int;
  accuracy : float;
  elapsed_time : float;
  dataset : string;
  model_name : string;
  (* Loss history for charts *)
  train_loss_history : point list;  (* newest first *)
  val_loss_history : point list;    (* newest first *)
  last_update_time : float;         (* for FPS optimization *)
}

type msg =
  | Tick of float
  | TogglePause
  | Quit

(* Constants *)

(* Chart Configuration *)
let max_loss_points = 500  (* Keep last 500 points for chart *)
let update_interval = 0.1  (* Update metrics every 0.1 seconds for smooth FPS *)
let steps_per_epoch = 600   (* Steps per epoch for epoch calculation *)

(* Color Palette *)
let header_bg = Ansi.Color.of_rgb 30 80 100
let header_row1_bg = Ansi.Color.of_rgb 25 70 90  
let header_row2_bg = Ansi.Color.of_rgb 35 90 110 
let footer_bg = Ansi.Color.grayscale ~level:3
let hint = Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:14) ()

(* Metric Colors *)
let step_color = Ansi.Color.cyan
let epoch_color = Ansi.Color.cyan
let accuracy_color = Ansi.Color.green
let title_color = Ansi.Color.white
let label_color = Ansi.Color.grayscale ~level:14

(* Chart Colors *)
let train_loss_color = Ansi.Color.cyan
let val_loss_color = Ansi.Color.magenta


(* ========== Helper Functions ========== *)

(* Deterministic noise function for realistic variation *)
let frac x = x -. Float.floor x

let noise seed step =
  frac (Float.sin (Float.of_int (step + (seed * 7919)) *. 12.9898) *. 43758.5453)

(* Clamp value between bounds *)
let clamp lo hi x = if x < lo then lo else if x > hi then hi else x

(* Take first n elements from list *)
let rec take n xs =
  if n <= 0 then []
  else match xs with [] -> [] | x :: tl -> x :: take (n - 1) tl

(* Synthetic loss generation using exponential decay + sin/cosine *)
let train_loss_of step =
  let s = Float.of_int step in
  (* Exponential decay base with sinusoidal oscillation *)
  let base = (2.4 *. exp (-.s /. 220.0)) +. (0.08 *. sin (s /. 7.0)) in
  (* Add noise for realism *)
  let n = (noise 42 step -. 0.5) *. 0.16 in
  max 0.03 (base +. n)

let val_loss_of step =
  let s = Float.of_int step in
  (* Validation loss decays slower and has more noise *)
  let base = (2.7 *. exp (-.s /. 250.0)) +. (0.10 *. sin (s /. 9.0)) in
  let n = (noise 43 step -. 0.5) *. 0.18 in
  (* Validation loss is typically slightly higher than train loss *)
  max 0.03 (base +. 0.15 +. n)

(* Synthetic accuracy generation *)
let accuracy_of step =
  let s = Float.of_int step in
  clamp 0.0 100.0
    (18.0
    +. (82.0 *. (1.0 -. exp (-.s /. 260.0)))
    +. ((noise 44 step -. 0.5) *. 6.0)
    +. (2.0 *. sin (s /. 13.0)))

(* Generate confusion matrix data (10x10 for MNIST) *)
let confusion_matrix_data step =
  let n = 10 in  (* MNIST has 10 classes *)
  let accuracy_factor = accuracy_of step /. 100.0 in  (* Use current accuracy to influence diagonal strength *)
  Array.concat
    (List.map Array.of_list
       (List.init n (fun y ->
            List.init n (fun x ->
                (* Diagonal elements (correct predictions) should be high *)
                (* Off-diagonal elements (misclassifications) should be low *)
                let base = if x = y then (0.7 +. (accuracy_factor *. 0.25)) else (0.05 *. (1.0 -. accuracy_factor)) in
                let wobble =
                  (noise (45 + x + (y * 17)) (step + (x * 19) + (y * 23)) -. 0.5)
                  *. 0.15
                in
                let v = clamp 0.0 1.0 (base +. wobble) in
                (Float.of_int x, Float.of_int y, v)))))

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

(* Chart Drawing Functions *)

let line_style color =
  Ansi.Style.make ~fg:color ()

let axis_style =
  Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:12) ~dim:true ()

let grid_style =
  Ansi.Style.make ~fg:(Ansi.Color.grayscale ~level:6) ~dim:true ()

(* Draw loss curves chart *)
let draw_loss_chart m grid ~width ~height =
  (* Get recent points, reverse to oldest-first for chart *)
  let train_loss =
    List.rev (take max_loss_points m.train_loss_history)
    |> Array.of_list
  in
  let val_loss =
    List.rev (take max_loss_points m.val_loss_history)
    |> Array.of_list
  in
  
  (* Only draw if we have data *)
  if Array.length train_loss > 0 && Array.length val_loss > 0 then
    let chart =
      Charts.empty ()
      |> Charts.with_frame (Charts.manual_frame ~margins:(0, 1, 1, 4) ())
      |> Charts.with_axes
           ~x:
             (Charts.Axis.default |> Charts.Axis.with_ticks 5
             |> Charts.Axis.with_style axis_style)
           ~y:
             (Charts.Axis.default |> Charts.Axis.with_ticks 4
             |> Charts.Axis.with_style axis_style)
      |> Charts.with_grid
           (Charts.Gridlines.default
           |> Charts.Gridlines.with_style grid_style
           |> Charts.Gridlines.with_x true
           |> Charts.Gridlines.with_y true)
      |> Charts.line ~resolution:`Braille2x4
           ~style:(line_style train_loss_color)
           ~x:(fun p -> p.x)
           ~y:(fun p -> p.y)
           train_loss
      |> Charts.line ~resolution:`Braille2x4
           ~style:(line_style val_loss_color)
           ~x:(fun p -> p.x)
           ~y:(fun p -> p.y)
           val_loss
    in
    ignore (Charts.draw chart grid ~width ~height)

(* Draw confusion matrix heatmap *)
let draw_confusion_matrix m grid ~width ~height =
  let pts = confusion_matrix_data m.step in
  let chart =
    Charts.empty ()
    |> Charts.with_frame (Charts.manual_frame ~margins:(0, 0, 1, 2) ())
    |> Charts.with_axes
         ~x:
           (Charts.Axis.default |> Charts.Axis.with_ticks 10
           |> Charts.Axis.with_style axis_style
           |> Charts.Axis.with_format (fun _ v ->
               Printf.sprintf "%d" (int_of_float v)))
         ~y:
           (Charts.Axis.default |> Charts.Axis.with_ticks 10
           |> Charts.Axis.with_style axis_style
           |> Charts.Axis.with_format (fun _ v ->
               Printf.sprintf "%d" (int_of_float v)))
    |> Charts.heatmap ~auto_value_range:true ~agg:`Avg
         ~x:(fun (x, _, _) -> x)
         ~y:(fun (_, y, _) -> y)
         ~value:(fun (_, _, v) -> v)
         pts
  in
  ignore (Charts.draw chart grid ~width ~height)

let view_footer () =
  box ~padding:(padding 1) ~background:footer_bg
    [
      text ~style:hint "[Space] pause/resume  •  [Q] quit";
    ]

let view_loss_chart m =
  box ~border:true ~title:"Loss Curves" ~padding:(padding 1)
    ~size:{ width = px 50; height = pct 50 }
    [
      canvas
        ~draw:(fun grid ~width ~height -> draw_loss_chart m grid ~width ~height)
        ~size:{ width = pct 100; height = pct 100 }
        ();
    ]

let view_confusion_matrix m =
  box ~border:true ~title:"Confusion Matrix (Validation)" ~padding:(padding 1)
    ~size:{ width = px 50; height = pct 50 }
    [
      canvas
        ~draw:(fun grid ~width ~height -> draw_confusion_matrix m grid ~width ~height)
        ~size:{ width = pct 100; height = pct 100 }
        ();
    ]

let view m =
  box ~flex_direction:Column
    ~size:{ width = pct 100; height = pct 100 }
    [
      view_header m;
      box ~flex_grow:1. ~padding:(padding 1) ~flex_direction:Row ~justify_content:Center ~gap:(gap 2)
        [
          view_loss_chart m;
          view_confusion_matrix m;
        ];
      view_footer ();
    ]

(* TEA Core *)

let init () =
  let initial_step = 0 in
  let initial_train_loss = train_loss_of initial_step in
  let initial_val_loss = val_loss_of initial_step in
  ( {
      state = Running;
      step = initial_step;
      epoch = 0;
      accuracy = accuracy_of initial_step;
      elapsed_time = 0.0;
      dataset = "MNIST";
      model_name = "Simple CNN";
      train_loss_history = [ { x = Float.of_int initial_step; y = initial_train_loss } ];
      val_loss_history = [ { x = Float.of_int initial_step; y = initial_val_loss } ];
      last_update_time = 0.0;
    },
    Cmd.none )

let update msg m =
  match msg with
  | Tick dt ->
      let new_elapsed =
        if m.state = Running then m.elapsed_time +. dt else m.elapsed_time
      in
      let new_update_time = m.last_update_time +. dt in
      
      (* Update metrics at optimized interval for smooth FPS *)
      if m.state = Running && new_update_time >= update_interval then
        let new_step = m.step + 1 in
        let new_epoch = new_step / steps_per_epoch in
        let new_accuracy = accuracy_of new_step in
        let new_train_loss = train_loss_of new_step in
        let new_val_loss = val_loss_of new_step in
        
        (* Add new points to history (newest first) *)
        let new_train_point = { x = Float.of_int new_step; y = new_train_loss } in
        let new_val_point = { x = Float.of_int new_step; y = new_val_loss } in
        
        let new_train_history = take max_loss_points (new_train_point :: m.train_loss_history) in
        let new_val_history = take max_loss_points (new_val_point :: m.val_loss_history) in
        
        ( {
            m with
            step = new_step;
            epoch = new_epoch;
            accuracy = new_accuracy;
            elapsed_time = new_elapsed;
            train_loss_history = new_train_history;
            val_loss_history = new_val_history;
            last_update_time = 0.0;  (* Reset update timer *)
          },
          Cmd.none )
      else
        (* Just update elapsed time, don't update metrics yet *)
        ({ m with elapsed_time = new_elapsed; last_update_time = new_update_time }, Cmd.none)
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

