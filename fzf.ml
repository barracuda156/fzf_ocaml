open Core
open Async

type t = {
  mutable items: string list
; mutable filtered_items : string list
; prompt: string
; header: string option
; mutable selected_index : int
; mutable all_filtered : string list
; spinner : Spinner.t
; mutable entered_text : string option
}

let create ~prompt ~header =
  {
    items = []
  ; filtered_items = []
  ; prompt = prompt
  ; header = header
  ; selected_index = 0
  ; all_filtered = []
  ; spinner = Spinner.create ~spin_every:(sec 0.2)
  ; entered_text = None
  }

let filter_items_and_selection t entered_text =
  let { items; filtered_items = _; prompt = _; header = _; selected_index = _; all_filtered = _; spinner = _; entered_text = _} = t in
  t.entered_text <- entered_text;
  let filtered_items =
    match entered_text with
    | None -> items
    | Some text ->
      let pattern = String.Search_pattern.create text in
      items
      |> List.filter ~f:(fun item ->
          Option.is_some @@ String.Search_pattern.index ~in_:item pattern)
  in
  (* Reset selection to LAST item (appears at TOP after display reversal) *)
  let max_idx = List.length filtered_items - 1 in
  t.selected_index <- if max_idx >= 0 then max_idx else 0;
  t.all_filtered <- filtered_items;
  t.filtered_items <- filtered_items
;;

let get_selected t =
  List.nth t.all_filtered t.selected_index
;;

let widget t screen =
  let open Tty_text in
  let {items = _; filtered_items = _; prompt; header = _; selected_index; all_filtered; spinner; entered_text } = t in
  let prompt_size = 1 in
  let item_count = screen.Screen_dimensions.height - prompt_size in

  (* Calculate visible window *)
  let visible_start = max 0 (selected_index - item_count + 1) in
  let visible_items =
    all_filtered
    |> (fun lst -> List.drop lst visible_start)
    |> (fun lst -> List.take lst (item_count + 1))
    |> List.rev  (* Reverse for display - last item at top *)
  in
  let selected_in_window = (List.length visible_items - 1) - (selected_index - visible_start) in
  let editor = Widget.text (prompt ^ (Option.value ~default:"" entered_text))
  in
  let lines =
    List.mapi visible_items ~f:(fun i text ->
      if i = selected_in_window then
        Widget.text ("> " ^ text)
      else
        Widget.text ("  " ^ text)
    )
  in
  let spinner = Option.map
      (Spinner.to_char spinner)
      ~f:(fun c -> Widget.text (String.of_char_list [c]))
  in
  let prompt =
    [spinner; Some editor]
    |> List.filter_opt
    |> Widget.horizontal_group
  in
  let padding =
    List.init (max 0 (item_count + 1 - List.length visible_items))
      ~f:(fun _ -> Widget.text "")
  in
  Widget.vertical_group
    (padding @ lines @ [prompt])
;;

let handle_input t input =
  match input with
  | Tty_text.User_input.Arrow_down -> begin
      if t.selected_index > 0 then
        t.selected_index <- t.selected_index - 1;
      `Continue t
    end
  | Tty_text.User_input.Arrow_up -> begin
      let max_index = List.length t.all_filtered - 1 in
      if t.selected_index < max_index then
        t.selected_index <- t.selected_index + 1;
      `Continue t
    end
  | Tty_text.User_input.Backspace -> begin
      match t.entered_text with
      | None -> `Continue t
      | Some text ->
        let new_entered_text =
          if (Int.(=) (String.length text) 1)
          then None
          else Some (String.sub text ~pos:0 ~len:(String.length text - 1))
        in filter_items_and_selection t new_entered_text;
        `Continue t
    end
  | Ctrl_c -> `Finished None
  | Char x -> begin
      let text = match t.entered_text with
        | None -> String.of_char_list [x]
        | Some text -> String.(text ^ (of_char_list [x]))
      in
      filter_items_and_selection t (Some text);
      `Continue t
    end
  | Return -> (`Finished (get_selected t))
  | Escape -> (`Finished None)
;;

let run user_input tty_text stdin ~prompt ~header =
  let stdin_reader =
    Pipe.map ~f:(fun x -> `Stdin x) (Reader.lines stdin)
  in
  let stdin_closed =  Pipe.create_reader ~close_on_exception:true (fun w ->
      let%bind _ = Reader.close_finished stdin in
      Pipe.write w `Stdin_closed)
  in
  let interleaved = Pipe.interleave
      [ stdin_reader
      ; Pipe.map user_input ~f:(fun x -> `Input x)
      ; stdin_closed]
  in
  let t = create ~prompt ~header in
  let last_rendered : (string option * string list * int) ref = ref (None, [], 0) in
  let how_often_to_render = (sec 0.1) in
  Render.every
    ~how_often_to_render
    ~render:(fun () ->
        if ([%compare.equal:string option * string list * int]
              !last_rendered (t.entered_text, t.all_filtered, t.selected_index))
        then Deferred.unit
        else begin
          last_rendered := (t.entered_text, t.all_filtered, t.selected_index);
          Tty_text.render tty_text (widget t (Tty_text.screen_dimensions tty_text))
        end)
    (fun () ->
       match%bind Pipe.read interleaved with
       | `Eof -> raise_s [%message "impossible?"]
       | `Ok `Stdin_closed ->
         Spinner.finish t.spinner;
         return (`Repeat ());
       | `Ok `Stdin x ->
         t.items <- x :: t.items;
         filter_items_and_selection t t.entered_text;
         return (`Repeat ());
       | `Ok `Input user_input ->
         match handle_input t user_input with
         | `Finished None ->
           return (`Finished None)
         | `Finished (Some x) ->
           return (`Finished (Some x))
         | `Continue _ ->
           return (`Repeat ())
    )
;;

(* Convert --flag=value to --flag value for Command library *)
let preprocess_argv () =
  let argv = Array.to_list Stdlib.Sys.argv in
  let rec process acc = function
    | [] -> List.rev acc
    | arg :: rest ->
        (* Match --flag=value pattern *)
        match String.lsplit2 arg ~on:'=' with
        | Some (flag, value) when String.is_prefix flag ~prefix:"--" ->
            (* Split into two separate args: --flag value *)
            process (value :: flag :: acc) rest
        | _ ->
            (* Keep as-is (not a --flag=value pattern) *)
            process (arg :: acc) rest
  in
  Array.of_list (process [] argv)
;;

let () =
  (* Preprocess argv to handle --flag=value *)
  let transformed_argv = preprocess_argv () in
  Command_unix.run
    ~argv:(Array.to_list transformed_argv)
  @@
  let open Command.Let_syntax in
  Command.async ~summary:"fzf" [%map_open
    let prompt = flag "--prompt" (optional_with_default "> " string)
        ~doc:"STRING Prompt string (default: '> ')"
    and header = flag "--header" (optional string)
        ~doc:"STRING Header string"
    and _preview = flag "--preview" (optional string)
        ~doc:"STRING Preview command (not supported, ignored)"
    in
    fun () ->
      let open Deferred.Let_syntax in
      let stdin = force Reader.stdin in
      match%bind
        Tty_text.with_rendering (fun (input, tty_text) ->
            run input tty_text stdin ~prompt ~header)
      with
      | None -> Deferred.unit
      | Some output ->
        let stdout = force Writer.stdout in
        Writer.write_line stdout output;
        Writer.flushed stdout
  ]
