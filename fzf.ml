open Core
open Async

type t = {
  mutable items: string list
; mutable filtered_items : string list
; mutable selected_index : int
; mutable selection_list : string list
; spinner : Spinner.t
; mutable entered_text : string option
}

let create () =
  {
    items = []
  ; filtered_items = []
  ; selected_index = 0
  ; selection_list = []
  ; spinner = Spinner.create ~spin_every:(sec 0.2)
  ; entered_text = None
  }

let filter_items_and_selection t entered_text =
  let { items; filtered_items = _; selected_index = _; selection_list = _; spinner = _; entered_text = _} = t in
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
  let selection_list, filtered_items =
    match filtered_items with
    | first :: rest -> ([first], rest)
    | [] -> ([], [])
  in
  (* Reset selection index when filter changes *)
  t.selected_index <- 0;
  t.selection_list <- selection_list;
  t.filtered_items <- filtered_items;
;;

let get_selected t =
  List.nth t.selection_list t.selected_index
;;

let widget t screen =
  let open Tty_text in
  let {items = _; filtered_items; selected_index = _; selection_list = _; spinner; entered_text } = t in
  let prompt_size = 1 in
  let item_count = screen.Screen_dimensions.height - prompt_size in
  let all_but_selection =
    List.take filtered_items item_count
  in
  let editor = Widget.text ("> " ^ (Option.value ~default:"" entered_text))
  in
  let lines =
    List.map all_but_selection ~f: (fun text ->
        Widget.text text)
  in
  let selected =
    match List.nth t.selection_list t.selected_index with
    | Some sel -> Some (Widget.text sel)
    | None -> None
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
    List.init (item_count - (List.length all_but_selection))
      ~f:(fun _ -> Widget.text "")
  in Widget.vertical_group
    (padding @ lines @ (List.filter_opt [selected]) @ [prompt])
;;

let handle_input t input =
  match input with
  | Tty_text.User_input.Arrow_down -> begin
      let all_items = t.selection_list @ t.filtered_items in
      let max_index = (List.length all_items) - 1 in
      if t.selected_index < max_index then begin
        t.selected_index <- t.selected_index + 1;
        (* If we move past current selection_list, rotate items *)
        if t.selected_index >= List.length t.selection_list then begin
          match t.filtered_items with
          | next :: rest ->
            t.selection_list <- t.selection_list @ [next];
            t.filtered_items <- rest
          | [] -> ()
        end
      end;
      `Continue t
    end
  | Tty_text.User_input.Arrow_up -> begin
      if t.selected_index > 0 then begin
        t.selected_index <- t.selected_index - 1;
        (* If we move back and selection_list is too long, rotate back *)
        if List.length t.selection_list > 1 &&
           t.selected_index < List.length t.selection_list - 1 then begin
          match List.rev t.selection_list with
          | last :: rest ->
            t.selection_list <- List.rev rest;
            t.filtered_items <- last :: t.filtered_items
          | [] -> ()
        end
      end;
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

let run user_input tty_text stdin =
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
  let t = create () in
  let last_rendered : (string option * (string list)) ref = ref (None, []) in
  let how_often_to_render = (sec 0.1) in
  Render.every
    ~how_often_to_render
    ~render:(fun () ->
        if ([%compare.equal:string option * string list]
              !last_rendered (t.entered_text, t.filtered_items))
        then Deferred.unit
        else begin
          last_rendered := (t.entered_text, t.filtered_items);
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

let () =
  Command_unix.run @@
  let open Command.Let_syntax in
  Command.async ~summary:"fzf" [%map_open
    let () = return () in
    fun () ->
      let open Deferred.Let_syntax in
      let stdin = force Reader.stdin in
      match%bind
        Tty_text.with_rendering (fun (input, tty_text) ->
            run input tty_text stdin)
      with
      | None -> Deferred.unit
      | Some output ->
        let stdout = force Writer.stdout in
        Writer.write_line stdout output;
        Writer.flushed stdout
  ]
