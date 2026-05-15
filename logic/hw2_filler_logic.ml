open! Core

module Color = struct
  type t = Red | Blue | Green | Yellow | Purple | Orange
  [@@deriving sexp, compare, equal, enumerate]

  let all : t list = all
end

module Player = struct
  type t = P1 | P2 [@@deriving sexp, compare, equal]

  let opposite = function P1 -> P2 | P2 -> P1
end

module Board = struct
  type t = Color.t array array [@@deriving sexp]

  let rows b = Array.length b
  let cols b = if rows b = 0 then 0 else Array.length b.(0)

  let in_bounds b (r, c) =
    0 <= r && r < rows b && 0 <= c && c < cols b

  let neighbors (r, c) = [ r - 1, c; r + 1, c; r, c - 1; r, c + 1 ]

  let copy (b : t) : t =
    Array.init (rows b) ~f:(fun r -> Array.copy b.(r))
end

module Territory = struct
  type t = bool array array

  let create ~rows ~cols : t =
    Array.init rows ~f:(fun _ -> Array.create ~len:cols false)

  let copy (m : t) : t =
    Array.init (Array.length m) ~f:(fun r -> Array.copy m.(r))

  let count (m : t) : int =
    Array.fold m ~init:0 ~f:(fun acc row ->
      acc + Array.count row ~f:Fn.id)
end

module Decision = struct
  type t =
    | In_progress of { whose_turn : Player.t }
    | Winner of Player.t
    | Tie
  [@@deriving sexp, compare, equal]

  let is_game_over = function Winner _ | Tie -> true | In_progress _ -> false
end

module Move = struct
  type t = Color.t [@@deriving sexp, compare, equal]
end

module Game_state = struct
  type t =
    { board : Board.t
    ; p1 : Territory.t
    ; p2 : Territory.t
    ; decision : Decision.t
    ; last_move : Move.t option
    }
  [@@deriving sexp]

  module Create_error = struct
    type t = Board_must_be_rectangular [@@deriving sexp, compare]
  end

  let%private assert_rectangular (b : Board.t) =
    let r = Board.rows b in
    let c = Board.cols b in
    r = 0
    || Array.for_all b ~f:(fun row -> Array.length row = c)

  (* Seed corners and expand to their initial same-color regions (without an opponent). *)
  let%private initial_region_from_anchor (b : Board.t) ((r0, c0) : int * int) : Territory.t =
    let rows, cols = Board.rows b, Board.cols b in
    let own = Territory.create ~rows ~cols in
    if rows = 0 || cols = 0 then own
    else
      let target = b.(r0).(c0) in
      let rec dfs stack =
        match stack with
        | [] -> ()
        | (r, c) :: tl ->
          if Board.in_bounds b (r, c)
             && not own.(r).(c)
             && Color.equal b.(r).(c) target
          then (
            own.(r).(c) <- true;
            dfs (Board.neighbors (r, c) @ tl)
          )
          else dfs tl
      in
      own.(r0).(c0) <- true;
      dfs (Board.neighbors (r0, c0));
      own

  let create (board : Board.t) : (t, Create_error.t list) Result.t =
    if not (assert_rectangular board) then Error [ Create_error.Board_must_be_rectangular ]
    else
      let p1 = initial_region_from_anchor board (0, 0) in
      let p2 = initial_region_from_anchor board (Board.rows board - 1, Board.cols board - 1) in
      Ok
        { board
        ; p1
        ; p2
        ; decision = In_progress { whose_turn = Player.P1 }
        ; last_move = None
        }

  module Move_error = struct
    type t =
      | Game_is_over
      | Illegal_color (* e.g., current player color or opponent's last color, if you enforce it *)
    [@@deriving sexp, compare]
  end

  let%private owned_mask (t : t) (p : Player.t) = match p with P1 -> t.p1 | P2 -> t.p2
  let%private opp_mask   (t : t) (p : Player.t) = match p with P1 -> t.p2 | P2 -> t.p1

  let%private sample_owned_color (b : Board.t) (own : Territory.t) : Color.t option =
    let rows, cols = Board.rows b, Board.cols b in
    let rec scan r c =
      if r = rows then None
      else if c = cols then scan (r + 1) 0
      else if own.(r).(c) then Some b.(r).(c)
      else scan r (c + 1)
    in
    scan 0 0

  (* Expand current player's territory to chosen color, without crossing the opponent. *)
  let%private expand (b : Board.t) ~(mine : Territory.t) ~(theirs : Territory.t) ~(chosen : Color.t) =
    let rows, cols = Board.rows b, Board.cols b in
    (* recolor owned cells and seed frontier *)
    let frontier =
      let acc = ref [] in
      for r = 0 to rows - 1 do
        for c = 0 to cols - 1 do
          if mine.(r).(c) then (b.(r).(c) <- chosen; acc := (r, c) :: !acc)
        done
      done;
      !acc
    in
    let rec dfs stack =
      match stack with
      | [] -> ()
      | (r, c) :: tl ->
        let tl =
          List.fold (Board.neighbors (r, c)) ~init:tl ~f:(fun tl (nr, nc) ->
            if Board.in_bounds b (nr, nc)
               && not theirs.(nr).(nc)
               && not mine.(nr).(nc)
               && Color.equal b.(nr).(nc) chosen
            then (
              mine.(nr).(nc) <- true;
              b.(nr).(nc) <- chosen;
              (nr, nc) :: tl
            ) else tl)
        in
        dfs tl
    in
    dfs frontier

  let get_legal_moves (t : t) : Move.t list =
    match t.decision with
    | Winner _ | Tie -> []
    | In_progress { whose_turn } ->
      let own = owned_mask t whose_turn in
      let opp = owned_mask t (Player.opposite whose_turn) in
      let current_color =
        Option.value_exn (sample_owned_color t.board own)
      in
      (* Classic GamePigeon rules typically forbid: pick your current color OR the opponent's current color. *)
      let opp_color = sample_owned_color t.board opp in
      Color.all
      |> List.filter ~f:(fun c ->
        not (Color.equal c current_color)
        && Option.for_all opp_color ~f:(fun oc -> not (Color.equal c oc)))

  let%private recompute_decision (t : t) : Decision.t =
    let rows, cols = Board.rows t.board, Board.cols t.board in
    let total = rows * cols in
    let c1 = Territory.count t.p1 in
    let c2 = Territory.count t.p2 in
    if c1 + c2 = total then
      if c1 > c2 then Winner Player.P1
      else if c2 > c1 then Winner Player.P2
      else Tie
    else
      match t.decision with
      | In_progress { whose_turn } -> In_progress { whose_turn }
      | _ -> In_progress { whose_turn = Player.P1 } (* shouldn't occur *)

  let make_move (t : t) (choice : Move.t) : (t, Move_error.t) Result.t =
    match t.decision with
    | Winner _ | Tie -> Error Move_error.Game_is_over
    | In_progress { whose_turn } ->
      let legal = get_legal_moves t in
      if not (List.exists legal ~f:(Color.equal choice))
      then Error Move_error.Illegal_color
      else
        let b' = Board.copy t.board in
        let p1' = Territory.copy t.p1 in
        let p2' = Territory.copy t.p2 in
        let mine, theirs =
          match whose_turn with
          | P1 -> p1', p2'
          | P2 -> p2', p1'
        in
        let current_color = sample_owned_color b' mine |> Option.value_exn in
        if Color.equal choice current_color
        then Ok { t with last_move = Some choice } (* no-op by rule, but shouldn't be legal *)
        else (
          expand b' ~mine ~theirs ~chosen:choice;
          let decision_after_fill =
            let tmp =
              { board = b'
              ; p1 = p1'
              ; p2 = p2'
              ; decision = In_progress { whose_turn } (* placeholder, updated below *)
              ; last_move = Some choice
              }
            in
            recompute_decision tmp
          in
          let decision =
            match decision_after_fill with
            | Winner _ | Tie -> decision_after_fill
            | In_progress _ -> In_progress { whose_turn = Player.opposite whose_turn }
          in
          Ok { board = b'; p1 = p1'; p2 = p2'; decision; last_move = Some choice }
        )
end