(** @author Loan Patris & Alexeï Czornyj & Styven Drui & Nicolas Moreau *)
(* 3 repères : global en px, interne au tetris, et local à la pièce *)

open CPutil;;

(* -------------------------- *)
(* -------------------------- *)
(*    fonctions utilitaires   *)
(* -------------------------- *)
(* -------------------------- *) 

let mywait(x : float) : unit =
  let y : float ref = ref (Sys.time()) in
  while (Sys.time() -. !y) < x
  do ()
  done
;;


(* --------------------------------- *)
(* --------------------------------- *)
(*   Types et fonctions graphique    *)
(* --------------------------------- *)
(* --------------------------------- *)

type t_point = {x : int ; y : int} ;;

(* ------------------------------------------------- *)
(* ------------------------------------------------- *)
(*    Types, formes, parametrage et initialisation   *)
(* ------------------------------------------------- *)
(* ------------------------------------------------- *)

(* Types *)

type 'a t_array = {len : int ; value : 'a array} ;;

type t_shape = {shape : t_point list ; x_len : int ; y_len : int ; 
                rot_rgt_base : t_point ; rot_rgt_shape : int ; 
                rot_lft_base : t_point ; rot_lft_shape : int} ;; 

type t_cur_shape = {base : t_point ref ; shape : int ref ; color : t_color ref} ;;


type t_param_time = {init : float ; extent : float ; ratio : float} ;;

type t_param_graphics = 
    {base : t_point ; dilat : int ; color_arr : t_color t_array} ;;

type t_param = 
  {time : t_param_time ; 
   mat_szx : int ; mat_szy : int ;
   graphics : t_param_graphics ; 
   shapes : t_shape t_array
} ;;

type t_play = {par : t_param ; cur_shape : t_cur_shape ; mat : t_color matrix} ;;


(* Initialisation de quelques formes et des parametres *)

let init_sh011() : t_shape = 
  {shape = [{x = 0 ; y = 0} ; {x = 1 ; y = 0} ; {x = 2 ; y = 0} ; {x = 3 ; y = 0}] ; 
  x_len = 4 ; y_len = 1 ; 
  rot_rgt_base = {x = 1 ;  y = 1} ; rot_rgt_shape = 1 ; 
  rot_lft_base = {x = 2 ; y = 1} ; rot_lft_shape = 1} 
;;
let init_sh112() : t_shape = 
  {shape = [{x = 0 ; y = 0} ; {x = 0 ; y = -1} ; {x = 0 ; y = -2} ; {x = 0 ; y = -3}] ; 
  x_len = 1 ; y_len = 4 ; 
  rot_rgt_base = {x = -2 ;  y = -1} ; rot_rgt_shape = 0 ; 
  rot_lft_base = {x = -1 ; y = -1} ; rot_lft_shape = 0} 
;;
let init_sh211() : t_shape = 
  {shape = [{x = 0 ; y = 0} ; {x = 0 ; y = -1} ; {x = 1 ; y = 0} ; {x = 1 ; y = -1}] ; 
  x_len = 2 ; y_len = 2 ; 
  rot_rgt_base = {x = 0 ;  y = 0} ; rot_rgt_shape = 2 ; 
  rot_lft_base = {x = 0 ;  y = 0} ; rot_lft_shape = 2} 
;;

let init_shapes() : t_shape t_array = 
  {len = 3 ; value = [| init_sh011() ; init_sh112() ; init_sh211() |]} 
;;
let init_color() : t_color t_array = 
  {len = 7 ; value = [|blue ; red ; green ; yellow ; cyan ; magenta ; grey|]} ;;

let init_param() : t_param = 
    {
    time = {init = 1.0 ; extent = 10.0 ; ratio = 0.8} ; 
    mat_szx = 15 ; mat_szy = 28 ;
    graphics = {base = {x = 50 ; y = 50} ; dilat = 20 ; color_arr = init_color()} ; 
    shapes = init_shapes()
    }
;;

let init_play() : t_play =
  let prm : t_param = init_param() in
  {
    par = prm;
    cur_shape = {base = ref {x = 9; y = 20}; shape = ref 0; color = ref blue};
    mat =
      [|
        [|blue;red|];
        [|blue; red|]
      |]
  }
;;


(** Draws the outline of the square.
    [p] starting point of the square in the work place
    [base_draw] starting point of the work place
    [dilat] dilation (length of the square)
    [col] choosen color
    author Loan *)

let draw_absolute_pt(p, base_draw, dilat, col : t_point * t_point * int * t_color) : unit =
  (
    set_color(col);
    draw_rect( (p.x*dilat) + base_draw.x ,( p.y * dilat) + base_draw.y, p.x + 1 * dilat, p.y + 1 * dilat);
  )
;;

(** Draws a square 
    [p] starting point of the square in the work place
    [base_draw] starting point of the work place
    [dilat] dilation (length of the square)
    [col] choosen color *)

let fill_absolute_pt(p, base_draw, dilat, col : t_point * t_point * int * t_color) : unit =
  (
    set_color(col);
    fill_rect((p.x * dilat) + base_draw.x + 1 , (p.y*dilat) + base_draw.y +1, (p.x + 1 * dilat) - 2,( p.y + 1 * dilat)-2);
  )
;;

(** Draws a square fulled by the color square
    [p] starting point of the square in the work place
    [base_draw] starting point of the work place
    [dilat] dilation (length of the square)
    [col] choosen color *) 

let drawfill_absolute_pt(p, base_draw, dilat, col : t_point * t_point * int * t_color) : unit =
  (
    draw_absolute_pt(p, base_draw, dilat, black);
    fill_absolute_pt(p, base_draw, dilat, col)
  )
;;

(** Draws the outline of a square
    [p] starting point defined relatively of base_point
    [base_point] local origin point
    [base_draw] display space
    [dilation] (length of the square)
    [col] color of the outline *)

let draw_relative_pt(p, base_point, base_draw, dilat, col : t_point * t_point * t_point * int * t_color) : unit =
    draw_absolute_pt({x = p.x + base_point.x; y = p.y + base_point.y}, {x = base_draw.x + base_point.x; y = base_draw.y + base_point.y}, dilat, col)
;;

(** Draws a square
    [p] starting point defined relatively of base_point
    [base_point] local origin point
    [dilat] dilation (length of the square)
    [col] color of the square *)

let fill_relative_pt(p, base_point, base_draw, dilat, col : t_point * t_point * t_point * int * t_color) : unit =
   fill_absolute_pt({x = p.x + base_point.x; y = p.y + base_point.y}, {x = base_draw.x + base_point.x; y = base_draw.y + base_point.y}, dilat, col)
;;

(** Draws a square fulled by the color square
    [p] starting point defined relatively of base_point
    [base_draw] starting local origin point
    [dilat] dilation (length of the square)
    [col] color of the square *)

let drawfill_relative_pt(p, base_point, base_draw, dilat, col : t_point * t_point * t_point * int * t_color) : unit =
   drawfill_absolute_pt({x = p.x + base_point.x; y = p.y + base_point.y}, {x = base_draw.x + base_point.x; y = base_draw.y + base_point.y}, dilat, col)
;;

(** Draws the outlines of multiples squares wich are referenced in the l t_point list
    [l] list of points
    [base_pt] local origin point
    [base_draw] display space
    [dilat] dilation (length of the square)
    [col] color of the square *)

let draw_pt_list(l, base_pt, base_draw, dilat, col : t_point list * t_point* t_point * int * t_color) : unit =
  for i = 0 to (len(l)-1)
  do
    draw_relative_pt(nth(l,i),base_pt,base_draw,dilat,col)
  done;
;;

(** Draws multiples full squares wich are referenced in the l t_point list
    [l] list of points
    [base_pt] local origin point
    [base_draw] display space
    [dilat] dilation (length of the square)
    [col] color of the square *)

let fill_pt_list(l, base_pt, base_draw, dilat, col : t_point list * t_point *t_point * int * t_color) : unit=
  for i = 0 to (len(l)-1)
  do
    fill_relative_pt(nth(l,i),base_pt,base_draw,dilat,col)
  done;
;;

(** Draws multiples full squares and their outlines wich are referenced in the l t_point list
    [l] list of points
    [base_pt] local origin point
    [base_draw] display space
    [dilat] dilation (length of the square)
    [col] color of the square *)

let drawfill_pt_list(l, base_pt, base_draw, dilat, col : t_point list * t_point * t_point * int * t_color) : unit=
  for i = 0 to (len(l)-1)
  do
    drawfill_relative_pt(nth(l,i),base_pt,base_draw,dilat,col)
  done;
;;

let draw_frame(base_draw, size_x, size_y, dilat : t_point * int * int * int) : unit =
  draw_rect(base_draw.x, base_draw.y, size_x * dilat, size_y * dilat)
;;

(*-------------------------------*)
(*     Fonction d'extraction     *)
(*-------------------------------*)

let getParam(play : t_play) : t_param =
  play.par
;;

let getCurShape(play : t_play) : t_cur_shape =
  play.cur_shape
;;

let getMat(play : t_play) : t_color matrix =
  play.mat
;;

let getTime(prm : t_param) : t_param_time =
  prm.time
;;

let getMat_szx(prm : t_param) : int =
  prm.mat_szx
;;

let getMat_szy(prm : t_param) : int =
  prm.mat_szy
;;

let getGraphics(prm : t_param) : t_param_graphics =
  prm.graphics
;;

let getShapes(prm : t_param) : t_shape t_array =
  prm.shapes
;;

let getInitTime(time : t_param_time) : float =
  time.init
;;

let getExtent(time : t_param_time) : float =
  time.extent
;;

let getRatio(time : t_param_time) : float =
  time.ratio
;;

let getBase(graphics : t_param_graphics) : t_point =
  graphics.base
;;

let getDilat(graphics : t_param_graphics) : int =
  graphics.dilat
;;

let getColorArr(graphics : t_param_graphics) : t_color t_array =
  graphics.color_arr
;;

let getShape(shape : t_shape) : t_point list =
  shape.shape
;;

let getXLen(shape : t_shape) : int =
  shape.x_len
;;

let getYLen(shape : t_shape) : int =
  shape.y_len
;;

let getRotRgtBase(shape : t_shape) : t_point  =
  shape.rot_rgt_base
;;

let getRotRgtShape(shape : t_shape) : int =
  shape.rot_rgt_shape
;;

let getRotLftBase(shape : t_shape) : t_point =
  shape.rot_lft_base
;;

let getRotLftShape(shape : t_shape) : int =
  shape.rot_lft_shape
;;

let getX(point : t_point) : int =
  point.x
;;

let getY(point : t_point) : int =
  point.y
;;

let color_choice(t : t_color t_array) : t_color =
  let rand_color : int = rand_int(0, t.len-1) in
  t.value.(rand_color)
;;

let cur_shape_choice(shapes, mat_szx, mat_szy, color_arr : t_shape t_array * int * int * t_color t_array) : t_cur_shape =
  let rand_shape : int = rand_int(0, shapes.len - 1) in
  let rand_x : int = rand_int(0, mat_szx - shapes.value.(rand_shape).x_len)
  and rand_color : int = rand_int(0, color_arr.len -1) in
  {base = ref {x = rand_x; y = mat_szy}; shape = ref rand_shape; color = ref color_arr.value.(rand_color)}
;;

let rec insert(cur, shape, param, mymat : t_cur_shape * t_point list * t_param * t_color matrix) : bool=
  let my_point : t_point = fst(shape) in
  if isempty(shape)
  then true
  else
    if mymat.(!(cur.base).y + my_point.y).(!(cur.base).x + my_point.x) = white
    then
    (
      mymat.(!(cur.base).y + my_point.y).(!(cur.base).x + my_point.x) <- !(cur.color);
      insert(cur, rem_fst(shape), param, mymat)
    )
    else
      false
;;

(* ----------------------------------------------- *)
(* ----------------------------------------------- *)
(*    Deplacements et controle des deplacements    *)
(* ----------------------------------------------- *)
(* ----------------------------------------------- *)

(* choix des deplacements suivant le caractere saisi*)

let valid_matrix_point(p, param : t_point * t_param ) : bool =
  (p.x >= 0 && p.x <= param.mat_szx - 1) && (p.y >= 0 && p.y <= param.mat_szy -1)
;;

let rec is_free_move(p, shape, mymat,param :t_point * t_point list * t_color matrix * t_param) : bool =
  if isempty(shape)
  then true
  else if  valid_matrix_point({x = p.x + fst(shape).x; y = p.y + fst(shape).y}, param)
       then  is_free_move(p, rem_fst(shape), mymat, param)
       else false
;;

let is_column_full(mymat, y, mat_szx : t_color matrix * int * int) : bool =
  let is_full : bool ref = ref true in
  for i = 0 to mat_szx - 1
  do
   if mymat.(y).(i) = white
   then is_full := false
  done;
  !is_full
;;


(*
let move(pl, dir : t_play * char) : bool = 
  (
  if dir = 't'
    then rotate_right(pl)
    else
      if dir = 'c'
      then rotate_left(pl)
      else
        if dir = 'd'
        then move_left(pl)
        else
          if dir = 'h'
          then move_right(pl)
          else () ;  
  (dir = 'v')
  )
;;
                 
(* ----------------------------------- *)
(* ----------------------------------- *)
(*    Suppression des lignes pleines   *)
(* ----------------------------------- *)
(* ----------------------------------- *)


(* --------------------- *)
(* --------------------- *)
(*   Une etape de jeu    *)
(* --------------------- *)
(* --------------------- *)


let newstep(pl, new_t, t, dt : t_play * float ref * float * float) : bool = 
  let the_end : bool ref = ref (!new_t -. t > dt) and dec : bool ref = ref false in
  let dir : char ref = ref 'x' and notmove : bool ref = ref false in
    (
    while not(!the_end)
    do 
      if key_pressed()
      then dir := read_key()
      else () ;
      dec := move(pl, !dir) ;
      dir := 'x' ; 
      new_t := Sys.time() ;
      the_end := !dec || (!new_t -. t > dt) ;
    done ; 
    if !dec 
    then (move_at_bottom(pl) ; notmove := true)
    else notmove := not(move_down(pl)) ;
    if !notmove
    then the_end := final_newstep(pl)
    else the_end := false;
    !the_end ;
    )
;;

(* ------------------------ *)
(* ------------------------ *)
(*    Fonction principale   *)
(* ------------------------ *)
(* ------------------------ *)


let jeuCP2() : unit =
  let pl : t_play = init_play() in
  let t : float ref = ref (Sys.time()) and new_t : float ref = ref (Sys.time()) in
  let dt : float ref = ref (time_init(pl.par)) and t_acc : float ref = ref (Sys.time()) in
  let the_end : bool ref = ref false in
    while not(!the_end)
    do
      the_end := newstep(pl, new_t, !t, !dt) ; 
      if ((!new_t -. !t_acc) > time_extent(pl.par))
      then 
        (
        dt := !dt *. time_ratio(pl.par) ; 
        t_acc := !new_t
        ) 
      else () ;
      t := !new_t
    done
;;
*)
