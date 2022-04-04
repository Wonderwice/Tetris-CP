(** *)
open CPutil;;
(* -------------------------- *)
(* -------------------------- *)
(*    fonctions utilitaires   *)
(* -------------------------- *)
(* -------------------------- *) 
(** mywait function is used to slow down some game steps
    {%html: <br> %}
    [x] the number of seconds in which the function is executed*)
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

(** type t_point is the reprensentation of a point on a 2 dimensions space *)
type t_point = {x : int ; y : int} ;;

(* ------------------------------------------------- *)
(* ------------------------------------------------- *)
(*    Types, formes, parametrage et initialisation   *)
(* ------------------------------------------------- *)
(* ------------------------------------------------- *)

(* Types *)
(** type t_array is used to represent an array with his len and value, it have a lower cost of use than the len() function *)
type 'a t_array = {len : int ; value : 'a array} ;;

(** type t_shape is the representation of a shape which is used on the tetris
    {%html: <br> %}
    [shape] stands for the list of points who compose the shape
    {%html: <br> %}
    [x_len] is for the lenght of the shape on the x axis
    {%html: <br> %}
    [y_len] is for the lenght of the shape on the y axis
    {%html: <br> %}
    [rot_rgt_base] and [rot_rgt_shape] describe the shape with a right 90 degrees rotation 
    {%html: <br> %}
    [rot_lft_base] and [rot_lft_shape]  describe the shape with a left 90 degrees rotation 
    @author doc Alexei *)
type t_shape = {shape : t_point list ; x_len : int ; y_len : int ; 
                rot_rgt_base : t_point ; rot_rgt_shape : int ; 
                rot_lft_base : t_point ; rot_lft_shape : int} ;; 

(** type t_cur_shape is use for the representation of the current shape in the display space
    {%html: <br> %}
    [base] stands for the relative origin of the shape
    {%html: <br> %}
    [shape] is the index of the current shape in a t_shape t_array
    {%html: <br> %}
    [color] is for the color of the shape
    @author doc  Alexei *)
type t_cur_shape = {base : t_point ref ; shape : int ref ; color : t_color ref} ;;

(**type t_param_time is the representation of the time parameters
{%html: <br> %}
[init] contains the initial duration (in seconds) of a movement
{%html: <br> %}
[extent] contains the duration between two accelerations
{%html: <br> %}
[ratio] contains the acceleration coefficient
@author doc Styven *)

type t_param_time = {init : float ; extent : float ; ratio : float} ;;

(**type t_param_graphics is the representation of the graphics settings
{%html: <br> %}
[base] contains the origin of the work space and it's defined in the display space
{%html: <br> %}
[dilat] contains the "scaling coefficient" between the workspace and the display space
{%html: <br> %}
[color_arr] contains an array of colors that can be used for shapes concrete
@author doc Styven *)

type t_param_graphics = 
  {base : t_point ; dilat : int ; color_arr : t_color t_array} ;;

(**type_t_param contains time settings and graphics settings,
two fields mat_szx and mat_szy containing the dimensions of the matrix representing the space of
work, as well as a table containing the available abstract shapes
{%html: <br> %}
[time] contains the time parameters
{%html: <br> %}
[mat_szx] contains the dimensions of the matrix on the x axis representing the space of work
{%html: <br> %}
[mat_szy] contains the dimensions of the matrix on the y axis representing the space of work
{%html: <br> %}
[graphics] contains the graphics settings
{%html: <br> %}
[shapes] represents an array containing the available abstract shapes
@author doc Styven *)

type t_param = 
  {time : t_param_time ; 
   mat_szx : int ; mat_szy : int ;
   graphics : t_param_graphics ; 
   shapes : t_shape t_array
  }
;;

(** type t_play used to represent game information
{%html: <br> %}
[par] contains game settings
{%html: <br> %}
[cur_shape] contains the description of the
current form, which moves until blocked; in this case, it is integrated into the matrix
game, and a new shape is chosen as the current shape
{%html: <br> %}
[mat] is the matrix describing the workspace
@author doc Styven *)

type t_play = {par : t_param ; cur_shape : t_cur_shape ; mat : t_color matrix};;


(* Initialisation de quelques formes et des parametres *)

(**init_sh011 is the representation of a horizontal bar and uses the t_shape type
@author doc Styven*)

let init_sh011() : t_shape = 
  {shape = [{x = 0 ; y = 0} ; {x = 1 ; y = 0} ; {x = 2 ; y = 0} ; {x = 3 ; y = 0}] ; 
  x_len = 4 ; y_len = 1 ; 
  rot_rgt_base = {x = 1 ;  y = 1} ; rot_rgt_shape = 1 ; 
  rot_lft_base = {x = 2 ; y = 1} ; rot_lft_shape = 1} 
;;

(**init_sh112 is the representation of a vertical bar and uses the t_shape type
@author doc Styven *)

let init_sh112() : t_shape = 
  {shape = [{x = 0 ; y = 0} ; {x = 0 ; y = -1} ; {x = 0 ; y = -2} ; {x = 0 ; y = -3}] ; 
  x_len = 1 ; y_len = 4 ; 
  rot_rgt_base = {x = -2 ;  y = -1} ; rot_rgt_shape = 0 ; 
  rot_lft_base = {x = -1 ; y = -1} ; rot_lft_shape = 0} 
;;

(**init_sh112 is the representation of a square and uses the t_shape type
@author doc Styven *)

let init_sh211() : t_shape = 
  {shape = [{x = 0 ; y = 0} ; {x = 0 ; y = -1} ; {x = 1 ; y = 0} ; {x = 1 ; y = -1}] ; 
  x_len = 2 ; y_len = 2 ; 
  rot_rgt_base = {x = 0 ;  y = 0} ; rot_rgt_shape = 2 ; 
  rot_lft_base = {x = 0 ;  y = 0} ; rot_lft_shape = 2} 
;;

(** init_shapes is an array of length 3 that groups the 3 available shapes
@author doc Styven*)

let init_shapes() : t_shape t_array = 
  {len = 3 ; value = [| init_sh011() ; init_sh112() ; init_sh211() |]} 
;;

(**init_color is an array of length 7 containing the available colors
@author doc Styven*)

let init_color() : t_color t_array = 
  {len = 7 ; value = [|blue ; red ; green ; yellow ; cyan ; magenta ; grey|]} ;;

(**init_param initialize game settings with defined parameters
@author doc Styven *)

let init_param() : t_param = 
    {
    time = {init = 1.0 ; extent = 10.0 ; ratio = 0.8} ; 
    mat_szx = 15 ; mat_szy = 28 ;
    graphics = {base = {x = 50 ; y = 50} ; dilat = 20 ; color_arr = init_color()} ; 
    shapes = init_shapes()
    }
;;

(** Draws the outline of the square.
    @param p coordinates of the square in the work place
    @param base_draw starting point of the work place
    @param dilat dilation (length of the square)
    @param col choosen color
    @author Alexei and doc Loan *)
let draw_absolute_pt(p, base_draw, dilat, col : t_point * t_point * int * t_color) : unit =
  (
    set_color(col);
    draw_rect(p.x * dilat + base_draw.x , p.y * dilat + base_draw.y, dilat - 1, dilat - 1);
  )
;;

(** Draws a square 
    @param p coordinates of the square in the work place
    @param base_draw starting point of the work place
    @param dilat dilation (length of the square)
    @param col choosen color 
    @author Alexei and doc Loan*)
let fill_absolute_pt(p, base_draw, dilat, col : t_point * t_point * int * t_color) : unit =
  (
    set_color(col);
    fill_rect(p.x * dilat + base_draw.x + 1, p.y * dilat + base_draw.y + 1, dilat - 3, dilat - 3);
  )
;;

(** Draws a square fulled by the color square
    @param p coordinates of the square in the work place
    @param base_draw starting point of the work place
    @param dilat dilation (length of the square)
    @param col choosen color
    @author Alexei and doc Loan*) 
let drawfill_absolute_pt(p, base_draw, dilat, col : t_point * t_point * int * t_color) : unit =
  (
    draw_absolute_pt(p, base_draw, dilat, black);
    fill_absolute_pt(p, base_draw, dilat, col)
  )
;;

(** Draws the outline of a square
    @param p starting point defined relatively of base_point
    @param base_point local origin point
    @param base_draw display space
    @param dilation (length of the square)
    @param col color of the outline
    @author Alexei*)
let draw_relative_pt(p, base_point, base_draw, dilat, col : t_point * t_point * t_point * int * t_color) : unit =
    draw_absolute_pt({x = p.x + base_point.x; y = p.y + base_point.y}, {x = base_draw.x + base_point.x; y = base_draw.y + base_point.y}, dilat, col)
;;

(** Draws a square
    @param p starting point defined relatively of base_point
    @param base_point local origin point
    @param dilat dilation (length of the square)
    @param col color of the square
    @author Alexei and doc Loan*)
let fill_relative_pt(p, base_point, base_draw, dilat, col : t_point * t_point * t_point * int * t_color) : unit =
   fill_absolute_pt({x = p.x + base_point.x; y = p.y + base_point.y}, {x = base_draw.x + base_point.x; y = base_draw.y + base_point.y}, dilat, col)
;;

(** Draws a square fulled by the color square
    @param p starting point defined relatively of base_point
    @param base_draw starting local origin point
    @param dilat dilation (length of the square)
    @param col color of the square
    @author Alexei and doc Loan*)
let drawfill_relative_pt(p, base_point, base_draw, dilat, col : t_point * t_point * t_point * int * t_color) : unit =
   drawfill_absolute_pt({x = p.x + base_point.x; y = p.y + base_point.y}, {x = base_draw.x + base_point.x; y = base_draw.y + base_point.y}, dilat, col)
;;

(** Draws the outlines of multiples squares wich are referenced in the l t_point list
    @param l list of points
    @param base_pt local origin point
    @param base_draw display space
    @param dilat dilation (length of the square)
    @param col color of the square
    @author Alexei and doc Loan*)
let draw_pt_list(l, base_pt, base_draw, dilat, col : t_point list * t_point* t_point * int * t_color) : unit =
  for i = 0 to (len(l)-1)
  do
    draw_relative_pt(nth(l,i),base_pt,base_draw,dilat,col)
  done;
;;

(** Draws multiples full squares wich are referenced in the l t_point list
    @param l list of points
    @param base_pt local origin point
    @param base_draw display space
    @param dilat dilation (length of the square)
    @param col color of the square
    @author Alexei and doc Loan*)
let fill_pt_list(l, base_pt, base_draw, dilat, col : t_point list * t_point *t_point * int * t_color) : unit=
  for i = 0 to (len(l)-1)
  do
    fill_relative_pt(nth(l,i),base_pt,base_draw,dilat,col)
  done;
;;

(** Draws multiples full squares and their outlines wich are referenced in the l t_point list
    @param l list of points
    @param base_pt local origin point
    @param base_draw display space
    @param dilat dilation (length of the square)
    @param col color of the square
    @author Alexeï and doc Loan *)
let drawfill_pt_list(l, base_pt, base_draw, dilat, col : t_point list * t_point * t_point * int * t_color) : unit=
  for i = 0 to (len(l)-1)
  do
    drawfill_relative_pt(nth(l,i),base_pt,base_draw,dilat,col)
  done;
;;

(**author Alexeï*)

let draw_frame(base_draw, size_x, size_y, dilat : t_point * int * int * int) : unit =
  for x = -1 to size_x
  do
    for y = -1 to size_y
    do
      if x = size_x || x = -1 || y = -1
      then fill_absolute_pt({x = x ; y = y}, base_draw, dilat, black)
    done
  done
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
  and y : int = mat_szy - shapes.value.(rand_shape).y_len
  and rand_color : int = rand_int(0, color_arr.len -1) in
  {base = ref {x = rand_x; y = y}; shape = ref rand_shape; color = ref color_arr.value.(rand_color)}
;;

let rec insert(cur, shape, param, mymat : t_cur_shape * t_point list * t_param * t_color matrix) : bool=
  if isempty(shape)
  then true
  else
    let my_point : t_point = fst(shape) in
    if mymat.((!(cur.base).y + my_point.y) - 1).((!(cur.base).x + my_point.x)) = white
    then
    (
      drawfill_pt_list(param.shapes.value.(!(cur.shape)).shape, !(cur.base), param.graphics.base, param.graphics.dilat, !(cur.color));
      insert(cur, rem_fst(shape), param, mymat)
    )
    else false
;;


let init_play() : t_play =
    let prm : t_param = init_param() in
    let play : t_play = { par = prm; cur_shape = cur_shape_choice( prm.shapes,prm.mat_szx, prm.mat_szy, prm.graphics.color_arr); mat = mat_make(prm.mat_szy, prm.mat_szx, white)}
    in
    (
    if insert(play.cur_shape, play.par.shapes.value.(!(play.cur_shape.shape)).shape, play.par, play.mat)
    then
      (
        draw_frame(play.par.graphics.base, play.par.mat_szx, play.par.mat_szy, play.par.graphics.dilat);
      );
    play
    )
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
  else
      let temp_point : t_point = fst(shape) in
      if  valid_matrix_point({x = p.x + temp_point.x; y = p.y + temp_point.y}, param)
      then  is_free_move(p, rem_fst(shape), mymat, param)
      else false
;;

let move_left(pl : t_play) : unit =
  let cur_shape : t_cur_shape = {base = ref {x = !(pl.cur_shape.base).x - 1; y = !(pl.cur_shape.base).y}; shape = pl.cur_shape.shape; color = pl.cur_shape.color} in
  if insert(cur_shape, pl.par.shapes.value.(!(cur_shape.shape)).shape, pl.par, pl.mat)
  then ()
;;

let move_right(pl : t_play) : unit =
    let cur_shape : t_cur_shape = {base = ref {x = !(pl.cur_shape.base).x + 1; y = !(pl.cur_shape.base).y - 1}; shape = pl.cur_shape.shape; color = pl.cur_shape.color} in
    if insert(cur_shape, pl.par.shapes.value.(!(cur_shape.shape)).shape, pl.par, pl.mat)
    then ()
;;

let move_down(pl : t_play) : bool =
    let cur_shape : t_cur_shape = {base = ref {x = !(pl.cur_shape.base).x; y = !(pl.cur_shape.base).y - 1}; shape = pl.cur_shape.shape; color = pl.cur_shape.color} in
      if is_free_move(!(cur_shape.base), pl.par.shapes.value.(!(cur_shape.shape)).shape, pl.mat, pl.par)
      then insert(cur_shape, pl.par.shapes.value.(!(cur_shape.shape)).shape, pl.par, pl.mat)
      else false
;;

let rotate_right(pl : t_play) : unit =
  ()
;;

let rotate_left(pl : t_play) : unit =
  ()
;;

let move_at_bottom(pl : t_play) : unit =
  let cur_shape : t_cur_shape = {base = ref {x = !(pl.cur_shape.base).x; y = !(pl.cur_shape.base).y - 1}; shape = pl.cur_shape.shape; color = pl.cur_shape.color} in
  while (!(cur_shape.base).y <> 0 || insert(cur_shape, pl.par.shapes.value.(!(cur_shape.shape)).shape, pl.par, pl.mat))
  do
    move_down(pl)
  done;
  ()
;;

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

let is_column_full(mymat, y, mat_szx : t_color matrix * int * int) : bool =
  let is_full : bool ref = ref true in
  for i = 0 to mat_szx - 1
  do
   if mymat.(y).(i) = white
   then is_full := false
  done;
  !is_full
;;

let decal(mymat, y, szx, szy, par : t_color matrix * int * int * int * t_param) : unit =
  for i = 0 to szy
  do
    if i > y
    then mymat.(i - 1) <- mymat.(i)
  done
;;

let clear_play(pl : t_play) : unit =
  for i = 0 to pl.par.mat_szy
  do
    if is_column_full(pl.mat, i, pl.par.mat_szx)
    then decal(pl.mat, i, pl.par.mat_szx, pl.par.mat_szy, pl.par)
  done
;;

let rec final_insert_aux(pl, my_point_list, mymat, cur : t_play * t_point list * t_color matrix * t_cur_shape) : bool =
  if isempty(my_point_list)
  then true
  else
    let my_point : t_point = fst(my_point_list) in
    if mymat.((!(cur.base).y + my_point.y) - 1).((!(cur.base).x + my_point.x)) = white
    then
      (mymat.((!(cur.base).y + my_point.y) - 1).((!(cur.base).x + my_point.x)) <- !(cur.color);
       final_insert_aux(pl, rem_fst(my_point_list), mymat, cur))
       else false
;;

let final_insert(pl : t_play) : bool =
  final_insert_aux(pl, pl.par.shapes.value.(!(pl.cur_shape.shape)).shape, pl.mat, pl.cur_shape)
;;

let final_newstep(pl : t_play) : bool =
  let new_cur_shape : t_cur_shape = cur_shape_choice(pl.par.shapes, pl.par.mat_szx, pl.par.mat_szy, pl.par.graphics.color_arr) in
  if is_free_move(!(pl.cur_shape.base),pl.par.shapes.value.(!(pl.cur_shape.shape)).shape,pl.mat, pl.par)
  then (* ??? *) true
  else
    (
      final_insert(pl);
      clear_play(pl);
      pl.cur_shape.base := !(new_cur_shape.base);
      pl.cur_shape.shape := !(new_cur_shape.shape);
      pl.cur_shape.color := !(new_cur_shape.color);
    (* ??? *) false
    )
;;

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
  let dt : float ref = ref (pl.par.time.init) and t_acc : float ref = ref (Sys.time()) in
  let the_end : bool ref = ref false in
    while not(!the_end)
    do
      the_end := newstep(pl, new_t, !t, !dt) ; 
      if ((!new_t -. !t_acc) > pl.par.time.extent)
      then 
        (
        dt := !dt *. pl.par.time.ratio; 
        t_acc := !new_t
        ) 
      else () ;
      t := !new_t
    done
;;
