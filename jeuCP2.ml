(** *)
(* -------------------------------------------- *)
(* -------------------------------------------- *)
(** {%html: <h2> Fonctions utilitaires </h2>%}  *)
(* -------------------------------------------- *)
(* -------------------------------------------- *)
(* open CPutil;; *)
(** mywait function is used to slow down some game steps
    {%html: <br> %}
    [x] the number of seconds in which the function is executed*)
let mywait(x : float) : unit =
  let y : float ref = ref (Sys.time()) in
  while (Sys.time() -. !y) < x
  do ()
  done
;;


(* ----------------------------------------------- *)
(* ----------------------------------------------- *)
(** {%html: <h2>Types et fonctions graphique</h2>%}*)
(* ----------------------------------------------- *)
(* ----------------------------------------------- *)

(** type t_point is the reprensentation of a point on a 2 dimensions space *)
type t_point = {x : int ; y : int} ;;

(* ------------------------------------------------- *)
(* ------------------------------------------------- *)
(** {%html: <h2>Types, formes, parametrage et initialisation</h2>%}*)
(* ------------------------------------------------- *)
(* ------------------------------------------------- *)

(** {%html: <h3>Types</h3>%}*)

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
    @author doc Alexei*)
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
    @author doc Alexei*)
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


(** {%html: <h2> Initialisation de quelques formes et des parametres </h2>%} *)

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
   @author doc Styven *)
let init_color() : t_color t_array = 
  {len = 7 ; value = [|blue ; red ; green ; yellow ; cyan ; magenta ; grey|]} ;;


(** init_param initialize game settings with defined parameters
   @author doc Styven *)
let init_param() : t_param = 
    {
    time = {init = 1.0 ; extent = 10.0 ; ratio = 0.8} ; 
    mat_szx = 10 ; mat_szy = 15;
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
    @param dilation in pixels(length of the square)
    @param col color of the outline
    @author Alexei and doc Loan*)
let draw_relative_pt(p, base_point, base_draw, dilat, col : t_point * t_point * t_point * int * t_color) : unit =
    draw_absolute_pt({x = p.x + base_point.x; y = p.y + base_point.y}, {x = base_draw.x; y = base_draw.y}, dilat, col)
;;

(** Draws a square
    @param p starting point defined relatively of base_point
    @param base_point local origin point
    @param dilat dilation in pixels(length of the square)
    @param col color of the square
    @author Alexei and doc Loan*)
let fill_relative_pt(p, base_point, base_draw, dilat, col : t_point * t_point * t_point * int * t_color) : unit =
   fill_absolute_pt({x = p.x + base_point.x; y = p.y + base_point.y}, {x = base_draw.x; y = base_draw.y}, dilat, col)
;;

(** Draws a square fulled by the color square
    @param p starting point defined relatively of base_point
    @param base_draw starting local origin point
    @param dilat dilation (length of the square)
    @param col color of the square
    @author Alexei and doc Loan*)
let drawfill_relative_pt(p, base_point, base_draw, dilat, col : t_point * t_point * t_point * int * t_color) : unit =
   drawfill_absolute_pt({x = p.x + base_point.x; y = p.y + base_point.y}, {x = base_draw.x; y = base_draw.y}, dilat, col)
;;

(** Draws the outlines of multiples squares wich are referenced in the l t_point list
    @param l list of points
    @param base_pt local origin point
    @param base_draw display space
    @param dilat dilation (length of the square)
    @param col color of the square
    @author Alexei and doc Nicolas*)
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
    @author Nicolas and doc Alexei *)
let fill_pt_list(l, base_pt, base_draw, dilat, col : t_point list * t_point *t_point * int * t_color) : unit=
  for i = 0 to (len(l)-1)
  do
    fill_relative_pt(nth(l,i),base_pt,base_draw,dilat,col)
  done;
;;

(** Draws multiples full squares and their outlines wich are referenced in the l t_point list
    @param l list of points
    @param base_pt local origin point
    @param base_draw draws display space
    @param dilat dilation (length of the square)
    @param col color of the square
    @author Sytven and doc Nicolas *)
let rec drawfill_pt_list(list, base_pt, base_draw, dilat, col : t_point list * t_point * t_point * int * t_color) : unit=
  if isempty(list)
  then ()
  else
    let fst_list : t_point = fst(list) in
    (
      drawfill_relative_pt(fst_list,base_pt,base_draw,dilat,col);
      drawfill_pt_list(rem_fst(list), base_pt, base_draw, dilat, col)
    )
;;

(** Draws multiples full squares and their outlines wich are referenced in the l t_point list
    @param size_y is for the y lenght
    @param size_x is for the x lenght
    @param base_draw draws display space
    @param dilat dilation (length of the square)
    @author Alexei *)
let draw_frame(base_draw, size_x, size_y, dilat : t_point * int * int * int) : unit =
  (* -1 en x est utilisé au lieu de 0 pour eviter la superposition avec les blocs *)
  for x = -1 to size_x - 1
  do
    drawfill_absolute_pt({x = x ; y = -1}, base_draw, dilat, black)    
  done;
  for y = -1 to size_y - 2
  do
    drawfill_absolute_pt({x = size_x ; y = y}, base_draw, dilat, black);
    drawfill_absolute_pt({x = -1 ; y = y}, base_draw, dilat, black)
  done
;;

(** affiche la zone d'affichage après qu'elle est été nettoyée
    @author Alexei *)
let draw_matrix(cur, param, mymat : t_cur_shape * t_param * t_color matrix) : unit = 
  let x : int = param.mat_szx
  and y : int = param.mat_szy
  and base_draw : t_point = param.graphics.base in
  (
    clear_graph();
    draw_frame(base_draw, x, y, param.graphics.dilat);
    for i  = 0 to y - 1
    do
      for j = 0 to x - 1
      do
        if mymat.(i).(j) <> white
        then drawfill_absolute_pt({x = j; y = i}, base_draw, param.graphics.dilat, mymat.(i).(j))
      done
    done
  )
;;

(* ----------------------------------------------- *)
(** {%html: <h2>Types et fonctions graphique</h2>%}*)
(* ----------------------------------------------- *)

(** {%html: <h3>Fonction d'extraction</h3>%}*)

(** sera ajouté à chaque fonction, pour toute fonction d'extraction author Alexei *)

(** get *)
let getParam(play : t_play) : t_param =
  play.par
;;

(** get *)
let getCurShape(play : t_play) : t_cur_shape =
  play.cur_shape
;;

(** get *)
let getMat(play : t_play) : t_color matrix =
  play.mat
;;

(** get *)
let getTime(prm : t_param) : t_param_time =
  prm.time
;;

(** get *)
let getMat_szx(prm : t_param) : int =
  prm.mat_szx
;;

(** get *)
let getMat_szy(prm : t_param) : int =
  prm.mat_szy
;;

(** get *)
let getGraphics(prm : t_param) : t_param_graphics =
  prm.graphics
;;

(** get *)
let getShapes(prm : t_param) : t_shape t_array =
  prm.shapes
;;

(** get *)
let getInitTime(time : t_param_time) : float =
  time.init
;;

(** get *)
let getExtent(time : t_param_time) : float =
  time.extent
;;

(** get *)
let getRatio(time : t_param_time) : float =
  time.ratio
;;

(** get *)
let getBase(graphics : t_param_graphics) : t_point =
  graphics.base
;;

(** get *)
let getDilat(graphics : t_param_graphics) : int =
  graphics.dilat
;;

(** get *)
let getColorArr(graphics : t_param_graphics) : t_color t_array =
  graphics.color_arr
;;

(** get *)
let getShape(shape : t_shape) : t_point list =
  shape.shape
;;

(** get *)
let getXLen(shape : t_shape) : int =
  shape.x_len
;;

(** get *)
let getYLen(shape : t_shape) : int =
  shape.y_len
;;

(** get *)
let getRotRgtBase(shape : t_shape) : t_point  =
  shape.rot_rgt_base
;;

(** get *)
let getRotRgtShape(shape : t_shape) : int =
  shape.rot_rgt_shape
;;

(** get *)
let getRotLftBase(shape : t_shape) : t_point =
  shape.rot_lft_base
;;

(** get *)
let getRotLftShape(shape : t_shape) : int =
  shape.rot_lft_shape
;;

(** get *)
let getX(point : t_point) : int =
  point.x
;;

(** get *)
let getY(point : t_point) : int =
  point.y
;;

(** {%html: <h3>Fonction d'affichage</h3>%}*)

(** color_choice  is a function that allows you to randomly choose a color
    @param t is a color array from which the colors are chosen
    @author Alexei and doc Styven *)
let color_choice(t : t_color t_array) : t_color =
  let rand_color : int = rand_int(0, t.len-1) in
  t.value.(rand_color)
;;

(** cur_shape_choice is a function that returns as value, the randomly chosen shape as well as its color and its position
    @param shapes is an array made up of the shapes available in the game
    @param mat_szx is the dimension of the matrix that corresponds to the workspace on the x axis
    @param mat_szy is the dimension of the matrix that corresponds to the workspace on the y axis
    @param color_arr is the color chart available
    @author Loan and doc Styven *)
let cur_shape_choice(shapes, mat_szx, mat_szy, color_arr : t_shape t_array * int * int * t_color t_array) : t_cur_shape =
  let rand_shape : int = rand_int(0, shapes.len - 1) in
  let rand_x : int = rand_int(0, mat_szx - shapes.value.(rand_shape).x_len)
  and y : int = mat_szy - 2
  and rand_color : int = rand_int(0, color_arr.len -1) in
  {base = ref {x = rand_x; y = y}; shape = ref rand_shape; color = ref color_arr.value.(rand_color)}
;;

(** insert performs the insertion of the described form in the display space. The result of the function is a boolean whose
    value is true if the shape can be inserted without "colliding" with a shape earlier, and false otherwise
    @param cur allow in particular to know the base point of the shape as well as its color
    @param shape contains the coordinates of the points of the abstract shape that remain to be tested and displayed
    @param param describes game settings
    @param mymat describes the workspace
    @author Styven and doc Styven *)
let rec insert(cur, shape, param, mymat : t_cur_shape * t_point list * t_param * t_color matrix) : bool =
  if isempty(shape)
  then true
  else
    let my_point : t_point = fst(shape) in
    if mymat.(!(cur.base).y + my_point.y).(!(cur.base).x + my_point.x) = white
    then
      (
        (drawfill_pt_list(shape, !(cur.base), param.graphics.base, param.graphics.dilat, !(cur.color)));
        insert(cur, rem_fst(shape), param, mymat)
      )
    else false
;;

(** init_play carries out the initialization of the game, i.e.:
    — initialize game settings and workspace;
    — chooses and inserts the first concrete form;
    — draws the frame of the display space;
    — results in the game description thus defined.
    @author Nicolas and doc Styven *)
let init_play() : t_play =
    let prm : t_param = init_param() in
    let play : t_play = { par = prm; cur_shape = cur_shape_choice(prm.shapes,prm.mat_szx, prm.mat_szy, prm.graphics.color_arr); mat = mat_make(prm.mat_szy, prm.mat_szx, white)}
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
(** {%html: <h2> Deplacements et controle des deplacements</h2>%}*)
(* ----------------------------------------------- *)
(* ----------------------------------------------- *)

(** {%html: <h3>Choix des deplacements suivant le caractere saisi</h3>%}*)

(** valid_matrix_point tests if a point p is valid with respect to the dimensions of the workspace
    @param p corresponds to the coordinates of a basis point
    @param param matches game settings
    @author Alexei and doc Styven *)
let valid_matrix_point(p, param : t_point * t_param ) : bool =
  (p.x >= 0 && p.x <= param.mat_szx - 1) && (p.y >= 0 && p.y <= param.mat_szy -1)
;;

(** is_free_move tests whether the shape described by the base point p and
    the list of shape points of an abstract shape does not collide with an inserted shape previously.
    @param p corresponds to the coordinates of a basis point
    @param shape contains the coordinates of the points of the abstract shape
    @param mymat describes the workspace
    @param param matches game settings
    @author Alexei and doc Styven *)
let rec is_free_move(p, shape, mymat,param :t_point * t_point list * t_color matrix * t_param) : bool =
  if isempty(shape)
  then true
  else
      let temp_point : t_point = fst(shape) in
      if  valid_matrix_point({x = p.x + temp_point.x; y = p.y + temp_point.y}, param)
            then if mymat.(temp_point.y + p.y).(temp_point.x + p.x) = white
                 then is_free_move(p, rem_fst(shape), mymat, param)
                 else false
      else false
;;

(** move_left move, if possible of the current form of a square on the left
    @param pl is used to represent game information
    @author Styven and doc Styven *)
let move_left(pl : t_play) : unit =
  let cur_shape : t_cur_shape = {base = ref {x = !(pl.cur_shape.base).x - 1; y = !(pl.cur_shape.base).y - 1}; shape = pl.cur_shape.shape; color = pl.cur_shape.color} in
  if is_free_move(!(cur_shape.base), pl.par.shapes.value.(!(cur_shape.shape)).shape, pl.mat, pl.par)
  then
    (
      draw_matrix(cur_shape, pl.par, pl.mat);
      insert(cur_shape, pl.par.shapes.value.(!(cur_shape.shape)).shape, pl.par, pl.mat);
      pl.cur_shape.base := !(cur_shape.base);
      pl.cur_shape.shape := !(cur_shape.shape);
      pl.cur_shape.color := !(cur_shape.color);
    )
;;

(** move_right move, if possible of the current form of a square on the right
    @param pl is used to represent game information
    @author Alexei and doc Styven *)
let move_right(pl : t_play) : unit =
    let cur_shape : t_cur_shape = {base = ref {x = !(pl.cur_shape.base).x + 1; y = !(pl.cur_shape.base).y - 1}; shape = pl.cur_shape.shape; color = pl.cur_shape.color} in
    if is_free_move(!(cur_shape.base), pl.par.shapes.value.(!(cur_shape.shape)).shape, pl.mat, pl.par)
    then
      (
        draw_matrix(cur_shape, pl.par, pl.mat);
        insert(cur_shape, pl.par.shapes.value.(!(cur_shape.shape)).shape, pl.par, pl.mat);
        pl.cur_shape.base := !(cur_shape.base);
        pl.cur_shape.shape := !(cur_shape.shape);
        pl.cur_shape.color := !(cur_shape.color);
      )
;;

(** move_down move, if possible of the current shape one box down (the Boolean result indicates whether the
    move has been made)
    @param pl is used to represent game information
    @author Alexei and doc Styven *)
let move_down(pl : t_play) : bool =
    let cur_shape : t_cur_shape = {base = ref {x = !(pl.cur_shape.base).x; y = !(pl.cur_shape.base).y - 1}; shape = pl.cur_shape.shape; color = pl.cur_shape.color} in
      if is_free_move(!(cur_shape.base), pl.par.shapes.value.(!(cur_shape.shape)).shape, pl.mat, pl.par)
      then
        (
           draw_matrix(cur_shape, pl.par, pl.mat);
           pl.cur_shape.base := !(cur_shape.base);
           pl.cur_shape.shape := !(cur_shape.shape);
           pl.cur_shape.color := !(cur_shape.color);
           insert(cur_shape, pl.par.shapes.value.(!(cur_shape.shape)).shape, pl.par, pl.mat);
        )
      else false
;;

(** rotate_right carry out, if possible, a rotate the current shape to the right
    @param pl is used to represent game information
    @author ? *)
let rotate_right(pl : t_play) : unit =
  ()
;;

(** rotate_left carry out, if possible, a rotate the current shape to the left
    @param pl is used to represent game information
    @author ? *)
let rotate_left(pl : t_play) : unit =
  ()
;;

(** move_at_bottom move the vertically current form as low as possible, until reaching the bottom of the workspace,
    or being blocked by a previously fixed form
    @param pl is used to represent game information
    @author Nicolas and doc Styven *)
let move_at_bottom(pl : t_play) : unit =
  let cur_shape : t_cur_shape = {base = ref {x = !(pl.cur_shape.base).x; y = !(pl.cur_shape.base).y - 1}; shape = pl.cur_shape.shape; color = pl.cur_shape.color} in
  while (!(cur_shape.base).y <> 0 || is_free_move(!(cur_shape.base), pl.par.shapes.value.(!(cur_shape.shape)).shape, pl.mat, pl.par))
  do
    move_down(pl)
  done;
  ()
;;

(** move wall lamp the displacement indicated by the character dir. The meaning of a character resulting in the application
    of an operation is obvious. The character 'v' is used to indicate a displacement of the lowest possible form: in this case, 
    the operation is not performed by the move function, but by its calling function new_step
    @param pl is used to represent game information
    @param dir  is used to associate a key with a movement
    @author doc Styven *)
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
                 
(* ------------------------------------------------- *)
(* ------------------------------------------------- *)
(** {%html: <h2>Suppression des lignes pleines</h2>%}*)
(* ------------------------------------------------- *)
(* ------------------------------------------------- *)

(** is_column_full tests if a column of the mymat matrix is ​​"full"
    @param mymat describes the workspace
    @param y  is the ordinate of the points of the column
    @param mat_szx is the length of a row of the matrix
    @author Loan and doc Styven *)
let is_column_full(y, mymat, mat_szx : int * t_color matrix * int) : bool =
  let is_full : bool ref = ref true in
  for i = 0 to mat_szx - 1
  do
   if mymat.(y).(i) = white
   then is_full := false
  done;
  !is_full
;;

(** decal shifts down the columns of the mymat matrix of ordinates strictly greater than y
    @param mymat describes the workspace 
    @param y is the ordinate of the points of the column
    @param szx and @param szy are the dimensions of the matrix
    @param par matches game settings
    @author Alexei and doc Syven *)
let decal(mymat, y, szx, szy, par : t_color matrix * int * int * int * t_param) : unit =
  for i = y to szy - 3
  do
    for j = 0 to szx - 1
    do
      mymat.(i).(j) <- mymat.(i + 1).(j)
    done
  done;
;;

let rec clear_play_aux(pl, i : t_play * int) : unit =
  if i = pl.par.mat_szy - 2
  then ()
  else
    if is_column_full(i, pl.mat, pl.par.mat_szx)
    then
      (decal(pl.mat, i, pl.par.mat_szx, pl.par.mat_szy, pl.par); clear_play_aux(pl, i))
    else clear_play_aux(pl, i+1)
;;

(** clear_play “cleans” the workspace; she deletes all "lines" solid
    @param pl is used to represent game information
    @author Styven and doc Styven *)
let clear_play(pl : t_play) : unit =
  clear_play_aux(pl,0)
;;
  
(*
let clear_play(pl : t_play) : unit =
  for i = 0 to pl.par.mat_szy - 2 (*la ligne szy - 1 ne peut être pleine*)
  do
    if is_column_full(i, pl.mat, pl.par.mat_szx)
    then decal(pl.mat, i, pl.par.mat_szx, pl.par.mat_szy, pl.par)
  done
;;
 *)

(** final_insert "freezes" the current form, described by the form description cur and
    the list of shape points; in other words, the shape is inserted into the matrix
    @author Alexei *)
let rec final_insert(cur, shape, mymat : t_cur_shape * t_point list * t_color matrix) : t_color matrix =
  if isempty(shape)
  then mymat
  else
    let my_point : t_point = fst(shape) in
    (
      (mymat.(!(cur.base).y + my_point.y).(!(cur.base).x + my_point.x) <- !(cur.color));
      final_insert(cur, rem_fst(shape), mymat)
    )
;;

(** final_newstep performs the last phase of a step of game. It tests if the current form can still move. 
    If so, the function does nothing, if not, it means the form does not can no longer move. 
    The form is inserted, the solid "lines" are deleted, a new shape is chosen.
    In any case, the boolean result of the function indicates whether the game should stop.
    @param pl is used to represent game information
    @author Nicolas and Alexei and doc Loan *)
let final_newstep(pl : t_play) : bool =
  let new_cur_shape : t_cur_shape = cur_shape_choice(pl.par.shapes, pl.par.mat_szx, pl.par.mat_szy, pl.par.graphics.color_arr)
  and the_end : bool = !(pl.cur_shape.base).y == pl.par.mat_szy - 2 in
  (
    if is_free_move(!(pl.cur_shape.base), pl.par.shapes.value.(!(pl.cur_shape.shape)).shape, pl.mat, pl.par)
    then 
      (
        final_insert(pl.cur_shape,  pl.par.shapes.value.(!(pl.cur_shape.shape)).shape, pl.mat);
        pl.cur_shape.base := !(new_cur_shape.base);
        pl.cur_shape.shape := !(new_cur_shape.shape);
        pl.cur_shape.color := !(new_cur_shape.color);
        clear_play(pl);

      );
    the_end
  )
;;

(* ----------------------------------- *)
(* ----------------------------------- *)
(** {%html: <h2>Une étape de jeu</h2>%}*)
(* ----------------------------------- *)
(* ----------------------------------- *)

(** newstep manages a "step" of play. A game step executes within a certain time dt; new_t and t parameters match
    respectively at the "current time" and at the time at which a new game stage has started.
    In a first phase, the function tests whether the player has entered a command: if so,
    the command is taken into account, by calling the move function, the result of which is stored
    in the mutable variable dec. The variable dir then takes another value, which does not correspond to
    no move command (here, the value 'x'). The value of dec indicates whether the command
    possibly entered corresponds to a displacement of the shape at the bottom of the workspace.
    If so, the command is executed, otherwise the shape moves one "line" down. If the
    shape can no longer move, the final_newstep function is applied *)
let newstep(pl, new_t, t, dt : t_play * float ref * float * float) : bool = 
  let the_end : bool ref = ref (!new_t -. t > dt) and dec : bool ref = ref false in
  let dir : char ref = ref 'x' and notmove : bool ref = ref false in
    (
    while not(!the_end)
    do 
      if key_pressed()
      then dir := read_key()
      else () ;
      dec := move(pl, !dir);
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

(* -------------------------------------- *)
(* -------------------------------------- *)
(** {%html: <h2>Fonction principale</h2>%}*)
(* -------------------------------------- *)
(* -------------------------------------- *)

(** jeuCP2 is a function which is essentially a loop making it possible to chain the stages of the game, and the management acceleration *)
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
