(** *)

open CPutil;;
open JeuCP2;;
(** read_test_result is an input control function
    @author Alexeï*)
let read_test_result() : string =
  let result : string ref = ref "" and the_end : bool ref = ref false in
  (
    while not(!the_end)
    do
      result := read_line();
      the_end := (!result = "OK" || !result = "KO")
    done ;
    !result
  )
;;

(** display_test_frame is used to make an graphic anchor during the tests 
    @author Alexeï*)
let display_test_frame(dilat : int) : unit =
  let x : int ref = ref 0
  and y : int ref = ref 0 in
  for i = 0 to 750/dilat (*permet d'obtenir combien de carré tracer; 750 car c'est la taille de la fenêtre graphique *)
  do
    set_color(black);
    x := !x + dilat;
    y := !y + dilat;
    fill_rect(!x - 5, !y - 5, 10,10)
  done
;;

(** test_draw_absolute_pt_functional test the correct printing of an unfilled square with correct parameters
    @author Alexeï *)
let test_draw_absolute_pt_functional(status : t_test_status) : unit =
  let test_step : t_test_step =  test_start(status,"draw_absolute_pt_functional") 
  and p : t_point = {x = 0; y = 0}
  and base_draw : t_point = {x = 50; y = 50}
  and dilat : int = 50
  and col : t_color = black
  and result : string ref = ref "" in
  (
    clear_graph();
    display_test_frame(dilat);
    let test_result : unit t_test_result = test_exec(test_step, draw_absolute_pt, (p, base_draw, dilat,col)) in
    (
      if test_is_success(test_result)
      then 
        (
          print_newline();
          print_string("Si vous avez bien un carré vide affiché en bas à gauche de la zone d'affichage, veuillez entrer : OK sinon KO  ");
          result := read_test_result();
          assert_equals(test_step, "saisie de l'utilisateur", !result, "OK")
        )
      else test_error(test_step) ;
      test_end(test_step)
    )
  )
;;

(** test_fill_absolute_pt_functional test the correct printing of a filled square with correct parameters
    @author Alexeï *)
let test_fill_absolute_pt_functional(status : t_test_status) : unit =
  let test_step : t_test_step =  test_start(status,"fill_absolute_pt_functional") 
  and p : t_point = {x = 0; y = 0}
  and base_draw : t_point = {x = 50; y = 50}
  and dilat : int = 50
  and col : t_color = red
  and result : string ref = ref "" in
  (
    clear_graph();
    display_test_frame(dilat);
    let test_result : unit t_test_result = test_exec(test_step, fill_absolute_pt, (p, base_draw, dilat,col)) in
    (
      if test_is_success(test_result)
      then 
        (
          print_newline();
          print_string("Si vous avez bien un carré plein de couleur rouge affiché en bas à gauche de la zone d'affichage, veuillez entrer : OK sinon KO  ");
          result := read_test_result();
          assert_equals(test_step, "saisie de l'utilisateur", !result, "OK")
        )
      else test_error(test_step) ;
      test_end(test_step)
    )
  )
;;
(** test_drawfill_absolute_pt_functional test the correct printing of a filled square with contour and with correct parameters
    @author Alexeï *)
let test_drawfill_absolute_pt_functional(status : t_test_status) : unit =
  let test_step : t_test_step =  test_start(status,"drawfill_absolute_pt_functional") 
  and p : t_point = {x = 0; y = 0}
  and base_draw : t_point = {x = 50; y = 50}
  and dilat : int = 50
  and col : t_color = red
  and result : string ref = ref "" in
  (
    clear_graph();
    display_test_frame(dilat);
    let test_result : unit t_test_result = test_exec(test_step, drawfill_absolute_pt, (p, base_draw, dilat,col)) in
    (
      if test_is_success(test_result)
      then 
        (
          print_newline();
          print_string("Si vous avez bien un carré plein de couleur rouge aux contours noirs affiché en bas à gauche de la zone d'affichage, veuillez entrer : OK sinon KO  ");
          result := read_test_result();
          assert_equals(test_step, "saisie de l'utilisateur", !result, "OK")
        )
      else test_error(test_step) ;
      test_end(test_step)
    )
  )
;;

(** test_draw_relative_pt_functional test the correct printing of an unfilled square which have a relative position, with correct parameters
    @author Alexeï *)
let test_draw_relative_pt_functional(status : t_test_status) : unit =
  let test_step : t_test_step =  test_start(status,"draw_relative_pt_functional") 
  and p : t_point = {x = 0; y = 0}
  and base_draw : t_point = {x = 50; y = 50}
  and base_point : t_point = {x = 1; y = 1}
  and dilat : int = 50
  and col : t_color = black
  and result : string ref = ref "" in
  (
    clear_graph();
    display_test_frame(dilat);
    let test_result : unit t_test_result = test_exec(test_step, draw_relative_pt, (p, base_point, base_draw, dilat,col)) in
    (
      if test_is_success(test_result)
      then 
        (
          print_newline();
          print_string("Si vous avez bien un carré vide affiché en bas à gauche, de coordonées (1,1) dans la zone d'affichage, veuillez entrer : OK sinon KO  ");
          result := read_test_result();
          assert_equals(test_step, "saisie de l'utilisateur", !result, "OK")
        )
      else test_error(test_step) ;
      test_end(test_step)
    )
  )
;;

(** test_fill_relative_pt_functional test the correct printing of a filled square which have a relative position, with correct parameters
    @author Alexeï *)
let test_fill_relative_pt_functional(status : t_test_status) : unit =
  let test_step : t_test_step =  test_start(status,"fill_relative_pt_functional") 
  and p : t_point = {x = 0; y = 0}
  and base_draw : t_point = {x = 50; y = 50}
  and base_point : t_point = {x = 1; y = 1}
  and dilat : int = 50
  and col : t_color = red
  and result : string ref = ref "" in
  (
    clear_graph();
    display_test_frame(dilat);
    let test_result : unit t_test_result = test_exec(test_step, fill_relative_pt, (p, base_point, base_draw, dilat,col)) in
    (
      if test_is_success(test_result)
      then 
        (
          print_newline();
          print_string("Si vous avez bien un carré plein rouge affiché en bas à gauche, de coordonées (1,1) dans la zone d'affichage, veuillez entrer : OK sinon KO  ");
          result := read_test_result();
          assert_equals(test_step, "saisie de l'utilisateur", !result, "OK")
        )
      else test_error(test_step) ;
      test_end(test_step)
    )
  )
;;

(** test_drawfill_relative_pt_functional test the correct printing of a filled square with contour which have a relative position, with correct parameters
    @author Alexeï *)
let test_drawfill_relative_pt_functional(status : t_test_status) : unit =
  let test_step : t_test_step =  test_start(status,"drawfill_relative_pt_functional") 
  and p : t_point = {x = 0; y = 0}
  and base_draw : t_point = {x = 50; y = 50}
  and base_point : t_point = {x = 1; y = 1}
  and dilat : int = 50
  and col : t_color = red
  and result : string ref = ref "" in
  (
    clear_graph();
    display_test_frame(dilat);
    let test_result : unit t_test_result = test_exec(test_step, drawfill_relative_pt, (p, base_point, base_draw, dilat,col)) in
    (
      if test_is_success(test_result)
      then 
        (
          print_newline();
          print_string("Si vous avez bien un carré plein rouge de contour noir affiché en bas à gauche, de coordonées (1,1) dans la zone d'affichage, veuillez entrer : OK sinon KO  ");
          result := read_test_result();
          assert_equals(test_step, "saisie de l'utilisateur", !result, "OK")
        )
      else test_error(test_step) ;
      test_end(test_step)
    )
  )
;;

(** test_draw_pt_list_functional test the correct printing of a list of unfilled square which have a relative position, with correct parameters
    @author Alexeï *)
let test_draw_pt_list_functional(status : t_test_status) : unit =
  let test_step : t_test_step =  test_start(status,"draw_pt_list_functional") 
  and p_list : t_point list = [{x = 0; y = 0}; {x = 0; y = 2}; {x = 2; y = 0}; {x = 2; y = 2}]
  and base_draw : t_point = {x = 50; y = 50}
  and base_point : t_point = {x = 0; y = 0}
  and dilat : int = 50
  and col : t_color = black
  and result : string ref = ref "" in
  (
    clear_graph();
    display_test_frame(dilat);
    let test_result : unit t_test_result = test_exec(test_step, draw_pt_list, (p_list, base_point, base_draw, dilat,col)) in
    (
      if test_is_success(test_result)
      then 
        (
          print_newline();
          print_string("Si vous avez bien 4 carrés de contour noir espacés d'un carré chacun dans la zone d'affichage, veuillez entrer : OK sinon KO  ");
          result := read_test_result();
          assert_equals(test_step, "saisie de l'utilisateur", !result, "OK")
        )
      else test_error(test_step) ;
      test_end(test_step)
    )
  )
;;

(** test_fill_pt_list_functional test the correct printing of a list of filled square which have a relative position, with correct parameters
    @author Alexeï *)
let test_fill_pt_list_functional(status : t_test_status) : unit =
  let test_step : t_test_step =  test_start(status,"fill_pt_list_functional") 
  and p_list : t_point list = [{x = 0; y = 0}; {x = 0; y = 2}; {x = 2; y = 0}; {x = 2; y = 2}]
  and base_draw : t_point = {x = 50; y = 50}
  and base_point : t_point = {x = 0; y = 0}
  and dilat : int = 50
  and col : t_color = red
  and result : string ref = ref "" in
  (
    clear_graph();
    display_test_frame(dilat);
    let test_result : unit t_test_result = test_exec(test_step, fill_pt_list, (p_list, base_point, base_draw, dilat,col)) in
    (
      if test_is_success(test_result)
      then 
        (
          print_newline();
          print_string("Si vous avez bien 4 carrés pleins rouges espacés d'un carré chacun dans la zone d'affichage, veuillez entrer : OK sinon KO  ");
          result := read_test_result();
          assert_equals(test_step, "saisie de l'utilisateur", !result, "OK")
        )
      else test_error(test_step) ;
      test_end(test_step)
    )
  )
;;

(** test_drawfill_pt_list_functional test the correct printing of a list of filled square with contour which have a relative position, with correct parameters
    @author Alexeï *)
let test_drawfill_pt_list_functional(status : t_test_status) : unit =
  let test_step : t_test_step =  test_start(status,"drawfill_pt_list_functional") 
  and p_list : t_point list = [{x = 0; y = 0}; {x = 0; y = 2}; {x = 2; y = 0}; {x = 2; y = 2}]
  and base_draw : t_point = {x = 50; y = 50}
  and base_point : t_point = {x = 0; y = 0}
  and dilat : int = 50
  and col : t_color = red
  and result : string ref = ref "" in
  (
    clear_graph();
    display_test_frame(dilat);
    let test_result : unit t_test_result = test_exec(test_step, drawfill_pt_list, (p_list, base_point, base_draw, dilat,col)) in
    (
      if test_is_success(test_result)
      then 
        (
          print_newline();
          print_string("Si vous avez bien 4 carrés pleins rouges au contour noir espacés d'un carré chacun dans la zone d'affichage, veuillez entrer : OK sinon KO  ");
          result := read_test_result();
          assert_equals(test_step, "saisie de l'utilisateur", !result, "OK")
        )
      else test_error(test_step) ;
      test_end(test_step)
    )
  )
;;

(** test_draw_frame_functional test the correct printing of the display frame, with correct parameters
    @author Alexeï *)
let test_draw_frame_functional(status : t_test_status) : unit =
  let test_step : t_test_step =  test_start(status,"draw_frame_functional")
  and size_x : int = 15
  and size_y : int = 28
  and dilat : int = 20
  and base_draw : t_point = {x = 50; y = 50}
  and result : string ref = ref "" in
    (
    clear_graph();
    let test_result : unit t_test_result = test_exec(test_step, draw_frame, (base_draw, size_x, size_y, dilat)) in
    (
      if test_is_success(test_result)
      then 
        (
          print_newline();
          print_string("Si vous avez bien 2 segments de longeur 28 et un de longueur 15, de couleur noire, veuillez entrer : OK sinon KO  ");
          result := read_test_result();
          assert_equals(test_step, "saisie de l'utilisateur", !result, "OK")
        )
      else test_error(test_step) ;
      test_end(test_step)
    )
  )

;;

(** Tests if the outcome of the function getParam is extracted from the type t_play
    @param status saves the result of the test function 
    @author Loan *)
let test_getParam_functional(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "getParam_functional")
  and play : t_play = init_play() in
  let test_result : t_param t_test_result = test_exec(test_step, getParam, play) in
  (
    if test_is_success(test_result)
    then assert_equals(test_step, "Le t_param est bien extrait du t_play", test_get(test_result), init_param())
    else test_error(test_step);
    test_end(test_step)
  )
;;

(** test_color_choice_functional test if the function return a random color in a t_color t_array
    @author Alexeï  *)
let test_color_choice_functional(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status, "color_choice_functional")
  and color_arr : t_color t_array = {len = 1; value = [|blue|]} in
  let test_result : t_color t_test_result = test_exec(test_step, color_choice, color_arr) in
  (
    if test_is_success(test_result)
    then assert_equals(test_step, "La fonction color_choice extrait bien une couleur du tableau", test_get(test_result), blue)
    else test_error(test_step);
    test_end(test_step)
  )
;;

(** test_cur_shape_choice_functional is used to test if cur_shape_choice return a valid t_cur_shape
    @author Alexeï*)
let test_cur_shape_choice_functional(status : t_test_status) : unit =
  let test_step : t_test_step = test_start(status,"cur_shape_choice_functional")
  and shapes : t_shape t_array = init_shapes()
  and mat_szx : int = 15
  and mat_szy : int = 28
  and colors : t_color t_array = init_color() in
  let test_result : t_cur_shape t_test_result = test_exec(test_step, cur_shape_choice, (shapes, mat_szx, mat_szy, colors)) in
  (
    assert_true(test_step, "la forme a bien été créée", test_is_success(test_result));
    test_end(test_step)
  )
;;

(** test_insert_functional is used to test if insert display correctly the cur_shape when he is valid
    @author Alexeï*)
let test_insert_functional(status : t_test_status ) : unit =
  let test_step : t_test_step = test_start(status, "insert_functional")
  and shapes : t_shape t_array = init_shapes() in
  let cur : t_cur_shape = cur_shape_choice(shapes, 15, 28, init_color()) in
  let shape : t_point list =  shapes.value.(!(cur.shape)).shape
  and param : t_param = init_param()
  and mymat : t_color matrix = mat_make(28, 15, white)
  and result : string ref = ref "" in
  (
    clear_graph();
    let test_result : bool t_test_result = test_exec(test_step, insert, (cur, shape, param, mymat)) in
    (
      if test_is_success(test_result)
      then 
        (
          print_newline();
          print_string("Si vous avez bien un carré ou une barre composé de 4 carrés, et de couleur aléatoire, veuillez entrer : OK sinon KO  ");
          result := read_test_result();
          assert_equals(test_step, "saisie de l'utilisateur", !result, "OK")
        )
      else test_error(test_step) ;
      test_end(test_step)
    )
  )
;;

(** test_cur_shape_choice_functional is used to test if insert return false when the cur_shape is on an already used area
    @author Alexeï*)
let test_insert_structural(status : t_test_status ) : unit =
  let test_step : t_test_step = test_start(status, "insert_structural")
  and shapes : t_shape t_array = init_shapes() in
  let cur : t_cur_shape = cur_shape_choice(shapes, 15, 28, init_color()) in
  let shape : t_point list =  shapes.value.(!(cur.shape)).shape
  and param : t_param = init_param()
  and mymat : t_color matrix = mat_make(28, 15, black) in
  (
    clear_graph();
    let test_result : bool t_test_result = test_exec(test_step, insert, (cur, shape, param, mymat)) in
    (
      if test_is_success(test_result)
      then assert_equals(test_step, "zone d'affichage occupée", test_get(test_result), false)
      else test_error(test_step);
      test_end(test_step)
    )
  ) 
;;

let test_run() : unit =
  let alltests : t_test_status = create_test_status() in
    (
      test_draw_absolute_pt_functional(alltests);
      test_fill_absolute_pt_functional(alltests);
      test_drawfill_absolute_pt_functional(alltests);
      test_draw_relative_pt_functional(alltests);
      test_fill_relative_pt_functional(alltests);
      test_drawfill_relative_pt_functional(alltests);
      test_draw_pt_list_functional(alltests);
      test_fill_pt_list_functional(alltests);
      test_drawfill_pt_list_functional(alltests);
      test_draw_frame_functional(alltests);
      test_getParam_functional(alltests);
      test_color_choice_functional(alltests);
      test_cur_shape_choice_functional(alltests);
      test_insert_functional(alltests);
      test_insert_structural(alltests);
    (* Print test status at the end *)
    print_test_report(alltests)
    )
;;
