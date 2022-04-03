open CPutil;;
open JeuCP2;;

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

let test_draw_absolute_pt_functional(status : t_test_status) : unit =
  let test_step : t_test_step =  test_start(status,"draw_absolute_pt_functional)") 
  and p : t_point = {x = 0; y = 0}
  and base_draw : t_point = {x = 50; y = 50}
  and dilat : int = 50
  and col : t_color = black
  and result : string ref = ref "" in
  (
    clear_graph();
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

let test_fill_absolute_pt_functional(status : t_test_status) : unit =
  let test_step : t_test_step =  test_start(status,"fill_absolute_pt_functional)") 
  and p : t_point = {x = 0; y = 0}
  and base_draw : t_point = {x = 50; y = 50}
  and dilat : int = 50
  and col : t_color = red
  and result : string ref = ref "" in
  (
    clear_graph();
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

let test_drawfill_absolute_pt_functional(status : t_test_status) : unit =
  let test_step : t_test_step =  test_start(status,"drawfill_absolute_pt_functional)") 
  and p : t_point = {x = 0; y = 0}
  and base_draw : t_point = {x = 50; y = 50}
  and dilat : int = 50
  and col : t_color = red
  and result : string ref = ref "" in
  (
    clear_graph();
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

let test_draw_relative_pt_functional(status : t_test_status) : unit =
  let test_step : t_test_step =  test_start(status,"draw_relative_pt_functional)") 
  and p : t_point = {x = 0; y = 0}
  and base_draw : t_point = {x = 50; y = 50}
  and base_point : t_point = {x = 1; y = 1}
  and dilat : int = 50
  and col : t_color = black
  and result : string ref = ref "" in
  (
    clear_graph();
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

let test_fill_relative_pt_functional(status : t_test_status) : unit =
  let test_step : t_test_step =  test_start(status,"fill_relative_pt_functional)") 
  and p : t_point = {x = 0; y = 0}
  and base_draw : t_point = {x = 50; y = 50}
  and base_point : t_point = {x = 1; y = 1}
  and dilat : int = 50
  and col : t_color = red
  and result : string ref = ref "" in
  (
    clear_graph();
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

let test_drawfill_relative_pt_functional(status : t_test_status) : unit =
  let test_step : t_test_step =  test_start(status,"drawfill_relative_pt_functional)") 
  and p : t_point = {x = 0; y = 0}
  and base_draw : t_point = {x = 50; y = 50}
  and base_point : t_point = {x = 1; y = 1}
  and dilat : int = 50
  and col : t_color = red
  and result : string ref = ref "" in
  (
    clear_graph();
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

let test_drawfill_relative_pt_functional(status : t_test_status) : unit =
  let test_step : t_test_step =  test_start(status,"drawfill_relative_pt_functional)") 
  and p : t_point = {x = 0; y = 0}
  and base_draw : t_point = {x = 50; y = 50}
  and base_point : t_point = {x = 1; y = 1}
  and dilat : int = 50
  and col : t_color = red
  and result : string ref = ref "" in
  (
    clear_graph();
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

let test_run() : unit =
  let alltests : t_test_status = create_test_status() in
    (
      test_draw_absolute_pt_functional(alltests);
      test_fill_absolute_pt_functional(alltests);
      test_drawfill_absolute_pt_functional(alltests);
      test_draw_relative_pt_functional(alltests);
      test_fill_relative_pt_functional(alltests);
      test_drawfill_relative_pt_functional(alltests);
    (* Print test status at the end *)
    print_test_report(alltests)
    )
;;
