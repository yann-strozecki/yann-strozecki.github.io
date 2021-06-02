/*
  =================
  INIT WITH EXAMPLE
  =================
*/

var initlist =    [1,0,0,2,3,0,0,0,0,4,5,6,7,0,0,0,0,0,0,0,0,8,9,10,11,12,13,14,15,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,16,17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,32];

var board = document.getElementById("board");
var output = document.getElementById("output");

var globalstate = initial_state(initlist);


print_board(board, globalstate);
update_info(initlist);

/*
  =============
  ADD LISTENERS
  =============
*/

document.getElementById('file_list').addEventListener('change', load_list, false);
document.getElementById('nextstep').addEventListener('click', next_step, false);
document.getElementById('play').addEventListener('click', toggle, false);
document.getElementById('reset').addEventListener('click', reset, false);
document.onkeydown = keyboard;
