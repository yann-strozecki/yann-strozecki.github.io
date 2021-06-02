/*
=======================================================
MANIPULATING STATES
=======================================================
*/

/*
  Return true if the entry e is a solution in the list (currently: if not 0 nor \n)
*/
function is_solution(e) {
    return(e != "0" && e != "\n" && e!="");
}

/* Return the best INC1 delay of a given list */
function incremental_delay(list) {
    var nbsol = 0;
    var bestdelay = 0;
    for(var i = 0; i < list.length; i++) {
        if (is_solution(list[i]))
            nbsol++;

        if (nbsol != 0 && (i+1)/nbsol > bestdelay)
            bestdelay = (i+1)/nbsol;
    }
    return Math.ceil(bestdelay);
}

/* Return the normal delay of a given list */
function normal_delay(list) {
    var dist = 0;
    var maxdist = 0;
    for(var i = 0; i < list.length; i++) {
        if (is_solution(list[i])) {
            maxdist =  (maxdist < dist) ? dist : maxdist;
            dist=0;
        }
        else {
            dist+=1;
        }
    }
    maxdist =  (maxdist < dist) ? dist : maxdist;
    return maxdist;
}

/*
  The state of the algorithm is a dictionnary state s.t.:
  - state["list"] = the list of solutions
  - state["delay"] = the considered INC1 delay (the delay is assumed to hold on state["list"])
  - state["currentpointer"] = the pointer currently moving (initially the last one)
  - state["budget"] = the remaining budget of the current pointer
  - state["pointer"] = a table s.t. state["pointer"][i] is the position of pointer i
*/


/*
  Return the initial state of the algorithm from a list and an INC1 delay.
*/
function initial_state(list) {
    var d = get_info(list);
    var state = {};
    var nbpointers = d["npointers"];
    var delay = d["incdelay"];
    var budget = 2*delay;

    state["list"] = list;
    state["currentpointer"] = nbpointers-1;
    state["delay"] = delay;
    state["budget"] = budget;
    state["pointers"] = [];

    state["totaltime"] = 0;
    state["maxdelay"] = 0;
    state["maxdelaytmp"] = 0;

    for(i = 0; i < nbpointers; i++) {
        state["pointers"].push(-1); // Pointer is outside
    }
    return state;
}

/*
  Compute one step of computation and changes state argument in place.
  Returns a dictionary describing changes:
  - res["type"] :
      - "end" means that the algorithm has ended
      - "move" means that the current pointer has moved forward
      - "change" means that the current pointer has changed to the previous one
  - res["solution"]: if not undefined then contains the solution found.

*/
function next_state(state) {
    var res = {};
    var current = state["currentpointer"];
    var delay = state["delay"];
    var budget = 2*delay;

    state["totaltime"] += 1;

    if (state["budget"] == 0) {
        if (state["currentpointer"] == 0) {
            res["type"] = "end";
            return res;
        }
        else {
            state["budget"] = budget;
            state["currentpointer"]--;
            res["type"] = "change";
            return res;
        }
    }
    else {
        /* Zone of the current pointer */
        var up = Math.pow(2,current)*delay;
        up = up < state["list"].length ? up : state["list"].length;
        var down = current == 0 ? 0 : Math.pow(2,current-1)*delay;
        var i = state["pointers"][current];

        state["maxdelaytmp"]++;

        if (i >= up-1) {
            /* Current pointer is done */
            if (current == 0) {
                res["type"] = "end";
                return res;
            }
            else {
                state["budget"] = budget;
                state["currentpointer"]--;
                res["type"] = "change";
                return res;
            }
        }
        else {
            /* Move current pointer to the right */
            res["type"] = "move";
            state["pointers"][current]++;
            i++;
            state["budget"]--;

            if (is_solution(state["list"][i]) && i < up && i >= down) {
                res["solution"] = state["list"][i];
                res["pointer"] = state["currentpointer"];
                state["currentpointer"] = state["pointers"].length-1;
                state["budget"] = budget;

                state["maxdelay"] = state["maxdelaytmp"] > state["maxdelay"] ? state["maxdelaytmp"] : state["maxdelay"];
                state["maxdelaytmp"] = 0;
            }
            return res;
        }
    }
}

/*
  ================
  DRAWING ON BOARD
  ================
*/

/* Add a separator at the end of board */
function add_sep(board) {
    var sep = document.createElement("div");
    sep.classList.add("sep");
    board.appendChild(sep);
}

function print_board(board, state) {
    var delay = state["delay"];
    var list = state["list"];

    var bound = delay;

    board.innerHTML = "";
    add_sep(board);

    var init = document.createElement("div");

    init.appendChild(document.createTextNode("START"));
    init.id = "start";

    var divpointer = document.createElement("div");
    divpointer.classList.add("pointers_hold");
    init.appendChild(divpointer);

    board.appendChild(init);

    for(i=0; i < list.length; i++) {
        var d = document.createElement("div");
        var v = is_solution(list[i]) ? list[i] : "0";
        var t = document.createTextNode(list[i]);
        d.appendChild(t);
        d.classList.add("cell");

        if (is_solution(list[i])) {
            d.classList.add("solution");
        }

        divpointer = document.createElement("div");
        divpointer.classList.add("pointers_hold");
        d.appendChild(divpointer);

        board.appendChild(d);

        if (i+1==bound) {
            bound *= 2;
            add_sep(board);
        }
    }

    allcell = board.getElementsByClassName("cell");

    for(j = 0; j < state["pointers"].length; j++) {
        var pointer = document.createElement("div");
        pointer.classList.add("pointer");
        if (j == state["currentpointer"])
            pointer.classList.add("current");

        pointer.id = "p"+j.toString();

        pointer.appendChild(document.createTextNode("p"+j.toString()));

        var currentcell;
        if (state["pointers"][j] == -1) {
            currentcell = document.getElementById("start");
        }
        else {
            currentcell = allcell[state["pointers"][j]];
        }
        var holder = currentcell.getElementsByClassName("pointers_hold")[0];
        holder.appendChild(pointer);
    }


    add_sep(board);
}

/*
  Update the board drawing from current state and changes returned by next_state
*/
function update_board(board, state, changes) {
    var current = state["currentpointer"];
    var p = document.getElementById("p"+current.toString());

    document.getElementById("odelay").innerHTML = state["maxdelay"];
    document.getElementById("totaltime").innerHTML = state["totaltime"];
    if (changes["solution"] != undefined) {
        var d = document.createElement("div");
        var t = document.createTextNode(changes["solution"]);
        d.classList.add("cell");
        d.classList.add("solution");

        d.appendChild(t);

        output.appendChild(d);
    }


    if (changes["type"] == "move") {
        allcell = board.getElementsByClassName("cell");
        currentcell = allcell[state["pointers"][current]];
        var holder = currentcell.getElementsByClassName("pointers_hold")[0];
        holder.appendChild(p);
    }
    if (changes["type"] == "change") {
        var old = document.getElementsByClassName("current");
        for(i=0;i<old.length;i++)
            old[i].classList.remove("current");
        p.classList.add("current");
    }
}

function get_info(list) {
    var idelay = incremental_delay(list);
    var l = list.length;
    return {
        "incdelay" : idelay,
        "normaldelay" : normal_delay(list),
        "length" : l,
        "npointers"  : Math.ceil(1+Math.log(l/idelay)/Math.log(2))
    };
}

function update_info(list) {
    var d = get_info(list);
    document.getElementById("incdelay").innerHTML = d["incdelay"];
    document.getElementById("npointers").innerHTML = d["npointers"];
    document.getElementById("listsize").innerHTML = d["length"];
    document.getElementById("normaldelay").innerHTML = d["normaldelay"];
    document.getElementById("maxdelay").innerHTML = 2*d["npointers"]*d["incdelay"];
}

function next_step() {
    var change = next_state(globalstate);
    update_board(board, globalstate, change);
    return change;
}


var hold = true;

function toggle() {
    hold = !hold;
    if (!hold) {
        document.getElementById('play').innerHTML = "Hold";
        play();
    } else {
        document.getElementById('play').innerHTML = "Play";
    }
}

function play() {
    var change = next_step();
    if (change["type"] == "end") {
        document.getElementById('play').innerHTML = "Finished";
        hold = true;
    } else if (!hold) {
        var speed = parseInt(document.getElementById("speed").value);
        setTimeout(() => play(), 200/speed);
    }

}

function keyboard(e) {
    e = e || window.event;
    if (e.keyCode == '39') {
        // right arrow
        next_step();
    }
}

function reset() {
    hold = true;
    document.getElementById('play').innerHTML = "Play";
    var list = globalstate["list"];

    globalstate = initial_state(list);

    print_board(board, globalstate);
    document.getElementById("output").innerHTML = "";
    update_info(list);
}

/*
  ===============
  HANDLING EVENTS
  ===============
*/

function load_list(e) {
    var listfile = e.target.files[0];
    var fr = new FileReader();
    fr.onload = function() {
        list = this.result.split('\n');


        var board = document.getElementById("board");
        document.getElementById("output").innerHTML = "";

        globalstate = initial_state(list);
        print_board(board, globalstate);
        update_info(list);
    };

    fr.readAsText(listfile);
};

