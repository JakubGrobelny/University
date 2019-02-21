var number_of_squares_per_quarter = 4;

function intersect(x0, y0, x1, y1, size0, size1) {
    x0 = parseInt(x0, 10);
    x1 = parseInt(x1, 10);
    y0 = parseInt(y0, 10);
    y1 = parseInt(y1, 10);
    size0 = parseInt(size0, 10);
    size1 = parseInt(size1, 10);

    return (x0 <= x1 + size1) && (x0 + size0 >= x1) && 
           (y0 <= y1 + size1) && (y0 + size0 >= y1);
}

function collides_with_others(index, squares) {
    var square = squares[index];
    const size = square.offsetWidth;
    const sx = getComputedStyle(square).left;
    const sy = getComputedStyle(square).top;

    for (var i = 0; i < squares.length; i++) {
        if (i != index) {
            other_square = squares[i];
            osx = getComputedStyle(other_square).left;
            osy = getComputedStyle(other_square).top;

            if (intersect(sx, sy, osx, osy, size, size)) {
                return true;
            }
        }
    }

    var central = document.getElementById('central');

    return intersect(
        sx,
        sy,
        getComputedStyle(central).left,
        getComputedStyle(central).top,
        size,
        central.offsetWidth
    )
}

function randomize_x_y(min_x, min_y, max_x, max_y) {
    return {
        x: Math.floor(Math.random() * (max_x - min_x)) + min_x + 'px',
        y: Math.floor(Math.random() * (max_y - min_y)) + min_y + 'px'
    }
}

function set_pos_to_randomized(square, min_x, min_y, max_x, max_y) {
    var coords = randomize_x_y(min_x, min_y, max_x, max_y);
    square.style.left = coords.x;
    square.style.top = coords.y;
}

function randomize_region(x, y, width, height, index, squares) {
    width -= squares[0].offsetWidth;
    height -= squares[0].offsetHeight;

    start = index * number_of_squares_per_quarter;
    end = start + number_of_squares_per_quarter;

    for (var i = start; i < end; i++) {
        set_pos_to_randomized(squares[i], x, y, x + width, y + height);
        
        while (collides_with_others(i, squares)) {
            set_pos_to_randomized(squares[i], x, y, x + width, y + height);
        }
    }
}

function randomize_squares(squares) {
    const board_width = document.getElementById('board').offsetWidth;
    const board_height = document.getElementById('board').offsetHeight;

    const region_width = board_width / 2;
    const region_height = board_height / 2;

    var index = 0;

    for (var x = 0; x <= 1; x++) {
        for (var y = 0; y <= 1; y++) {
            randomize_region(
                x * region_width,
                y * region_height,
                region_width,
                region_height,
                index,
                squares
            );
            index++;
        }
    }
}

function target_onhover(target) {
    if (returned) {
        if (target.classList.contains('visited')) {
            time += 1;
        }
        else {
            target.classList.add('visited');
            returned = false;
        }
    }
}

function generate_squares(n) {
    var board = document.getElementById('board');
    for (var i = 0; i < 4*n; i++) {
        board.innerHTML += '<div class="target" onmouseover="target_onhover(this);"></div>';
    }
    var squares = document.getElementsByClassName('target');
    randomize_squares(squares);
    set_on_hover(squares);
    return squares;
}

function reset_squares() {
    targets = [];
    var board = document.getElementById('board');
    board.innerHTML = '<div id="central"></div>';
}

var targets = generate_squares(number_of_squares_per_quarter);

document.getElementById('slider').onchange = () => {
    time = 0;
    var slider = document.getElementById('slider');
    var text = document.getElementById('slider_val');

    text.innerText = slider.value * 4;

    reset_squares();
    number_of_squares_per_quarter = parseInt(slider.value, 10);
    targets = generate_squares(number_of_squares_per_quarter);
}

function all_targets_visited(targets) {
    for (var i = 0; i < targets.length; i++) {
        if (!targets[i].classList.contains('visited')) {
            return false;
        }
    }
    return true;
}

var timer_on = false;
var time = 0; // czas w 0.01 sekundy
var returned = false;

function set_all_to_unvisited(targets) {
    for (var i = 0; i < targets.length; i++) {
        targets[i].classList.remove('visited');
    }
}

function reset_game(targets) {
    time = 0;
    set_all_to_unvisited(targets);
}

function end_game() {
    scores = document.getElementById('scores_table');
    scores.innerHTML += '<li>#' + 4 * number_of_squares_per_quarter + ': ' 
                               + time + 's'
                      + '</li>';
}

function set_on_hover(targets) {
    var central = document.getElementById('central');

    central.onmouseover = () => {
        timer_on = false;
        if (all_targets_visited(targets)) {
            end_game();
            reset_game(targets);
        }
        returned = true;
    }

    central.onmouseout = () => {
        timer_on = true;
    }
}

function update_timer() {
    setTimeout( () => {
        if (timer_on) {
            time++;
        }
        timer = document.getElementById('time');
        timer.innerText = time + 's';
        update_timer();
    },
    1000);

}

setTimeout(update_timer, 1000);
