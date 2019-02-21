var canvas = document.getElementById("canv");

var height = canvas.height;
var width = canvas.width;

var context = canvas.getContext("2d");

function draw_pixel(x, y) {
    context.fillRect(x, y, 1, 1);
}

function generate_triangle(n) {
    points = [
        [0, height],
        [width, height],
        [width/2, 0]
    ]
    
    d = points[0];

    for (var i = 0; i < n; i++) {
        draw_pixel(d[0], d[1]);
        point_index = Math.floor(Math.random() * 3);
        selected = points[point_index];
        d = [(d[0] + selected[0]) / 2, (d[1] + selected[1]) / 2]
    }
}

generate_triangle(1000000);
