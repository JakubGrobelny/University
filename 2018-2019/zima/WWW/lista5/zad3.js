var selected_color = "black";

function on_drag(event) {
    let target = event.currentTarget;
    let style = getComputedStyle(target);
    selected_color = style.backgroundColor;
}

function allow_drop(event) {
    event.preventDefault();
}

function on_drop(event) {
    event.preventDefault();
    event.target.style.backgroundColor = selected_color;
}