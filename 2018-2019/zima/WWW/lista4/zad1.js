// a)
content.innerHTML = '<div id="menu"></div>';

// b)
menu.style.border = '5px solid blue';

// c)
menu.innerHTML = '<h1><a href="#" id="red" style="color:red">Red</a><br>\
                  <a href="#" id="black" style="color:black">Black</a><br>\
                  <a href="#" id="brown" style="color:brown">Brown</a></h1>';

function changeBorder(color, element) {
    return () => element.style.border = '5px solid ' + color;
}

red.onclick = changeBorder("red", menu);
black.onclick = changeBorder("black", menu);
brown.onclick = changeBorder("brown", menu);
