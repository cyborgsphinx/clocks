function createWedge(start, end) {
    return `M100,100 L${start.x},${start.y} A100,100 0 0,1 ${end.x},${end.y} Z`
}

function calculateWedge(wedgeNumber, wedges, radius, origin) {
    var TAU = Math.PI * 2;
    return {
        x: origin.x + radius * Math.cos(wedgeNumber * TAU / wedges),
        y: origin.y + radius * Math.sin(wedgeNumber * TAU / wedges)
    };
}

function createClock(current, total) {
    var ns = "http://www.w3.org/2000/svg";
    var svg = document.getElementById("canvas");
    svg.innerHTML = "";
    var start = {
        x: 100,
        y: 0
    };
    for (var i = 0; i < total; i++) {
        var end = calculateWedge(i, total, 100, {x: 100, y:100});
        var path = document.createElementNS(ns, "path");
        path.setAttribute("d", createWedge(start, end));
        if (i < current) {
            path.setAttribute("fill", "red");
        } else {
            path.setAttribute("fill", "white");
        }
        path.setAttribute("stroke", "black")
        svg.appendChild(path);
        start = end;
    }
}

function setup() {
    var current = document.getElementById("current");
    var maximum = document.getElementById("maximum");
    createClock(current.value, maximum.value);
}