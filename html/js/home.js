
var pixels = new Array(256);
for (var i = 0; i < 256; i++) {
    pixels[i] = document.getElementById('pixel-' + i);
}

function displayFrame(e)
{
    var data = e.data;
    for (i = 0; i < 256; i++) {
        base = 8 + i * 8 + 2;
        var color = '#' + data.substring(base + 4, base + 6) + data.substring(base + 2, base + 4) + data.substring(base, base + 2);
        pixels[i].style.backgroundColor = color;
    }
}

$(document).ready(function () {
    var evtSource = new EventSource('/events');
    evtSource.addEventListener('frame', displayFrame);
    evtSource.onmessage = function (event) { console.log('untyped event received: ', event); };
});

