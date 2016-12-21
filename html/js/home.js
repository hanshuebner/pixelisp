
var pixels = new Array(256);
for (var i = 0; i < 256; i++) {
    pixels[i] = document.getElementById('pixel-' + i);
}

function displayFrame(e)
{
    var data = e.data;
    for (i = 0; i < 256; i++) {
        // The first 4 bytes (8 hex digits) are the start bytes, then
        // each pixel is 8 digits long.  The first two digits for each
        // pixel are the synchronization bits + brightness.  Data is
        // coming as BGR, so we need to reverse the order to yield a
        // CSS color.
        base = 8 + i * 8 + 2;
        var color = '#' + data.substring(base + 4, base + 6) + data.substring(base + 2, base + 4) + data.substring(base, base + 2);
        pixels[i].style.backgroundColor = color;
    }
}

function resize() {
    var ledSize = Math.max(15, Math.min(25, ($(window).width() - 100) / 16));
    $('#frame').css('width', (ledSize * 16 + 16) + 'px');
    $('#frame div').css('height', ledSize + 'px');
    $('#frame .pixel').css('width', ledSize + 'px');
}

$(document).ready(function () {
    var evtSource = new EventSource('/events');
    evtSource.addEventListener('frame', displayFrame);
    evtSource.onmessage = function (event) { console.log('untyped event received: ', event); };
    $(window).resize(resize);
    resize();
});

