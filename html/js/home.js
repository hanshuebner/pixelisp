
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

function brightnessToAlpha(level)
{
    return (7 - level) * (0.5 / 7);
}

function setBrightness(level)
{
    $.rule('#frame .mask')[0].style.background = 'rgba(0, 0, 0, ' + brightnessToAlpha(level) + ')';
}

function resize() {
    var ledSize = Math.max(15, Math.min(25, ($(window).width() - 100) / 16));
    $('#frame').css('width', (ledSize * 16 + 16) + 'px');
    $('#frame .pixel').css('height', ledSize + 'px');
    $('#frame .pixel').css('width', ledSize + 'px');
}

$(document).ready(function () {
    var evtSource = new EventSource('/events');
    evtSource.addEventListener('frame', displayFrame);
    evtSource.addEventListener('brightness', function(e) { setBrightness(e.data); });
    evtSource.onmessage = function (event) { console.log('untyped event received: ', event); };
    $(window).resize(resize);
    resize();

    $('#script-form select').on('change', function () {
        console.log('select script', $(this).val());
        $.post('/script?name=' + $(this).val());
    });

    var oldBrightness;

    $('#brightness')
        .bootstrapSlider()
        .on('slide', function (event) {
            if (event.value != oldBrightness) {
                $.post('/display/brightness?level=' + event.value);
                oldBrightness = event.value;
            }
        });
    $(document).on('settings', function (e, settings) {
        setBrightness(settings.display.brightness);
    });
});

