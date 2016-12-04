$(document).ready(function () {
    $('#chill-factor')
        .slider({ reversed: true });
    $('#chill-factor')
        .on('slide', function (event) {
            $.post('/chill?factor=' + event.value);
        });

    $('#brightness')
        .slider();
    $('#brightness')
        .on('slide', function (event) {
            $.post('/brightness?level=' + event.value);
        });
});
