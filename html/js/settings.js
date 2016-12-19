$(document).ready(function () {
    $('#chill-factor')
        .bootstrapSlider({ reversed: true });
    $('#chill-factor')
        .on('slide', function (event) {
            $.post('/chill?factor=' + event.value);
        });

    $('#brightness')
        .bootstrapSlider();
    $('#brightness')
        .on('slide', function (event) {
            $.post('/brightness?level=' + event.value);
        });
});
