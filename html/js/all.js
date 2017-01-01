
$(document).ready(function () {
    $('.power-icon').on('click', function () { $.post('/power?switch=toggle') });
    $.get('/settings', function (settings) {
        document.settings = settings;
        $(document).trigger('settings', settings);
    });
});
