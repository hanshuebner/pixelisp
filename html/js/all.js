
$(document).ready(function () {
    $('.power-icon').on('click', function () { $.post('/power?switch=toggle') });
});
