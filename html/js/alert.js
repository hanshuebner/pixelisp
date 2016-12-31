$(document).ready(function () {

    function currentParams() {
        return $.param({
            message: $('#message').val(),
            loop: $('#forever').is(':checked') ? 'forever' : $('#loop').val(),
            color: $('#color').val()
        });
    }

    function refreshCurl() {
        $('#url').html(currentParams());
    }

    $('#color').autocomplete({
        source: "/color-names"
    });
    $('form').submit(function (e) { e.preventDefault(); });
    $('#set').on('click', function (event, ui) {
        $.post('/alert/set?' + currentParams());
    });
    $('#cancel').on('click', function (event, ui) {
        $.post('/alert/cancel');
    });
    $('#abort').on('click', function (event, ui) {
        $.post('/alert/cancel?abort=1');
    });
    $('input')
        .on('change', refreshCurl)
        .on('keyup', refreshCurl)
        .on('select', refreshCurl);
    refreshCurl();
});
