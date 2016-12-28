$(document).ready(function () {
    $('#clock-styles input').on('change',
                                function () {
                                    $.post('/clock/style?style=' + $(this).val());
                                });
    $('#render-seconds input').on('change',
                                  function () {
                                      $.post('/clock/seconds?render=' + ($(this).is(':checked') ? 1 : 0));
                                  });
    window.setInterval(function () {
        $('#clock-styles img').map(function () {
            $(this).attr('src', $(this).attr('src'))
        });
    }, 1000);
});
