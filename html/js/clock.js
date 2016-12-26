$(document).ready(function () {
    $('#clock-styles input').on('change',
                                function () {
                                    $.post('/clock/style?style=' + $(this).val());
                                });
});
