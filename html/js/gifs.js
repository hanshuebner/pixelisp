$(document).ready(function () {
    $('.image-preview img').on('click',
                               function () {
                                   $.get('/load-gif?name=' + $(this).attr('data-image-name'));
                               });
});
