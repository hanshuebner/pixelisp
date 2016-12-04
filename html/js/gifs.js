$(document).ready(function () {
    $('.image-preview').on('click',
                           function () {
                               $.get('/load-gif?name=' + $(this).attr('data-image-name'));
                           });
});
