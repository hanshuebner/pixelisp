var evtSource = new EventSource('/events');
evtSource.addEventListener('animation-loaded',
                           function (e) {
                               console.log('image loaded', e.data);
                               $('#current-image').attr('src', '/gif/' + e.data + '.gif');
                               $('#current-image-name').html(e.data);
                           });

evtSource.onmessage = function (event) { console.log('untyped event received: ', event); };

$('.image-preview').on('click',
                       function () {
                           $.get('/load-gif?name=' + $(this).attr('data-image-name'));
                       });

$('#chill-factor').slider({
    formatter: function (value) {
        if (value > 0) {
            $.get('/chill?factor=' + value);
        }
    }});
