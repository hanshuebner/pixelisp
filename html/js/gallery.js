function updateImageList(container) {
    var images = $(container)
        .find('.gf-thumbnail')
        .toArray()
        .map(function (img) {
            return $(img).attr('data-image-name');
        });
    console.log('images:', images);
    $.post('/gallery/playlist', JSON.stringify(images), null, 'json');
}

function removeAnimationFromSequence(event, ui) {
    console.log('removeAnimationFromSequence', this, event, ui);
    event.preventDefault();

    $(this).parent().parent().remove();
    updateImageList($('.user-image-container'));
}

function addImage(event, ui) {
    if (ui.draggable.hasClass('available')) {
        ui.draggable.clone()
            .detach()
            .removeClass('available')
            .removeClass('draggable')
            .appendTo(this)
            .find('.delete')
            .on('click', removeAnimationFromSequence);
        updateImageList(this);
    }
}

function deleteAnimation(event, ui) {
    console.log('deleteAnimation', this, event, ui);
    event.preventDefault();
    var imageContainer = $(this).parent().parent();
    var imageName = imageContainer.find('.gf-thumbnail').attr('data-image-name');
    bootbox.confirm({
        title: 'Confirm Deletion',
        message: 'Do you want to delete animation ' + imageName + ' from the Game Frame?',
        buttons: {
            confirm: {
                label: 'Yes',
                className: 'btn-success'
            },
            cancel: {
                label: 'No',
                className: 'btn-danger'
            }
        },
        callback: function (result) {
            if (result) {
                console.log('deleteAnimation', imageContainer);
                $.post('/delete-gif?name=' + imageName)
                    .done(function () {
                        imageContainer.remove();
                    });
            }
        }
    });
}

$(document).ready(function () {
    $('.image-preview img').on('click',
                               function () {
                                   $.get('/load-gif?name=' + $(this).attr('data-image-name'));
                               });
    $('.available').draggable({
        containment: '.pixelisp',
        cursor: 'move',
        helper: 'clone',
        revert: 'invalid'
    });
    $('.available .delete')
        .on('click', deleteAnimation);
    $('.user-image-container')
        .droppable({
            accept: '.image',
            hoverClass: 'hovering',
            drop: addImage
        })
        .sortable()
        .on('sortstop', function (event, ui) {
            updateImageList(this);
        });
    $('.user-image-container .delete')
        .on('click', removeAnimationFromSequence);
    $('.file-input')
        .on('change', function (event, ui) {
            $('#upload-form')[0].submit();
        });
    if (document.querySelector('.popup-message')) {
        $(document).idle({
            onIdle: function () {
                $('.popup-message').fadeOut(400, function () { $('.popup-message').remove() });
            },
            idle: 3000
        });
    }

    /* settings related */
    var oldChillFactor;
    $('#chill-factor')
        .bootstrapSlider({ reversed: true })
        .on('slide', function (event) {
            if (event.value != oldChillFactor) {
                $.post('/gallery/chill?factor=' + event.value);
                oldChillFactor = event.value;
            }
        });
});
