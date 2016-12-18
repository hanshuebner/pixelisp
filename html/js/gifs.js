function updateImageList(container) {
    var images = $(container)
        .find('.thumbnail')
        .toArray()
        .map(function (img) {
            return $(img).attr('data-image-name');
        });
    console.log('images', images);
}

function removeAnimationFromSequence(event, ui) {
    console.log('removeAnimationFromSequence', this, event, ui);

    $(this).parent().parent().parent().remove();
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
    var imageContainer = $(this).parent().parent().parent();
    var imageName = imageContainer.find('.thumbnail').attr('data-image-name');
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
                imageContainer.remove();
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
        containment: '.game-frame',
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
});
