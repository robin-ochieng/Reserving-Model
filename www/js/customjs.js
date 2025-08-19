// Drag & Drop helpers for file inputs
$(document).ready(function() {
  // Prevent default drag behaviors
  $(document).on('dragover', function(e) {
    e.preventDefault();
    e.stopPropagation();
  });
  
  $(document).on('drop', function(e) {
    e.preventDefault();
    e.stopPropagation();
  });
  
  // Handle drag and drop for upload areas
  $('.custom-file-upload-area').on('dragover', function(e) {
    e.preventDefault();
    e.stopPropagation();
    $(this).addClass('drag-over');
  });
  
  $('.custom-file-upload-area').on('dragleave', function(e) {
    e.preventDefault();
    e.stopPropagation();
    $(this).removeClass('drag-over');
  });
  
  $('.custom-file-upload-area').on('drop', function(e) {
    e.preventDefault();
    e.stopPropagation();
    $(this).removeClass('drag-over');
    $(this).addClass('file-dropped');
    
    var files = e.originalEvent.dataTransfer.files;
    if (files.length > 0) {
      var fileInput = $(this).find('input[type="file"]')[0];
      fileInput.files = files;
      $(fileInput).trigger('change');
    }
    
    setTimeout(() => {
      $(this).removeClass('file-dropped');
    }, 300);
  });
  
  // Update UI when file is selected
  $('input[type="file"]').on('change', function() {
    var fileName = this.files[0] ? this.files[0].name : 'No file selected';
    $(this).closest('.custom-file-upload-area').find('.ms-Text--small:last').text(fileName);
    
    if (this.files[0]) {
      $(this).closest('.custom-file-upload-area').addClass('has-file');
    } else {
      $(this).closest('.custom-file-upload-area').removeClass('has-file');
    }
  });
});

// Enable drag-and-drop on Shiny's Bootstrap file inputs inside .custom-file-wrapper
$(document).ready(function() {
  $('.custom-file-wrapper .input-group').each(function() {
    var $dropZone = $(this);
    var $fileInput = $dropZone.find('input[type=file]');
    $dropZone.on('dragover dragenter', function(e) {
      e.preventDefault();
      e.stopPropagation();
      $(this).addClass('drag-over');
    });
    $dropZone.on('dragleave dragend', function(e) {
      e.preventDefault();
      e.stopPropagation();
      $(this).removeClass('drag-over');
    });
    $dropZone.on('drop', function(e) {
      e.preventDefault();
      e.stopPropagation();
      $(this).removeClass('drag-over');
      var files = e.originalEvent.dataTransfer.files;
      if (files.length > 0) {
        $fileInput[0].files = files;
        $fileInput.trigger('change');
      }
    });
  });
});

// One-time registration of custom download handler used by Shiny
if (!window.__saccosDownloadHandlerRegistered) {
  if (window.Shiny && typeof Shiny.addCustomMessageHandler === 'function') {
    Shiny.addCustomMessageHandler('downloadFile', function(message) {
      try {
        var link = document.createElement('a');
        link.href = message.dataUri;
        link.download = message.filename || 'download';
        document.body.appendChild(link);
        link.click();
        document.body.removeChild(link);
      } catch (e) {
        console.error('Download failed', e);
      }
    });
  }
  window.__saccosDownloadHandlerRegistered = true;
}

