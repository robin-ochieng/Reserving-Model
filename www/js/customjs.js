// Add this script to handle drag and drop functionality
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

  $(document).ready(function() {
    // Enable drag and drop for file inputs
    $('.custom-file-wrapper .input-group').each(function() {
      var $dropZone = $(this);
      var $fileInput = $dropZone.find('input[type=file]');
      
      // Prevent default drag behaviors
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
      
      // Handle dropped files
      $dropZone.on('drop', function(e) {
        e.preventDefault();
        e.stopPropagation();
        $(this).removeClass('drag-over');
        
        var files = e.originalEvent.dataTransfer.files;
        if (files.length > 0) {
          // Trigger file input change
          $fileInput[0].files = files;
          $fileInput.trigger('change');
        }
      });
    });
  });

