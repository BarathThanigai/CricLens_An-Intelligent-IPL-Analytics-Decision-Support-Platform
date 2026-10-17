// CricLens UI helpers (presentation only).
(function () {
  // Enter submits the existing Ask CricLens module input; it does not change server logic.
  $(document).on('keydown', 'input[id$="-user_query"]', function (e) {
    if (e.key === 'Enter') {
      e.preventDefault();
      Shiny.setInputValue(this.id, this.value, { priority: 'event' });
      var submit = document.getElementById(this.id.replace(/user_query$/, 'submit_btn'));
      if (submit) submit.click();
    }
  });
})();
