function onTableLoadFinish(table) {

  // Prevent the first column from being selectable
  table.on('user-select', function(e, dt, type, cell, originalEvent) {
   console.log('triggered');
   if (cell.index().column === 2) {
    e.preventDefault();
   }
 });
}
