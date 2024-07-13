
function onTableLoadFinish(table) {

  //Attach checkboxState to the table
  table.checkboxState = {};

  // Prevent the first column from being selectable
  table.on('user-select', function(e, dt, type, cell, originalEvent) {
   console.log('triggered');
   if (cell.index().column === 2) {
    e.preventDefault();
   }
 });

  table.on('select', function(e, dt, type, indexes) {
      if (type === 'row') {

          // Backup all checkboxes
          storeCheckboxState(table);

          const selectedData = table.rows(indexes).data()[0];
          const selectedToa = selectedData[table.column("toa:name").index()];

          // Iterate through all rows to calculate the time difference
          table.rows().every(function (rowIdx, tableLoop, rowLoop) {
              const rowData = this.data();
              const rowToa = rowData[table.column("toa:name").index()];

              // Calculate the time difference between the selected row and the current row
              const timeDifference = calculateDateTimeDifference(selectedToa, rowToa);

              // Update the timediff column with the calculated time difference
              rowData[table.column("timediff:name").index()] = timeDifference;
              this.data(rowData).invalidate();
          });


          // Redraw the table to reflect the changes
          table.draw(false);

          // Restore all checked boxes
          restoreCheckboxState(table);
      }
  });

  table.on('deselect',  function(e, dt, type, indexes) {

    // Backup all checkboxes
    storeCheckboxState(table);

    // Loop through all rows
    table.rows().every(function (rowIdx, tableLoop, rowLoop) {
       const rowData = this.data();

      // Set the timediff column equal to "-"
      rowData[table.column("timediff:name").index()] = "-";

      this.data(rowData).invalidate();
    });


    // Redraw the table to reflect the changes
    table.draw(false);

    // Restore all checked boxes
    restoreCheckboxState(table);
  })


}

  function calculateDateTimeDifference(dateTime1, dateTime2) {
      // Parse the input date-time strings into Date objects
      const date1 = new Date(dateTime1);
      const date2 = new Date(dateTime2);

      // Check if the dates are valid
      if (isNaN(date1.getTime()) || isNaN(date2.getTime())) {
          throw new Error("Invalid date format");
      }

      const sign = ((date2 - date1) >= 0) ? "+" : "-";
      // Calculate the difference in milliseconds
      const differenceInMillis = Math.abs(date2 - date1);

      // Calculate the difference in seconds
      const differenceInSeconds = Math.floor(differenceInMillis / 1000);

      // Calculate hours, minutes, and seconds
      const hours = Math.floor(differenceInSeconds / 3600);
      const minutes = Math.floor((differenceInSeconds % 3600) / 60);
      const seconds = differenceInSeconds % 60;

      // Format the output based on the values
      if (hours > 0) {
          return `${sign}${hours}h ${minutes}m ${seconds}s`;
      } else if (minutes > 0) {
          return `${sign}${minutes}m ${seconds}s`;
      } else {
          return `${sign}${seconds}s`;
      }
  }

function storeCheckboxState(table) {
  // Clear state of previous checkboxes.
  table.checkboxState = {};

  // Store the state of current checkboxes
  table.rows().every(function () {
      var rowId = this.index();
      var isChecked = $(this.node()).find('input[type="checkbox"]').prop('checked');
      table.checkboxState[rowId] = isChecked;
  });
}

function restoreCheckboxState(table) {
    // Restore the state of checkboxes
    table.rows().every(function () {
        var rowId = this.index();
        var isChecked = table.checkboxState[rowId];
        if (isChecked !== undefined) {
            $(this.node()).find('input[type="checkbox"]').prop('checked', isChecked);
        }
    });
}

