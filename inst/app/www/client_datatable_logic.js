const dateLimits = {"from" : null, "to" : null};
var checkboxState = {};
var spectroImages = {};

Shiny.addCustomMessageHandler('showDateTimeSlider', function(e) {
  //Find the datetime slider element and display it.
  $("div#match_calls_1-datetime_slider_container")[0].style.display = "block";
});


Shiny.addCustomMessageHandler('filterTableByDate', function(d) {
  //Query the DOM to find the datatable
  // Check if Datatable has been initialized
  var table = ($.fn.DataTable) ? $("#match_calls_1-unmatched_calls #DataTables_Table_0").DataTable() : null;
  if (table !== null) {
    // Update the date limit values.
    dateLimits.from = d.from;
    dateLimits.to = d.to;
    //Redraw the table 300ms afterwards
    setTimeout(function() {
              table.draw(false);
    }, 300);
  }
});

function refreshTableImages(table) {
  console.log("REFRESHING IMAGES");
  console.log(Object.keys(spectroImages))
  table.column("spectrogram:name").nodes().each(function(cell, i) {
    if (typeof spectroImages[i] !== "undefined") {
      table.cell(cell).data(`<img src=${spectroImages[i]} width="20px">`);
      table.cell(cell).invalidate();
    }
  });
  table.draw(false);
}

function hasTableLoaded() {
  return ($.fn.DataTable) && $.fn.DataTable.isDataTable("#match_calls_1-unmatched_calls #DataTables_Table_0");
}

Shiny.addCustomMessageHandler('updateTableSpectrogramImages', function(arr) {
  for (const imageObj of arr) {
    //Update the state of spectrogram images
    spectroImages[imageObj.rowId] = imageObj.src;
  }
  if(hasTableLoaded()) {
     refreshTableImages($("#match_calls_1-unmatched_calls #DataTables_Table_0").DataTable());
  }
});

function onTableLoadFinish(table) {

  refreshTableImages(table);
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


  //Custom filtering function that checks whether min <= toa <= max
  // This took AGES to get right!
  // I was sending non ISO-8601 compliant date strings from the server
  // and no error was thrown on the client!
  // Also, this function is a relic buried deep inside old datatable forums
  // Source: https://datatables.net/forums/discussion/7207/fn-datatableext-afnfiltering-push-what-is-this
  // Source2: https://github.com/DataTables/Plugins/blob/master/filtering/row-based/range_dates.js
  // DO NOT TOUCH!
   $.fn.dataTableExt.afnFiltering.push(function(settings, data, dataIndex) {
    if (dateLimits === undefined || dateLimits.from === null || dateLimits.to === null) {
      return true;
    }
    var from = new Date(dateLimits.from);
    var to = new Date(dateLimits.to);
    var iterDate = new Date(data[4]); // use data from the toa column
    return ((iterDate >= from) && (iterDate <= to)); //keep row only if it is in range.
  })
/*
$.fn.dataTable.ext.search.push(function (settings, data, dataIndex) {
  if (!sddsds) {
    console.log("data:");
    console.log(data);
    console.log("data4");
    console.log(data[4]);
    sddsds = true;
  }
  return true;
});
*/


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
  // Store the state of current checkboxes
  table.rows().every(function () {
      var rowId = this.index();
      var isChecked = $(this.node()).find('input[type="checkbox"]').prop('checked');
      checkboxState[rowId] = isChecked;
  });
}

function restoreCheckboxState(table) {
    // Restore the state of checkboxes
    table.rows().every(function () {
        var rowId = this.index();
        var isChecked = checkboxState[rowId];
        if (isChecked !== undefined) {
            $(this.node()).find('input[type="checkbox"]').prop('checked', isChecked);
        }
    });
}

