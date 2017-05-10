//Runs a function when corresponding .json file is loaded
//variable in the slot where "data" is corresponds to json data


//d3.json('MOCK_DATA.json', function (error,data) {

  var data = JSON.parse(sessionStorage.tempDB);

  function tabulate(data, columns) {
		var table = d3.select('body').append('table')
		var thead = table.append('thead')
		var	tbody = table.append('tbody');
    table.attr('id', 'tableBody');

    /*
    //Loading Bar Variables
    var progress = document.getElementById("Progress");
    var width = 1;
    var id = setInterval(frame, 10);
    */

    //thead.append('tr')
    //  .attr('id', 'allSorters');

    // append extra header row to hold filter selection boxes
    thead.append('tr')
      .attr('id', 'allFilters');

		// append the header row
		thead.append('tr')
      .attr('id', 'allHeaders')
		  .selectAll('th')
		  .data(columns).enter()
		  .append('th')
      .attr('class', 'columnHeader') //sets class of all header elements
      .attr('id', function (column) { return column; }) //sets id of header elements
		    .text(function (column) { return column; });


		// create a row for each object in the data
		var rows = tbody.selectAll('tr')
		  .data(data)
		  .enter()
		  .append('tr')
      .attr('class', 'record');

		// create a cell in each row for each column
		var cells = rows.selectAll('td')
		  .data(function (row) {
		    return columns.map(function (column) {
		      return {column: column, value: row[column]};
		    });
		  })
		  .enter()
		  .append('td')
		    .text(function (d) { return d.value; });

    //**Gives each record a class name equal to it's column header name**
    var columnNames = document.getElementsByClassName('columnHeader'); //Array of all column names
    var records = document.getElementsByClassName('record'); //Array of all records (tr's)
    var currentRecord; //Variable to hold record being changed

    for (var iRecord = 0; iRecord < records.length; iRecord++){ //Iterate through each record
      currentRecord = records[iRecord].getElementsByTagName('td'); //Set current record

      for (var iName = 0; iName < columnNames.length; iName++){ //Iterate through each column name
          currentRecord[iName].className = columnNames[iName].id; //Assign class to corresponding record
      }
    }

	  return table;
	}

  var columnNames = ['companyName', 'divisionName', 'clienttypeName', 'usertypeName', 'datausageName', 'regionName', 'qtr&YrCompleted', 'address', 'city', 'state', 'zipcode', 'contactName', 'email', 'phone', 'bloombergRep', 'sORNumber', 'notes'];
	// render the table(s)
	tabulate(data, columnNames);

  var divCheckBox = document.getElementById('columnViewList'); //Div element for column view checkboxes
  //var sorterHeader = document.getElementById('allSorters'); //Tr element for sorter boxes
  var filterHeader = document.getElementById('allFilters'); //Tr element for filter boxes

  // Creates Select All check box
  var selectAllBox = document.createElement('input');
  var selectAllLabel = document.createElement('label');
  selectAllBox.type = "checkbox"
  selectAllBox.id = "selectall";
  selectAllBox.checked = true;
  selectAllBox.setAttribute("onchange", "selectAll()");
  selectAllLabel.innerHTML = "Select All";
  divCheckBox.appendChild(selectAllBox);
  divCheckBox.appendChild(selectAllLabel);
  divCheckBox.appendChild(document.createElement('br'));

  // Creates checkbox for each column name
  for (var i = 0; i < columnNames.length; i++) {
    var newCheckBox = document.createElement('input');
    var newLabel = document.createElement('label');
    newLabel.innerHTML = (columnNames[i]);
    newCheckBox.type = "checkbox";
    newCheckBox.className = "checkBox";
    newCheckBox.id = columnNames[i];
    newCheckBox.checked = true;
    newCheckBox.setAttribute("onchange", "filterTable()");
    divCheckBox.appendChild(newCheckBox);
    divCheckBox.appendChild(newLabel);
    divCheckBox.appendChild(document.createElement('br'));
  }

/*
  //Creates sort buttons
  for (var i = 0; i < columnNames.length; i++) {
    var newCell = document.createElement('th'); //Cell above each column to hold buttons
    newCell.setAttribute("class", columnNames[i]);

    var newAscend = document.createElement('input');
    newAscend.setAttribute("type", "button");
    newAscend.setAttribute("value", "Sort AZ");
    newAscend.setAttribute("onclick", "sort(" + i + ")");
    var newDescend = document.createElement('input');
    newDescend.setAttribute("type", "button");
    newDescend.setAttribute("value", "Sort ZA");

    newCell.append(newAscend);
    newCell.append(newDescend);
    sorterHeader.append(newCell);
  }
*/

  var columns = document.getElementsByClassName("columnHeader");
  var columnWidth;
  var newFilterBox;
  var blankOption;
  var newFilterOptions = [];
  var newFilterOptions_Elements;
  for (var i = 0; i < columns.length; i++) {
    //Creates filterbox (dropdown allowing mulitple selections) using 'Chosen' jquery plugin
    columnWidth = columns[i].offsetWidth;
    newFilterBox = document.createElement('select');
    newFilterBox.setAttribute('data-placeholder', "Select Filters");

    //Creates blank option for default
    blankOption = document.createElement('option');
    blankOption.value = "";
    newFilterBox.appendChild(blankOption);

    //Reterives all records in column
    newFilterOptions = [];
    newFilterOptions_Elements = document.getElementsByClassName(columns[i].id);
    for (iOptions = 0; iOptions < newFilterOptions_Elements.length; iOptions++) {
      newFilterOptions.push(newFilterOptions_Elements[iOptions].innerHTML);
    }
    newFilterOptions= Array.from(new Set(newFilterOptions)); //Shorten list to unique records only
    for(iOptions = 0; iOptions < newFilterOptions.length; iOptions++) { //Append unique records as options to filterbox
      var newOption = document.createElement('option');
      newOption.value = newFilterOptions[iOptions];
      newOption.innerHTML = newFilterOptions[iOptions];
      newFilterBox.appendChild(newOption);
    }

    newFilterBox.setAttribute('multiple', 'true');
    newFilterBox.setAttribute('style', "width:" + columnWidth + "px;");
    newFilterBox.setAttribute('class', "chosen-dropdown");
    newFilterBox.setAttribute('tabindex', "11");
    newFilterBox.setAttribute('onchange', "filterRows()");


    //Appends filter to top of respective column
    var newTh = document.createElement('th');
    newTh.className = columns[i].id;
    allFilters.appendChild(newTh);
    newTh.appendChild(newFilterBox);
    $(".chosen-dropdown").chosen()
  }

//});
