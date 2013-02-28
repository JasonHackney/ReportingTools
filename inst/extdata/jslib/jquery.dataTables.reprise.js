function getFilterClass(i, el)
{
    if($(el).hasClass("filter-num"))  {
	return({type: "number-range"});
    } else if($(el).hasClass("filter-cat")) {
	var table = $(this).parents(".dataTable");
	var unique_values = [];
	// unfortuantely $.unique() works only on DOM elements
	table.find("td:nth-child(" + (i+1) + ")").each(function() {
		var value = $(this).text();
		if ($.inArray(value, unique_values) < 0)
		    unique_values.push(value);
	    }).get();
	return({type: "select",
		    values: unique_values});
    } else if($(el).hasClass("filter-date")) {
	return({type: "date-range"});
    } else if($(el).hasClass("filter-string")) {
	return({type: "text"});
    } else {
	return([null]);
    }  
}

//split this out into a function so that shiny can call it when it needs it.
function configureTable(i, el) {
        var filterClasses = $(this).find("th").map(getFilterClass).get();
        
        $(this).dataTable({
            "oLanguage": {
                "sSearch": "Search all columns:"
            },
            "aLengthMenu": [[10, 50, 100, -1], [10, 50, 100, "All"]],
            "iDisplayLength": 10,
            "bStateSave": true,
            "sPaginationType": "full_numbers",
            "aaSorting":[[0,'asc']],
            "aoColumnDefs": [
                { "bSortable": false, "aTargets": [ "sort-off" ] },
                { "sType" : "scientific", "aTargets": [ "sort-num" ] },
                { "sType" : "string", "aTargets": [ "sort-string" ] },
                { "sType" : "html", "aTargets": [ "sort-html" ] },
                { "sType" : "num-html", "aTargets": [ "sort-html-num" ] },
                { "sType" : "date", "aTargets": [ "sort-date" ] }
            ]
        }).columnFilter({sPlaceHolder: "head:before",
                         aoColumns : filterClasses
                        });
} 

$(document).ready(function() {
    $(".dataTable").each(configureTable);
});

