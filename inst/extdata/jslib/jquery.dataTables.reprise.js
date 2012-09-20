$(document).ready(function() {
        var filterClasses = $("#example th").map(function(i, el) {
                if($(el).hasClass("filter-num"))  {
                    return({type: "number-range"});
                } else {
                    return([null]);
                }                              
            }).get();

        var oTable = $('#example').dataTable({
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
        { "sType" : "num-html", "aTargets": [ "sort-html-num" ] }
                                 ]
            }).columnFilter({sPlaceHolder: "head:before",
                             aoColumns : filterClasses
                })             
            });
