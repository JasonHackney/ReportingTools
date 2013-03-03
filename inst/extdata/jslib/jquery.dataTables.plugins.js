/**
 * @summary     DataTables
 * @description Paginate, search and sort HTML tables
 * @version     1.9.2
 * @file        jquery.dataTables.js
 * @author      Allan Jardine (www.sprymedia.co.uk)
 * @contact     www.sprymedia.co.uk/contact
 *
 * @copyright Copyright 2008-2012 Allan Jardine, all rights reserved.
 *
 * This source file is free software, under either the GPL v2 license or a
 * BSD style license, available at:
 *   http://datatables.net/license_gpl2
 *   http://datatables.net/license_bsd
 * 
 * This source file is distributed in the hope that it will be useful, but 
 * WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY 
 * or FITNESS FOR A PARTICULAR PURPOSE. See the license files for details.
 * 
 * For details please refer to: http://www.datatables.net
 */


jQuery.extend(jQuery.fn.dataTableExt.oSort, {
        "string-robust-pre": function ( a ) {
            return a.replace( /<.*?>/g, "" ).toLowerCase();
        },
            
        "string-robust-asc": function ( x, y ) {            
            return ((x < y) ? -1 : ((x > y) ? 1 : 0));
        },
		
        "string-robust-desc": function ( x, y ) {
            return ((x < y) ? 1 : ((x > y) ? -1 : 0));
        },
     } );


var robustParseNum = function(x){
    var fx = parseFloat(x);
    if(!isNaN(fx)){
        return fx;
    }
    x = x.replace( /<.*?>/g, "" );
    fx = parseFloat(x);
    if(!isNaN(fx)){
        return fx;
    }
    return x;
};

var numStrSort = function(x, y) {
    var fx = robustParseNum(x);
    var fy = robustParseNum(y);
    
    if (isNaN(fx)) {
        if (isNaN(fy)) {
            x = x.replace( /<.*?>/g, "" );
            y = y.replace( /<.*?>/g, "" );
            return ("" + x).localeCompare("" + y);
        }
        return 1;
    }
    if (isNaN(fy)) {
        return -1;
    }
    return ((fx < fy) ? -1 : ((fx > fy) ? 1 : 0));
};

$.extend($.fn.dataTableExt.oSort, {
        "num-robust-asc" :  numStrSort,
            "num-robust-desc" : function(x, y) {
            return numStrSort(y, x);
        }
    });

		


                             
