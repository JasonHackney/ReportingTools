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
jQuery.extend( jQuery.fn.dataTableExt.oSort, {
        "num-html-pre": function ( a ) {
            var x = a.replace( /<.*?>/g, "" );
            return parseFloat( x );
        },
         
        "num-html-asc": function ( a, b ) {
            return ((a < b) ? -1 : ((a > b) ? 1 : 0));
        },
           
        "num-html-desc": function ( a, b ) {
            return ((a < b) ? 1 : ((a > b) ? -1 : 0));
        }
    } );

jQuery.extend( jQuery.fn.dataTableExt.oSort, {
    "scientific-pre": function ( a ) {
        return parseFloat(a);
    },
 
    "scientific-asc": function ( a, b ) {
        return ((a < b) ? -1 : ((a > b) ? 1 : 0));
    },
 
    "scientific-desc": function ( a, b ) {
        return ((a < b) ? 1 : ((a > b) ? -1 : 0));
    }
} );
             
/* type detection of numbers in html string */
jQuery.fn.dataTableExt.aTypes.unshift( function ( sData ) {
        sData = typeof sData.replace == 'function' ?
            sData.replace( /<.*?>/g, "" ) : sData;
        sData = $.trim(sData);
        
        var sValidFirstChars = "0123456789-";
        var sValidChars = "0123456789.";
        var Char;
        var bDecimal = false;
        
        /* Check for a valid first char (no period and allow negatives) */
        Char = sData.charAt(0); 
        if (sValidFirstChars.indexOf(Char) == -1) 
            {
                return null;
            }
        
        
        /* Check all the other characters are valid */
        for ( var i=1 ; i < sData.length ; i++ ) 
            {
                Char = sData.charAt(i); 
                if (sValidChars.indexOf(Char) == -1) 
                    {
                        return null;
                    }
                
                /* Only allowed one decimal place... */
                if ( Char == "." )
                    {
                        if ( bDecimal )
                            {
                                return null;
                            }
                        bDecimal = true;
                    }
            }                                        
        return 'num-html';
    } );       

                             
