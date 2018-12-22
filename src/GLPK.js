// src/Math.js

/*
exports.pow = function(x) {
  return function(y) {
    return Math.pow(x,y);
  };
};
*/
// src/Math.js

// var glpk = require("glpk.js");

"use strict";

var glpk = require('../../bower_components/glpk.js/dist/glpk.min.js');
//var glpk = require('glpk');
/*
exports.pow = function(x) {
  return function(y) {
    return Math.pow(x,y);
  };
};
*/
exports.glp_create_prob = glpk.glp_create_prob;


var prog = "\n\n\nMaximize\n obj: + 786433 x_1 + 655361 x_2 + 589825 x_3 + 557057 x_4 + 540673 x_5\n + 532481 x_6 + 528385 x_7 + 526337 x_8 + 525313 x_9 + 524801 x_10\n + 524545 x_11 + 524417 x_12 + 524353 x_13 + 524321 x_14 + 524305 x_15\n\nSubject To\n cap: + 786433 x_1 + 655361 x_2 + 589825 x_3 + 557057 x_4 + 540673 x_5\n + 532481 x_6 + 528385 x_7 + 526337 x_8 + 525313 x_9 + 524801 x_10\n + 524545 x_11 + 524417 x_12 + 524353 x_13 + 524321 x_14 + 524305 x_15\n <= 4194303.5\n\nBounds\n 0 <= x_1 <= 1\n 0 <= x_2 <= 1\n 0 <= x_3 <= 1\n 0 <= x_4 <= 1\n 0 <= x_5 <= 1\n 0 <= x_6 <= 1\n 0 <= x_7 <= 1\n 0 <= x_8 <= 1\n 0 <= x_9 <= 1\n 0 <= x_10 <= 1\n 0 <= x_11 <= 1\n 0 <= x_12 <= 1\n 0 <= x_13 <= 1\n 0 <= x_14 <= 1\n 0 <= x_15 <= 1\n\nGenerals\n x_1\n x_2\n x_3\n x_4\n x_5\n x_6\n x_7\n x_8\n x_9\n x_10\n x_11\n x_12\n x_13\n x_14\n x_15\n\nEnd\n"

var prog = "\n\n\nMaximize\nobj: + 786433 x_1 + 655361 x_2 + 589825 x_3 + 557057 x_4 + 540673 x_5\n + 532481 x_6 + 528385 x_7 + 526337 x_8 + 525313 x_9 + 524801 x_10\n + 524545 x_11 + 524417 x_12 + 524353 x_13 + 524321 x_14 + 524305 x_15\n\nSubject To\n  + 786433 x_1 + 655361 x_2 + 589825 x_3 + 557057 x_4 + 540673 x_5\n + 532481 x_6 + 528385 x_7 + 526337 x_8 + 525313 x_9 + 524801 x_10\n + 524545 x_11 + 524417 x_12 + 524353 x_13 + 524321 x_14 + 524305 x_15 + 0.1\n <= 4194303.5 \nEnd\n"

exports.glp_read_lp_from_string = function(lp, str){
	return glpk.glp_read_lp_from_string(lp, null, str)
}

exports.smcp = function(){
	return new glpk.SMCP({presolve: GLP_ON});
}

exports.glpk_solve_lp = function (str){
	        //console.log("HERE")
            var lp = glpk.glp_create_prob();
            //console.log("HERE");
            //console.log(str);
            //console.log(prog);
            glpk.glp_read_lp_from_string(lp, null, str);
            console.log("HERE")
            glpk.glp_scale_prob(lp, glpk.GLP_SF_AUTO);
            console.log("HERE")
            var smcp = new glpk.SMCP({presolve: glpk.GLP_ON});
            glpk.glp_simplex(lp, smcp);
            var iocp = new glpk.IOCP({presolve: glpk.GLP_ON});
            glpk.glp_intopt(lp, iocp);
            console.log("obj: " + glpk.glp_mip_obj_val(lp));
            //console.log("HERE")
            var res = {};
            for(var i = 1; i <= glpk.glp_get_num_cols(lp); i++){
                //console.log(glpk.glp_get_col_name(lp, i)  + " = " + glpk.glp_mip_col_val(lp, i));
                res[glpk.glp_get_col_name(lp, i)] = glpk.glp_mip_col_val(lp, i);
                
            }
            return {vars : res, obj : glpk.glp_mip_obj_val(lp)}
        }
exports.lookupVar = function(str, res){
	return res[str]
}  






// export.
//var lp = glp_create_prob();
/*
 function run(){
            start = new Date(); 
	    logNode.innerText = "";
            var lp = glp_create_prob();
            glp_read_lp_from_string(lp, null, document.getElementById("source").value);
            glp_scale_prob(lp, GLP_SF_AUTO);
            var smcp = new SMCP({presolve: GLP_ON});
            glp_simplex(lp, smcp);
            var iocp = new IOCP({presolve: GLP_ON});
            glp_intopt(lp, iocp);
            log("obj: " + glp_mip_obj_val(lp));
            for(var i = 1; i <= glp_get_num_cols(lp); i++){
                log(glp_get_col_name(lp, i)  + " = " + glp_mip_col_val(lp, i));
            }
        }
        */