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
var glpk = require('glpk.js/dist/glpk.min.js');

/*
exports.pow = function(x) {
  return function(y) {
    return Math.pow(x,y);
  };
};
*/
exports.glp_create_prob = glpk.glp_create_prob;

exports.glp_read_lp_from_string = function(lp, str){
	return glpk.glp_read_lp_from_string(lp, null, str)
}

exports.smcp = function(){
	return new glpk.SMCP({presolve: GLP_ON});
}

exports.glpk_solve_lp = function (str){
            var lp = glpk.glp_create_prob();
            glpk.glp_read_lp_from_string(lp, null, str);
            glpk.glp_scale_prob(lp, GLP_SF_AUTO);
            var smcp = new glpk.SMCP({presolve: GLP_ON});
            glpk.glp_simplex(lp, smcp);
            var iocp = new glpk.IOCP({presolve: GLP_ON});
            glpk.glp_intopt(lp, iocp);
            log("obj: " + glpk.glp_mip_obj_val(lp));
            res = {};
            for(var i = 1; i <= glpk.glp_get_num_cols(lp); i++){
                log(glpk.glp_get_col_name(lp, i)  + " = " + glpk.glp_mip_col_val(lp, i));
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