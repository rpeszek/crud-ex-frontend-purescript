"use strict";

exports.getAppConfigItem = function(key) {
      return function(defVal) {
         return function() {
           if(window.appConfig && (window.appConfig[key] || window.appConfig[key] === "") && typeof window.appConfig[key] === "string") {
               return window.appConfig[key];
           } else {
               console.log("using default for " + key + " check window.appConfig");
               return defVal;
           }
         }; 
      };
};
