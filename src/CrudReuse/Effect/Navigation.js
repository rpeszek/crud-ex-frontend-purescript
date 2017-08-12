"use strict";

exports.navigateTo = function(hashStr) {
    return function() {
        window.location.hash = hashStr  //example '#/things/list'
    };
};
