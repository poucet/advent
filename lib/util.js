"use strict";
Object.defineProperty(exports, "__esModule", { value: true });
exports.isEmpty = exports.not = void 0;
function not(f) {
    return (input) => !f(input);
}
exports.not = not;
function isEmpty(i) {
    return i.length == 0;
}
exports.isEmpty = isEmpty;
