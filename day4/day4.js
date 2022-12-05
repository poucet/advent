"use strict";
var __awaiter = (this && this.__awaiter) || function (thisArg, _arguments, P, generator) {
    function adopt(value) { return value instanceof P ? value : new P(function (resolve) { resolve(value); }); }
    return new (P || (P = Promise))(function (resolve, reject) {
        function fulfilled(value) { try { step(generator.next(value)); } catch (e) { reject(e); } }
        function rejected(value) { try { step(generator["throw"](value)); } catch (e) { reject(e); } }
        function step(result) { result.done ? resolve(result.value) : adopt(result.value).then(fulfilled, rejected); }
        step((generator = generator.apply(thisArg, _arguments || [])).next());
    });
};
Object.defineProperty(exports, "__esModule", { value: true });
const file_1 = require("../lib/file");
const util_1 = require("../lib/util");
class Interval {
    constructor(start, end) {
        this.start = start;
        this.end = end;
    }
    static parseFrom(input) {
        const [a, b] = input.split('-');
        return new Interval(Number(a), Number(b));
    }
    contains(other) {
        return other.start >= this.start && other.end <= this.end;
    }
    overlaps(other) {
        return other.end >= this.start && this.end >= other.start;
    }
    toString() {
        return `${this.start}-${this.end}`;
    }
}
class Pair {
    constructor(a, b) {
        this.a = a;
        this.b = b;
    }
    static parseFrom(input, parse) {
        const [a, b] = input.split(',');
        return new Pair(parse(a), parse(b));
    }
    toString() {
        return `${this.a},${this.b}`;
    }
}
function parse(line) {
    return Pair.parseFrom(line, Interval.parseFrom);
}
function main() {
    return __awaiter(this, void 0, void 0, function* () {
        const fileData = yield (0, file_1.readFile)('day4/input.txt');
        const data = fileData.split('\n').filter((0, util_1.not)(util_1.isEmpty)).map(parse);
        const containing = data.filter(x => x.a.contains(x.b) || x.b.contains(x.a));
        const overlapping = data.filter(x => x.a.overlaps(x.b));
        console.log(data.length);
        console.log(containing.length);
        console.log(overlapping.length);
    });
}
main();
