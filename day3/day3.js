"use strict";
var __createBinding = (this && this.__createBinding) || (Object.create ? (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    var desc = Object.getOwnPropertyDescriptor(m, k);
    if (!desc || ("get" in desc ? !m.__esModule : desc.writable || desc.configurable)) {
      desc = { enumerable: true, get: function() { return m[k]; } };
    }
    Object.defineProperty(o, k2, desc);
}) : (function(o, m, k, k2) {
    if (k2 === undefined) k2 = k;
    o[k2] = m[k];
}));
var __setModuleDefault = (this && this.__setModuleDefault) || (Object.create ? (function(o, v) {
    Object.defineProperty(o, "default", { enumerable: true, value: v });
}) : function(o, v) {
    o["default"] = v;
});
var __importStar = (this && this.__importStar) || function (mod) {
    if (mod && mod.__esModule) return mod;
    var result = {};
    if (mod != null) for (var k in mod) if (k !== "default" && Object.prototype.hasOwnProperty.call(mod, k)) __createBinding(result, mod, k);
    __setModuleDefault(result, mod);
    return result;
};
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
const fs = __importStar(require("fs"));
function readFile(path) {
    return new Promise((resolve, reject) => {
        fs.readFile(path, 'utf8', (err, data) => {
            if (err) {
                reject(err);
            }
            else {
                resolve(data);
            }
        });
    });
}
function score(char) {
    const s = char.charCodeAt(0);
    if (s >= 65 && s <= 90) {
        return s - 64 + 26;
    }
    if (s >= 97 && s <= 122) {
        return s - 96;
    }
    return 0;
}
function splitInHalf(s) {
    return [s.slice(0, s.length / 2), s.slice(s.length / 2, s.length)];
}
function common(a, b) {
    return [...new Set(a.split('').filter(c => b.includes(c)))].join('');
}
function sum(numbers) {
    if (numbers.length === 0)
        return 0;
    if (numbers.length === 1)
        return numbers[0];
    return numbers.reduce((p, c) => p + c);
}
function groupBy3(input) {
    const result = [];
    for (let i = 0; i < input.length; ++i) {
        if (i % 3 == 0) {
            result.push([input[i]]);
        }
        else {
            result[result.length - 1].push(input[i]);
        }
    }
    return result;
}
function main() {
    return __awaiter(this, void 0, void 0, function* () {
        const input = yield readFile('day3/input.txt');
        const lines = input.split('\n');
        const chars = groupBy3(lines).map(arr => arr.reduce(common));
        console.log(sum(chars.map(score)));
    });
}
main();
