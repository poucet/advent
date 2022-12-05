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
var RPS;
(function (RPS) {
    RPS[RPS["ROCK"] = 1] = "ROCK";
    RPS[RPS["PAPER"] = 2] = "PAPER";
    RPS[RPS["SCISSORS"] = 3] = "SCISSORS";
})(RPS || (RPS = {}));
const OPPONENT = new Map([
    ['A', RPS.ROCK],
    ['B', RPS.PAPER],
    ['C', RPS.SCISSORS],
]);
const LOSE = new Map([
    [RPS.ROCK, RPS.SCISSORS],
    [RPS.PAPER, RPS.ROCK],
    [RPS.SCISSORS, RPS.PAPER],
]);
const WIN = new Map([
    [RPS.SCISSORS, RPS.ROCK],
    [RPS.ROCK, RPS.PAPER],
    [RPS.PAPER, RPS.SCISSORS],
]);
function sum(numbers) {
    if (numbers.length === 0)
        return 0;
    if (numbers.length === 1)
        return numbers[0];
    return numbers.reduce((p, c) => p + c);
}
function makeChoices(rock, paper, scissors) {
    const choices = new Map();
    choices.set(rock, RPS.ROCK);
    choices.set(paper, RPS.PAPER);
    choices.set(scissors, RPS.SCISSORS);
    return choices;
}
function score(hand) {
    var _a, _b;
    if (!hand)
        return 0;
    let [o, s] = hand.split(' ');
    const oppchoice = OPPONENT.get(o);
    if (!oppchoice)
        return 0;
    switch (s) {
        case 'X': {
            return 0 + ((_a = LOSE.get(oppchoice)) !== null && _a !== void 0 ? _a : 0);
        }
        case 'Y': {
            return oppchoice + 3;
        }
        case 'Z':
            return 6 + ((_b = WIN.get(oppchoice)) !== null && _b !== void 0 ? _b : 0);
        default:
            return 0;
    }
}
function main() {
    return __awaiter(this, void 0, void 0, function* () {
        const input = yield readFile('day2/input.txt');
        const self = makeChoices('X', 'Y', 'Z');
        const scores = input.split('\n').map(l => score(l));
        console.log(sum(scores));
    });
}
main();
