import {readFile} from '../lib/file';
import {sum} from '../lib/util';

enum RPS {
  ROCK = 1,
  PAPER = 2,
  SCISSORS = 3,
}

const SELF = new Map<string, RPS>([
  ['X', RPS.ROCK],
  ['Y', RPS.PAPER],
  ['Z', RPS.SCISSORS],
]);

const OPPONENT = new Map<string, RPS>([
  ['A', RPS.ROCK],
  ['B', RPS.PAPER],
  ['C', RPS.SCISSORS],
]);

const LOSE = new Map<RPS, RPS>([
  [RPS.ROCK, RPS.SCISSORS],
  [RPS.PAPER, RPS.ROCK],
  [RPS.SCISSORS, RPS.PAPER],
]);

const WIN = new Map<RPS, RPS>([
  [RPS.SCISSORS, RPS.ROCK],
  [RPS.ROCK, RPS.PAPER],
  [RPS.PAPER, RPS.SCISSORS],
]);

function makeChoices(
  rock: string,
  paper: string,
  scissors: string
): Map<string, RPS> {
  const choices = new Map<string, RPS>();
  choices.set(rock, RPS.ROCK);
  choices.set(paper, RPS.PAPER);
  choices.set(scissors, RPS.SCISSORS);
  return choices;
}

function score(hand: string) {
  if (!hand) return 0;
  const [o, s] = hand.split(' ');
  const oppchoice = OPPONENT.get(o);
  const selfChoice = SELF.get(s);
  if (!oppchoice || !selfChoice) throw new Error(`Unknown choices: ${o}, ${s}`);
  if (LOSE.get(oppchoice) === selfChoice) return 0 + selfChoice;
  if (WIN.get(oppchoice) === selfChoice) return 6 + selfChoice;
  return 3 + selfChoice;
}

function pick(hand: string) {
  if (!hand) return 0;
  const [o, s] = hand.split(' ');
  const oppchoice = OPPONENT.get(o);
  if (!oppchoice) throw new Error(`Unknown choices: ${o}`);
  switch (s) {
    case 'X': {
      return 0 + (LOSE.get(oppchoice) ?? 0);
    }
    case 'Y': {
      return oppchoice + 3;
    }
    case 'Z':
      return 6 + (WIN.get(oppchoice) ?? 0);
    default:
      return 0;
  }
}

async function main1() {
  const input = await readFile('day2/input2.txt');
  const scores = input.split('\n').map(l => score(l));
  console.log(sum(scores));
}

async function main2() {
  const input = await readFile('day2/input2.txt');
  const self = makeChoices('X', 'Y', 'Z');
  const scores = input.split('\n').map(l => pick(l));
  console.log(sum(scores));
}

main1();
main2();
