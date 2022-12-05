import {readFile} from '../lib/file';

enum RPS {
  ROCK = 1,
  PAPER = 2,
  SCISSORS = 3,
}

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

function sum(numbers: Array<number>): number {
  if (numbers.length === 0) return 0;
  if (numbers.length === 1) return numbers[0];
  return numbers.reduce((p, c) => p + c);
}

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
  let [o, s] = hand.split(' ');
  const oppchoice = OPPONENT.get(o);
  if (!oppchoice) return 0;
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

async function main() {
  const input = await readFile('day2/input.txt');
  const self = makeChoices('X', 'Y', 'Z');
  const scores = input.split('\n').map(l => score(l));
  console.log(sum(scores));
}

main();
