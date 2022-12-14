import {readFile} from '../lib/file';

function score(char: string) {
  const s = char.charCodeAt(0);
  if (s >= 65 && s <= 90) {
    return s - 64 + 26;
  }
  if (s >= 97 && s <= 122) {
    return s - 96;
  }
  return 0;
}

function splitInHalf(s: string) {
  return [s.slice(0, s.length / 2), s.slice(s.length / 2, s.length)];
}

function common(a: string, b: string): string {
  return [...new Set(a.split('').filter(c => b.includes(c)))].join('');
}

function sum(numbers: Array<number>): number {
  if (numbers.length === 0) return 0;
  if (numbers.length === 1) return numbers[0];
  return numbers.reduce((p, c) => p + c);
}

function groupBy3(input: Array<string>): Array<Array<string>> {
  const result: Array<Array<string>> = [];
  for (let i = 0; i < input.length; ++i) {
    if (i % 3 === 0) {
      result.push([input[i]]);
    } else {
      result[result.length - 1].push(input[i]);
    }
  }
  return result;
}

async function main1() {
  const input = await readFile('day3/input2.txt');
  const lines = input.split('\n');
  const pairs = lines.map(splitInHalf);
  console.log(sum(pairs.map(([x, y]) => score(common(x, y)))));
}

main1();

async function main2() {
  const input = await readFile('day3/input2.txt');
  const lines = input.split('\n');
  const chars = groupBy3(lines).map(arr => arr.reduce(common));
  console.log(sum(chars.map(score)));
}

main2();
