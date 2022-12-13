import {readFile} from '../lib/file';

function parse(input: Array<string>) {
  console.log(input);
  return [eval(input[0]), eval(input[1])];
}

function compare(left: any, right: any): 1 | 0 | -1 {
  if (typeof left === 'number' && typeof right === 'number') {
    if (left < right) return -1;
    if (left > right) return 1;
    return 0;
  }
  if (typeof left === 'number') {
    left = [left];
  }
  if (typeof right === 'number') {
    right = [right];
  }
  if (left.length === 0 && right.length === 0) return 0;
  if (left.length === 0) return -1;
  if (right.length === 0) return 1;
  for (let i = 0; i < left.length && i < right.length; ++i) {
    const c = compare(left[i], right[i]);
    if (c !== 0) return c;
  }
  if (left.length < right.length) return -1;
  if (left.length > right.length) return 1;
  return 0;
}

async function main1() {
  const input = await readFile('day13/sample.txt');
  const lines = input.split('\n');

  let i = 1;
  let sum = 0;
  while (lines.length > 0) {
    const [l, r] = parse(lines.splice(0, 3));
    if (compare(l, r) < 0) {
      sum += i;
    }
    i += 1;
  }
  console.log(sum);
}

// main1();

async function main2() {
  const input = await readFile('day13/input.txt');
  const lines = input.split('\n');

  const all = [];
  while (lines.length > 0) {
    const [l, r] = parse(lines.splice(0, 3));
    all.push(l);
    all.push(r);
  }

  all.sort(compare);
  const i = all.findIndex(x => compare(x, [[2]]) >= 0);
  all.splice(i, 0, [[2]]);
  const j = all.findIndex(x => compare(x, [[6]]) >= 0);
  all.splice(j, 0, [[6]]);
  console.log((i + 1) * (j + 1));
}

main2();
