import {readFile} from '../lib/file';

function isAllDiff(input: string): boolean {
  return input.length == new Set(input.split('')).size;
}

async function main1() {
  const data = await readFile('day6/input.txt');
  console.log(data);
  for (let i = 0; i < data.length - 3; ++i) {
    if (isAllDiff(data.slice(i, i + 4))) {
      console.log(i + 4);
      break;
    }
  }
}


async function main2() {
  const data = await readFile('day6/input.txt');
  for (let i = 0; i < data.length - 13; ++i) {
    if (isAllDiff(data.slice(i, i + 14))) {
      console.log(i +14);
      break;
    }
  }
}

main2();
