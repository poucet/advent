import {readFile} from '../lib/file';
import {sum} from '../lib/util';

enum EntryType {
  File = 1,
  Directory = 2,
}

interface Entry {
  entryType: EntryType;
  name: string;
  size: number;
}

class File implements Entry {
  readonly entryType = EntryType.File;
  constructor(readonly name: string, readonly size: number) {}
}

class Directory implements Entry, Iterable<Directory> {
  readonly entryType = EntryType.Directory;
  private contents: Map<string, Entry> = new Map<string, Entry>();
  constructor(readonly name: string, readonly parent: Directory | null) {}

  get fullname(): string {
    if (this.parent) {
      return `${this.parent.fullname}${this.name}/`;
    } else {
      return `${this.name}/`;
    }
  }

  cd(input: string): Directory | null {
    if (input == '..') return this.parent;
    const entry = this.contents.get(input);
    if (!entry || entry.entryType != EntryType.Directory) return null;
    return entry as Directory;
  }

  get size(): number {
    let s = 0;
    for (const e of this.contents.values()) {
      s += e.size;
    }
    return s;
  }

  addEntry(entry: Entry) {
    this.contents.set(entry.name, entry);
  }

  public *[Symbol.iterator](): Iterator<Directory> {
    yield this;
    for (const e of this.contents.values()) {
      if (e.entryType == EntryType.Directory) {
        for (const d of e as Directory) {
          yield d;
        }
      }
    }
  }
}

function parseEntry(pwd: Directory, input: string) {
  const parts = input.split(' ');
  let entry: Entry;
  if (parts[0] == 'dir') {
    entry = new Directory(parts[1], pwd);
  } else {
    entry = new File(parts[1], Number(parts[0]));
  }
  pwd.addEntry(entry);
  return entry;
}

function parseInput(input: string, pwd: Directory) {
  const root = pwd;
  const lines = input.split('\n');
  for (const line of lines) {
    if (line.startsWith('$')) {
      const cmd = line.slice(2);
      if (cmd === 'ls') continue;
      if (cmd.startsWith('cd')) {
        const dir = cmd.slice(3);
        if (dir == '/') {
          pwd = root;
        } else {
          const t = pwd.cd(dir);
          if (!t) {
            throw new Error(
              `Could not find directory ${dir} inside of ${pwd.fullname}`
            );
          }
          pwd = t;
        }
      } else {
        throw new Error(`Unknown command: ${cmd}`);
      }
    } else {
      parseEntry(pwd, line);
    }
  }
}

async function main() {
  const input = await readFile('day7/input.txt');
  const root = new Directory('', null);
  parseInput(input, root);
  const leftSpace = 70000000 - root.size;
  const neededSpace = 30000000 - leftSpace;
  console.log('Left Space', leftSpace, 'neededSpace', neededSpace);
  let s = 0;
  let m = root.size;
  for (const dir of root) {
    if (dir.size <= 100000) {
      s += dir.size;
    }
    if (dir.size > neededSpace) {

      m = Math.min(dir.size, m);
    }
  }
  console.log('Total', s);
  console.log('Freeing up', m);
}

main();
