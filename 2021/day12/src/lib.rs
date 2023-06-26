
use std::collections::HashMap;
use std::collections::HashSet;
use std::hash::Hash;
use std::rc::Rc;

pub struct Graph {
  edges: HashMap<usize, HashSet<usize>>
}

impl Graph {
  pub fn new() -> Self {
    Graph {
      edges: HashMap::new()
    }
  }

  pub fn add_edge(&mut self, a: usize, b: usize) {
    self.edges.entry(a).or_insert(HashSet::new()).insert(b);
  }

  pub fn num_nodes(&self) -> usize {
    self.edges.len()
  }

  pub fn num_edges(&self) -> usize {
    self.edges.values().map(|l| l.len()).sum()
  }
  pub fn neighbors(&self, a: &usize) -> HashSet<usize> {
    self.edges.get(a).unwrap_or(&HashSet::new()).clone()
  }
}

pub struct InternTable<T: Eq + PartialEq> {
  nodes: HashMap<Rc<T>, usize>,
  rnodes: Vec<Rc<T>>,
}

impl<T: Eq + PartialEq + Hash> InternTable<T> {
  pub fn new() -> Self {
    InternTable { nodes:  HashMap::new(), rnodes: Vec::new() }
  }

  pub fn intern(&mut self, a: T) -> usize {
    match self.nodes.get(&a) {
      Some(v) => *v,
      None => {
        let v = Rc::new(a);
        let k = *self.nodes.entry(v.clone()).or_insert(self.rnodes.len());
        self.rnodes.push(v);
        k
      } 
    }
  }

  pub fn get(&self, a: &T) -> Option<usize> {
    self.nodes.get(a).copied()
  }

  
  pub fn rget(&self, a: usize) -> Option<&T> {
    match self.rnodes.get(a) {
      Some(x) => Some(&*x),
      None    => None
    }
  }
}

pub struct LabeledGraph<T: Eq + PartialEq> {
  graph: Graph,
  intern: InternTable<T>
}


impl<T: Eq + PartialEq + Hash> LabeledGraph<T> {
  pub fn new() -> Self {
    LabeledGraph {
      graph: Graph::new(),
      intern: InternTable::new()
    }
  }

  pub fn add_edge(&mut self, a: T, b: T) {
    let a = self.intern.intern(a);
    let b = self.intern.intern(b);
    self.graph.add_edge(a, b)
  }

  pub fn num_nodes(&self) -> usize {
    self.graph.num_nodes()
  }

  pub fn num_edges(&self) -> usize {
    self.graph.num_edges()
  }

  pub fn neighbors(&self, a: &T) -> Vec<&T> {
    let mut result = Vec::new();
    match self.intern.get(a) {
      Some(k) => {
        let nbs = self.graph.neighbors(&k);
        for n in nbs {
          result.push(self.intern.rget(n).unwrap())
        }
        result
      }
      None => result
    }
  }
}

pub fn parse_input(input: &str) -> LabeledGraph<String> {
  let mut graph = LabeledGraph::new();
  for l in input.lines() {
    let (a, b) = l.split_once("-").unwrap();
    graph.add_edge(a.to_string(), b.to_string());
    graph.add_edge(b.to_string(), a.to_string());
  }
  graph
}

fn is_small_cave(a: &str) -> bool {
  a.chars().next().unwrap().is_lowercase()
}

pub fn exercise1(graph: &LabeledGraph<String>) -> usize {
  let start = graph.intern.get(&"start".to_string()).unwrap();
  let mut stack = Vec::new();
  let mut paths = 0;
  stack.push((start, HashSet::new()));
  while let Some((n, mut visited)) = stack.pop() {
    match graph.intern.rget(n) {
      Some(k) => {
        if k == "end" {
          // No need to explore further.
          paths += 1;
        } else {
          // Only store visited information for small caves.
          if is_small_cave(k) {
            visited.insert(n);
          }
          for m in graph.graph.neighbors(&n) {
            if !visited.contains(&m) {
              stack.push((m, visited.clone()))
            }
          }
        }
      }
      None => unreachable!()
    }
  }
  paths
}

pub fn exercise2(graph: &LabeledGraph<String>) -> usize {
  let start = graph.intern.get(&"start".to_string()).unwrap();
  let mut stack = Vec::new();
  let mut paths = 0;
  stack.push((start, HashSet::new(), false));
  while let Some((n, mut visited, doubled)) = stack.pop() {
    match graph.intern.rget(n) {
      Some(k) => {
        if k == "end" {
          // No need to explore further.
          paths += 1;
        } else {
          // Only store visited information for small caves.
          if is_small_cave(k) {
            visited.insert(n);
          }
          for m in graph.graph.neighbors(&n) {
            match graph.intern.rget(m) {
              Some(l) => {
                if l == "start" {
                  continue
                }
                if !visited.contains(&m) {
                  stack.push((m, visited.clone(), doubled))
                } else if !doubled {
                  stack.push((m, visited.clone(), true))
                }
              },
              None => unreachable!()              
            }

          }
        }
      }
      None => unreachable!()
    }
  }
  paths
}

#[cfg(test)]
mod tests {
  use super::*;

  #[test]
  fn it_parses() {
    let contents = include_str!("../test.txt");
    let graph = parse_input(contents);
    assert_eq!(6, graph.num_nodes());
    assert_eq!(7 * 2, graph.num_edges());
    assert_eq!(2, graph.neighbors(&"start".to_string()).len());
    assert_eq!(4, graph.neighbors(&"A".to_string()).len());
  }

  #[test]
  fn it_passes_exercise1() {
    let contents = include_str!("../test.txt");
    let graph = parse_input(contents);
    assert_eq!(10, exercise1(&graph))
  }

  #[test]
  fn it_passes_exercise2() {
    let contents = include_str!("../test.txt");
    let graph = parse_input(contents);
    assert_eq!(36, exercise2(&graph))
  }

  #[test]
  fn it_is_small_cave() {
    assert!(is_small_cave("ab"));
    assert!(!is_small_cave("AB"));
  }
}