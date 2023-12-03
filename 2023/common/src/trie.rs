use std::collections::HashMap;
use std::hash::Hash;

pub enum Trie<K, V> {
  Node(Option<V>, HashMap<K, Box<Trie<K, V>>>),
}

impl<K, V> Trie<K, V> {
  pub fn new() -> Self {
    Trie::Node(None, HashMap::new())
  }
}
impl<K, V> Trie<K, V>
where
  K: Eq + Hash,
{
  pub fn get<I>(&self, k: I) -> Option<&V>
  where
    I: IntoIterator<Item = K>,
  {
    let i = k.into_iter();
    let mut n = self;
    for x in i {
      match n.recurse(&x) {
        None => return None,
        Some(m) => {
          n = m;
        }
      }
    }
    n.inner_ref().as_ref()
  }

  pub fn insert<I>(&mut self, k: I, v: V) -> Option<V>
  where
    I: IntoIterator<Item = K>,
  {
    let i = k.into_iter();
    let mut n = self;
    for x in i {
      n = n.recurse_mut(x);
    }
    n.inner_mut_ref().replace(v)
  }

  pub fn recurse<'a>(&'a self, k: &K) -> Option<&'a Trie<K, V>> {
    match self {
      Trie::Node(_, m) => match m.get(&k) {
        None => None,
        Some(b) => Some(b.as_ref()),
      },
    }
  }

  fn recurse_mut(&mut self, k: K) -> &mut Trie<K, V> {
    match self {
      Trie::Node(_, m) => match m.entry(k) {
        std::collections::hash_map::Entry::Occupied(o) => o.into_mut(),
        std::collections::hash_map::Entry::Vacant(v) => v.insert(Box::new(Trie::new())),
      },
    }
  }

  pub fn inner_ref(&self) -> &Option<V> {
    match self {
      Trie::Node(o, _) => o,
    }
  }

  fn inner_mut_ref(&mut self) -> &mut Option<V> {
    match self {
      Trie::Node(o, _) => o,
    }
  }
}
