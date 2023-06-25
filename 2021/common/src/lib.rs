use std::str::FromStr;
use std::fmt::Debug;

pub fn parse_lines<T>(contents: &str) -> Vec<T> 
where
    T: FromStr,
    <T as FromStr>::Err : Debug
{
    contents
        .lines()
        .take_while(|p| !p.is_empty())
        .map(|x| x.parse().unwrap())
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;
}
