use std::collections::{HashMap, HashSet};

struct Entry<'a> {
    current: &'a str,
    children: &'a Vec<&'a str>,
    next_child_index: usize,
}

fn main() {
    let mut edges = HashMap::new();
    edges.insert("A", vec!["B", "C"]);
    edges.insert("B", vec!["D"]);
    edges.insert("C", vec!["D"]);
    edges.insert("D", vec!["E", "F"]);
    edges.insert("E", vec!["A"]);
    edges.insert("F", vec!["A"]);

    let cycles = find_cycles(&edges);

    for cycle in cycles {
        println!("{:?}", cycle.join("->"));
    }
}

fn find_cycles<'a>(edges: &'a HashMap<&str, Vec<&str>>) -> Vec<Vec<&'a str>> {
    let mut completed = HashSet::new();
    let mut visited = HashSet::new();
    let mut cycles = Vec::new();

    for (&start, nexts) in edges {
        let mut stack = vec![Entry {
            current: start,
            children: nexts,
            next_child_index: 0,
        }];

        while let Some(entry) = stack.last_mut() {
            visited.insert(entry.current);

            if entry.next_child_index < entry.children.len() {
                let next = entry.children[entry.next_child_index];
                entry.next_child_index += 1;

                if visited.contains(next) {
                    let mut cycle = Vec::new();
                    for entry in stack.iter().rev() {
                        cycle.push(entry.current);
                        if entry.current == next { break; }
                    }
                    cycles.push(cycle);
                } else {
                    match edges.get(next) {
                        None => {
                            completed.insert(next);
                        }
                        Some(children) => {
                            stack.push(Entry {
                                current: next,
                                children,
                                next_child_index: 0,
                            })
                        }
                    }
                }
            } else {
                completed.insert(entry.current);
                visited.remove(entry.current);
                stack.pop();
            }
        }
    }

    return cycles;
}