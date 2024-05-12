use std::collections::{HashMap, HashSet};

/*
 未解決の疑問
 - strは構造体ではないのか? なぜ&strのように参照渡しするのか?
 */

struct Entry {
    current: String,
    nexts: Vec<String>,
}

fn main() {
    let mut edges = HashMap::new();

    edges.insert("A".to_string(), vec!["B".to_string(), "E".to_string()]);
    edges.insert("B".to_string(), vec!["C".to_string(), "D".to_string()]);
    edges.insert("C".to_string(), vec!["A".to_string()]);
    edges.insert("D".to_string(), vec![]);
    edges.insert("E".to_string(), vec!["B".to_string()]);

    println!("edges = {edges:?}");

    let mut completed = HashSet::new();

    for (start, nexts) in edges.iter() {
        if completed.contains(start) { continue }

        let mut visited = HashSet::new();
        let mut stack = vec![Entry {
            current: start.clone(),
            nexts: nexts.clone(), // TODO: 非効率
        }];

        while let Some(mut entry) = stack.pop() {
            if !visited.contains(&entry.current) {
                println!("Enter {}", &entry.current);

                visited.insert(entry.current.clone());
                // print!("Enter {entry.current}"); // TODO: なぜだめ?
            }

            if let Some(next) = entry.nexts.pop() {
                let current = entry.current.clone(); // TODO: 非効率
                stack.push(entry);

                if visited.contains(&next) {
                    println!("Loop {} -> {next}", current);
                } else {
                    // 次の子ノードへ
                    stack.push(Entry {
                        nexts: edges.get(&next).unwrap().clone(), // TODO: 非効率
                        current: next,
                    });
                }
            } else {
                println!("Leave {}", &entry.current);

                completed.insert(entry.current.clone());
                visited.remove(&entry.current);
            }
        }
    }
}
