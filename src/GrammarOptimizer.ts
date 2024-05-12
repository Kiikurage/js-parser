import type { Grammar, Pattern } from "./Grammar";
import { token } from "./Matcher";

export function optimize(grammars: Grammar[]): Grammar[] {
	const grammarMap = new Map(
		grammars.map((grammar) => [grammar.left, grammar]),
	);

	// 再帰の検出
	const nextTokenMap = new Map<string, Set<string>>();
	for (const grammar of grammars) {
		const nextTokens = new Set<string>();
		nextTokenMap.set(grammar.left, nextTokens);

		for (const pattern of grammar.right) {
			if (pattern.matchers.length === 0) continue;
			const firstMatcher = pattern.matchers[0];

			for (const nextToken of firstMatcher.tokenType) {
				nextTokens.add(nextToken);
			}
		}
	}
	const recursions = findCycles(nextTokenMap);

	for (const recursion of recursions) {
		for (const token of recursion) {
			const grammar = grammarMap.get(token);
			if (grammar === undefined) continue;

			// 再帰がエスケープ可能かどうかを確認
			let escapable = false;
			for (const pattern of grammar.right) {
				if (pattern.matchers.length === 0) {
					escapable = true;
					break;
				}
				if (!pattern.matchers[0].tokenType.includes(token)) {
					escapable = true;
					break;
				}
			}
			if (!escapable) continue;

			// 間接再帰を直接再帰に変換
			const directLeftRecursionGrammar = convertToDirectLeftRecursion(
				grammar,
				recursion,
				grammarMap,
			);

			// 再帰を削除
			const optimizedGrammars = eliminateDirectLeftRecursion(
				directLeftRecursionGrammar,
			);
			for (const grammar of optimizedGrammars) {
				grammarMap.set(grammar.left, grammar);
			}
		}
	}

	return [...grammarMap.values()];
}

// @see "C. Left-recursion elimination" in
// 		https://www.antlr.org/papers/allstar-techreport.pdf
function eliminateDirectLeftRecursion(grammar: Grammar): Grammar[] {
	const grammars: Grammar[] = [];

	const alphas: Pattern[] = []; // Pattern without left recursion
	const betas: Pattern[] = []; // Pattern with left recursion

	for (const pattern of grammar.right) {
		if (pattern.matchers.length > 0) {
			const firstMatcher = pattern.matchers[0];
			if (firstMatcher.tokenType.includes(grammar.left)) {
				betas.push(pattern);
				continue;
			}
		}

		alphas.push(pattern);
	}

	if (betas.length === 0) {
		grammars.push(grammar);
		return grammars;
	}

	const newLeft = `${grammar.left}'`;
	const newGrammarForAlpha: Grammar = {
		left: grammar.left,
		right: [],
		params: [],
	};
	const newGrammarForBeta: Grammar = { left: newLeft, right: [], params: [] };

	// For each pattern in alphas, generate new rule like
	// 	OriginalDestination ::= (the pattern) OriginalDestination'
	for (const alpha of alphas) {
		const newPattern: Pattern = {
			matchers: [...alpha.matchers, token(newLeft)],
			when: {},
		};
		newGrammarForAlpha.right.push(newPattern);
	}

	// And for each pattern in betas, generate new rule like
	// 	OriginalDestination' ::= (the pattern without recursive part) OriginalDestination'
	for (const beta of betas) {
		const newPattern: Pattern = {
			matchers: [...beta.matchers.slice(1), token(newLeft)],
			when: {},
		};
		newGrammarForBeta.right.push(newPattern);
	}

	// And add empty rule
	newGrammarForBeta.right.push({ matchers: [], when: {} });

	return [
		newGrammarForAlpha,
		...eliminateDirectLeftRecursion(newGrammarForBeta),
	];
}

function convertToDirectLeftRecursion(
	grammar: Grammar,
	recursion: string[],
	grammarMap: Map<string, Grammar>,
): Grammar {
	const newGrammar: Grammar = { left: grammar.left, right: [], params: [] };
	const queue = [...grammar.right];
	while (queue.length > 0) {
		const pattern = queue.shift();
		if (pattern === undefined) break;

		if (pattern.matchers.length === 0) {
			newGrammar.right.push(pattern);
			continue;
		}

		const recursiveTokens = pattern.matchers[0].tokenType.filter((token) =>
			recursion.includes(token),
		);
		if (
			recursiveTokens.length === 0 ||
			(recursiveTokens.length === 1 && recursiveTokens[0] === grammar.left)
		) {
			newGrammar.right.push(pattern);
			continue;
		}

		for (const recursiveToken of recursiveTokens) {
			const nextGrammar = grammarMap.get(recursiveToken);
			if (nextGrammar === undefined) continue;

			for (const nextPattern of nextGrammar.right) {
				const newPattern: Pattern = {
					matchers: [...nextPattern.matchers, ...pattern.matchers.slice(1)],
					when: pattern.when,
				};
				queue.push(newPattern);
			}
		}
	}

	return newGrammar;
}

/**
 * A -> [B, D]
 * B -> C
 * C -> A
 *
 * => [A,B,C]
 */
function findCycles(edges: Map<string, Set<string>>): string[][] {
	const cycles: string[][] = [];
	const completed = new Set<string>();

	for (const [token, next] of edges) {
		if (completed.has(token)) continue;

		const visited = new Set<string>();
		const stack: { token: string; next: string[] }[] = [
			{ token, next: [...next] },
		];

		while (stack.length > 0) {
			const current = stack.pop();
			if (current === undefined) break;
			visited.add(current.token);

			const nextToken = current.next.pop();
			if (nextToken === undefined) {
				visited.delete(current.token);
				completed.add(current.token);
			} else {
				stack.push(current);

				if (visited.has(nextToken)) {
					const cycleLength = stack.findLastIndex((s) => s.token === nextToken);
					const cycle = stack.slice(cycleLength).map((s) => s.token);
					if (cycle.some((c) => !completed.has(c))) {
						cycles.push(cycle);
					}
				} else {
					stack.push({
						token: nextToken,
						next: [...(edges.get(nextToken) ?? [])],
					});
				}
			}
		}
	}

	return cycles;
}
