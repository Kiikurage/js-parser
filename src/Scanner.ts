import type { Grammar } from "./Grammar";
import { optimize } from "./GrammarOptimizer";
import type { Token } from "./Token";

export class Scanner {
	grammars: Map<string, Grammar>;
	currentGoals: string[] = [];
	constructor(grammars: Grammar[]) {
		this.grammars = new Map();
		for (const grammar of optimize(grammars)) {
			this.grammars.set(grammar.left, grammar);
		}
	}

	scan(input: string, goal: string, _offset = 0): Token | null {
		this.currentGoals.push(goal);
		const grammar = this.grammars.get(goal);
		if (grammar === undefined) throw new Error(`Unknown goal: ${goal}`);
		const tokens: Token[] = [];

		for (const pattern of grammar.right) {
			let offset = _offset;
			let matched = true;
			const children: Token[] = [];

			for (const matcher of pattern.matchers) {
				if (goal !== "WhiteSpace" && goal !== "LineTerminator") {
					offset = this.skipWhiteSpaceOrLineTerminator(input, offset);
				}

				if (offset > input.length) {
					matched = false;
					break;
				}

				const token = matcher.match(this, input, offset);

				if (token === null) {
					if (!matcher.optional) {
						matched = false;
						break;
					}
				} else {
					offset = token.end;
					children.push(token);
				}
			}
			if (!matched) continue;
			tokens.push({
				type: goal,
				text: input.substring(_offset, offset),
				children,
				begin: _offset,
				end: offset,
			});
		}

		if (tokens.length === 0) {
			this.currentGoals.pop();
			return null;
		}

		let longestToken = tokens[0];
		for (const token of tokens) {
			if (token.end > longestToken.end) {
				longestToken = token;
			}
		}

		this.currentGoals.pop();
		return longestToken;
	}

	private skipTokens(input: string, token: string, _offset: number): number {
		let offset = _offset;
		while (offset < input.length) {
			const t = this.scan(input, token, offset);
			if (t !== null) {
				offset = t.end;
				continue;
			}
			break;
		}
		return offset;
	}

	private skipWhiteSpaceOrLineTerminator(
		input: string,
		_offset: number,
	): number {
		let offset = _offset;
		while (offset < input.length) {
			const currentOffset = offset;
			offset = this.skipTokens(input, "WhiteSpace", offset);
			offset = this.skipTokens(input, "LineTerminator", offset);
			if (offset === currentOffset) break;
		}
		return offset;
	}
}
