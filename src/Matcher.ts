import { Parameter } from "./Grammar";
import type { Scanner } from "./Scanner";
import type { Token } from "./Token";

export type MatcherLike = Matcher | RegExp | string;

export namespace MatcherLike {
	export function toMatcher(matcher: MatcherLike): Matcher {
		if (matcher instanceof Matcher) return matcher;
		if (typeof matcher === "string") {
			if (
				(matcher.startsWith('"') && matcher.endsWith('"')) ||
				(matcher.startsWith("'") && matcher.endsWith("'"))
			) {
				return new TextMatcher(matcher.slice(1, -1), false, []);
			}

			let type = matcher;
			let optional = false;
			if (type.endsWith("opt")) {
				type = type.slice(0, -3);
				optional = true;
			}

			let parameters: Parameter[] = [];
			const parameterPart = type.match(/\[[^\]]+]$/);
			if (parameterPart !== null) {
				parameters = Parameter.parse(parameterPart[0]);
				type = type.slice(0, -parameterPart[0].length);
			}
			return new TokenMatcher(type, optional, parameters);
		}
		if (matcher instanceof RegExp) {
			return new RegExpMatcher(matcher, false, []);
		}

		throw new Error(`Invalid matcher: ${matcher}`);
	}
}

export abstract class Matcher {
	protected constructor(
		readonly name: string,
		readonly optional: boolean,
		readonly parameters: Parameter[],
	) {}

	abstract match(scanner: Scanner, input: string, offset: number): Token | null;

	andNot(matcher: MatcherLike): Matcher {
		return new AndNotMatcher(this, matcher, this.optional, this.parameters);
	}

	abstract get tokenType(): string[];
}

class AndNotMatcher extends Matcher {
	readonly matcher: Matcher;
	readonly notMatcher: Matcher;

	constructor(
		matcher: MatcherLike,
		notMatcher: MatcherLike,
		optional: boolean,
		parameters: Parameter[],
	) {
		const _matcher = MatcherLike.toMatcher(matcher);
		const _notMatcher = MatcherLike.toMatcher(notMatcher);
		super(_matcher.name, optional, parameters);

		this.matcher = _matcher;
		this.notMatcher = _notMatcher;
	}

	match(scanner: Scanner, input: string, offset: number): Token | null {
		const result = this.matcher.match(scanner, input, offset);
		if (result === null) return null;

		if (this.notMatcher.match(scanner, result.text, 0) !== null) return null;

		return result;
	}

	get tokenType(): string[] {
		return [...this.matcher.tokenType, ...this.notMatcher.tokenType];
	}
}

class TextMatcher extends Matcher {
	constructor(
		readonly text: string,
		optional: boolean,
		parameters: Parameter[],
	) {
		super(`TextMatcher(${text})`, optional, parameters);
	}

	match(scanner: Scanner, input: string, offset: number): Token | null {
		if (input.startsWith(this.text, offset)) {
			return {
				type: this.name,
				text: this.text,
				begin: offset,
				end: offset + this.text.length,
				children: [],
			};
		}

		return this.insertSemicolon(scanner, input, offset);
	}

	private insertSemicolon(
		scanner: Scanner,
		input: string,
		offset: number,
	): Token | null {
		if (this.text !== ";") return null;

		const INSERTED_SEMICOLON: Token = {
			type: this.name,
			text: "",
			begin: offset,
			end: offset,
			children: [],
		};

		const currentGoal = scanner.currentGoals[scanner.currentGoals.length - 1];
		// Override: Must not produce new EmptyStatement
		if (currentGoal === "EmptyStatement") return null;

		// Override: Must not insert semicolon in the header of for statement
		if (currentGoal === "ForStatement") return null;

		// 1-1. When following token is LineTerminator
		const lineTerminator = scanner.scan(input, "LineTerminator", offset);
		if (lineTerminator !== null) return INSERTED_SEMICOLON;

		// 1-2. When following token is "{"
		if (input.startsWith("}", offset)) return INSERTED_SEMICOLON;

		// TODO: 1-3 end of do-while loop

		// 2. When the position is EOF
		if (offset === input.length) return INSERTED_SEMICOLON;

		return null;
	}

	get tokenType(): string[] {
		return [];
	}
}

class TokenMatcher extends Matcher {
	constructor(
		readonly type: string,
		optional: boolean,
		parameters: Parameter[],
	) {
		super(type, optional, parameters);
	}

	match(scanner: Scanner, input: string, offset: number): Token | null {
		return scanner.scan(input, this.type, offset);
	}

	get tokenType(): string[] {
		return [this.type];
	}
}

class RegExpMatcher extends Matcher {
	readonly regexp: RegExp;

	constructor(regexp: RegExp, optional: boolean, parameters: Parameter[]) {
		super(`RegExp(${regexp.source})`, optional, parameters);

		let pattern = regexp.source;
		if (!pattern.startsWith("^")) pattern = `^${pattern}`;

		this.regexp = new RegExp(pattern, regexp.flags);
	}

	match(scanner: Scanner, input: string, offset: number): Token | null {
		const ma = input.substring(offset).match(this.regexp);
		if (ma === null) return null;

		return {
			type: this.name,
			text: ma[0],
			begin: offset,
			end: offset + ma[0].length,
			children: [],
		};
	}

	get tokenType(): string[] {
		return [];
	}
}

class AnyMatcher extends Matcher {
	constructor(
		readonly length: number,
		optional: boolean,
		parameters: Parameter[],
	) {
		super("Any", optional, parameters);
	}

	match(scanner: Scanner, input: string, offset: number): Token | null {
		if (offset + this.length > input.length) return null;

		return {
			type: this.name,
			text: input.substring(offset, offset + this.length),
			begin: offset,
			end: offset + this.length,
			children: [],
		};
	}

	get tokenType(): string[] {
		return [];
	}
}

class LookAheadIsNotMatcher extends Matcher {
	readonly matcher: Matcher;

	constructor(
		matcher: MatcherLike,
		optional: boolean,
		parameters: Parameter[],
	) {
		const _matcher = MatcherLike.toMatcher(matcher);
		super(_matcher.name, optional, parameters);
		this.matcher = _matcher;
	}

	match(scanner: Scanner, input: string, offset: number): Token | null {
		return this.matcher.match(scanner, input, offset) === null
			? { type: this.name, text: "", begin: offset, end: offset, children: [] }
			: null;
	}

	get tokenType(): string[] {
		return this.matcher.tokenType;
	}
}

class OneOfMatcher extends Matcher {
	readonly matchers: Matcher[];

	constructor(
		matcherLikes: MatcherLike[],
		optional: boolean,
		parameters: Parameter[],
	) {
		const matchers = matcherLikes.map(MatcherLike.toMatcher);
		super(
			`OneOf(${matchers.map((m) => m.name).join(",")})`,
			optional,
			parameters,
		);
		this.matchers = matchers;
	}

	match(scanner: Scanner, input: string, offset: number): Token | null {
		for (const matcher of this.matchers) {
			const result = matcher.match(scanner, input, offset);
			if (result !== null) return result;
		}

		return null;
	}

	get tokenType(): string[] {
		return this.matchers.flatMap((m) => m.tokenType);
	}
}

export function token(type: string): Matcher {
	return new TokenMatcher(type, false, []);
}

export function text(text: string): Matcher {
	return new TextMatcher(text, false, []);
}

export function any(length: number): Matcher {
	return new AnyMatcher(length, false, []);
}

export function nextIsNot(matcher: MatcherLike): Matcher {
	return new LookAheadIsNotMatcher(matcher, false, []);
}

export function oneOf(...matchers: MatcherLike[]): Matcher {
	return new OneOfMatcher(matchers, false, []);
}
