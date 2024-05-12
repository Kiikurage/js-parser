import { type Matcher, MatcherLike } from "./Matcher";

type SpecLine = (string | MatcherLike)[];

export namespace Parameter {
	export function parse(source: string): Parameter[] {
		if (source.at(0) !== "[" || source.at(-1) !== "]") return [];

		return source
			.slice(1, -1)
			.split(",")
			.map((part) => part.trim())
			.map((part) => {
				let name = part;
				let prefix: ParameterPrefix;
				switch (part.at(0)) {
					case "+":
						prefix = "must";
						name = name.slice(1);
						break;
					case "~":
						prefix = "mustNot";
						name = name.slice(1);
						break;
					case "?":
						prefix = "both";
						name = name.slice(1);
						break;
					default:
						prefix = "none";
						break;
				}

				return { name, prefix };
			});
	}
}
export type ParameterPrefix = "must" | "mustNot" | "both" | "none";

export interface Parameter {
	name: string;
	prefix: ParameterPrefix;
}

export interface Grammar {
	left: string;
	right: Pattern[];
	params: Parameter[];
}

export interface Pattern {
	matchers: Matcher[];
	when: Record<string, boolean>;
}

export function grammars(
	strings: TemplateStringsArray,
	...placeholders: MatcherLike[]
): Grammar[] {
	const lines = splitSpecToLines(strings, placeholders);
	return parseLines(lines);
}

function splitSpecToLines(
	strings: TemplateStringsArray,
	placeholders: MatcherLike[],
): SpecLine[] {
	let lines: SpecLine[] = [[]];

	for (const string of strings) {
		let currentLine = lines.pop();
		if (currentLine === undefined) throw new Error("Invalid spec");

		for (const line of string.split("\n")) {
			let trimmedLine = line.trim();
			while (trimmedLine.length > 0) {
				const ma = trimmedLine.match(
					/(?:("[^" ]*")|([^\[ ]*))?(?:\[[^\]]+])?(?:opt)?/,
				);
				if (ma === null) break;
				if (ma[0].length === 0) break;

				currentLine.push(ma[0]);
				trimmedLine = trimmedLine.substring(ma[0].length).trim();
			}
			lines.push(currentLine);
			currentLine = [];
		}

		const placeholder = placeholders.shift();
		if (placeholder !== undefined) {
			lines[lines.length - 1].push(placeholder);
		}
	}

	lines = lines
		.filter((line) => line.length !== 0)
		.filter(
			(line) => !(typeof line[0] === "string" && line[0].startsWith("//")),
		);

	return lines;
}

function parseLines(lines: SpecLine[]): Grammar[] {
	const grammars: Grammar[] = [];

	let currentGrammar: Grammar | null = null;

	for (const line of lines) {
		if (line[line.length - 1] === "::" || line[line.length - 1] === ":") {
			if (line.length !== 2) throw new Error(`Invalid line: ${line.join(" ")}`);

			const ma = (line[0] as string).match(/^([^\]]+)(\[[^\]]+])?$/);
			if (ma === null) throw new Error(`Invalid line: ${line.join(" ")}`);

			const left = ma[1];
			const paramsPart = ma[2] ?? "";
			const params = Parameter.parse(paramsPart);

			currentGrammar = { left, right: [], params };
			grammars.push(currentGrammar);
		} else {
			if (currentGrammar === null) throw new Error("Invalid line");

			const elements = line.slice();
			const firstToken = elements[0];

			const when: Record<string, boolean> = {};
			if (typeof firstToken === "string" && /^\[[^\]]+]$/.test(firstToken)) {
				elements.shift();
				const parameters = Parameter.parse(firstToken);
				for (const parameter of parameters) {
					switch (parameter.prefix) {
						case "must":
							when[parameter.name] = true;
							break;
						case "mustNot":
							when[parameter.name] = false;
							break;
					}
				}
			}

			const matchers = elements.map(MatcherLike.toMatcher);
			currentGrammar.right.push({ matchers, when });
		}
	}

	return grammars;
}
