import { describe, expect, test } from "bun:test";
import { ECMAScriptSyntacticGrammar } from "./ECMAScript";
import { Scanner } from "./Scanner";

describe("SyntacticGrammar", () => {
	const scanner = new Scanner(ECMAScriptSyntacticGrammar);

	test.each([
		["Script", "test"],
		["Script", "_CLASS"],
		["Script", "obj"],
		["Script", "'a b c'"],
		["Script", "1+2*3"],
		["Script", "/ab/u"],
		[
			"Module",
			`
let counter = 0;
export function generateId() {
    return counter++;
}
`,
		],
	])("[%s] %s", (type, input) => {
		const token = scanner.scan(input, type);
		expect(token?.type).toBe(type);
		expect(token?.begin).toBe(0);
		expect(token?.end).toBe(input.length);
	});
});

// TODO
// セミコロンの自動補完
//	もうこれ以上パースできないとき、次の条件を満たすならセミコロンを挿入する
//		- [x] 行末(next === LineTerminator) // OK
//		- [x] 次のトークンが "}"
//		- [ ] do-while文の末尾のセミコロン
//		- [x] 入力されたソースコード全体の末尾 // OK
//		- [x] EmptyStatementを生み出してはならない // OK
//		- [ ] for文のヘッダー内ならNG
//
// rustで再実装
// TypeScript対応
// トランスパイル
//   新しい文法を足してみたい
//		データクラス / if式
