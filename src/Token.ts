export interface Token {
	type: string;
	text: string;
	children: Token[];
	begin: number;
	end: number;
}
