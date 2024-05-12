# Parser

- ECMAScriptのParserを自作してみたい
- 好きな言語機能拡張を加えたい
  - dataclass
    ```js
    // class definition
    data class Point {
      x: number;
      y: number;
      norm() {
        return Math.sqrt(this.x ** 2 + this.y ** 2);
      }
    }
    
    // constructor
    const p1 = new Point(3, 4);
    const p2 = new Point(3, 4);
    
    // clone (with partial modification)
    p.clone({ x: 1 });

    // equality
    p1.equal(p2);
    ```
  - if-else expression / try-catch expression / throw expression
  - for expression
  - Eff