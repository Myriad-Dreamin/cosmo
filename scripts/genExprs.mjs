import { readFileSync, writeFileSync } from "fs";

const exprs = readFileSync(
  "packages/cosmo/src/main/scala/cosmo/ir/Exprs.scala",
  "utf8"
);

const template = /\/\/ region: Exprs([\s\S]+?)\/\/ endregion: Exprs/g.exec(exprs);

const inp = readFileSync(
  "packages/cosmo/src/main/scala/cosmo/Item.scala",
  "utf8"
);
const out = inp.replace(
  /\/\/ region: Exprs([\s\S]+?)\/\/ endregion: Exprs/gm,
 `
// region: Exprs
object untyp {
  type T = Expr;
  type E = Expr;
  final case class ItemE(item: Term) extends Expr {}
  ${template[1]}
}
object typed {
  type T = Term;
  type E = Term;
  ${template[1]}
}
// endregion: Exprs
`
);

writeFileSync("packages/cosmo/src/main/scala/cosmo/Item.scala", out, "utf8");
