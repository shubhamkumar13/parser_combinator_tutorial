import Lake
open Lake DSL

package «parser-combinator-tutorial» where
  -- add package configuration options here

lean_lib «ParserCombinatorTutorial» where
  -- add library configuration options here

@[default_target]
lean_exe «parser-combinator-tutorial» where
  root := `Main
