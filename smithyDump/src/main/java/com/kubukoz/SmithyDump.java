package com.kubukoz;

import software.amazon.smithy.model.Model;
import software.amazon.smithy.model.node.Node;
import software.amazon.smithy.model.shapes.ModelSerializer;
import software.amazon.smithy.model.transform.ModelTransformer;
import software.amazon.smithy.model.loader.IdlTokenizer;
import software.amazon.smithy.syntax.Formatter;
import software.amazon.smithy.syntax.TokenTree;

public class SmithyDump {

  public static String dump(String input) {
    Model model = Model
      .assembler()
      .addUnparsedModel("input.smithy", input)
      .discoverModels()
      .assemble()
      .unwrap();

    return Node.printJson(
      ModelSerializer
        .builder()
        .build()
        .serialize(
          ModelTransformer.create().flattenAndRemoveMixins(model)
        )
    );
  }

  public static String format(String input) {
    IdlTokenizer tokenizer = IdlTokenizer.create("input.smithy", input);
    TokenTree tree = TokenTree.of(tokenizer);

    return Formatter.format(tree);
  }

}
