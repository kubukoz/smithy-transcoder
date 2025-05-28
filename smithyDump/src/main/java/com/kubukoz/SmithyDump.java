package com.kubukoz;

import software.amazon.smithy.model.Model;
import software.amazon.smithy.model.loader.ModelAssembler;
import software.amazon.smithy.model.node.Node;
import software.amazon.smithy.model.shapes.ModelSerializer;
import software.amazon.smithy.model.transform.ModelTransformer;
import software.amazon.smithy.model.loader.IdlTokenizer;
import software.amazon.smithy.syntax.Formatter;
import software.amazon.smithy.syntax.TokenTree;
import java.util.Arrays;

public class SmithyDump {

  public static String dump(String[][] inputs) {
    final ModelAssembler assembler = Model.assembler();

    Arrays.stream(inputs).forEach(entry -> {
      assembler.addUnparsedModel(entry[0], entry[1]);
    });

    Model model = assembler
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
