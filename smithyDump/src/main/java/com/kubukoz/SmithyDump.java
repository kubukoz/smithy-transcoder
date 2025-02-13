package com.kubukoz;

import software.amazon.smithy.model.Model;
import software.amazon.smithy.model.node.Node;
import software.amazon.smithy.model.shapes.ModelSerializer;
import software.amazon.smithy.model.transform.ModelTransformer;

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

}
