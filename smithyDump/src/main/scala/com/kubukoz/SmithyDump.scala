package com.kubukoz

import software.amazon.smithy.model.Model
import software.amazon.smithy.model.node.Node
import software.amazon.smithy.model.shapes.ModelSerializer
import software.amazon.smithy.model.transform.ModelTransformer

object SmithyDump {

  def dump(input: String): String = {
    val model = Model
      .assembler()
      .addUnparsedModel("input.smithy", input)
      .assemble()
      .unwrap();

    Node.printJson(
      ModelSerializer
        .builder()
        .build()
        .serialize(
          ModelTransformer.create().flattenAndRemoveMixins(model)
        )
    )
  }

}
