$version: "2"

namespace smithytranscoder

use smithy4s.meta#typeclass

@trait
@typeclass(targetType: "cats.Hash", interpreter: "smithy4s.interopcats.SchemaVisitorHash")
structure hash {}

structure InputData {
    @required
    modelText: String

    @required
    format: Format

    @required
    input: String
}

@hash
union Format {
    json: JsonFormat
    protobuf: Unit
    xml: Unit
    simpleRestJson: SimpleRestJsonFormat
}

@mixin
structure HasFieldFilter {
    @required
    fieldFilter: FieldFilter
}

structure JsonFormat with [HasFieldFilter] {}

structure SimpleRestJsonFormat with [HasFieldFilter] {}

@trait(selector: "enum > member")
string renderName

@traitValidators({
    "forceRenderNames.AllMembersMustHaveRenderName": { severity: "ERROR", message: "All members of a shape with @forceRenderNames must have a @renderName", selector: "> member:not([trait|smithytranscoder#renderName])" }
})
@trait(selector: "enum")
structure forceRenderNames {}

@forceRenderNames
enum FieldFilter {
    @renderName("Default")
    DEFAULT

    @renderName("EncodeAll")
    ENCODE_ALL

    @renderName("SkipUnsetOptions")
    SKIP_UNSET_OPTIONS

    @renderName("SkipEmptyOptionalCollection")
    SKIP_EMPTY_OPTIONAL_COLLECTION

    @renderName("SkipNonRequiredDefaultValues")
    SKIP_NON_REQUIRED_DEFAULT_VALUES
}
