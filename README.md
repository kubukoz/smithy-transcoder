# smithy-transcoder

Web app helping explain how a particular Smithy schema and input would get represented using several wire formats.

The application is implemented using Scala.js with [the Calico library](https://www.armanbilge.com/calico),
with a little bit of Java used to interact with the smithy-model JVM library - the Java bits are accessed through [CheerpJ](https://cheerpj.com/).

Further processing of the Smithy model is done with [Smithy4s](https://disneystreaming.github.io/smithy4s/) and its [Dynamic module](https://disneystreaming.github.io/smithy4s/docs/guides/dynamic).

Possible improvements for the future:

- Attempt to discover poor connections so that the JVM doesn't eagerly get loaded on those
- Responsive design for any kind of mobile support
- Encoding/decoding to more formats
- Running CheerpJ in [Web Workers](https://developer.mozilla.org/en-US/docs/Web/API/Web_Workers_API/Using_web_workers)
- Automatically updating the content-length header in HTTP requests
- UI cleanup
- Embedding in Smithy4s documentation
- Code formatting
- Textarea replacement (Codemirror, Monaco, etc.)
