#include <v8.h>

using namespace v8;

static_assert(sizeof(Handle<Value>) == 8,
    "We treat Handle<Value> as a 64-bit value, but your compiler thinks it "
    "has a different size.  You can request support for your platform by "
    "opening a ticket at https://github.com/sol/v8/issues."
    );

extern "C" {

Handle<Value> mkUndefined() {
  return Undefined();
}

void runScript(const InvocationCallback jsPrint, const char* input) {
  // Create a stack-allocated handle scope.
  HandleScope scope;

  // Create a template for the global object and set the built-in global
  // functions.
  Local<ObjectTemplate> global = ObjectTemplate::New();
  global->Set(String::New("print"), FunctionTemplate::New(jsPrint));

  // Each processor gets its own context so different processors do not affect
  // each other.
  Persistent<Context> context = Context::New(NULL, global);

  // Enter the created context for compiling and running the script.
  Context::Scope context_scope(context);

  // Compile the source code.
  Script::Compile(String::New(input))->Run();

  // Dispose the persistent context.
  context.Dispose();
}
}
