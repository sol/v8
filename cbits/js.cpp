#include <v8.h>

using namespace v8;

static_assert(sizeof(Persistent<Context>) == sizeof(void*),
  "We treat Persistent<Context>"
  "as a pointer, but your compiler thinks it has a different size.  You can "
  "request support for your platform by opening a ticket at "
  "https://github.com/sol/v8/issues."
  );

extern "C" {

Handle<Value> argumentsGet(int i, const Arguments& args) {
  return args[i];
}

Persistent<Context> contextNew(const InvocationCallback jsPrint) {
  HandleScope scope;
  Local<ObjectTemplate> global = ObjectTemplate::New();
  global->Set(String::New("print"), FunctionTemplate::New(jsPrint));
  return Context::New(NULL, global);
}

void contextDispose(Persistent<Context> context) {
  context.Dispose();
}

void contextEnter(Handle<Context> context) {
  context->Enter();
}

void contextExit(Handle<Context> context) {
  context->Exit();
}

void c_contextAddFunction(Handle<Context> context, char* name, const InvocationCallback f) {
  HandleScope scope;
  Local<Function> fun = FunctionTemplate::New(f)->GetFunction();
  context->Global()->Set(String::New(name), fun);
}

Local<Value> c_runScript(const char* input) {
  HandleScope scope;
  Local<Value> value = Script::Compile(String::New(input))->Run();
  return scope.Close(value);
}

}
