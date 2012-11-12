#include <v8.h>
#include <string.h>

using namespace v8;

static_assert(sizeof(Handle<Value>) == sizeof(void*),
  "We treat Handle<Value> "
  "as a pointer, but your compiler thinks it has a different size.  You can "
  "request support for your platform by opening a ticket at "
  "https://github.com/sol/v8/issues."
  );

static_assert(sizeof(Local<String>) == sizeof(void*),
  "We treat Local<String>"
  "as a pointer, but your compiler thinks it has a different size.  You can "
  "request support for your platform by opening a ticket at "
  "https://github.com/sol/v8/issues."
  );

static_assert(sizeof(Persistent<Context>) == sizeof(void*),
  "We treat Persistent<Context>"
  "as a pointer, but your compiler thinks it has a different size.  You can "
  "request support for your platform by opening a ticket at "
  "https://github.com/sol/v8/issues."
  );

extern "C" {

Handle<Value> mkUndefined() {
  return Undefined();
}

Handle<Value> argumentsGet(int i, const Arguments& args) {
  return args[i];
}

typedef void (*ActionCallback)(void);

void c_withHandleScope(ActionCallback action) {
  HandleScope scope;
  action();
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

Context::Scope* enterContext(Handle<Context> context) {
  return new Context::Scope(context);
}

void leaveContext(Context::Scope* scope) {
  delete scope;
}

Local<Value> c_runScript(const char* input) {
  HandleScope scope;
  Local<Value> value = Script::Compile(String::New(input))->Run();
  return scope.Close(value);
}

Local<String> valueToString(Handle<Value> value) {
  return value->ToString();
}

int stringUtf8Length(Handle<String> str) {
  return str->Utf8Length();
}

void stringUtf8Value(Handle<String> str, char* dst) {
  String::Utf8Value src(str);
  memcpy(dst, *src, src.length());
}
}
